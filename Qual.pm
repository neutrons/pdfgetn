#-----------------------------------------------------------------------------
# PDFqual called from PDFgetN directly
#-----------------------------------------------------------------------------

package Qual;
use strict;

($Qual::debug)=0;

############################## Subroutines ###########################
sub pdfQual{ #########################################################

  my ($cmd,@arg) = @_;
  foreach (split('\s',$cmd)){
    push (@arg,$_);
  }
  my $out;

  &QualInit();
  my $ret=&processArgs(@arg);
  unless ($ret=~/info/ || $ret=~/help/) {
    &calcSQ();
    &calcGR();
    &getPDFFIT();
    $out=&printParam();
  } else {
    $out=&printHelp($ret);
  }

  return $out;
}

sub QualInit{ ##########################################################
  ($Qual::version)=1.1;
  ($Qual::PI)=3.1415927; ($Qual::PISQ)=$Qual::PI*$Qual::PI;
  ($Qual::weight)=2;

  ($Qual::Qmin,$Qual::Qmax,$Qual::Qlow)=(10000,0,0.6);
  ($Qual::sq_avg,$Qual::sq_fac,$Qual::gr_int,$Qual::gr_wgt,$Qual::Rlow,$Qual::r_a,$Qual::r_b,$Qual::Rmax)=(0,0,0,0,2.0,0,0,0);
  ($Qual::sq_lim,$Qual::Qlim)=(0,1.75);
  ($Qual::history)=0;
}

sub calcSQ{ ##########################################################
  my(@Q,@S);
  ##### check that the S(Q) exists
  if(-r "$Qual::filename.sq"){
    (print "Found $Qual::filename.sq ... ")if($Qual::debug);
  }else{
    return 0;
  }

  ##### get the value of Qmax
  &getQmax();
  if($Qual::Qlow){
    if($Qual::Qlow>1){
      # do nothing
    }else{
      $Qual::Qlow=sprintf("%.4f",$Qual::Qlow*$Qual::Qmax);
    }
  }

  ##### read in the history information
  &readHistory("$Qual::filename.sq");
  (print STDOUT "\n")if($Qual::debug);

  ##### skip over the history
  open(FILE,"$Qual::filename.sq") || die "Could not open $Qual::filename.sq: $!";
  $_=<FILE>; if(/history/i){
    while(<FILE>){
      (last)if(/\Qstart data\E/);
    }
    $_=<FILE>;
  }

  ##### skip over information with a hash a the first character
  if(/^\#/){
    while(<FILE>){
      (last)unless(/^\#/);
    }
  }

  ##### read in the data
  my($int)=0;
  chomp();
  s/^\s+//;
  ($Q[$int],$S[$int])=(split /\s+/,$_)[0,1]; $int++;
  while(<FILE>){
    s/^\s+//;
    ($Q[$int],$S[$int])=(split /\s+/,$_)[0,1]; $int++;
  }
  close(FILE) || die "Could not close $Qual::filename.sq: $!";

  ##### calculate sq_avg
  $Qual::sq_avg=&integrate(\@Q,\@S,$Qual::Qlow,$Qual::Qmax)/($Qual::Qmax-$Qual::Qlow);

  ##### calculate sq_lim
  $Qual::sq_lim=&integrate(\@Q,\@S,$Qual::Qmin,$Qual::Qlim)/($Qual::Qlim-$Qual::Qmin);
  $Qual::sq_lim=$Qual::sq_avg-$Qual::sq_lim;

  ##### calculate sq_int
  my(@QsqS);
  for( $int=0 ; $int<@S ; $int++ ){
    $QsqS[$int]=$Q[$int]*$Q[$int]*($S[$int]-1);
  }
  $Qual::sq_fac=&integrate(\@Q,\@QsqS);

} # end of calcSQ

sub calcGR{ ##########################################################
  my(@r,@G);
  ##### check that PDF exists
  if(-r "$Qual::filename.gr"){
    (print "Found $Qual::filename.gr ... ")if($Qual::debug);
  }else{
    return 0;
  }

  ##### read in the history information
  &readHistory("$Qual::filename.gr");
  (print STDOUT "\n")if($Qual::debug);

  ##### skip over the history
  open(FILE,"$Qual::filename.gr") || die "Could not open $Qual::filename.gr: $!";
  $_=<FILE>; if(/history/i){
    while(<FILE>){
      (last)if(/\Qstart data\E/);
    }
    $_=<FILE>;
  }

  ##### skip over information with a hash a the first character
  if(/^\#/){
    while(<FILE>){
      (last)unless(/^\#/);
    }
  }

  ##### read in the data
  my($int)=0;
  chomp();
  s/^\s+//;
  ($r[$int],$G[$int])=(split /\s+/,$_)[0,1]; $int++;
  while(<FILE>){
    s/^\s+//;
    ($r[$int],$G[$int])=(split /\s+/,$_)[0,1];
    ($Qual::Rmax=$r[$int])if($r[$int]>$Qual::Rmax); $int++;
  }
  close(FILE) || die "Could not close $Qual::filename.gr: $!";

  ##### calculate rhofit
  &calcRhoFit(\@r,\@G);

  ##### calculate g_low
  my(@E1,@E2);
  for( $int=0 ; $int<@r ; $int++ ){
#    $E1[$int]=($r[$int]**$Qual::weight)
#      *(($G[$int]+4*$Qual::PI*$Qual::rhofit*$r[$int])**2);
#    $E2[$int]=(4*$Qual::PI*$Qual::rhofit*$r[$int])**2;
    $E1[$int]=abs($r[$int]*$G[$int]+4*$Qual::PI*($r[$int]**2)*$Qual::rhofit);
    $E2[$int]=4*$Qual::PI*$Qual::rhofit*($r[$int]**2);
    $E1[$int]=$E1[$int]*$E1[$int];   # square it
    $E2[$int]=$E2[$int]*$E2[$int];   # square it
    (last)if($r[$int]>=$Qual::Rlow);
  }
  {
      my($temp)=&integrate(\@r,\@E2);
      ($Qual::gr_low=&integrate(\@r,\@E1)/$temp)if($temp);
  }

  ##### caculate gr_int
  my(@rG);
  for( $int=0 ; $int<@r ; $int++ ){
    $rG[$int]=$r[$int]*$G[$int];
  }
  $Qual::gr_int=&integrate(\@r,\@rG);
} # end of calcGR

sub calcRhoFit{
  my($r0,$G0)=@_;
  my($Rtot,$Gtot)=(0,0);
  my(@r,@G,@W);
  for(my $int=0 ; $int<@$r0 ; $int++ ){
    $r[$int]=$$r0[$int];
    $G[$int]=$$G0[$int];
    $W[$int]=$r[$int]**$Qual::weight;
    (last)if($r[$int]>=$Qual::Rlow);
  }

    for( my $int=0 ; $int<@r ; $int++ ){
      $Gtot+=$W[$int]*$G[$int];
      $Rtot+=$W[$int]*$r[$int];
    }
  $Qual::rhofit=-$Gtot/(4*$Qual::PI*$Rtot);
  return $Qual::rhofit;
}

sub getPDFFIT{ #######################################################
  (return)unless($Qual::outfile);
  open(FILE,"$Qual::outfile") || die "Could not open $Qual::outfile: $!";
  while(<FILE>){
    if(/\QLattice\E/){
      @Qual::lattice=(split /\s+/,$_)[4,5,6];
    }elsif(/\Qangles\E/){
      @Qual::angle=(split /\s+/,$_)[4,5,6];
    }elsif(/\QScale factor\E/){
      if(($Qual::scale==1.0)||($Qual::scale==0.0)){
	$Qual::scale=(split /\s+/,$_)[4];
      }
    }elsif(/\Qexpected\E/){
      $Qual::Rexpect=(split /\s+/,$_)[5];
    }elsif(/\Qweighted\E/){
      $Qual::Rweight=(split /\s+/,$_)[3];
    }elsif(/\QIterations\E/){
      $Qual::iterations=(split /\s+/,$_)[5];
    }
  }
  close(FILE) || die "Could not close $Qual::outfile: $!";
} # end of getPDFFIT

sub getQmax{ #########################################################
  (return)if($Qual::Qmin&&$Qual::Qmax);
  my($Q)=0;
  open(FILE,"$Qual::filename.sq") || die "Could not open $Qual::filename.sq: $!";
  $_=<FILE>; if(/history/i){
    while(<FILE>){
      (last)if(/\Qstart data\E/);
    }
    $_=<FILE>;
  }

  if(/^\#/){
    while(<FILE>){
      (last)unless(/^\#/);
    }
  }

  while(<FILE>){
    chomp();
    s/^\s+//;
    $Q=(split /\s+/,$_)[0];
    ($Qual::Qmin=$Q)if(($Q>0)&&($Q<$Qual::Qmin));
    ($Qual::Qmax=$Q)if($Q>$Qual::Qmax);
  }
  close(FILE) || die "Could not close $Qual::filename.sq: $!";

  $Qual::Qmin=sprintf("%.4f",$Qual::Qmin);
  $Qual::Qmax=sprintf("%.4f",$Qual::Qmax);
  (print STDOUT "Qmin=$Qual::Qmin  Qmax=$Qual::Qmax\n")if($Qual::debug);
} # end of getQmax

sub integrate{ #######################################################
  my($x,$y,$min,$max)=@_;
  my($return)=0;

  ($min="$$x[0]")unless($min);
  ($max="$$x[@$x-1]")unless($max);

  ### trapezoidal rule
  for( my $int=1 ; $int<@{$x} ; $int++ ){
    if($$x[$int] >= $min){
      (last)if($$x[$int] > $max);
      $return+=0.5*($$y[$int]+$$y[$int-1])*($$x[$int]-$$x[$int-1]);
    }
  }

  return $return;
} # end of integrate

sub printHelp{ #######################################################
    my ($what)=@_;
    my $out;

    if ($what=~/help/) {
      $out = 'USAGE: pdfqual [filename] <-help> <-o> <-r> <-Q> <-l> <-R> <-ra> <-rb>'."\n";
      $out.= '  -help  Print the help information'."\n";
      $out.= '  -info  Print the description of the tests'."\n";
      $out.= '  -o     Select the PDFFIT output filename. '."\n";
      $out.= '         This defaults to "filename.out".'."\n";
      $out.= '  -r     Specify the number density rho_0.'."\n";
      $out.= '  -L     Specify the maximum R for G_low and rho_fit.'."\n";
      $out.= '  -l     Specify the maximum Q for averaging low-Q portion of S(Q). '."\n";
      $out.= '  -Q     Specify the minimum Q for averaging S(Q). '."\n";
      $out.= '         This defaults to '."$Qual::Qlow".'*Qmax.'."\n";
      $out.= '  -R     Specify the maximum R for integrating G(r).'."\n";
      $out.= '         This defaults to '."$Qual::Rmax.\n";
      $out.= '  -ra    Minimum r for integrating RDF. rho_0 must be specfied'."\n";
      $out.= '  -rb    Maximum r for integrating RDF. rho_0 must be specfied'."\n";
    }elsif($what=~/info/){
      $out =sprintf("===== PDFqual v%.1f =====\n",$Qual::version);

      $out.= 'This program is used to quantify how good a PDF is.'."\n";
      $out.= 'PDFquality presently uses the following criteria'."\n";
      $out.= 'to determine the quality of the PDF:'."\n\n";

      $out.= ' * <S(Q)> from Qlow to Qmax - This should be 1.0.'."\n\n";

      $out.= ' * Integral of Q^2[S(Q)-1]dQ from Qmin to Qmax'."\n";
      $out.= '   This should be equal to 2*pi^2*rho0. The '."\n";
      $out.= '   requirement for this to be true is that S(Q) has'."\n";
      $out.= '   reached its asymptotic value.'."\n\n";

      $out.= ' * Integral of rG(r)dr from Rmin to Rmax.'."\n";
      $out.= '   This should be equal to -1. This is only true'."\n";
      $out.= '   if the PDF has reached its asymptotic value'."\n";
      $out.= '   of zero before Rmax.'."\n\n";

      $out.= ' * G_low - the integral of the difference between'."\n";
      $out.= '   G(r) and the theoretical form between zero and'."\n";
      $out.= '   Rlow. This should be zero'."\n\n";

      $out.= ' * rho_fit is the least squares refined value of'."\n";
      $out.= '   rho used in calculation G_low. This should'."\n";
      $out.= '   equal rho0.'."\n\n";

      $out.= 'The data presented in the table produced is split '."\n";
      $out.= 'into four sections. The first section is parameters'."\n";
      $out.= 'used to calculate the PDF. The second section is'."\n";
      $out.= 'factors related to S(Q) while the third is related'."\n";
      $out.= 'to the PDF. The fourth section is information taken'."\n";
      $out.= 'from the PDFFIT refinement. If the file used to'."\n";
      $out.= 'calculate the information in any section is missing'."\n";
      $out.= 'then the section will not appear.'."\n\n";
    }
    return $out;
} # end of printHelp

sub printParam{ ######################################################
  my $out;

  $out ="###############################################################\n";
  $out.="# FILENAME: ";
  if(-r "$Qual::filename.sq"){
    $out.=sprintf("%15s ",$Qual::filename.'.sq');
  }else{ 
    $out.="                "; 
  }
  if(-r "$Qual::filename.gr"){
    $out.=sprintf("%15s ",$Qual::filename.'.gr');
  }else{ 
    $out.="                "; 
  }

  if(-r "$Qual::outfile"){
    $out.=sprintf("%15s ",$Qual::outfile);
  }else{ 
   $out.="                "; 
  }
  $out.="  #\n";

  if($Qual::rho0){
    $out.=sprintf('# '."    rho0: %8.4f",$Qual::rho0);
    $out.='                                          #'."\n";
  }

  if($Qual::history){
    $out.= '######################################################';
    $out.= '#########'."\n";
    $out.=sprintf('# '."rho_eff: %8.4f   ",$Qual::rhoeff);
    $out.=sprintf("sigma_eff: %8.4f",$Qual::sigeff);
    $out.= '                     #'."\n";
    $out.=sprintf('# '."S\'(Q) = %8.4f S(Q) + %8.4f",$Qual::fac_a,$Qual::fac_b);
    if($Qual::fac_c!=0){
      $out.=sprintf(" + %8.4f Q",$Qual::fac_c);
    }else{ $out.= '             '; }
    $out.= "               #\n";
  }

  if(-r "$Qual::filename.sq"){
    $out.= '######################################################';
    $out.= '#########'."\n";
    $out.=sprintf('# '."Qmin: %9.4f A^-1  Qlim: %9.4f A^-1",$Qual::Qmin,$Qual::Qlim);
    $out.=sprintf("      %8s    #\n",'');
    $out.=sprintf('# '."Qlow: %9.4f A^-1  Qmax: %9.4f A^-1",$Qual::Qlow,$Qual::Qmax);
    $out.=sprintf("      %8s    #\n",'');
    $out.= '#=====================================================';
    $out.= '========#'."\n";
    $out.=sprintf('# '."               %10s  %7s  ",'data','theory');
    $out.=sprintf("%9s               #\n",'%diff');
    $out.=sprintf('# '."<S(Q)>       : %10.4f  %7.4f  ",$Qual::sq_avg,1);
    if($Qual::sq_avg){
      $out.=sprintf("%9.2f               #\n",100*abs(1.0-$Qual::sq_avg));
    }else{ $out.=sprintf( "%9s               #\n",' '); }
    $out.=sprintf('# '."S_lim        : %10.4f  %7s  ",$Qual::sq_lim,'');
    $out.=sprintf( "%9s               #\n",' ');
    $out.=sprintf('# '."Q^2[S-1]dQ   : %10.4f  ",$Qual::sq_fac);
    if($Qual::rho0>0){
      my($temp)=-2*$Qual::PISQ*$Qual::rho0;
      $out.=sprintf("%7.4f  ",$temp);
      $out.=sprintf("%9.2f               #\n",100*abs($temp-$Qual::sq_fac)/$temp);
    }else{
      $out.=sprintf("%7s  %9s               #\n",' ',' ');
    }
  }

  if(-r "$Qual::filename.gr"){
    $out.= '######################################################';
    $out.= '#########'."\n";
    $out.=sprintf('# '."Rlow: %9.4f A     Rmax: %9.4f  ",$Qual::Rlow,$Qual::Rmax);
    $out.= "                     #\n";
    $out.= '#=====================================================';
    $out.= '========#'."\n";
    $out.=sprintf('# '."               %10s  %7s  ",'data','theory');
    $out.=sprintf("%9s               #\n",'%diff');
    $out.=sprintf('# '."rGdr         : %10.4f  %7.4f  ",$Qual::gr_int,-1);
    if($Qual::gr_int){
      $out.=sprintf("%9.2f               #\n",100*abs(1.0+$Qual::gr_int));
    }else{ printf "%9s               #\n",' '; }
    $out.=sprintf('# '."G_low        : %10.4f  %7.4f  ",$Qual::gr_low,0);
    $out.=sprintf( "%8s                #\n",' ');
    $out.=sprintf('# '."rho_fit      : %10.4f  ",$Qual::rhofit);
    if($Qual::rho0>0){
      $out.=sprintf("%7.4f  ",$Qual::rho0);
      if($Qual::rhofit){
	$out.=sprintf("%9.2f               #\n",100*abs($Qual::rho0-$Qual::rhofit)/$Qual::rho0);
      }else{ $out.=sprintf( "%9s               #\n",' '); }
    }else{ $out.=sprintf( "%7s  %8s                #\n",' ',' '); }
  }

  if(-r $Qual::outfile){
    $out.= '######################################################';
    $out.= '#########'."\n";
    $out.=sprintf('# '."SCALE       : %12s   ",$Qual::scale);
    $out.=sprintf("ITERATIONS:          %4d",$Qual::iterations);
    $out.= "      #\n";
    $out.=sprintf('# '."LATTICE     : %12s  %12s  %12s",
          $Qual::lattice[0],$Qual::lattice[1],$Qual::lattice[2]);
    $out.= "      #\n";
    $out.=sprintf('# '."ANGLE       : %12s  %12s  %12s",
          $Qual::angle[0],$Qual::angle[1],$Qual::angle[2]);
    $out.= "      #\n";
    $out.=sprintf('# '."Rexp        : %12s      ",$Qual::Rexpect);
    $out.=sprintf("Rweight:  %12s",$Qual::Rweight);
    $out.= "      #\n";
  }

  $out.= '######################################################';
  $out.= '#########'."\n";

  return $out;
} # end of printParam

sub processArgs{ #####################################################
  my(@arg)=@_;

  for( my($int)=0 ; $int<=$#arg ; $int++ ){
    if($arg[$int]=~/^-?\Q-help\E/){
      return "help";
    }elsif($arg[$int]=~/\Q-info\E/){
      return "info";
    }elsif($arg[$int]=~/\Q-r\E/){
      $int++;
      $Qual::rho0="$arg[$int]";
    }elsif($arg[$int]=~/\Q-o\E/){
      $int++;
      $Qual::outfile="$arg[$int]";
      ($Qual::outfile.='.out')unless(-r $Qual::outfile);
    }elsif($arg[$int]=~/\Q-Q\E/){
      $int++;
      $Qual::Qlow="$arg[$int]";
    }elsif($arg[$int]=~/\Q-R\E/){
      $int++;
      $Qual::Rmax="$arg[$int]";
    }elsif($arg[$int]=~/\Q-l\E/){
      $int++;
      $Qual::Qlim="$arg[$int]";
    }elsif($arg[$int]=~/\Q-L\E/){
      $int++;
      $Qual::Rlow="$arg[$int]";
    }elsif($arg[$int]=~/\Q-ra\E/){
      $int++;
      $Qual::r_a="$arg[$int]";
    }elsif($arg[$int]=~/\Q-rb\E/){
      $int++;
      $Qual::r_b="$arg[$int]";
    }elsif((-r "$arg[$int]")||(-r "$arg[$int].gr")||(-r "$arg[int].sq")){
      $Qual::filename="$arg[$int]";
      $Qual::filename=~s/\Q.sq\E$//;
      $Qual::filename=~s/\Q.gr\E$//;
      if(-r "$Qual::filename.out"){
	$Qual::outfile="$Qual::filename.out";
      }
    }
  }
  ### quick debuging information
  (print "FILENAME:$Qual::filename\n")if($Qual::debug);
  (print "OUTFILE :$Qual::outfile\n")if($Qual::debug);
  (print "RHO_EFF :$Qual::rho0\n")if($Qual::debug);
  (print "QLOW    :$Qual::Qlow\n")if($Qual::debug);
  (print "RLOW    :$Qual::Rmax\n")if($Qual::debug);

  ### check that $filename is okay
  if($Qual::filename){
    unless((-r "$Qual::filename.sq")||(-r "$Qual::filename.gr")){
      print STDERR "$Qual::filename.sq and $Qual::filename.gr need to exist\n";
      exit;
    }
  }else{
    print STDERR "Invalid filename specified: $Qual::filename\n";
    exit;
  }

  ### check that $outfile is okay
  if($Qual::outfile){
    unless(-r $Qual::outfile){
      print STDERR "Invalid PDFFIT output filename: $Qual::outfile\n";
      exit;
    }
  }

  ### check that $rho0 is okay
  if($Qual::rho0){
    my($temp)="$Qual::rho0";
    $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid rho0 ($Qual::rho0): must be a number\n";
      exit;
    }
    $Qual::rho0=sprintf("%.4f",$Qual::rho0);
  }else{
    $Qual::rho0=0;
  }

  ### check the $Qlow is okay
  if($Qual::Qlow){
    my($temp)="$Qual::Qlow"; $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid Qlow ($Qual::Qlow): must be a number\n";
      exit;
    }
  }else{
    $Qual::Qlow=0;
  }

  ### check the $Qlim is okay
  if($Qual::Qlim){
    my($temp)="$Qual::Qlim"; $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid Qlim ($Qual::Qlim): must be a number\n";
      exit;
    }
  }else{
    $Qual::Qlim=0;
  }

  ### check the $Qual::Rmax is okay
  if($Qual::Rmax){
    my($temp)="$Qual::Rmax"; $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid Rlow ($Qual::Rmax): must be a number\n";
      exit;
    }
  }

  ### check that $r_a is okay
  if($Qual::r_a){
    my($temp)="$Qual::r_a"; $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid r_a ($Qual::r_a): must be a number\n";
      exit;
    }
  }

  ### check that $r_b is okay
  if($Qual::r_b){
    my($temp)="$Qual::r_b"; $temp=~s/\.//;
    if($temp=~/\D+/){
      print STDERR "Invalid r_b ($Qual::r_b): must be a number\n";
      exit;
    }
  }

  ### quick debuging information
  (print "AFTER ERROR CHECKING\n")if($Qual::debug);
  (print "FILENAME:$Qual::filename\n")if($Qual::debug);
  (print "OUTFILE :$Qual::outfile\n")if($Qual::debug);
  (print "RHO_EFF :$Qual::rho0\n")if($Qual::debug);
  (print "QLOW    :$Qual::Qlow\n")if($Qual::debug);
  (print "RLOW    :$Qual::Rmax\n")if($Qual::debug);
  (print "R_A     :$Qual::r_a\n")if($Qual::debug);
  (print "R_B     :$Qual::r_b\n")if($Qual::debug);

  return "";

} # end of processArgs

sub readHistory{ #####################################################
  my($hstfile)=@_;

  (return 0)if($Qual::history);

  ##### check that the file has a history preprended
  open(FILE,$hstfile) || die "Could not open $hstfile: $!";
  $_=<FILE>; if(/history/i){
    (print STDOUT "Found History in $hstfile ... ")if($Qual::debug);
  }else{ return 0; }

  ##### find the information we want from the history
  while(<FILE>){
    chomp();
    if(/\QeffDensity\E/){
      (print STDOUT "effDensity=")if($Qual::debug);
      $Qual::rhoeff=(split 'effDensity=',$_)[1];
      (print STDOUT "$Qual::rhoeff\n")if($Qual::debug);
    }elsif(/\QmodEqn\E/){
      (print STDOUT "modEqn=")if($Qual::debug);
      ($Qual::fac_a,$Qual::fac_b,$Qual::fac_c)=(split /\s+/,$_)[1,2,3];
      $Qual::fac_a=~s/\QmodEqn=\E//; $Qual::fac_a=~s/\Q*S(Q)\E//;
      $Qual::fac_b=~s/\Q+\E//;
      $Qual::fac_c=~s/\Q+\E//; $Qual::fac_c=~s/\Q*Q\E//;
      (print STDOUT $Qual::fac_a.'*S(Q) +'.$Qual::fac_b.' +'.$Qual::fac_c.'Q'."\n")if($Qual::debug);
    }elsif(/\QRun Information\E/){
      (print STDOUT "radius=")if($Qual::debug);
      my($line); $line=<FILE>; $line=<FILE>; $line=<FILE>; $line=<FILE>;
      $Qual::sigeff=(split /\s+/,$line)[2];
      $Qual::sigeff=~s/\Qradius=\E//;
      (print STDOUT "$Qual::sigeff\n")if($Qual::debug);
    }elsif(/\QDamp\E/){
      ($Qual::Qmin,$Qual::Qmax)=(split /\s+/,$_)[2,3];
      $Qual::Qmin=~s/\QQmin=\E//;
      $Qual::Qmax=~s/\QQmax=\E//;
      (print STDOUT "Qmax=$Qual::Qmax  Qmin=$Qual::Qmin\n")if($Qual::debug);
    }elsif(/\Qstart data\E/){
      last;
    }
  }

  ##### finish up
  close(FILE) || die "Could not close $hstfile: $!";
  $Qual::history=1;
  return $Qual::history;
} # end of readHistory

sub readIn{ ##########################################################
    my($question,$answer);
    $question=$_[0];
    print STDOUT "$question";
    chomp($answer=<STDIN>);
    return $answer;
} # end of readIn
