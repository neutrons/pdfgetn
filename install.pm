######################################################################
# Created on January 14, 2000 by Peter Peterson
# Last modified on May, 4, 2000
######################################################################
use strict;
my $val;
my @temp;
my $version;

#################### Step 0: Get version #############################

open(FILE,"PDFgetN.pl") || die "Could not open PDFgetN.pl: $!";
        PDF: while(<FILE>){
            if(/\$xpdf::version/){ $version="$_"; 
	    $version =~ s/\$xpdf::version//;
	    $version =~ s/"//g;
	    $version =~ s/;//g;
	    $version =~ s/=//g;
	    $version =~ s/\s//g;
            last PDF; }
        }
        close(FILE) || die "Could not close PDFgetN: $!";

#################### Step 1: Welcome the user ########################
print STDOUT ">>>>>>>>>> Welcome to the PDFgetN version $version ";
print STDOUT "installer <<<<<<<<<<\n";
unless(-w './'){
    print STDOUT "! Current working directory is not writable by user, \n";
    print STDOUT "! you cannot install PDFgetN without write permission.\n";
    exit;
}
unless(-e 'install.pm'){
    print STDOUT "! Install script must be executed from within the \n";
    print STDOUT "! unpacked distribution.\n";
    exit;
}

##### check the install directory #####
my($instPrefix)='/usr/local';
print STDOUT ">> Specify an install prefix [$instPrefix]: ";
$val=<STDIN>; 
if($val=~m%/\n%){
    chomp($val); chop($val);
}else{ chomp($val); }
($instPrefix="$val")if($val);
unless(-e $instPrefix){
    $val="$instPrefix";
    my(@temp)=(split m%/%,$val);
    $val=''; for( my $int=1 ; $int<@temp-1 ; $int++ ){ 
	$val.="/$temp[$int]"; }
    unless(-e $val){
	print STDOUT "! $val does not exist : exiting install\n";
	exit;
    }
    unless(-w $val){
	print STDOUT "! $val is not writable by user, you cannot\n";
	print STDOUT "! install PDFgetN without write permission.\n";
	exit;
    }
}
&makeDir("$instPrefix");

##### check out the binary distribution directory #####
my($binDir)="$instPrefix/bin";
&makeDir("$binDir");

##### check out the library distribution directory #####
my($libDir)="$instPrefix/lib";
&makeDir("$libDir");
$libDir.='/PDFgetN-'.$version;
unless (-e $libDir) {
   mkdir("$libDir",'0755') || die "Could not create $libDir: $!";
   chmod 0755,"$libDir";
}
my($execDir)="$libDir/binary";
unless (-e $execDir) {
   mkdir("$execDir",'0755') || die "Could not create $execDir: $!";
   chmod 0755,"$execDir";
}

print STDOUT "% main executable will be put in: $binDir\n";
print STDOUT "% distribution files will be put in: $libDir\n";

#################### Step 2: Confirm perl information ################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
##### location of perl #####
my($perlLocation)=&inPath('perl');
if($perlLocation=~/:/){
    my(@choices)=(split /:/,$perlLocation);
    $perlLocation='';
    while(!$perlLocation){
	for( my $int=0 ; $int<@choices ; $int++ ){
	    print STDOUT ">> $int : $choices[$int]\n";
	}
	print STDOUT ">> Which version of perl would you like to use [0]:  ";
	$val=<STDIN>; chomp($val);
	($perlLocation="$choices[$val]")if(($val>=0)&&($val<=@choices));
    }
    print STDOUT "% Using perl installed at $perlLocation\n";
    print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
    print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
}

##### check Perl version #####
my($perlVersion);
my($perlMessage)="!!!!!!!!!! Perl not found !!!!!!!!!!\n";
$perlMessage.="! The Perl version 5.0 does not exist in your path.\n";
$perlMessage.="! Download Perl from http://www.perl.com/CPAN/ \n";
$perlMessage.="! Then rerun this install script.\n";
$perlMessage.=   "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
$perlVersion="$]";
print STDOUT "% Found Perl version $perlVersion\n";
unless($perlVersion>=5.0){
    print STDOUT "$perlMessage";
    exit;
}

##### check Perl/Tk version #####
my($TkVersion);
{
    my($TkMessage)="!!!!!!!!!! Perl/Tk not found !!!!!!!!!!\n";
    $TkMessage.="! The Perl/Tk version 800 module does not exist in your ";
    $TkMessage.="installation of perl. \n";
    $TkMessage.="! Download the module named Tk.800.###.tar.gz from";
    $TkMessage.="\n! http://www.perl.com/CPAN-local/modules/by-module/Tk/\n";
    $TkMessage.="! Then rerun this install script.\n";
    $TkMessage.=   "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";

    my($TkLocation)=&inINC('Tk');
    if($TkLocation){
	open(FILE,$TkLocation) || die "Could not open $TkLocation: $!";
	TK: while(<FILE>){
	    if((/Tk::VERSION/)&&(/=/)){ $TkVersion="$_"; last TK; }
	}
	close(FILE) || die "Could not close $TkLocation: $!";
	chomp($TkVersion);
	$TkVersion=~s/\s+//g;	$TkVersion=~s/\D+//;
	while($TkVersion=~/\d+\.\d+\D+/){
	    chop($TkVersion);
	}

	print STDOUT "% Found Perl/Tk version $TkVersion\n";

	unless($TkVersion>=800.0){
	    print STDOUT "$TkMessage";
	    #exit;
	}
    }else{
	print STDOUT "$TkMessage";
	#exit;
    }
}

#################### Step 3: Get compiler information ################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
##### make #####
my($make)='make';
$val=&inPath("$make");
unless($val=~/$make/){ $make='make'; $val=&inPath("$make"); }
while(1){
    if($val=~/$make/){
	last;
    }else{
	print STDOUT ">> Specify make program (Ctrl-C to exit): ";
	$make=<STDIN>; chomp($make);
	(next)unless($make);
	$val=&inPath("$make");
    }
}
print STDOUT "% Using make program: $make\n";

##### f77 #####
my($f77)='g77';
my($f77Flags)='-O0 -fno-f2c';
$val=&inPath("$f77");
unless($val=~/$f77/){ $f77='f77'; $val=&inPath("$f77"); }
while(1){
    if($val=~/$f77/){
	last;
    }else{
	print STDOUT ">> Specify a FORTRAN 77 compiler (Ctrl-C to exit): ";
	$f77=<STDIN>; chomp($f77);
	(next)unless($f77);
	$val=&inPath("$f77");
    }
}
print STDOUT ">> Specify FORTRAN 77 compiler flags [$f77Flags]: ";
$val=<STDIN>; chomp($val);
($f77Flags="$val")if($val);
($f77Flags='')if($val eq 'none');
print STDOUT "% Using FORTRAN compiler: $f77 with flags: $f77Flags\n";

##### C #####
my($gcc)='gcc';
my($gccFlags)='-g';
$val=&inPath("$gcc");
unless($val=~/$gcc/){ $gcc='cc'; $val=&inPath("$gcc"); }
while(1){
    if($val=~/$gcc/){
	last;
    }else{
	print STDOUT ">> Specify a C compiler (Ctrl-C to exit): ";
	$gcc=<STDIN>; chomp($gcc);
	(next)unless($gcc);
	$val=&inPath("$gcc");
    }
}
print STDOUT ">> Specify C compiler flags [$gccFlags]: ";
$val=<STDIN>; chomp($val);
($gccFlags="$val")if($val);
($gccFlags='')if($val eq 'none');
print STDOUT "% Using C compiler: $gcc with flags: $gccFlags\n";

#################### Step 4: Check for extra programs ################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
##### kuplot #####
my($kuplotVersion)='';
my($kuplotMessage)="!!!!!!!!!! KUPLOT not found !!!!!!!!!!\n";
$kuplotMessage.="! KUPLOT Version 4.1 does not exist in your path. ";
$kuplotMessage.="Download KUPLOT from \n";
$kuplotMessage.="! http://www.pa.msu.edu/cmp/billinge-group/programs/";
$kuplotMessage.="discus/kuplot.html\n";
$kuplotMessage.="! for plotting capabilities within PDFgetN. ";
$kuplotMessage.="NOTE THIS IS NOT NECESSARY.\n";
$kuplotMessage.=   "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
$val=&inPath('kuplot');
my($kuplotbin)='';
if($val=~/kuplot/){
    $kuplotbin=$val;
    open(FILE,">k.in") || die "Could not open k.in: $!";
    print FILE "set prompt,off\nexit\n";
    close(FILE) || die "Could not close k.in: $!";
    system("kuplot < k.in > k.out");
    unlink('k.in') || die "Could not remove k.in: $!";
    open(FILE,'k.out') || die "Could not open k.out: $!";
    foreach (<FILE>){
	($kuplotVersion="$_")if(/Version/);
    }
    close(FILE) || die "Could not close k.out: $!";
    unlink('k.out') || die "Could not remove k.out: $!";
    chomp($kuplotVersion);
    $kuplotVersion=(split /Version/,$kuplotVersion)[1];
    $kuplotVersion=~s/\*//;   $kuplotVersion=~s/\s+//g;
    print STDOUT "% Found KUPLOT version $kuplotVersion\n";
    $kuplotVersion=~s/[a-z]//i;
    (print STDOUT "\n$kuplotMessage")unless($kuplotVersion>=4.1);
}else{
    print STDOUT "$kuplotMessage";
}

##### OpenGENIE #####
my($genieMessage)="!!!!!!!!!! OpenGENIE not found !!!!!!!!!!\n";
$genieMessage.="! OpenGENIE does not exist in your path.\n";
$genieMessage.="! Downlaod OpenGENIE from http://www.isis.rl.ac.uk/OpenGENIE";
$genieMessage.=" to preprocess \n";
$genieMessage.="! NORM data files. NOTE THIS IS NOT NECESSARY.\n";
$genieMessage   .="!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n";
$val=&inPath('genie');
if($val){
    print STDOUT "% Found OpenGENIE\n";
}else{
    print STDOUT "$genieMessage";
}
print STDOUT ">> Press Enter to continue ... "; <STDIN>;

#################### Step 5: Edit the Makefiles ######################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
##### in fortran/ #####
&makefile('fortran');

##### in bin2asc/ #####
&makefile('bin2asc');

##### in raw2asc/ipns #####
&makefile('raw2asc/ipns');

#################### Step 6: Edit the PDF* files #####################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
foreach(<*>){
    if(-d $_){
	# do not include directories
    }else{
	if((/PDF/) && !(/\Q.orig\E/) && (/\Q.pl\E/)){
	    &editPDF("$_");
	    chmod 0755,"$_";
	}else{
	    if(/install/){
		# do nothing
	    }else{
		chmod 0644,"$_";
	    }
	}
    }
}

#################### Step 6: Run the makefiles #######################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
$val=system("cd fortran ; $make clean ; $make install ");
system('cd ../');
$val=$val/256; if ($val){
    print STDOUT "! Installed failed. Must rerun install.\n";
    exit;
}
$val=system("cd bin2asc ; $make clean ; $make install ");
system('cd ../');
$val=$val/256; if ($val){
    print STDOUT "! Installed failed. Must rerun install.\n";
    exit;
}
$val=system("cd raw2asc/ipns ; $make clean ; $make install ");
system('cd ../../');
$val=$val/256; if ($val){
    print STDOUT "! Installed failed. Must rerun install.\n";
    exit;
}


#################### Step 7: Copy over the perl files ################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
##### copy perl files #####
print STDOUT "% copying perl files . . .";
foreach(<*>){
    (next)if(-d $_);
    if(/install/){
	# do nothing
    }elsif(/\Q.orig\E/){
	# do nothing
    }else{
	$val=system("cp $_ $libDir/$_");
	$val=$val/256; if($val){
	    print STDOUT "\n! Installed failed. Must rerun install.\n";
	    exit;
	}
	chmod 0644,"$libDir/$_";
	(chmod 0755,"$libDir/$_")if(/PDF/);
    }
}
print STDOUT "done\n";

##### copy tutorial directory #####
print STDOUT "% copying sample data . . .";
unless (-e "$libDir/tutorial") {
   mkdir("$libDir/tutorial",'0755') || 
        die "Could not create $libDir/tutorial: $!";
   chmod 0755,"$libDir/tutorial";
}

foreach(<tutorial/*>){
    (next)if(-d $_);
    $val=system("cp $_ $libDir/$_");
    $val=$val/256; if($val){
	print STDOUT "\n! Installed failed. Must rerun install.\n";
	exit;
    }
    chmod 0644,"$libDir/$_";
}
print STDOUT "done\n";

##### copy templates directory #####
print STDOUT "% copying templates . . .";
unless (-e "$libDir/templates") {
  mkdir("$libDir/templates",'0755') || 
        die "Could not create $libDir/templates: $!";
  chmod 0755,"$libDir/templates";
}

foreach(<templates/*>){
    (next)if(-d $_);
    $val=system("cp $_ $libDir/$_");
    $val=$val/256; if($val){
	print STDOUT "\n! Installed failed. Must rerun install.\n";
	exit;
    }
    chmod 0644,"$libDir/$_";
}
print STDOUT "done\n";

#################### Step 8: Make links ##############################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
print STDOUT "% Creating links to PDFgetN ...";
if(-e "$binDir/PDFgetN"){
    unlink("$binDir/PDFgetN") || warn "PDFgetN link is not renewed\n";
}
symlink("$libDir/PDFgetN.pl","$binDir/PDFgetN") || warn "PDFgetN link was not created\n";
if(-e "$binDir/pdfgetn"){
    unlink("$binDir/pdfgetn") || warn "pdfgetn link is not renewed\n";
}
symlink("$libDir/PDFgetN.pl","$binDir/pdfgetn") || warn "pdfgetn link was not created\n";
print STDOUT "done\n";
if(-e "$binDir/pdfqual"){
    unlink("$binDir/pdfqual") || warn "pdfqual link is not renewed\n";
}
symlink("$libDir/PDFqual.pl","$binDir/pdfqual") || warn "pdfqual link was not created\n";
if($kuplotbin){
    symlink("$kuplotbin","$libDir/binary/kuplot") || warn "kuplot link was not created\n";
}

print STDOUT "% Cleaning up ...";
system ("rm PDF*.orig");
unlink("fortran/Makefile.orig") || warn "Could not delete Makefile.orig \n";
unlink("bin2asc/Makefile.orig") || warn "Could not delete Makefile.orig \n";
unlink("raw2asc/ipns/Makefile.orig") || 
       warn "Could not delete Makefile.orig \n";

system("cd fortran ; $make clean ; cd .. ");
system("cd bin2asc ; $make clean ; cd .. ");
system("cd raw2asc/ipns ; $make clean ; cd .. ");

print STDOUT "done\n";

#################### Step 9: Congratulations #########################
print STDOUT ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
print STDOUT "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n";
print STDOUT "% PDFgetN successfully installed.\n"; 
print STDOUT "% to execute PDFgetN type 'pdfgetn' or 'PDFgetN'\n";
$val=&inPath('');
unless($val=~/$binDir/){
    print STDOUT "% with $binDir in your path.\n";
}
unless($val=~/$execDir/){
    print STDOUT "% \n";
    print STDOUT "% To execute this different programs outside \n";
    print STDOUT "% PDFgetN, you might want to add the following to\n";
    print STDOUT "% your PATH: $execDir\n";
}
print STDOUT "% \n";
print STDOUT "% The tutorial files are located in the directory \n";
print STDOUT "% $libDir/tutorial ... \n";


#################### subroutines #####################################
########## string inPath(string)
# checks the user's path (environment variable) for the file specified
# by string and returns a colon delimited list of the locations of the
# file.
##############################
sub inPath{
    my($progname)=@_;
    my($val,$dir);
    my(@progpath);

    my(@path);
    $val=$ENV{'PATH'};
    @path=(split /:/,$val);

    foreach $dir ( @path ){
	$dir="$dir/$progname";
	if(-e $dir){
	    push(@progpath,$dir);
	}
    } 

    $val='';

  OUTER: for( my $i=0 ; $i<@progpath ; $i++ ){
      for( my $j=$i+1 ; $j<@progpath ; $j++ ){
	  (next OUTER)if("$progpath[$i]\n" eq "$progpath[$j]\n");
      }
      $val.="$progpath[$i]:";
  }

    (return '')unless($val);

    $val.=':';
    $val=~s/:://;

    return $val;
}

########## string inINC(string)
# checks @INC for string.pm or string (in that order) and returns
# a colon delimited list of the locations of the module.
##############################
sub inINC{
    my($incname)=@_;
    my($val,$dir);
    my(@incpath);

    foreach $dir ( @INC ){
	$dir="$dir/$incname";
	if(-e "$dir.pm"){
	    push(@incpath,"$dir.pm");
	}elsif(-e $dir){
	    push(@incpath,$dir);
	}
    } 

    $val='';

  OUTER: for( my $i=0 ; $i<@incpath ; $i++ ){
      for( my $j=$i+1 ; $j<@incpath ; $j++ ){
	  (next OUTER)if("$incpath[$i]\n" eq "$incpath[$j]\n");
      }
      $val.="$incpath[$i]:";
  }

    (return '')unless($val);

    $val.=':';
    $val=~s/:://;

    return $val;
}

########## void makfile(directory)
# This subroutine will replace all the appropriate variables for the
# compilers in the directory specified
##############################
sub makefile{
    my($filename)="@_";
    $filename.="/Makefile";

    my($f77Key)='F77';
    my($f77FlagKey)='FFLAGS';
    my($gccKey)='CC';
    my($gccFlagKey)='CCFLAGS';
    my($dirKey)='INSTALL_DIR';

    rename("$filename","$filename.orig") || die "Could not move $filename: $!";
    open(OLD,"$filename.orig") || die "Could not open $filename.orig: $!";
    open(NEW,">$filename") || die "Could not open $filename: $!";
    print STDOUT "% editing $filename . . . ";
    my $line;
    foreach $line (<OLD>){
	if($line=~/$f77Key\s+=/){
	    $line="$f77Key  =  $f77\n";
	}elsif($line=~/$f77FlagKey\s+=/){
	    $line="$f77FlagKey  = $f77Flags\n";
	}elsif($line=~/$gccKey\s+=/){
	    $line="$gccKey  = $gcc\n";
	}elsif($line=~/$gccFlagKey\s+=/){
	    $line="$gccFlagKey  = $gccFlags\n";
	}elsif($line=~/$dirKey\s+=/){
	    $line="$dirKey  = $execDir\n";
	}
	print NEW "$line";
	}
    print STDOUT "done\n";
    close(NEW) || die "Could not close $filename: $!";
    close(OLD) || die "Could not close $filename.orig: $!";
}

########## void editPDF(filename) 
# This subroutine will put the correct location of perl in the top of
# the perl file given by filename. If the first line does not contain
# '#!/' and '/perl' in it then an extra line will be added at the top
# of the file. Also the tutorial location will be changed ..
############################## 
sub editPDF{
    my($filename)=@_;
    my $line;

    unless(-e $filename){
	print STDOUT "! $filename missing: exiting install\n";
	exit;
    }

    print STDOUT "% editing $filename . . . ";
    rename($filename,"$filename.orig") || die "Could not move $filename: $!";
    open(OLD,"$filename.orig") || die "Could not open $filename.orig: $!";
    open(NEW,">$filename") || die "Could not open $filename: $!";
    $line=<OLD>;
    if(($line=~m%#!/%)&&($line=~m%/perl%)){
	$line="#!$perlLocation\n";
    }else{
	print NEW "#!$perlLocation\n";
    }
    print NEW "$line";
    my $d=localtime();
    foreach $line (<OLD>){
	if($line=~/\$xpdf\:\:build=/) {
	   $line="\$xpdf\:\:build=\"$d\";\n";
	} 
	print NEW "$line";
    }
    print STDOUT "done\n";
    close(NEW) || die "Could not close $filename: $!";
    close(OLD) || die "Could not open $filename.orig: $!";
}

########## void makeDir(directory) 
# This subroutine will confirm that directory exists and is
# writable. If the directory does not exist it will ask to create it.
############################## 
sub makeDir{
    my($dir)=@_;
    unless(-e "$dir/"){
	my($val);
	print STDOUT ">> $dir does not exist, create directory [y]: ";
	$val=<STDIN>; chomp($val);
	if($val=~/\bn/){
	    print STDOUT "% PDFgetN was not installed, rerun the install\n";
	    exit;
	}
	(mkdir "$dir", 0755 ) || die "Could not create $dir: $!";
	chmod 0755,"$dir";
    }
    unless(-w "$dir/"){
	print STDOUT "! $dir is not writable by user, you cannot \n";
	print STDOUT "! install PDFgetN without write permission.\n";
	exit;
    }

}
