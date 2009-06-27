
###################
# Created by Peter Peterson on February 5, 1999
# Last Modified on December 9, 1999
####################
#
#  This is part of the PDFgetN distribution written by Peter Peterson,
# Matthias Gutman, Thomas Proffen, and Simon Billinge.
# 
# Copyright 2000 Michigan State University Board of Trustees
# 
# Use and distribution of this program is subject to the terms laid out
# in the license in LICENSE.txt included with this distribution.  A copy
# of the license  can be obtained from Michigan  State University office
# of Libraries, Computing and Technology (517-353-0722).  
#
####################
=head1 NAME

    File.pm - version 1

=head1 OVERVIEW of File

    This is a set of basic tools intended for file handling. While
    this is actually a redundence of what is available already from
    perl natively, it allows for specification of function calls and
    returns.

=head2 Included files

    none

=head2 Modifier flags

    none

=head1 FUNCTIONS

=over 4

=cut

use Tk;
package File;
use strict;

#####
=pod

=item int checkExist(filename)

    Checks if file named filename exists. Returns 1 if true and 0 if
    false.

=cut

sub checkExist{
    my($filename,$val);
    $filename=$_[0];
    if(-e $filename){ 
	$val=1;
    } else {
	$val=0;
    }
    return $val;
}

#####
=pod

=item int checkDir(dirname)

    Checks if directory dirname exists. Returns 1 if true and 0 if
    false.

=cut

sub checkDir{
    my($dirname,$val);
    $dirname=$_[0];
    if(-d $dirname){ 
	$val=1;
    } else {
	$val=0;
    }
    return $val;
}

#####
=pod

=item int checkExec(filename)

    Checks if filename is executable by the current user. Returns 1 if
    true and 0 if false.

=cut

sub checkExec{
    my($filename,$val);
    $filename=$_[0];
    if(checkExist($filename)){
	if(-x $filename){ 
	    $val=1;
	} 
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int checkRead(filename)

    Checks if filename is readable by the current user. Returns 1 if
    true and 0 if false.

=cut

sub checkRead{
    my($filename,$val);
    $filename=$_[0];
    if(checkExist($filename)){
	if(-r $filename){ 
	    $val=1;
	} 
    }
    unless($val==1){
	$val=0;
    }
}

#####
=item int checkWrite(filename)

    Checks if filename is writable by the current user. Returns 1 if
    true and 0 if false.

=cut

sub checkWrite{
    my($filename,$val);
    $filename=$_[0];
    if(checkExist($filename)){
	if(-w $filename){ 
	    $val=1;
	} 
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int makeRead(filename)

    Makes filename readable by the owner and group. Returns 1 if
    successful and 0 if not.

=cut

sub makeRead{
    my($filename,$val);
    $filename=$_[0];
    if(checkRead($filename)){
	$val=1;
    } elsif (checkExist($filename)){
	if(chmod(0440,$filename)){
	    $val=1;
	}
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int makeWrite(filename)

    Makes filename writable by the owner and readable by the owner and
    group. Returns 1 if successful and 0 if not.

=cut

sub makeWrite{
    my($filename,$val);
    $filename=$_[0];
    if(checkWrite($filename)){
	$val=1;
    } elsif (checkExist($filename)){
	if(chmod(0640,$filename)){
	    $val=1;
	}
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int openRead(filename)

    Makes filename ready to be read from. Filename must still be
    opened after this function call. Returns 1 if successful and 0 if
    not.

=cut

sub openRead{
    my($filename,$val);
    $filename=$_[0];
    if(makeRead($filename)){
	$val=1;	    
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int openWrite(filename)

    Makes filename ready to be written to. Filename must still be
    opened after this function call. Returns 1 if successful and 0 if
    not.

=cut

sub openWrite{
    my($filename,$val);
    $filename=$_[0];
    unless(checkExist($filename)){
	$val=1;
    } elsif(makeWrite($filename)){
	$val=1;	    
    }
    unless($val==1){
	$val=0;
    }
}

#####
=pod

=item int remove(filename)

    Removes filename from the current directory. Also returns true if the 
    file does not exist. Returns 1 if successful and 0 if not.

=cut

sub remove{
    my($filename,$val);
    $filename=$_[0];
    if(checkExist($filename)){
	if(makeWrite($filename)){
	    if(unlink($filename)){
		$val=1;
	    }
	}
    } else {
	$val=1;
    }
    unless($val==1){
	$val=0;
    }
    return $val;
}

#####
=pod

=item int runOk(progname)

    Checks the program's log file to see if the phrase "successful
    termination of progname" is in it. This phrase means that the
    fortran code ran successfully. Returns 1 if successful and 0 if
    not.

=cut

sub runOk{
    my($progname)=@_;
    my($filename,$phrase,$val);
    
    if ($FLAG::debug) {return 1;}

    $filename="$progname.log";
    $phrase="successful termination";
    $val=0;

    if(openRead($filename)){
	open(FILE,$filename) || warn "Could not open $filename: $!";
	while(<FILE>){
	    if(/$phrase/i){
		$val=1;
		last;
	    }
	}
	close(FILE) || warn "Could not close $filename: $!";
    }    
    return $val;
}

#####
=pod

=item void loadDialog(widget)

    This routine displays the load file dialog and reads the
    history file specified.

=cut

sub loadDialog{
    my($widget)=@_;

    if(my $filename=&File::fileDialog($widget,"load","")){
	# read in a history file
	if(&Hist::read("$filename")){
	    &body::makeTable_f($body::sample_f);
	    &body::makeBankTable($body::bank_ftab);
	    $xpdf::mess="$filename loaded";
	} else {
	    $xpdf::mess="Could not load $filename";
	}
    }
}

#####
=pod

=item void saveDialog(widget,filename)

    This routine displays the save file dialog and saves the
    history file specified. It also renames the G(r) and S(Q)
    files to name.gr and name.sq, respecitvely.

=cut

sub saveDialog{
    my($widget,$filename)=@_;
    my $sname = $filename;

    if($filename=&File::fileDialog($widget,"save",$filename)){
	my $runname=(split /,/,$GLOBE::runFile)[0];
	$filename =~s/.hst//;
	unless ($filename eq $runname) {
	    rename $runname.".gr",$filename.".gr";
	    rename $runname.".sq",$filename.".sq";
	}

	&Hist::save("$filename.hst");
	$xpdf::mess="Files $filename.hst, .sq and .gr saved";

    	if($FLAG::noOut){
          &body::prep_clean($runname);
          &rcorps::clean($runname);
          &rsoqd::clean ($runname);
          &rblend::clean($runname);
          &rdamp::clean ($runname);
          &rft::clean   ($runname);

	  &File::remove("soq2gsa.inp");
	  &File::remove("soq2gsa.log");
    	}

	return $filename;
    }
    return 0;
}

#####
=pod

=item splitpath(path)

    Splits path and filename (should work for Windows & Unix)

=back

=cut

sub splitpath{
  my($path)=@_;

  if ($path=~/[\\,\/]/) { $path=~s/(^.*[\\,\/]).+$/$1/; }
  else                  { $path =""; }
  return $path;
}


#####
=pod

=item setpath(file)

    This subroutine splits path and filename and sets directory.
    The path is stored in $GLOBE::workdir.

=back

=cut

sub setpath{
  use Cwd;

  my($file)=@_;

  my $path=&File::splitpath($file);
  if ($path) {
    if (&File::checkDir($path)) {
      chdir($path);

      # Double the \ to make it work in regex !
      $GLOBE::workdir =~ s/\\/\\\\/g;

      $file =~ s/$GLOBE::workdir//;
      $file =~ s/[\\,\/]//g;
      $GLOBE::workdir=$path;
    } else {
      die "Could not change directory to $path: $!";
    }
  } else {
    $GLOBE::workdir= cwd;
  }
  return $file;
}


#####
=pod

=item getRefFile(file,widget)

    Opens file dialog for selecting reference file ..

=back

=cut

sub getRefFile{
    my($file,$widget)=@_;
    my(@itypes)=(["G(r) files", "*.gr"],
                 ["S(Q) files", "*.sq"],
                 ["All files",  "*.*"]);

    $file=$widget->getOpenFile(-filetypes=>\@itypes);
    if ($file) {
      my $path=&File::splitpath($file);
      if ($path) {
        $path =~ s/\\/\\\\/g;
        $file =~ s/$path//;
        $file =~ s/[\\,\/]//g;
      }
      $file=~s/\.gr$//;
      $file=~s/\.sqb$//;
      $file=~s/\.sq$//;

      $_[0]=$file;
     }
}

#####

#####
=pod

=item getFile(file,widget,name,ext)

    Opens file dialog for selecting file ..

=back

=cut

sub getFile{
    my($file,$widget,$name,$ext)=@_;
    my(@itypes)=([$name, $ext],["All files","*.*"]);

    $file=$widget->getOpenFile(-filetypes=>\@itypes);
    if ($file) {
      my $path=&File::splitpath($file);
      if ($path) {
        $path =~ s/\\/\\\\/g;
        $file =~ s/$path//;
        $file =~ s/[\\,\/]//g;
      } 
   $_[0]=$file;
   }
}

#####
=pod

=item fileDialog(widget,lsave)

    Opens file dialog for selecting files ..

=back

=cut

sub fileDialog{
    my($widget,$what,$default)=@_;
    my(@loadtypes)=(["History files", ".hst"],
                    ["S(q) files",    ".sq"],
                    ["PDF files",     ".gr"],
                    ["All files",     "*"]);
    my(@savetypes)=(["History files", ".hst"],
                    ["All files",     "*"]);

    if ($what eq 'load') {
        my $name=$widget->getOpenFile(-filetypes => \@loadtypes);
	## change in corresponding directory ..
	if ($name) { &File::setpath($name); }
	return $name;
    } elsif ($what eq 'save') {
        return $widget->getSaveFile(-filetypes => \@savetypes,
                                    -initialfile=>$default,
	                            -initialdir=>$GLOBE::workdir,
                                    -defaultextension=>".hst");
    } else {
      return 0;
    }
}

#####
=pod

=item batchProcess(widget)

    Processed set of runs from batch file.

=back

=cut

sub batchProcess{
    my($widget)=@_;
    my(@types)=(["All files","*"]);
    my(@runs,@temp,@out);

    my $name=$widget->getOpenFile();
    if ($name) {
      unless (-e $name) {
        &body::printStatus("Can not read $name ..");
        return 0;
      }
      open (BATCH,"$name"); 
      my $line=<BATCH>; # Header line
      my $pp=0;
      while ($line=<BATCH>) {
        $pp++; chomp $line; $line=~s/"//g;
        ($runs[$pp],$temp[$pp],$out[$pp])=split (/\t/,$line);
      }

      for (my $ip=1; $ip<=$pp; $ip++) {
        &body::printStatus("Processing run $ip / $pp ..");
        $GLOBE::runFile=$runs[$ip];
        my $runname=(split /,/,$GLOBE::runFile)[0];
        $line=&body::prep_getTitle($runname);
        chomp($line); $GLOBE::title=$line;
        $GLOBE::temp=$temp[$ip];

        unless (&body::autoNormalize()) {return 0;}

        &body::printStatus("Saving $out[$ip] ..");
        unless ($out[$ip] eq $runname) {
            rename $runname.".gr",$out[$ip].".gr";
            rename $runname.".sq",$out[$ip].".sq";
        }

        &Hist::save("$out[$ip].hst");
        if($FLAG::noOut){
          &body::prep_clean($runname);
          &rcorps::clean($runname);
          &rsoqd::clean ($runname);
          &rblend::clean($runname);
          &rdamp::clean ($runname);
          &rft::clean   ($runname);
          &File::remove("soq2gsa.inp");
          &File::remove("soq2gsa.log");
        }
      }
      close BATCH;
      &body::printStatus("Finished ..");
      return 1;
    }
    return 0;
}

#####
=pod

=item saveGsasDialog()

    Open dialog to save SOQ file as GSAS file

=back

=cut

sub saveGsasDialog{

        my $fname=(split /,/,$GLOBE::runFile)[0];
        $fname=$fname.".soq";
        my $oname=$fname.".gsa";
	my $gsasAuto=1;

	my $error="";
	unless ($GLOBE::prepName eq 'gsas') {
	  $error.="This option is only supported for GSAS templates ..\n";
	}
	unless (&File::checkExist($fname)) {
	  $error.="Create SOQ file (raw) first ..\n";
	}
	unless (&File::checkExist($GLOBE::instParamFile)) {
	  $error.="Specify instrument parameter file ..\n";
        }

	if ($error) {
	  &help::displayHelp($xpdf::mw,"Error ...",$error);
	} else {
	  my($gsasDia)=$xpdf::mw->DialogBox(-title=>'SOQ to GSAS ..',
                                            -buttons=>['Save','Cancel']);
  	  my($title)=$gsasDia->Frame(-relief=>'ridge',-borderwidth=>2,
                                     -bg=>"$colors::headerbg")
                                     ->pack(-side=>'top',-expand=>1,
                                     -fill=>'x',-pady=>5);
          $title->Label(-text=>"Convert to GSAS: $fname",
                -font=>$colors::fontH,
                -bg=>"$colors::headerbg",-fg=>"$colors::headerfg"
               )->pack(-expand=>1,-fill=>'x');

	  my($g2)=$gsasDia->Frame(-relief=>'ridge',-borderwidth=>2)
                          ->pack(-side=>'top',-expand=>1,-fill=>'x');

	  $g2->Label(-text=>'Input raw S(Q) file:',-font=>$colors::fontM)
	     ->grid(-column=>0,-row=>0,-sticky=>'e');
	  $g2->Label(-textvariable=>\$fname,-font=>$colors::fontM,
	             -fg=>"$colors::headerfg")
	     ->grid(-column=>1,-row=>0,-sticky=>'w');
	  $g2->Label(-text=>'Output GSAS file:',-font=>$colors::fontM)
	     ->grid(-column=>0,-row=>1,-sticky=>'e');
	  $g2->Label(-textvariable=>\$oname,-font=>$colors::fontM,
	             -fg=>"$colors::headerfg")
	     ->grid(-column=>1,-row=>1,-sticky=>'w');
	  $g2->Label(-text=>'Instrument parameter file:',-font=>$colors::fontM)
	     ->grid(-column=>0,-row=>2,-sticky=>'e');
	  $g2->Label(-textvariable=>\$GLOBE::instParamFile,
                     -font=>$colors::fontM,
	             -fg=>"$colors::headerfg")
	     ->grid(-column=>1,-row=>2,-sticky=>'w');
	  $g2->Label(-text=>'Autoscale GSAS output:',-font=>$colors::fontM)
	     ->grid(-column=>0,-row=>3,-sticky=>'e');
	  $g2->Checkbutton(-variable=>\$gsasAuto,
             -selectcolor=>"$colors::selectcol",
             -text=>'',-font=>$colors::fontM)
	     ->grid(-column=>1,-row=>3,-sticky=>'w');

	  my $what=$gsasDia->Show;
  	  $gsasDia->destroy();

	  if ($what eq 'Save') {
	    if(&File::openWrite("soq2gsa.inp")){
	      &OS::openFile(">soq2gsa.inp") ||
	           die "Could not open soq2gsa.inp: $!";
	      print GLOBE::FILE "$fname\n";
	      print GLOBE::FILE "$GLOBE::instParamFile\n";
	      if ($gsasAuto) { print GLOBE::FILE "Y\n";}
	      else           { 
	        print GLOBE::FILE "N\n";
	        print GLOBE::FILE "1.0,0.0\n";
	        print GLOBE::FILE "Y\n";
	      }
	      print GLOBE::FILE "$GLOBE::numBankProcess\n";
	      for (my $j=0; $j<$GLOBE::numBankProcess; $j++) {
	        print GLOBE::FILE 
                    "$GLOBE::blendQmin[$j],$GLOBE::blendQmax[$j]\n";
	      }
	      print GLOBE::FILE "N\n";
	      close(GLOBE::FILE) || die "Could not close soq2gsa.inp: $!";

	      my $runcmd=File::Spec->canonpath("$GLOBE::execpath/soq2gsa");
	      system ("$runcmd < soq2gsa.inp > soq2gsa.log") == 0
	              or die "Could not run soq2gsa: $!";

	      $xpdf::mess="GSAS file $oname saved ..";  
            } else {
	      $xpdf::mess="Error running soq2gsa ..";  
	    }
	  }

	}
}

#####
=pod

=item runList(file)

    Open file selector list with run titles

=back

=cut

sub runList{
    my($what, $file, $widget)=@_;
    my @array;
    my $i=0;
    my $line;

    my $title="Select files: ".$what;
    my $dialog=$widget->Dialog(-title=>$title,-buttons=>['Select','Cancel']);

    ## Header
    my $f1=$dialog->Frame(-relief=>'ridge', -borderwidth=>2,
                              -bg=>"$colors::headerbg")
	              ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    $f1->Label(-text=>$title, -fg=>"$colors::headerfg",-font=>$colors::fontH,
                              -bg=>"$colors::headerbg")->pack();

    ## Create list box with filenames and run titles
    while (<*.$GLOBE::fileExt>) 
      {$array[$i]=$_; $array[$i]=~s/.$GLOBE::fileExt//; $i++;}
    sort(@array);
    
    my $list=$dialog->Scrolled("Listbox", -width=>65, -height=>8,
                               -scrollbars=>'se',-font=>$colors::fontM, 
                               -selectmode=>'extended')
	            ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    my @entry=@array;
    for ($i=0; $i<=$#array; $i++) {
      $line ="$entry[$i] -  ";
      $line.=&body::prep_getTitle($array[$i]);
      $list->insert('end',$line);
    }

    ## Show dialog
    my $ret=$dialog->Show();

    ## Get selection
    if ($ret eq 'Select') {
      my @sel=$list->curselection();
      $file=$array[$sel[0]];
      for ($i=1; $i<=$#sel; $i++) {
        $file.=",$array[$sel[$i]]";
      }
      if ($what eq 'Sample') {
        $line=&body::prep_getTitle($array[$sel[0]]);
        chomp($line);
        $GLOBE::title=$line;
      }
      $_[1]=$file;
    }
    $dialog->destroy();
}

1;				# return true if loaded by another program
