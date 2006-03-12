####################
# Created by Peter Peterson on February 18, 1999
# Last Modified on April 3, 2001
####################
#
#  This is part of the PDFgetN distribution written by Peter Peterson,
# Matthias Gutmann, Thomas Proffen, and Simon Billinge.
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

    rsoqd.pm - version 1

=head1 OVERVIEW of rsoqd

    This is the wrapper for soqd. It is intended to operate in two modes: 

=over 4

=item main()

    Called when the user wants to run soqd and alter the input values 
    specific to soqd.

=item run()

    Called when the user just wants to run soqd and not change any of the 
    input values.

=back

=head2 Assumptions 

    The temporary history file for this run already exists.

=head2 Included files

    File.pm, Hist.pm, Stdio.pm

=head2 Modifier flags

=over 4

=item -help

    Displays the help information.

=back

=head1 TECHNICAL DOCUMENTATION

=head2 Global Variables Used

=over 4

=item @GLOBE::bank

    Array of bank numbers.

=item @GLOBE::blendQmax

    Maximum Q to be blended. This is used as the upper limit of window
    modified by S'=a*S+b.

=item @GLOBE::blendQmin

    Minimum Q to be blended. This is used as lower limit of window
    modified by S'=a*S+b.

=item $GLOBE::canNumber

    Run number for container.

=item $GLOBE::dataModSoqd

    1 for data not to be modified; >1 for number of groups to be
    modified.  This should be equal to the total number of banks if
    any are to be modified.

=item $GLOBE::dataSmooth

    0 for no data smoothing; >0 for data smoothing.

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken, must be one of the 
    supported values.

=item $GLOBE::minProcessOutput

    0 for no output log file; 1 for representative data logged in output 
    file; 2 for some data stored in log file; 3 for maximum output log 
    file.

=item $GLOBE::numBankProcess

    The number of banks to process.

=item @GLOBE::runFile

    The name of the data file(s).

=item $GLOBE::samPlazcek

     Flag to apply Plazcek corrections for sample.

=item @GLOBE::soqdA

    Multiplicitive factor used in the equation S'=a*S+b.

=item @GLOBE::soqdB

    Additive factor used in the equation S'=a*S+b.

=item $GLOBE::title

    Analysis run title. Usually material and temperature of run.

=item $GLOBE::vanNumber

    Run number for vanadium.

=item $GLOBE::vanPlazcek

    Flag to apply Plazcek corrections for vanadium.

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";

use strict;
package rsoqd;
my($filename);

$rsoqd::inpname="soqd.inp";
$rsoqd::logname="soqd.log";


#####
=pod

=item string check()

    Checks all of the variables (except filenames) necessary to write
    the input file for SOQD. Returns a string of errors if
    unsuccessful and 'soqd' if successful.

=cut

sub check{
  my($return)='';
  my($start)="\nMust specify";
  my($edit)="(Edit all)";
  my($temp);
  
  unless(&body::cInt($GLOBE::numBankProcess)){
    $return.="$start number of Banks as an integer $edit.";
  }
  unless(&body::cInt($GLOBE::minProcessOutputSoqd)){
    $return.="$start minProcOut for soqd as an integer $edit.";
  }
  unless(&body::cInt($GLOBE::dataSmoothSoqd)){
    $return.="$start smoothData for soqd as an integer $edit.";
  }
  unless(&body::cInt($GLOBE::dataModSoqd)){
    $return.="$start modifyData for soqd as an integer $edit.";
  }elsif($GLOBE::dataModSoqd>1){
    if($GLOBE::dataModSoqd==@GLOBE::bank){
      for( my($int)=0 ; $int<$GLOBE::dataModSoqd ; $int++ ){
	unless(&body::cInt($GLOBE::bank[$int])){
	  unless($return=~/$start bank numbers/){
	    $return.="$start bank numbers as integers.";
	  }
	}
	unless(&body::cFloat($GLOBE::soqdA[$int])){
	  unless($return=~/'a' for bank/){
	    $return.="$start 'a' for bank as a number.";
	  }
	}
	unless(&body::cFloat($GLOBE::soqdB[$int])){
	  unless($return=~/'b' for bank/){
	    $return.="$start 'b' for bank as a number.";
	  }
	}
      }
    }else{
      $return.="$start modifyData for soqd as either zero or the";
      $return.="total number of banks $edit.";
    }
  }
  unless(($GLOBE::vanPlazcek eq '1')||($GLOBE::vanPlazcek eq '0')){
    $return.="$start vanPlazcek as 1 or 0.";
  }
  unless(($GLOBE::samPlazcek eq '1')||($GLOBE::samPlazcek eq '0')){
    $return.="$start samPlazcek as 1 or 0.";
  }
  
  ($return='soqd')unless($return);
  return $return;
}

#####
=pod

=item void clean()

    Invokes cleanup of File.pm with the logfile and inputfile associated with 
    this wrapper. It passes the value of askQuestion to cleanup. It also 
    removes the output file from soqd.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rsoqd::logname);
    &File::remove($rsoqd::inpname);
    &File::remove($filename.".soq");
    &File::remove($filename.".sqa");
    &File::remove("soqd_sabs.dat");
    &File::remove("soqd_vabs.dat");
    &File::remove("soqd_cabs.dat");
    &File::remove("soqd_splc.dat");
    &File::remove("soqd_vplc.dat");
    &File::remove("soqd_smsc.dat");
    &File::remove("soqd_vmsc.dat");
}

#####
=pod

=item int run(void)

    Runs soqd and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {$val=&writeInput(); return $val;}

    if(&writeInput()){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	$filename=$fname.".soq";
	unless(&File::remove($filename)){       # remove previous soq file 
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rsoqd::logname)){
	    open(LOGFILE,">$rsoqd::logname") || 
                 die "Could not open $rsoqd::logname: $!";
	    $rsoqd::date=&Stdio::getDate();
	    print LOGFILE "******************** $rsoqd::date\n";
	    close(LOGFILE) || die "Could not close $rsoqd::logname: $!";
	    (print STDOUT "running soqd\n")unless($xpdf::exist);

	    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/soqd");
            system("$runcmd < $rsoqd::inpname >> $rsoqd::logname") == 0
                   or die "Could not run soqd: $!";

	    &convert();
	    $val=&File::runOk('soqd');
	}
    } else {
	print STDERR 
        "$rsoqd::inpname could not be written so soqd will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file soqd.inp.

=back

=cut

sub writeInput{
  my($dataDirec,$int);
  
  $dataDirec=$GLOBE::direc;
  if(&File::openWrite($rsoqd::inpname)){
    &OS::openFile(">$rsoqd::inpname") || 
      die "Could not open $rsoqd::inpname: $!";
    
    my $fsname=(split /,/,$GLOBE::runFile)[0];
    my $fvname=(split /,/,$GLOBE::vanFile)[0];
    my $fcname=(split /,/,$GLOBE::canFile)[0];
    print GLOBE::FILE "$GLOBE::title\n";
    print GLOBE::FILE "$GLOBE::machineName\n";
    print GLOBE::FILE "$fsname\n";
    print GLOBE::FILE "$fvname\n";
    print GLOBE::FILE "$fcname\n";
    print GLOBE::FILE "\n";
    print GLOBE::FILE "$GLOBE::numBankProcess,$GLOBE::minProcessOutputSoqd,";
    print GLOBE::FILE "$GLOBE::dataSmoothSoqd,$GLOBE::dataModSoqd\n";
    if($GLOBE::dataModSoqd>1){
      for( my($int)=0 ; $int<$GLOBE::dataModSoqd ; $int++ ){
	$GLOBE::soqdA[$int]=sprintf("%.4f",$GLOBE::soqdA[$int]);
	$GLOBE::soqdB[$int]=sprintf("%.4f",$GLOBE::soqdB[$int]);
	printf GLOBE::FILE "%d,",$GLOBE::bank[$int];
	printf GLOBE::FILE "%.4f,%.4f\n",$GLOBE::soqdA[$int],$GLOBE::soqdB[$int];
      }
    }
    print GLOBE::FILE "$dataDirec$fsname.cor\n";
    my ($vflag,$sflag);
    if ($GLOBE::vanPlazcek) {$vflag='T';} else {$vflag='F';}
    if ($GLOBE::samPlazcek) {$sflag='T';} else {$sflag='F';}
    print GLOBE::FILE "$vflag$sflag\n";
    
    close(GLOBE::FILE) || die "Could not close $rsoqd::inpname: $!";
  }
}

1;				# returns true if loaded into another program

#####
=pod

=item void convert(void)

    Converts the bin file produced by soqd (.soq) into an ascii file (.sqa)
    which can be plotted by kuplot.

=cut

sub convert{
    my $outfile=$filename;
    $outfile=~s/\Q.soq\E//;
    $outfile="$outfile".".sqa";

    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/soq2asc");
    system ("$runcmd $filename $outfile") == 0
           or die "Could not run soq2asc: $!";
}

