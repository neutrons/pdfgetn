####################
# Created by Peter Peterson on February 26, 1999
# Last Modified on January 11, 2000
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

    rblend.pm - version 1

=head1 OVERVIEW of rblend

    This is the wrapper for blend. It is intended to operate in two modes:

=over 4

=item main()

    Called when the user wants to run blend and alter the input values 
    specific to blend.

=item run()

    Called when the user wants to run blend and not change any of the input 
    values.

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

=item @GLOBE::angle

    Detector angle.

=item @GLOBE::bank

    Array of the bank numbers.

=item $GLOBE::blendBank

    The banks to be blended.

=item @GLOBE::blendQmax

    The maximum Q value to be processed.

=item @GLOBE::blendQmin

    The minimum Q value to be processed.

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken, must be one of the 
    supported values.

=item $GLOBE::numBankBlend

    The number of banks to be blended.

=item $GLOBE::numBankProcess

    The number of banks to be processed. This is the number of banks that 
    are considered after ****prep is run.

=item $GLOBE::blendDQ

    Grid size BLEND rebins data on (in A**-1).

=item @GLOBE::runFile

    The name of the data file(s).

=item @GLOBE::blendMatchBref

    Reference bank for automatic bank matching in blend step. 0=none

=item @GLOBE::blendMatchScal

    Flag to use scale factor for automatic bank matching in blend step.

=item @GLOBE::blendMatchOffset

    Flag to use offset for automatic bank matching in blend step.

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";


use strict;
package rblend;
my($filename);

$rblend::inpname="blend.inp";
$rblend::logname="blend.log";

#####
=pod

=item int append(void)

    This prepend the history file to the ascii version of the bld file.

=cut

sub append{
    my $line;
    my $outfile="$filename";
    my $val=0;
    my $fname=(split /,/,$GLOBE::runFile)[0];
    my $tempfile="$fname.tmp";
    $outfile=~s/\Q.bld\E//;
    $outfile="$outfile".".sqb";
    rename("$outfile","$tempfile");
    if(&File::checkExist($tempfile)){
	if(&File::openWrite($outfile)){
	    open(OUT,">$outfile") || die "Could not open $outfile: $!";
	    if(&File::openRead($GLOBE::historyname)){
		open(HST,$GLOBE::historyname) || 
                     die "Could not open $GLOBE::historyname: $!";
		while($line=<HST>){
		    print OUT "$line";
		}
		close(HST) || die "Could not close $GLOBE::historyname: $!";
		print OUT "##### start data\n";
		if(&File::openRead($tempfile)){
		    open(IN,$tempfile) || die "Could not open $tempfile: $!";
		    while($line=<IN>){
			print OUT "$line";
		    }
		    close(IN) || die "Could not close $tempfile: $!";
		    &File::remove($tempfile);
		}
	    }
	    close(OUT) || die "Could not close $outfile: $!";
	    $val=1;
	}
    } 
    return $val;
}

#####
=pod

=item string check()

    Checks all of the variables (except filenames) necessary to write
    the input file for BLEND. Returns a string of errors if
    unsuccessful and 'blend' if successful.

=cut

sub check{
    my($return)='';
    my($start)="\nMust specify";
    my($edit)="(Edit all)";
    my($temp);

    if ($GLOBE::soqCorrFile) {
      unless(&File::checkRead($GLOBE::soqCorrFile)) {
  	  $return.="Correction file $GLOBE::soqCorrFile can not be opened.";
      }
    }

    unless($GLOBE::blendMatchScal eq 'T' || $GLOBE::blendMatchScal eq 'F') {
	$return.="$start matchScal must be T or F $edit.";
    }

    unless($GLOBE::blendMatchOffset eq 'T' || $GLOBE::blendMatchOffset eq 'F') {
	$return.="$start matchOffset must be T or F $edit.";
    }

    unless(&body::cFloat($GLOBE::blendDQ)){
	$return.="$start deltaQ as real number.";
    }

    unless($GLOBE::numBankBlend > 0){
	$return.="Select al least one back for blending.";
    }

    my $match=0;
    unless(&body::cInt($GLOBE::numBankBlend)){
	$return.="$start numBanks for blend as an integer $edit.";
    }else{
	for( my $int=0 ; $int<$GLOBE::numBankBlend ; $int++ ){
            if($GLOBE::blendBank[$int] eq $GLOBE::blendMatchBref) {$match=1;}
	    unless(&body::cInt($GLOBE::blendBank[$int])){
		unless($return=~/blend bank numbers/){
		    $return.="$start blend bank numbers as integers.";
		}
	    }
	}
    }

    unless($match || $GLOBE::blendMatchBref eq 0) {
	$return.="$start Reference bank for matching invalid.";
    }
    unless(&body::cInt($GLOBE::numBankProcess)){
	$return.="$start banks as an integer $edit.";
    }else{
	for( my $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	    my($bank)="$GLOBE::bank[$int]";
	    unless(&body::cInt($bank)){
		unless($return=~/$start bank numbers/){
		    $return.="$start bank numbers as integers.";
		}
	    }
	    unless(&body::cFloat($GLOBE::angle[$int])){
		unless($return=~/bank angles/){
		    $return.="$start bank angles as numbers.";
		}
	    }
	    unless(&body::cFloat($GLOBE::blendQmin[$int])){
		unless($return=~/bank Qmin/){
		    $return.="$start bank Qmin as a number.";
		}
	    }
	    unless(&body::cFloat($GLOBE::blendQmax[$int])){
		unless($return=~/bank Qmax/){
		    $return.="$start bank Qmax as a number.";
		}
	    }
	}
    }

    ($return='blend')unless($return);
    return $return;
}

#####
=pod

=item void clean()

    Removes file created and used by the program BLEND.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rblend::logname);
    &File::remove($rblend::inpname);
    &File::remove($filename.".bld");
    &File::remove($filename.".sqb");
    &File::remove("blen_bin.dat");
    &File::remove("blen_corr.dat");
}

#####
=pod

=item void convert(void)

    Converts the bin file produced by blend (.bld) into an ascii file (.sqb) 
    which can be plotted by kuplot.

=cut

sub convert{
    my $outfile=$filename;
    $outfile=~s/\Q.bld\E//;
    $outfile="$outfile".".sqb";

    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/soq2asc");
    system ("$runcmd $filename $outfile") == 0
           or die "Could not run soq2asc: $!";
}

#####
=pod

=item int run(void)

    Runs blend and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {return &writeInput();}

    if(&writeInput()){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	$filename=$fname.".bld";
	unless(&File::remove($filename)){       # remove previous bld file
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rblend::logname)){
	    open(LOGFILE,">$rblend::logname") || 
                 die "Could not open $rblend::logname: $!";
	    $rblend::date=&Stdio::getDate();
	    print LOGFILE "******************** $rblend::date\n";
	    close(LOGFILE) || die "Could not close $rblend::logname: $!";
	    (print STDOUT "running blend\n")unless($xpdf::exist);

            my $runcmd=File::Spec->canonpath("$GLOBE::execpath/blend");
            system ("$runcmd < $rblend::inpname >> $rblend::logname") == 0
                   or die "Could not run blend: $!";

	    &convert();
	    &append();
	    $val=&File::runOk('blend');
	}
    } else {
	print STDERR 
              "$rblend::inpname could not be written so blend will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file blend.inp

=back

=cut

sub writeInput{
    my($int);
       
    if(&File::openWrite($rblend::inpname)){
	&OS::openFile(">$rblend::inpname") || 
             die "Could not open $rblend::inpname: $!";

	my $fname=(split /,/,$GLOBE::runFile)[0];

	print GLOBE::FILE "$GLOBE::machineName\n";
	print GLOBE::FILE "$fname.bld\n";
	print GLOBE::FILE "$GLOBE::direc"."$fname".".soq\n";
	print GLOBE::FILE "$GLOBE::numBankBlend\n";
	for( $int=0 ; $int<$GLOBE::numBankBlend ; $int++ ){
	    unless($int==0){
		print GLOBE::FILE ",";
	    }
	    print GLOBE::FILE "$GLOBE::blendBank[$int]";
	}
	print GLOBE::FILE "\n";
	for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	    if($GLOBE::cb[$int]){
		printf GLOBE::FILE "%.1f,",$GLOBE::angle[$int];
		printf GLOBE::FILE "%.2f,",$GLOBE::blendQmin[$int];
		printf GLOBE::FILE "%.2f\n",$GLOBE::blendQmax[$int];
	    }
	}
	print GLOBE::FILE "$GLOBE::blendDQ\n";
	if ($GLOBE::blendMatchBref) {
	  print GLOBE::FILE "$GLOBE::blendMatchBref\n";
	  print GLOBE::FILE "$GLOBE::blendMatchScal,$GLOBE::blendMatchOffset\n";
        } else {
	  print GLOBE::FILE "0\n";
        }
	print GLOBE::FILE "$GLOBE::soqCorrFile\n";
	close(GLOBE::FILE) || die "Could not close $rblend::inpname: $!";
    }
}

1;				# returns true if loaded into another program
