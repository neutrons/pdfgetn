####################
# Created by Peter Peterson on March 5, 1999
# Last Modified on January 6, 2000
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

    rft.pm - version 1

=head1 OVERVIEW of rft

    This is the wrapper for ft. It is intended to operate in two modes:

=over 4

=item main()

    Called when the user wants to run ft and alter the input values specific 
    to ft.

=item run()

    Called when the user wants to run ft and not change any of the input 
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

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::ftError

    One to calculate errors or zero not to.

=item $GLOBE::ftMaxR

    Maximum R value in angstroms.

=item $GLOBE::ftNumPoint

    Number of points in R to calculate.

=item $GLOBE::ftNumDensity

    Number denisty RHO0, only used to calculate rho(r) from G(r)
    in the final PDF output file (ASCII).

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken, must be one of the 
    supported values.

=item $GLOBE::runFile

    The name of the data file.

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";

use strict;
package rft;
my($filename);

$rft::inpname="ft.inp";
$rft::logname="ft.log";

#####
=pod

=item int append(void)

    This prepends the history file to the ascii version of the pdf file and 
    renames the pdf file to have the .pdf extension.

=cut

sub append{
    my $line;
    my $outfile="$filename";
    my $val=0;
    my $fname=(split /,/,$GLOBE::runFile)[0];
    my $tempfile="$fname.tmp";
    $outfile=~s/\Q.pdf\E//;
    $outfile="$outfile".".gr";
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
    the input file for FT. Returns a string of errors if
    unsuccessful and 'ft' if successful.

=cut

sub check{
    my($return)='';
    my($start)="\nMust specify";
    my($edit)="(Edit all)";
    my($temp);

    unless(($GLOBE::ftError eq '0')||($GLOBE::ftError eq '1')){
	$return.="$start calcError for ft as either 0 or 1 $edit.";
    }
    unless(&body::cInt($GLOBE::ftNumPoint)){
	$return.="$start numRpoints for ft as an integer.";
    }
    unless($GLOBE::ftNumPoint <= 15000){
	$return.="$start Maximum number of points for G(r) exceeded.";
    }
    unless(&body::cFloat($GLOBE::ftMaxR)){
	$return.="$start maxR for ft as a number.";
    }

    unless(&body::cFloat($GLOBE::ftNumDensity)){
	$return.="$start numDensity needs to be a number $edit.";
    }

    ($return='ft')unless($return);
    return $return;
}

#####
=pod

=item void clean()

    Removes files used and created by FT.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rft::logname);
    &File::remove($rft::inpname);
    &File::remove($filename.".pdf");
}

#####
=pod

=item void convert(void)

    Converts the bin file produced by ft (.pdf) into an ascii file (.gr) 
    which can be plotted by kuplot.

=cut

sub convert{
    my $outfile=$filename;
    $outfile=~s/\Q.pdf\E//;
    $outfile="$outfile".".gr";
    
    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/pdf2asc");
    system ("$runcmd $filename $outfile $GLOBE::ftIntMaxR") == 0
           or die "Could not run pdf2asc: $!";
}

#####
=pod

=item int run(void)

    Runs ft and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {return &writeInput();}

    if(&writeInput()){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	$filename=$fname.".pdf";
	unless(&File::remove($filename)){       # remove previous pdf file
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rft::logname)){
	    open(LOGFILE,">$rft::logname") || 
                die "Could not open $rft::logname: $!";
	    $rft::date=&Stdio::getDate();
	    print LOGFILE "******************** $rft::date\n";
	    close(LOGFILE) || die "Could not close $rft::logname: $!";
	    (print STDOUT "running ft\n")unless($xpdf::exist);

	    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/ft");
            system("$runcmd < $rft::inpname >> $rft::logname") == 0
                   or die "Could not run ft: $!";

	    &convert();
	    &append();
	    $val=&File::runOk('ft');
	}
    } else {
	print STDERR "$rft::inpname could not be written so ft will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file ft.inp

=back

=cut

sub writeInput{
    my($dataDirec,$int);

    $dataDirec=$GLOBE::direc;
    if(&File::openWrite($rft::inpname)){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	&OS::openFile(">$rft::inpname") || 
             die "Could not open $rft::inpname: $!";
	print GLOBE::FILE "$dataDirec"."$fname".".asq\n";
	print GLOBE::FILE "$GLOBE::ftError\n";
	printf GLOBE::FILE "%d,%.1f,%.8f\n",$GLOBE::ftNumPoint,$GLOBE::ftMaxR,
	                             $GLOBE::ftNumDensity;
	close(GLOBE::FILE) || die "Could not close $rft::inpname: $!";
    }
}

1;				# returns true if loaded into another program
