####################
# Created by Peter Peterson on March 4, 1999
# Last Modified on January 13, 2000
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

    rdamp.pm - version 1

=head1 OVERVIEW of rdamp

    This is the wrapper for damp. It is intended to operate in two modes:

=over 4

=item main()

    Called when the user wants to run damp and alter the input values 
    specific to damp.

=item run()

    Called when the user wants to run damp and not change any of the input 
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

=item $GLOBE::dampAx

    The first parameter in the modifying eqn: S(Q)=AX*S(Q)+BX+CX*Q

=item $GLOBE::dampBx

    The second parameter in the modifying eqn: S(Q)=AX*S(Q)+BX+CX*Q

=item $GLOBE::dampCx

    The third parameter in the modifying eqn: S(Q)=AX*S(Q)+BX+CX*Q

=item $GLOBE::dampFunc

    Defines the damping function to be used. The number should be 1, 
    2, or 4.

=item $GLOBE::dampQmax

    The maximum Q to be processed.

=item $GLOBE::dampQmin

    The minimum Q to be processed.

=item $GLOBE::dampQstart

    The Q to start damping at.

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken, must be one of the 
    supported values.

=item @GLOBE::runFile

    The name of the data file(s).

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";

use strict;
package rdamp;
my($filename);

$rdamp::inpname="damp.inp";
$rdamp::logname="damp.log";


#####
=pod

=item int append(void)

    This prepends the history file to the ascii version of the asq file and 
    renames the asq file to have the asq extension.

=cut

sub append{
    my $line;
    my $outfile="$filename";
    my $val=0;
    my $fname=(split /,/,$GLOBE::runFile)[0];
    my $tempfile="$fname.tmp";
    $outfile=~s/\Q.asq\E//;
    $outfile="$outfile".".sq";
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
    the input file for DAMP. Returns a string of errors if
    unsuccessful and 'damp' if successful.

=cut

sub check{
    my($return)='';
    my($start)="\nMust specify";
    my($edit)="(Edit all)";
    my($temp);

    unless((&body::cFloat($GLOBE::dampAx))&&(&body::cFloat($GLOBE::dampBx))&&(&body::cFloat($GLOBE::dampCx))){
	$return.="$start modEqn as 'number*S(Q) +number +number*Q' $edit.";
    }
    unless(&body::cFloat($GLOBE::dampQmin)){
	$return.="$start combined Qmin as a number.";
    }
    unless(&body::cFloat($GLOBE::dampQmax)){
	$return.="$start combined Qmax as a number.";
    }
    unless(&body::cInt($GLOBE::dampFunc)){
	$return.="$start dampFuncType as a positive integer $edit.";
    }else{
	unless(&body::cFloat($GLOBE::dampQstart)){
	    $return.="$start combined Qdamp as a number.";
	}elsif($GLOBE::dampFunc==0){
	    $GLOBE::dampQstart="$GLOBE::dampQmax";
	}elsif($GLOBE::dampFunc<0){
	    $return.="$start dampFuncType as a positive integer $edit.";
	}else{
	    unless(($GLOBE::dampQstart>=$GLOBE::dampQmin)&&
                   ($GLOBE::dampQstart<=$GLOBE::dampQmax)){
		$return.="$start combined must be between Qmin and Qmax\n";
	    }
	}
    }
    unless(&body::cInt($GLOBE::dampExtraToZero)){
	$return.="$start dampExtraToZero as an integer $edit.";
    }

    ($return='damp')unless($return);
    return $return;
}

#####
=pod

=item void clean()

    Removes files created and used by the program DAMP.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rdamp::logname);
    &File::remove($rdamp::inpname);
    &File::remove($filename.".asq");
}

#####
=pod

=item void convert(void)

    Converts the bin file produced by damp (.asq) into an ascii file (.sq)
    which can be plotted by kuplot.

=cut

sub convert{
    my $outfile=$filename;
    $outfile=~s/\Q.asq\E//;
    $outfile="$outfile".".sq";

    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/soq2asc");
    system ("$runcmd $filename $outfile $GLOBE::dampAveMin") == 0
	or die "Could not run soq2asc: $!";
}

#####
=pod

=item int run(void)

    Runs damp and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {return &writeInput();}

    if(&writeInput()){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	$filename=$fname.".asq";
	unless(&File::remove($filename)){       # remove previous asq file
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rdamp::logname)){
	    open(LOGFILE,">$rdamp::logname") || 
                die "Could not open $rdamp::logname: $!";
	    $rdamp::date=&Stdio::getDate();
	    print LOGFILE "******************** $rdamp::date\n";
	    close(LOGFILE) || die "Could not close $rdamp::logname: $!";
	    (print STDOUT "running damp\n")unless($xpdf::exist);

	    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/damp");
            system ("$runcmd < $rdamp::inpname >> $rdamp::logname") == 0
                   or die "Could not run damp: $!";

	    &convert();
	    &append();
	    $val=&File::runOk('damp');
	}
    } else {
	print STDERR 
              "$rdamp::inpname could not be written so damp will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file damp.inp

=back

=cut

sub writeInput{
    my($dataDirec,$int);

    $dataDirec=$GLOBE::direc;
    if(&File::openWrite($rdamp::inpname)){
	my $fname=(split /,/,$GLOBE::runFile)[0];
	&OS::openFile(">$rdamp::inpname") || 
             die "Could not open $rdamp::inpname: $!";
	print  GLOBE::FILE "$dataDirec"."$fname".".bld\n";
	printf GLOBE::FILE "%.4f,%.4f,%.4f\n",$GLOBE::dampAx,
                                       $GLOBE::dampBx,$GLOBE::dampCx;
	printf GLOBE::FILE "1,%.2f,%.2f\n",$GLOBE::dampQmin,$GLOBE::dampQmax;
	printf GLOBE::FILE "%.2f,%d\n",$GLOBE::dampQstart,$GLOBE::dampFunc;
	print  GLOBE::FILE "$GLOBE::dampExtraToZero\n";
	close(GLOBE::FILE) || die "Could not close $rdamp::inpname: $!";
    }
}

1;				# returns true if loaded into another program
