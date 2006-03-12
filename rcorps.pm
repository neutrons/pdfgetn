####################
# Created by Peter Peterson on February 4, 1999
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

    rcorps.pm - version 1

=head1 OVERVIEW of rcorps

    This is the wrapper for sepdprep. It is intended to operate in two  modes: 

=over 4

=item main()

    Called when the user wants to run corps and alter  the input values 
    specific to corps. Meant specifically for command line interface.

=item run()

    Called when the user just wants to run corps and not change any of the 
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

=item $GLOBE::canAbsorpCS

    Isotopically averaged value of absorption cross section.

=item $GLOBE::canAtomDensity

    Container atomic number density.

=item $GLOBE::canCorrect

    (T)rue to calculate container corrections or (F)alse not to.

=item $GLOBE::canScattCS

    Isotopically averaged value of scattering cross section.

=item $GLOBE::canWallThick

    Container wall thickness.

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::effSampleDensity

    The effective density of the sample.

=item @GLOBE::element

    An array of the atomic symbols of the the sample material.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken, must be one of the 
    supported values.

=item $GLOBE::minProcessOutputCorps

    Zero for minimum process output from corps and greater than zero 
    otherwise.

=item $GLOBE::numBanksMissCorps

    The number of banks to miss in corps. If greater than one the banks need 
    to be specified.

=item @GLOBE::runFile

    The name of the data file.

=item %GLOBE::samAtomAbsorpCS

    The isotopically averaged value of absorption cross section of a 
    given element. Only elements in the sample material need to be 
    known.

=item %GLOBE::samAtomCoherCS

    The isotopically averaged value of coherent cross section of a 
    given element. Only elements in the sample material need to be 
    known.

=item %GLOBE::samAtomIncoherCS

    The isotopically averaged value of incoherent cross section of a 
    given element. Only elements in the sample material need to be 
    known.

=item %GLOBE::samAtomMass

    The atomic mass of a given element. Only elements in the sample 
    material need to be known.

=item %GLOBE::samAtomNumOf

    The atomic number of a given element. Only elements in the sample 
    material need to be known.

=item $GLOBE::samCorrect

    (T)rue to calculate sample corrections or (F)alse not to.

=item $GLOBE::samHeight

    Height of the sample in beam.

=item $GLOBE::samRadius

    Radius of the vanadium in beam.

=item $GLOBE::temp

    The temperature at which the run was made.

=item $GLOBE::vanCorrect

    (T)rue to calculate vanadium corrections or (F)alse not to.

=item $GLOBE::vanHeight

    Height of the vanadium in beam.

=item $GLOBE::vanRadius

    Radius of the vanadium in beam.

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";

use strict;
package rcorps;
my($filename);

$rcorps::inpname="cor.inp";
$rcorps::logname="corps.log";
$filename="";

#####
=pod

=item string check()

    Checks all of the variables (except filenames) necessary to write
    the input file for CORPS. Returns a string of errors if
    unsuccessful and 'corps' if successful.

=cut

sub check{
    my($return)='';
    my($start)="\nMust specify";
    my($edit)="(Edit all)";
    my($temp);

    unless(&body::cInt($GLOBE::numBanksMissCorps)){
	$return.="$start numBanksMiss for corps $edit.";
    }elsif($GLOBE::numBanksMissCorps>0){
	$return.="\nnumBanksMiss nonzero is not supported for this version $edit."
    }
    unless(&body::cInt($GLOBE::minProcessOutputCorps)){
	$return.="$start minProcOut for corps $edit.";
    }

    unless(($GLOBE::vanCorrect eq 'T')||($GLOBE::vanCorrect eq 'F')){
	$return.="$start runCorrection for vanadium as T or F $edit.";
    }
    if($GLOBE::vanCorrect=~/T/){
	unless(&body::cFloat($GLOBE::vanHeight)){
	    $return.="$start vanadium height as a number.";
	}
	unless(&body::cFloat($GLOBE::vanRadius)){
	    $return.="$start vanadium radius as a number.";
	}
    }

    unless(($GLOBE::samCorrect eq 'T')||($GLOBE::samCorrect eq 'F')){
	$return.="$start runCorrection for sample as T or F $edit.";
    }
    if($GLOBE::samCorrect=~/T/){
	unless(&body::cFloat($GLOBE::samHeight)){
	    $return.="$start sample height as a number.";
	}
	unless(&body::cFloat($GLOBE::samRadius)){
	    $return.="$start sample radius as a number.";
	}
	unless(&body::cFloat($GLOBE::effSampleDensity)){
	    $return.="$start sample effective density as a number.";
	}
	unless(&body::cInt($GLOBE::numElements)){
	    $return.="$start an integer number of elements $edit.";
	}else{
	    unless($GLOBE::numElements>0){
		$return.="$start the material.";
	    }
	}
	for( my $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	    my($ele)="$GLOBE::element[$int]";
	    unless(&body::cFloat($GLOBE::samAtomNumOf{$ele})){
		unless($return=~/atomic number of elements/){
		    $return.="$start atomic number of elements as a number.";
		}
	    }
	    unless(&body::cFloat($GLOBE::samAtomMass{$ele})){
		unless($return=~/atomic mass/){
		    $return.="$start atomic mass as a number.";
		}
	    }
	    unless(&body::cFloat($GLOBE::samAtomCoherCS{$ele})){
		unless($return=~/coherent cross section/){
		    $return.="$start coherent cross section as a number.";
		}
	    }
	    unless(&body::cFloat($GLOBE::samAtomIncoherCS{$ele})){
		unless($return=~/incoherent cross section/){
		    $return.="$start incoherent cross section as a number.";
		}
	    }
	    unless(&body::cFloat($GLOBE::samAtomAbsorpCS{$ele})){
		unless($return=~/absorption cross section/){
		    $return.="$start absorption cross section as a number.";
		}
	    }
	}
    }

    unless(($GLOBE::canCorrect eq 'T')||($GLOBE::canCorrect eq 'F')){
	$return.="$start runCorrection for container as T or F $edit.";
    }
    if($GLOBE::canCorrect=~/T/){
	unless(&body::cFloat($GLOBE::canWallThick)){
	    $return.="$start container wall thickness as a number.";
	}
	unless(&body::cFloat($GLOBE::canAtomDensity)){
	    $return.="$start container atomDensity as a number $edit.";
	}
	unless(&body::cFloat($GLOBE::canScattCS)){
	    $return.="$start container scattCS as a number $edit.";
	}
	unless(&body::cFloat($GLOBE::canAbsorpCS)){
	    $return.="$start container absorpCS as a number $edit.";
	}
    }

    ($return='corps')unless($return);
    return $return;
}

#####
=pod

=item void clean()

    Removes files created and needed by CORPS.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rcorps::logname);
    &File::remove($rcorps::inpname);
    &File::remove($filename.".cor");
}

#####
=pod

=item int run(void)

    Runs corps and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {return &writeInput();}

    my $fname=(split /,/,$GLOBE::runFile)[0];
    $filename=$fname.".cor";
    if(&writeInput()){
	unless(&File::remove($filename)){       # remove previous corfile 
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rcorps::logname)){
	    open(LOGFILE,">$rcorps::logname") || 
                die "Could not open $rcorps::logname: $!";
	    $rcorps::date=&Stdio::getDate();
	    print LOGFILE "******************** $rcorps::date\n";
	    close(LOGFILE) || die "Could not close $rcorps::logname: $!";
	    (print STDOUT "running corps\n")unless($xpdf::exist);

	    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/corps");
	    system ("$runcmd < $rcorps::inpname >> $rcorps::logname") == 0
                   or die "Could not run corps: $!";

	    $val=&File::runOk('corps');
	}
    } else {
	print STDERR 
              "$rcorps::inpname could not be written so corps will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file cor.inp.

=back

=cut

sub writeInput{
    my($outfile,$int);
    &File::openWrite($rcorps::inpname);
    &OS::openFile(">$rcorps::inpname") || 
         die "Could not open $rcorps::inpname: $!";

    my $fname=(split /,/,$GLOBE::runFile)[0];
    print GLOBE::FILE "$GLOBE::machineName\n";
    print GLOBE::FILE "$fname.int\n";
    print GLOBE::FILE "$GLOBE::numBanksMissCorps,$GLOBE::minProcessOutputCorps\n";
    if($GLOBE::numBanksMissCorps>0){
	# add a line in the input file
    }
    $outfile=$GLOBE::direc.$fname.".cor";
    print GLOBE::FILE "$outfile\n";
    print GLOBE::FILE "$GLOBE::vanCorrect\n";
    if($GLOBE::vanCorrect eq "T"){
	print GLOBE::FILE "$GLOBE::vanHeight,$GLOBE::vanRadius\n";
    }
    print GLOBE::FILE "$GLOBE::samCorrect\n";
    if($GLOBE::samCorrect eq "T"){
	print GLOBE::FILE "$GLOBE::samHeight,$GLOBE::samRadius,";
	print GLOBE::FILE "$GLOBE::effSampleDensity,";
	printf GLOBE::FILE "%.1f\n",$GLOBE::temp;
	for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	    print GLOBE::FILE "$GLOBE::samAtomNumOf{$GLOBE::element[$int]},";
	    print GLOBE::FILE "$GLOBE::samAtomMass{$GLOBE::element[$int]},";
	    print GLOBE::FILE "$GLOBE::samAtomCoherCS{$GLOBE::element[$int]},";
	    print GLOBE::FILE "$GLOBE::samAtomIncoherCS{$GLOBE::element[$int]},";
	    print GLOBE::FILE "$GLOBE::samAtomAbsorpCS{$GLOBE::element[$int]}\n";
	}
	print GLOBE::FILE "0.0\n";  # ends atomic information
	print GLOBE::FILE "$GLOBE::canCorrect\n";
	if($GLOBE::canCorrect eq "T"){
	    print GLOBE::FILE "$GLOBE::canWallThick,$GLOBE::canAtomDensity,";
	    print GLOBE::FILE "$GLOBE::canScattCS,$GLOBE::canAbsorpCS\n";
	}
    }
    close(GLOBE::FILE) || die "Could not close $rcorps::inpname: $!";
}

#####
=pod

=item void convert(void)

    Just a dummy here ..

=back

=cut

sub convert{
    return 1;
}

1;				# if loaded into another perl script this 
				# returns true
