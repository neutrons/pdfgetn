####################
# Created by Peter Peterson on February 12, 1999
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

    rprepariel.pm - version 1.3

=head1 OVERVIEW of rprepariel

    This is the wrapper for prepariel. It is intended to operate in two modes: 

=over 4

=item run()

    Called when the user just wants to run prepariel and not change any of the 
    input values.

=back

=head2 Assumptions

    The temporary history file for this run already exists. The history file 
    describes a sample. The data is in the current working directory.

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

=item @GLOBE::backAdd

    Array of constants to add to the background.

=item @GLOBE::backMult

    Array of constans to multiply the background by.

=item $GLOBE::backFile

    File name of the background measurement.

=item @GLOBE::bankAdd

    The bank number which is having a constant added to its data and 
    background.

=item @GLOBE::bankMult

    The bank number which is having a constant Multiplied to its data and 
    background.

=item @GLOBE::dataAdd

    This array only needs to be specified if $GLOBE::numBanksAdd > 0. The 
    elements are the value added to the data in each bank specified by 
    @GLOBE::banksAdd.

=item @GLOBE::dataMult

    This array only needs to be specified if $GLOBE::numBanksMult > 0. 
    The elements are the value multiplying the data in each bank specified 
    by @GLOBE::banksMult.

=item $GLOBE::dataType

    Can be either (s)ample, (v)anadium, or (c)anister. At the moment the 
    program only works for sample data.

=item $GLOBE::direc

    The current working directory. This is where the data is presumed to 
    be located and where all the files are written to. The program does 
    not try to go to other directories to find anything at the moment.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken.

=item $GLOBE::prepOutput

    Zero for minimum process output from prepariel and greater than zero 
    otherwise.

=item $GLOBE::numBanksAdd

    The number of banks to apply an addative correction to.

=item $GLOBE::numBanksMiss

    The number of banks to skip, if greater than zero the banks should be 
    specified.

=item $GLOBE::numBanksMult

    The number of banks to apply a multiplicative correction to.

=item $GLOBE::runFile

    The data filename.

=item $GLOBE::vanKillThresh

    Threshhold used for removing Bragg peaks from vanadium data.


=item $GLOBE::backKillThresh

    Threshhold used for removing Bragg peaks from background data.

=item $GLOBE::vBackKillThresh

    Threshhold used for removing Bragg peaks from vanadium background data.

=item $GLOBE::cBackKillThresh

    Threshhold used for removing Bragg peaks from container background data.

=item $GLOBE::fileExt

    File extension for data files.

=back

=head2 Functions

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/Stdio.pm";

use strict;
package rprepariel;
my($filename);

$rprepariel::logname = "prepariel.log";
$rprepariel::inpname = "prep_s.inp";

#####
=pod

=item string check()

    Checks all of the variables (except filenames) necessary to write
    the input file for PREPARIEL. Returns a string of errors if
    unsuccessful and 'prep' if successful.

=cut

sub check{
    my($return)='';
    my($start)="\nMust specify";
    my($edit)="(Edit all)";
    my($temp);

    unless(($GLOBE::dataType=~/s/)||($GLOBE::dataType=~/v/)||($GLOBE::dataType=~/c/)){
	$return.="$start dataType as 's','v', or 'c'.";
    }
    if($GLOBE::dataType=~/s/){
	unless(&body::cInt($GLOBE::smoothSam)){
	    $return.="$start smooth for run as an integer $edit.";
	}
	for( my $int=0 ; $int<3 ; $int++ ){
	    unless(&body::cFloat($GLOBE::smoothParamSam[$int])){
		unless($return=~/smoothing parameters for run/){
		    $return.="$start smoothing paramaters for run as numbers $edit.";
		}
	    }
	}
    }elsif($GLOBE::dataType=~/v/){
	unless(&body::cInt($GLOBE::smoothVan)){
	    $return.="$start smooth for vanadium as an integer $edit.";
	}
	for( my $int=0 ; $int<3 ; $int++ ){
	    unless(&body::cFloat($GLOBE::smoothParamVan[$int])){
		unless($return=~/smoothing parameters for vanadium/){
		    $return.="$start smoothing paramaters for vanadium as numbers $edit.";
		}
	    }
	}
    }elsif($GLOBE::dataType=~/c/){
	unless(&body::cInt($GLOBE::smoothCan)){
	    $return.="$start smooth for container as an integer $edit.";
	}
	for( my $int=0 ; $int<3 ; $int++ ){
	    unless(&body::cFloat($GLOBE::smoothParamCan[$int])){
		unless($return=~/smoothing parameters for container/){
		    $return.="$start smoothing paramaters for container as numbers $edit.";
		}
	    }
	}
    }

    unless(&body::cInt($GLOBE::prepOutput)){
	$return.="$start prepOutput as an integer $edit.";
    }
    unless(&body::cFloat($GLOBE::backKillThresh)){
        $return.="$start backKillThresh as an integer $edit.";
    }
    unless(&body::cFloat($GLOBE::vBackKillThresh)){
        $return.="$start vBackKillThresh as an integer $edit.";
    }
    unless(&body::cFloat($GLOBE::cBackKillThresh)){
        $return.="$start cBackKillThresh as an integer $edit.";
    }
    unless(&body::cFloat($GLOBE::vanKillThresh)){
        $return.="$start vanKillThresh as an integer $edit.";
    }
    unless(&body::cInt($GLOBE::numBanksMiss)){
	$return.="$start numBanksMiss as an integer $edit.";
    }elsif($GLOBE::numBanksMiss>0){
	$return.="\nnumBanksMiss nonzero is not supported for this version $edit.";
    }

    if($GLOBE::dataType=~/s/){
	unless(&body::cInt($GLOBE::numBanksAdd)){
	    $return.="$start numBanksAdd as an integer $edit.";
	}elsif($GLOBE::numBanksAdd>0){
	    for( my $int=0 ; $int<$GLOBE::numBanksAdd ; $int++ ){
		my($bank)="$GLOBE::bankAdd[$int]";
		unless(&body::cInt($bank)){
		    unless($return=~/addBank/){
			$return.="$start addBank as an integer $edit.";
		    }
		}
		unless(&body::cFloat($GLOBE::dataAdd[$int])){
		    unless($return=~/additive data constant/){
			$return.="$start additive data constant as an integer.";
		    }
		}
		unless(&body::cFloat($GLOBE::backAdd[$int])){
		    unless($return=~/additive background constant/){
			$return.="$start additive background constant as an integer.";
		    }
		}
	    }
	}
	unless(&body::cInt($GLOBE::numBanksMult)){
	    $return.="$start numBanksMult as an integer $edit.";
	}elsif($GLOBE::numBanksMult>0){
	    for( my $int=0 ; $int<$GLOBE::numBanksMult ; $int++ ){
		my($bank)="$GLOBE::bankMult[$int]";
		unless(&body::cInt($bank)){
		    unless($return=~/multBank/){
			$return.="$start multBank as an integer $edit.";
		    }
		}
		unless(&body::cFloat($GLOBE::dataMult[$int])){
		    unless($return=~/multiplicitive data constant/){
			$return.="$start multiplicitive data constant as an integer.";
		    }
		}
		unless(&body::cFloat($GLOBE::backMult[$int])){
		    unless($return=~/multiplicitive background constant/){
			$return.="$start multiplicitive background constant as an integer.";
		    }
		}
	    }
	}
    }


    ($return='prep')unless($return);
    return $return;
}

#####
=pod

=item void clean(askQuestions)

    This removes all files used and created by the PREPstep.

=cut

sub clean{
    my($filename)=@_;

    &File::remove($rprepariel::logname);

    $rprepariel::inpname=~s/_[s,v,c]/_s/;
    &File::remove($rprepariel::inpname);
    $rprepariel::inpname=~s/_[s,v,c]/_v/;
    &File::remove($rprepariel::inpname);
    $rprepariel::inpname=~s/_[s,v,c]/_c/;
    &File::remove($rprepariel::inpname);

    &File::remove($filename.".int");
    &File::remove($filename.".ain");
    &File::remove($filename.".braw");
    &File::remove($filename.".bsmo");
    &File::remove($filename.".craw");
    &File::remove($filename.".csmo");
    &File::remove($filename.".vraw");
    &File::remove($filename.".vsmo");
}

#####
=pod

=item int run(void)

    Runs prepariel and redirects output from the screen to the logfile.

=cut

sub run{
    my($val);

    if ($FLAG::debug) {return &writeInput();}

    if(&writeInput()){
	unless(&File::remove($filename)){       # remove previous intfile 
	    print STDERR "$filename could not be removed\n";
	    exit(-1);
	}
	if(&File::openWrite($rprepariel::logname)){
	    open(LOGFILE,">>$rprepariel::logname") || 
                 die "Could not open $rprepariel::logname: $!";
	    $rprepariel::date=&Stdio::getDate();
	    print LOGFILE "******************** $rprepariel::date\n";
	    close(LOGFILE) || die "Could not close $rprepariel::logname: $!";
	    (print STDOUT "running prepariel\n")unless($xpdf::exist);

            my $runcmd=File::Spec->canonpath("$GLOBE::execpath/prepariel");
            system("$runcmd < $rprepariel::inpname >> $rprepariel::logname")==0
		  or die "Could not run prepariel: $!";

	    &convert();
	    $val=&File::runOk('prepariel');
	}
    } else {
	print STDERR 
        "$rprepariel::inpname could not be written so prepariel will not run\n";
	$val=0;
    }
    return $val;
}

#####
=pod

=item void writeInput(void)

    Create the input file sam.inp.

=back

=cut

sub writeInput{
    my($dataDirec,$int,$outfile,@slist,@blist,$snum,$bnum,$smooth,@smoothpar);
    $outfile="";      # leave here so user can't change output file

    $dataDirec=$GLOBE::direc;

    if ($GLOBE::dataType=~/s/) {
        $rprepariel::inpname=~s/_[s,v,c]/_s/;
	@slist=split(/,/,$GLOBE::runFile);
	@blist=split(/,/,$GLOBE::backFile);
	$smooth=$GLOBE::smoothSam;
	$smoothpar[0]=$GLOBE::smoothParamSam[0];
        $smoothpar[1]=$GLOBE::smoothParamSam[1];
	$smoothpar[2]=$GLOBE::smoothParamSam[2];}
    if ($GLOBE::dataType=~/v/) {
        $rprepariel::inpname=~s/_[s,v,c]/_v/;
	@slist=split(/,/,$GLOBE::vanFile);
	@blist=split(/,/,$GLOBE::vBackFile);
	$smooth=$GLOBE::smoothVan;
	$smoothpar[0]=$GLOBE::smoothParamVan[0];
        $smoothpar[1]=$GLOBE::smoothParamVan[1];
	$smoothpar[2]=$GLOBE::smoothParamVan[2];}
    if ($GLOBE::dataType=~/c/) {
        $rprepariel::inpname=~s/_[s,v,c]/_c/;
	@slist=split(/,/,$GLOBE::canFile);
	@blist=split(/,/,$GLOBE::cBackFile);
	$smooth=$GLOBE::smoothCan;
	$smoothpar[0]=$GLOBE::smoothParamCan[0];
        $smoothpar[1]=$GLOBE::smoothParamCan[1];
	$smoothpar[2]=$GLOBE::smoothParamCan[2];}

    $snum=@slist;
    $bnum=@blist;
    if (! $blist[0] =~ /[a-z,A-Z,0-9]/) {$bnum=0;}

    if(&File::openWrite($rprepariel::inpname)){
	&OS::openFile(">$rprepariel::inpname") || 
             die "Could not open $rprepariel::inpname: $!";
	print GLOBE::FILE "$GLOBE::direc\n";
	print GLOBE::FILE "$GLOBE::machineName\n";
	print GLOBE::FILE "$slist[0]\n" ;
	print GLOBE::FILE "$GLOBE::dataType\n";
	print GLOBE::FILE "$snum\n";
	for ($int=1; $int<=$snum; $int++) {
	   print GLOBE::FILE "$slist[$int-1].$GLOBE::fileExt\n";
	}
	print GLOBE::FILE "$bnum\n";
	if ($bnum > 0) {
	   for ($int=1; $int<=$bnum; $int++) {
	      print GLOBE::FILE "$blist[$int-1].$GLOBE::fileExt\n";
	   }
	}
	print GLOBE::FILE "$GLOBE::prepOutput,";
	print GLOBE::FILE "$GLOBE::numBanksMiss,";

	if($GLOBE::dataType=~/s/){
	    print GLOBE::FILE "$GLOBE::numBanksAdd,$GLOBE::numBanksMult\n";
	} else {
	    print GLOBE::FILE "0,0\n";
	}
	if($GLOBE::numBanksMiss>0) {
	    # add a line
	}
	print GLOBE::FILE "$smooth\n";
	if($smooth>0) {
	    print GLOBE::FILE "$smoothpar[0],";
            print GLOBE::FILE "$smoothpar[1],";
	    print GLOBE::FILE "$smoothpar[2]\n";
	}
	if($GLOBE::dataType=~/s/){
	    if($GLOBE::numBanksAdd>0) {
		for( $int=0 ; $int<$GLOBE::numBanksAdd ; $int ++ ){
		    print GLOBE::FILE "$GLOBE::bankAdd[$int],";
		    print GLOBE::FILE "$GLOBE::dataAdd[$int],";
		    print GLOBE::FILE "$GLOBE::backAdd[$int]\n";
		}
	    }
	    if($GLOBE::numBanksMult>0) {
		for( $int=0 ; $int<$GLOBE::numBanksMult ; $int++ ){
		    print GLOBE::FILE "$GLOBE::bankMult[$int],";
		    print GLOBE::FILE "$GLOBE::dataMult[$int],";
		    print GLOBE::FILE "$GLOBE::backMult[$int]\n";
		}
	    }
	}

        if($GLOBE::dataType=~/v/){
            print GLOBE::FILE "$GLOBE::vanKillThresh\n";
        }

        if($bnum > 0) {
            if   ($GLOBE::dataType=~/s/){
                print GLOBE::FILE "$GLOBE::backKillThresh\n";
            }
            elsif($GLOBE::dataType=~/v/){
                print GLOBE::FILE "$GLOBE::vBackKillThresh\n";
            }
            elsif($GLOBE::dataType=~/c/){
                print GLOBE::FILE "$GLOBE::cBackKillThresh\n";
            }
        }

	print GLOBE::FILE "\n";
	close(GLOBE::FILE) || die "Could not close $rprepariel::inpname: $!";
    }
    $filename = "$slist[0].int";

}

#####
=pod

=item void writehist()

    Writes the prep dependent part of the history file.

=cut

sub writehist{
    my ($big, $int);

    ## line 01: Key values
    print Hist::HFILE "##### prepariel     ";
    print Hist::HFILE "prepOutput=$GLOBE::prepOutput     ";
    print Hist::HFILE "numBanksMiss=$GLOBE::numBanksMiss      ";
    print Hist::HFILE "fileExt=$GLOBE::fileExt\n";

    ## line 04: Additive corrections
    print Hist::HFILE "numBanksAdd=$GLOBE::numBanksAdd\n";
    if($GLOBE::numBanksAdd>0){
       print Hist::HFILE "Bank#  addData  addBack\n";
       for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
        if($GLOBE::bankAdd[$int]){
	  printf Hist::HFILE "   %d  ",$GLOBE::bankAdd[$int];
	  printf Hist::HFILE "%10.4f  ",$GLOBE::dataAdd[$int];
	  printf Hist::HFILE "%10.4f\n",$GLOBE::backAdd[$int];
	}
       }
    }

    ## line 05: Multiplicatice corrections
    print Hist::HFILE "numBanksMult=$GLOBE::numBanksMult\n";
    if($GLOBE::numBanksMult>0){
       print Hist::HFILE "Bank#  mulData  mulBack\n";
       for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
        if($GLOBE::bankMult[$int]){
	  printf Hist::HFILE "   %d  ",$GLOBE::bankMult[$int];
	  printf Hist::HFILE "%10.4f  ",$GLOBE::dataMult[$int];
	  printf Hist::HFILE "%10.4f\n",$GLOBE::backMult[$int];
	}
       }
    }
}

#####
=pod

=item void readhist()

    Reads the prep dependent part of the history file.

=cut

sub readhist{
    my ($line, $int);

    ## line 01: Key values
    chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    ($GLOBE::prepOutput)=(split /\s+/,$line)[2];
    $GLOBE::prepOutput=~s/\QprepOutput=\E//;
    ($GLOBE::numBanksMiss)=(split /\s+/,$line)[3];
    $GLOBE::numBanksMiss=~s/\QnumBanksMiss=\E//;
    ($GLOBE::fileExt)=(split /\s+/,$line)[4];
    $GLOBE::fileExt=~s/\QfileExt=\E//;
    unless ($GLOBE::fileExt) {$GLOBE::fileExt="asc";}

    ## line 04: Additive corrections
    chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    ($GLOBE::numBanksAdd)=(split /\s+/,$line)[0];
    $GLOBE::numBanksAdd=~s/\QnumBanksAdd=\E//;
    if($GLOBE::numBanksAdd>0){
	chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
	for( $int=0 ; $int<$GLOBE::numBanksAdd ; $int++ ){
          chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
	  ($GLOBE::bankAdd[$int])=(split /\s+/,$line)[0];
	  ($GLOBE::dataAdd[$int])=(split /\s+/,$line)[1];
	  ($GLOBE::backAdd[$int])=(split /\s+/,$line)[2];
        }
    }
    ## line 05: Multiplicative corrections
    chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    ($GLOBE::numBanksMult)=(split /\s+/,$line)[0];
    $GLOBE::numBanksMult=~s/\QnumBanksMult=\E//;
    if($GLOBE::numBanksMult>0){
	chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
	for( $int=0 ; $int<$GLOBE::numBanksMult ; $int++ ){
          chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
	  ($GLOBE::bankMult[$int])=(split /\s+/,$line)[0];
	  ($GLOBE::dataMult[$int])=(split /\s+/,$line)[1];
	  ($GLOBE::backMult[$int])=(split /\s+/,$line)[2];
        }
    }
}

#####
=pod

=item void convert(void)

    Converts the bin file produced by prepariel (.int) into an ascii file (.ain)
    which can be plotted by kuplot.

=cut

sub convert{
    my $outfile=$filename;
    $outfile=~s/\Q.int\E//;
    $outfile="$outfile".".ain";

    my $runcmd=File::Spec->canonpath("$GLOBE::execpath/int2asc");
    system ("$runcmd $filename $outfile") == 0
           or die "Could not run int2asc: $!";

}

#####
=pod

=item getTitle(file)

    This routine returns the title fron run file $file.

=cut

sub getTitle{
    my ($file)=@_;
    my $dummy;
    my $tag="#C  Title";

    open (RUN, "<$file.$GLOBE::fileExt");
    while (<RUN>) {
      if (/^$tag/) {$dummy=$_; $dummy=~s/^$tag//g; chomp($dummy); last;}
    }
    close (RUN);
    return $dummy;
}

1;
