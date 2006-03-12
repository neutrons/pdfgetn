####################
# Created by Peter Peterson on May 28, 1999
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

    Hist.pm - version 1.3

=head1 OVERVIEW of Hist

    This program is handles the reading and writing of the history
    file as well as updating the temporary history file.  This is the
    only program which knows the different formats of the history
    file. Outside programs are intended to use read(), save(), and
    update() only.

=head2 Included files

    File.pm, Stdio.pm

=head2 Modifier flags

    none

=head1 FUNCTIONS

=over 4

=cut

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Stdio.pm";
require "$GLOBE::binpath/rprepsepd.pm";
require "$GLOBE::binpath/rprepnorm.pm";
require "$GLOBE::binpath/rprepariel.pm";
require "$GLOBE::binpath/rprepgsas.pm";

package Hist;
use strict;

$GLOBE::historyname=".temp.hst";	# name of the temporary history file

#####
=pod

=item void getHistName(void)

    Finds the specified history file or prompts user for the name of a
    history file. This is for use in the original command line version.

=cut

sub getHistName{
    my($val);
    # confirm that a history file was specified
    unless($xpdf::histfile){
        print STDOUT "No history file specified, ";
        $xpdf::histfile=&Stdio::readIn("give name: ");
    }

    chomp($xpdf::histfile);

    # confirm that the history file exists
    if(&File::checkExist($xpdf::histfile)){
        print STDOUT "Found history file: $xpdf::histfile\n";
    } else {
        print STDOUT "$xpdf::histfile does not exist\n";
        exit;
    }

}

#####
=pod

=item int read(filename)

    Called to read a history file. This function fills in the global
    variables with the appropriate variables from the history file.

=cut

sub read{
    my($filename)=@_;
    my($int,$line);

    if(!&File::openRead("$filename")){
	return 0; }
    else {
	open(HFILE,$filename) || die "Could not open $filename: $!";

	##### Read in the history first printing the date and user
	chomp($line=<HFILE>);
	chomp($line=<HFILE>);

	### Table 01: Run information ###
	## line 01: Key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	$GLOBE::samCorrect=(split /\s+/,$line)[3];
	$GLOBE::samCorrect=~s/\QrunCorrection=\E//;

	## line 02: machine and run numbers and smoothing information

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::prepName,$GLOBE::machineName)=(split /\s+/,$line)[0,1];
	$GLOBE::prepName=~s/\Qprep=\E//;
	$GLOBE::machineName=~s/\Qmachine=\E//;

	## Activate iparm field for hipd or gsas ..
	unless($FLAG::noGui) {
	  if ($GLOBE::prepName=~/hipd/ ||
              $GLOBE::prepName=~/gsas/   ) {
	    $body::iparm_f->pack(-fill=>'x',-padx=>2,-pady=>2,-expand=>1);
	  } else {
	    $body::iparm_f->pack('forget');
	  }
	}

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::runFile,$GLOBE::backFile)=(split /\s+/,$line)[0,1];
	$GLOBE::runFile=~s/\Qrun=\E//;
	$GLOBE::backFile=~s/\Qbackground=\E//;

    	chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    	($GLOBE::smoothSam)=(split /\s+/,$line)[0];
    	$GLOBE::smoothSam=~s/\Qsmooth=\E//;
     	($GLOBE::smoothParamSam[0])=(split /\s+/,$line)[1];
     	 $GLOBE::smoothParamSam[0]=~s/\QsmoothParam=\E//;
     	($GLOBE::smoothParamSam[1])=(split /\s+/,$line)[2];
     	($GLOBE::smoothParamSam[2])=(split /\s+/,$line)[3];
     	($GLOBE::backKillThresh)=(split /\s+/,$line)[4];
     	 $GLOBE::backKillThresh=~s/\QbackKillThresh=\E//;
        unless ($GLOBE::backKillThresh) { $GLOBE::backKillThresh = "-1.0"; }

	## line 03: dimensions

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::samRadius,$GLOBE::samHeight)=(split /\s+/,$line)[2,3];
	$GLOBE::samRadius=~s/\Qradius=\E//;
	$GLOBE::samHeight=~s/\Qheight=\E//;

	## line 04: temp and title

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::temp)=(split /\s+/,$line)[0];
	$GLOBE::temp=~s/\Qtemp=\E//;
	$line=~s/temp=$GLOBE::temp\s+runTitle=//;
	$GLOBE::title="$line";
	chomp($line=<HFILE>);  $line=~s/^\s+//;

	### Table 02: Vanadium information ###
	## line 01: Key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::vanCorrect)=(split /\s+/,$line)[2];
	$GLOBE::vanCorrect=~s/\QrunCorrection=\E//;

	## line 02: run numbers and smoothing

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::vanFile,$GLOBE::vBackFile)=(split /\s+/,$line)[0,1];
	$GLOBE::vanFile=~s/\Qrun=\E//;
	$GLOBE::vBackFile=~s/\Qbackground=\E//;

    	chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    	($GLOBE::smoothVan)=(split /\s+/,$line)[0];
    	$GLOBE::smoothVan=~s/\Qsmooth=\E//;
     	($GLOBE::smoothParamVan[0])=(split /\s+/,$line)[1];
     	 $GLOBE::smoothParamVan[0]=~s/\QsmoothParam=\E//;
     	($GLOBE::smoothParamVan[1])=(split /\s+/,$line)[2];
     	($GLOBE::smoothParamVan[2])=(split /\s+/,$line)[3];
     	($GLOBE::vanKillThresh)=(split /\s+/,$line)[4];
     	 $GLOBE::vanKillThresh=~s/\QvanKillThresh=\E//;
        unless ($GLOBE::vanKillThresh) { $GLOBE::vanKillThresh = "4.0"; }
     	($GLOBE::vBackKillThresh)=(split /\s+/,$line)[5];
     	 $GLOBE::vBackKillThresh=~s/\QvBackKillThresh=\E//;
        unless ($GLOBE::vBackKillThresh) { $GLOBE::vBackKillThresh = "-1.0"; }

	## line 03: dimensions

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::vanRadius,$GLOBE::vanHeight)=(split /\s+/,$line)[2,3];
	$GLOBE::vanRadius=~s/\Qradius=\E//;
	$GLOBE::vanHeight=~s/\Qheight=\E//;
	chomp($line=<HFILE>); $line=~s/^\s+//;

	### Table 03: Container information ###
	## line 01: Key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::canCorrect)=(split /\s+/,$line)[2];
	$GLOBE::canCorrect=~s/\QrunCorrection=\E//;

	## line 02: run numbers

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::canFile,$GLOBE::cBackFile)=(split /\s+/,$line)[0,1];
	$GLOBE::canFile=~s/\Qrun=\E//;
	$GLOBE::cBackFile=~s/\Qbackground=\E//;

    	chomp($line=<Hist::HFILE>); $line=~s/^\s+//;
    	($GLOBE::smoothCan)=(split /\s+/,$line)[0];
    	$GLOBE::smoothCan=~s/\Qsmooth=\E//;
     	($GLOBE::smoothParamCan[0])=(split /\s+/,$line)[1];
     	 $GLOBE::smoothParamCan[0]=~s/\QsmoothParam=\E//;
     	($GLOBE::smoothParamCan[1])=(split /\s+/,$line)[2];
     	($GLOBE::smoothParamCan[2])=(split /\s+/,$line)[3];
     	($GLOBE::cBackKillThresh)=(split /\s+/,$line)[4];
     	 $GLOBE::cBackKillThresh=~s/\QcBackKillThresh=\E//;
        unless ($GLOBE::cBackKillThresh) { $GLOBE::cBackKillThresh = "-1.0"; }

	## line 03: dimensions

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::canWallThick,$GLOBE::canAtomDensity)=(split /\s+/,$line)[0,1];
	$GLOBE::canWallThick=~s/\QwallThick=\E//;
	$GLOBE::canAtomDensity=~s/\QatomDensity=\E//;

	## line 04: scattering information

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::canScattCS,$GLOBE::canAbsorpCS)=(split /\s+/,$line)[2,3];
	$GLOBE::canScattCS=~s/\QscattCS=\E//;
	$GLOBE::canAbsorpCS=~s/\QabsorpCS=\E//;
	chomp($line=<HFILE>);   $line=~s/^\s+//;

	### Table 04: Sample information ###
	## line 01: Key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::numElements)=(split /\s+/,$line)[3];
	$GLOBE::numElements=~s/\QnumElements=\E//;
	($GLOBE::laue)=(split /\s+/,$line)[4];
	$GLOBE::laue=~s/\QNormLaue=\E//;

	## line 02: table head

	chomp($line=<HFILE>); $line=~s/^\s+//;

	## line 03-??: Information in table

	for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	 chomp($line=<HFILE>); $line=~s/^\s+//;
	 ($GLOBE::element[$int])=(split /\s+/,$line)[0];
	 $GLOBE::samAtomNumOf{$GLOBE::element[$int]}=(split /\s+/,$line)[1];
	 $GLOBE::samAtomMass{$GLOBE::element[$int]}=(split /\s+/,$line)[2];
	 $GLOBE::samAtomCoherCS{$GLOBE::element[$int]}=(split /\s+/,$line)[3];
	 $GLOBE::samAtomIncoherCS{$GLOBE::element[$int]}=(split /\s+/,$line)[4];
	 $GLOBE::samAtomAbsorpCS{$GLOBE::element[$int]}=(split /\s+/,$line)[5];
	}

	if (($GLOBE::numElements > 0) && (!$GLOBE::laue)) {
	  &table::getLaue();
	}

	## line ??+1: powder density

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::sampleDensity,$GLOBE::effSampleDensity)=
                                         (split /\s+/,$line)[0,1];
	$GLOBE::sampleDensity=~s/\Qdensity=\E//;
	$GLOBE::effSampleDensity=~s/\QeffDensity=\E//;
	chomp($line=<HFILE>);        # seperate tables
	
	### Table 05: Bank information ###
	## line 01: Key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::numBankProcess,$GLOBE::blendDQ,
         $GLOBE::blendMatchBref,$GLOBE::blendMatchScal,
         $GLOBE::blendMatchOffset)=(split /\s+/,$line)[1,2,3,4,5];
	$GLOBE::numBankProcess=~s/\QBanks=\E//;
	$GLOBE::blendDQ=~s/\QdeltaQ=\E//;
        unless ($GLOBE::blendDQ) { $GLOBE::blendDQ = 0.02; }
	$GLOBE::blendMatchBref=~s/\QmatchRef=\E//;
        unless ($GLOBE::blendMatchBref) { $GLOBE::blendMatchBref=0;}
	$GLOBE::blendMatchScal=~s/\QmatchScal=\E//;
        unless ($GLOBE::blendMatchScal) { $GLOBE::blendMatchScal="T";}
	$GLOBE::blendMatchOffset=~s/\QmatchOffset=\E//;
        unless ($GLOBE::blendMatchOffset) { $GLOBE::blendMatchOffset="T";}

	## line 02: table head

	chomp($line=<HFILE>);

	# line 03-??: table information

	for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	    chomp($line=<HFILE>); $line=~s/^\s+//;
	    $GLOBE::bank[$int]=(split /\s+/,$line)[0];
	    $GLOBE::angle[$int]=(split /\s+/,$line)[1];
	    $GLOBE::blendQmin[$int]=(split /\s+/,$line)[2];
	    $GLOBE::blendQmax[$int]=(split /\s+/,$line)[3];

	    $GLOBE::cb[$int]=0;
	}
	chomp($line=<HFILE>);

	### Table 06: Program specific information ###
	## line 01: Key values

	chomp($line=<HFILE>);

	## line 02.0: Ft key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::ftError)=(split /\s+/,$line)[2];
	$GLOBE::ftError=~s/\QcalcError=\E//;

	## line 02.1: Ft information

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::ftNumPoint,$GLOBE::ftMaxR,$GLOBE::ftNumDensity,
	 $GLOBE::ftIntMaxR)=(split /\s+/,$line)[0,1,2,3];
	$GLOBE::ftNumPoint=~s/\QnumRpoints=\E//;
	$GLOBE::ftMaxR=~s/\QmaxR=\E//;
	$GLOBE::ftNumDensity=~s/\QnumDensity=\E//;
	unless($GLOBE::ftNumDensity) { $GLOBE::ftNumDensity="0.0"; }
	$GLOBE::ftIntMaxR=~s/\QintMaxR=\E//;
	unless($GLOBE::ftIntMaxR) { $GLOBE::ftIntMaxR="1.5"; }

	## line 03.0: Damp key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::dampQmin,$GLOBE::dampQmax,$GLOBE::dampQstart,
         $GLOBE::dampAveMin)=(split /\s+/,$line)[2,3,4,5];
	$GLOBE::dampQmin=~s/\QQmin=\E//;
	$GLOBE::dampQmax=~s/\QQmax=\E//;
	$GLOBE::dampQstart=~s/\QstartDampQ=\E//;
	if ($GLOBE::dampAveMin) {
	  $GLOBE::dampAveMin=~s/\QQAveMin=\E//;
        } else {
	  $GLOBE::dampAveMin=0.60;
        }

	## line 03.1: damp function information

	chomp($line=<HFILE>); $line=~s/^\s+//;
	my @sline=split(/\s+/,$line);
	($GLOBE::dampFunc)=$sline[0];
	$GLOBE::dampFunc=~s/\QdampFuncType=\E//;
	$GLOBE::dampAx= $sline[1];
	$GLOBE::dampBx= $sline[2];
	$GLOBE::dampCx= $sline[3];
	$GLOBE::dampAx=~s/\QmodEqn=\E//;
	$GLOBE::dampAx=~s/\Q*S(Q)\E//;
	$GLOBE::dampBx=~s/\Q+\E//;
	$GLOBE::dampCx=~s/\Q+\E//;
	$GLOBE::dampCx=~s/\Q*Q\E//;

	if ($#sline==4) {
	($GLOBE::dampExtraToZero)=$sline[4];
	$GLOBE::dampExtraToZero=~s/\QdampExtraToZero=\E//;
	} else {
	    $GLOBE::dampExtraToZero=0.0;
	}

	## line 04: Blend key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::numBankBlend)=(split /\s+/,$line)[2];
	$GLOBE::numBankBlend=~s/\QnumBanks=\E//;
	$line=~s/\Q## Blend\E//;
	$line=~s/\s+numBanks=$GLOBE::numBankBlend\s+banks=//;
	for( $int=0 ; $int<$GLOBE::numBankBlend ; $int++ ){
	    $GLOBE::blendBank[$int]=(split /,/,$line)[$int];
	}
	for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	    my $bank;
	    $GLOBE::cb[$int]=0;
	    foreach $bank (@GLOBE::blendBank){
		if($GLOBE::bank[$int]==$bank){
		    $GLOBE::cb[$int]=1;
		}
	    }
	}
	chomp($line=<HFILE>); $line=~s/^\s+//;
        if ($line=~/soqCorrFile=/) {
	  $GLOBE::soqCorrFile=$line;
	  $GLOBE::soqCorrFile=~s/\QsoqCorrFile=\E//;
	  chomp($line=<HFILE>); $line=~s/^\s+//;
        }

	## line 05.0: Soqd key values

	($GLOBE::minProcessOutputSoqd)=(split /\s+/,$line)[2];
	$GLOBE::minProcessOutputSoqd=~s/\QminProcOut=\E//;

	## line 05.1: correction information

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::samPlazcek,$GLOBE::vanPlazcek)=(split /\s+/,$line)[0,1];
	($GLOBE::dataSmoothSoqd,$GLOBE::dataModSoqd)=(split /\s+/,$line)[2,3];
	$GLOBE::samPlazcek=~s/\QsamPlazcek=\E//;
	$GLOBE::vanPlazcek=~s/\QvanPlazcek=\E//;
	$GLOBE::samPlazcek=~s/\QsamPlaczek=\E//;
	$GLOBE::vanPlazcek=~s/\QvanPlaczek=\E//;
	if ($GLOBE::samPlazcek eq 'F') {$GLOBE::samPlazcek=0;}
	if ($GLOBE::samPlazcek eq 'T') {$GLOBE::samPlazcek=1;}
	if ($GLOBE::vanPlazcek eq 'F') {$GLOBE::vanPlazcek=0;}
	if ($GLOBE::vanPlazcek eq 'T') {$GLOBE::vanPlazcek=1;}
	$GLOBE::dataSmoothSoqd=~s/\QsmoothData=\E//;
	$GLOBE::dataModSoqd=~s/\QmodifyData=\E//;

	if($GLOBE::dataModSoqd>1){

	  ## line 05.2 bank-by-bank a
	  chomp($line=<HFILE>); $line=~s/^\Qa=\E//;
	  @GLOBE::soqdA=(split ',',$line);

	  ## line 05.3 bank-by-bank b
	  chomp($line=<HFILE>); $line=~s/^\Qb=\E//;
	  @GLOBE::soqdB=(split ',',$line);
	  
	}

	## line 06: Corps key values

	chomp($line=<HFILE>); $line=~s/^\s+//;
	($GLOBE::minProcessOutputCorps)=(split /\s+/,$line)[2];
	$GLOBE::minProcessOutputCorps=~s/\QminProcOut=\E//;
	($GLOBE::numBanksMissCorps)=(split /\s+/,$line)[3];
	$GLOBE::numBanksMissCorps=~s/\QnumBanksMiss=\E//;
	chomp($line=<HFILE>);    $line=~s/^\s+//;

	### Table 07: Get the prep program information ###

        if ($GLOBE::prepName =~ /norm/) {
	    &rprepnorm::readhist();
	}
	elsif ($GLOBE::prepName =~ /ariel/) {
	    &rprepariel::readhist();
        }
	elsif ($GLOBE::prepName =~ /sepd/) {
	    &rprepsepd::readhist();
        }
	elsif ($GLOBE::prepName =~ /hipd/) {
	    &rprepgsas::readhistHIPD();
        }
	elsif ($GLOBE::prepName =~ /gsas/) {
	    &rprepgsas::readhist();
        }
	close HFILE;

        unless ($FLAG::noGui > 0) {&Plot::button_update();}
    }

    if(&update()){ return 1; } else { return 0; }

}


#####
=pod

=item int save(filename)

    Called to save the history file. Will use the appropriate format. Returns
    true if successful.

=cut

sub save{
    my($val,$int,$date);
    my($filename)=@_;

    if(&File::openWrite("$filename")){
	open(HFILE,">$filename") || die "Could not open $filename: $!";

	##### Format for the history file to be written with
	$date=&Stdio::getDate();
	print HFILE "History written: $date\n";
	print HFILE "produced by $GLOBE::user\n";

	### Table 01: Run information

	## line 01: Key values
	print HFILE "##### Run Information     ";
	print HFILE "runCorrection=$GLOBE::samCorrect\n";

	## line 02: prep name, machine and run file
	$GLOBE::machineName =~ s/\ //g;
	print HFILE "prep=$GLOBE::prepName     ";
	print HFILE "machine=$GLOBE::machineName\n";
	print HFILE "run=$GLOBE::runFile     ";
	print HFILE "background=$GLOBE::backFile\n";
        print HFILE "smooth=$GLOBE::smoothSam    ";
        print HFILE "smoothParam=$GLOBE::smoothParamSam[0] ";
        print HFILE "$GLOBE::smoothParamSam[1] ";
        print HFILE "$GLOBE::smoothParamSam[2]    ";
        print HFILE "backKillThresh=$GLOBE::backKillThresh\n";

	## line 03: dimensions
	print HFILE "in beam: radius=$GLOBE::samRadius     ";
	print HFILE "height=$GLOBE::samHeight\n";

	## line 04: temp and title
	print HFILE "temp=$GLOBE::temp     runTitle=$GLOBE::title\n";
	print HFILE "\n";          # seperate tables

	### Table 02: Vanadium information

	## line 01: Key values
	print HFILE "##### Vanadium     runCorrection=$GLOBE::vanCorrect\n";

	## line 02: run numbers
	print HFILE "run=$GLOBE::vanFile     background=$GLOBE::vBackFile\n";
        print HFILE "smooth=$GLOBE::smoothVan    ";
        print HFILE "smoothParam=$GLOBE::smoothParamVan[0] ";
        print HFILE "$GLOBE::smoothParamVan[1] ";
        print HFILE "$GLOBE::smoothParamVan[2]    ";
        print HFILE "vanKillThresh=$GLOBE::vanKillThresh    ";
        print HFILE "vBackKillThresh=$GLOBE::vBackKillThresh\n";

	## line 03: dimensions
	print HFILE "in beam: radius=$GLOBE::vanRadius     ";
	print HFILE "height=$GLOBE::vanHeight\n";
	print HFILE "\n";          # seperate tables

	### Table 03: Container information

	## line 01: Key values
	print HFILE "##### Container     runCorrection=$GLOBE::canCorrect\n";

	## line 02: run numbers
	print HFILE "run=$GLOBE::canFile     background=$GLOBE::cBackFile\n";
        print HFILE "smooth=$GLOBE::smoothCan    ";
        print HFILE "smoothParam=$GLOBE::smoothParamCan[0] ";
        print HFILE "$GLOBE::smoothParamCan[1] ";
        print HFILE "$GLOBE::smoothParamCan[2]    ";
        print HFILE "cBackKillThresh=$GLOBE::cBackKillThresh\n";

	## line 03: dimensions
	print HFILE "wallThick=$GLOBE::canWallThick     ";
	print HFILE "atomDensity=$GLOBE::canAtomDensity\n";

	## line 04: scattering information
	print HFILE "atomic information: scattCS=$GLOBE::canScattCS     ";
	print HFILE "absorpCS=$GLOBE::canAbsorpCS\n";
	print HFILE "\n";          # seperate tables

	### Table 04: Sample information

	## line 01: Key values
	print HFILE "##### Sample Material     ";
	print HFILE "numElements=$GLOBE::numElements     ";
	print HFILE "NormLaue=$GLOBE::laue\n";

	## line 02: table head
	print HFILE "Element relAtomNum atomMass ";
	print HFILE "atomCoherCS atomIncoherCS atomAbsorpCS\n";

	## line 03-??: Information in table
	for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	    printf HFILE "  %2s    ", $GLOBE::element[$int];
	    printf HFILE "%10.4f ", $GLOBE::samAtomNumOf{$GLOBE::element[$int]};
	    printf HFILE "%8.3f ", $GLOBE::samAtomMass{$GLOBE::element[$int]};
	    printf HFILE "%11.4f ", 
                   $GLOBE::samAtomCoherCS{$GLOBE::element[$int]};
	    printf HFILE "%13.4f ", 
                   $GLOBE::samAtomIncoherCS{$GLOBE::element[$int]};
	    printf HFILE "%12.5f\n", 
                   $GLOBE::samAtomAbsorpCS{$GLOBE::element[$int]};
	}

	## line ??+1: powder density
	print HFILE "density=$GLOBE::sampleDensity     ";
	print HFILE "effDensity=$GLOBE::effSampleDensity\n";
	print HFILE "\n";          # separate tables
	
	### Table 05: Bank information

	## line 01: Key values
	print HFILE "##### Banks=$GLOBE::numBankProcess";
	print HFILE "  deltaQ=$GLOBE::blendDQ";
	print HFILE "  matchRef=$GLOBE::blendMatchBref";
	print HFILE "  matchScal=$GLOBE::blendMatchScal";
	print HFILE "  matchOffset=$GLOBE::blendMatchOffset\n";

	## line 02: table head
	print HFILE "bank   angle   blendQmin   blendQmax       ";
	print HFILE "(0.0 means no info)\n";

	# line 03-??: table information
	for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	    printf HFILE "  %d    ",$GLOBE::bank[$int];
	    printf HFILE "%5.1f ",$GLOBE::angle[$int];
	    printf HFILE "  %9.2f  ",$GLOBE::blendQmin[$int];
	    printf HFILE "  %5.2f   \n",$GLOBE::blendQmax[$int];
	}
	print HFILE "\n";          # seperate tables

	### Table 06: Program specific information

	## line 01: Key values
	print HFILE "##### Program Specific Information\n";

	## line 02.0: Ft key values
	print HFILE "## Ft     calcError=$GLOBE::ftError     ";
	print HFILE "(1 for true, 0 for false)\n";

	## line 02.1: Ft distance information
	print HFILE "numRpoints=$GLOBE::ftNumPoint     ";
	print HFILE "maxR=$GLOBE::ftMaxR     ";
	print HFILE "numDensity=$GLOBE::ftNumDensity   ";
	print HFILE "intMaxR=$GLOBE::ftIntMaxR\n";

	## line 03.0: Damp key values
	print HFILE "## Damp     Qmin=$GLOBE::dampQmin     ";
	print HFILE "Qmax=$GLOBE::dampQmax     ";
	print HFILE "startDampQ=$GLOBE::dampQstart    ";
	print HFILE "QAveMin=$GLOBE::dampAveMin\n";

	## line 03.1: damp function information
	print HFILE "dampFuncType=$GLOBE::dampFunc     ";
	printf HFILE "modEqn=%.4f*S(Q) ",$GLOBE::dampAx;
	printf HFILE "+%.4f ",$GLOBE::dampBx;
	printf HFILE "+%.4f*Q     ",$GLOBE::dampCx;
	printf HFILE "dampExtraToZero=$GLOBE::dampExtraToZero\n";

	## line 04: Blend key values
	print HFILE "## Blend     numBanks=$GLOBE::numBankBlend     banks=";
	for( $int=0 ; $int<$GLOBE::numBankBlend ; $int++ ){
	    print HFILE "$GLOBE::blendBank[$int]";
	    (print HFILE ',')unless($int==($GLOBE::numBankBlend-1));
	}
	print HFILE "\n";
        print HFILE "soqCorrFile=$GLOBE::soqCorrFile\n";

	## line 05.0: Soqd key values
	print HFILE "## Soqd     minProcOut=$GLOBE::minProcessOutputSoqd\n";

	## line 05.1: correction information
	print HFILE "samPlazcek=$GLOBE::samPlazcek     ";
	print HFILE "vanPlazcek=$GLOBE::vanPlazcek     ";
	print HFILE "smoothData=$GLOBE::dataSmoothSoqd     ";
	print HFILE "modifyData=$GLOBE::dataModSoqd\n";

	if($GLOBE::dataModSoqd>1){
	  ## line 05.2 bank-by-bank a
	  print HFILE 'a=';
	  for( my($int)=0 ; $int<$GLOBE::dataModSoqd ; $int++ ){
	    print HFILE "$GLOBE::soqdA[$int]";
	    (print HFILE ',')if($int+1<$GLOBE::dataModSoqd);
	  } print HFILE "\n";

	  ## line 05.3 bank-by-bank b
	  print HFILE 'b=';
	  for( my($int)=0 ; $int<$GLOBE::dataModSoqd ; $int++ ){
	    print HFILE "$GLOBE::soqdB[$int]";
	    (print HFILE ',')if($int+1<$GLOBE::dataModSoqd);
	  } print HFILE "\n";
	}

	## line 06: Corps key values
	print HFILE "## Corps     ";
	print HFILE "minProcOut=$GLOBE::minProcessOutputCorps     ";
	print HFILE "numBanksMiss=$GLOBE::numBanksMissCorps\n";
	print HFILE "\n";          # seperate tables

	### Table 07: Machine specific information

        if ($GLOBE::prepName =~ /norm/) {
	    &rprepnorm::writehist();
	}
	elsif ($GLOBE::prepName =~ /ariel/) {
	    &rprepariel::writehist();
        }
	elsif ($GLOBE::prepName =~ /sepd/) {
	    &rprepsepd::writehist();
        }
	elsif ($GLOBE::prepName =~ /gsas/ ||
	       $GLOBE::prepName =~ /hipd/) {
	    &rprepgsas::writehist();
        }
	close HFILE;

	return 1;
    }
    return 0;
}

#####
=pod

=item int update(void)

    Update the temporary history file using the appropriate format. Returns 
    true if successful.

=cut

sub update{
    my $val=0;

    unless(&File::remove("$GLOBE::historyname")){
	print STDERR "Could not update temporary history\n";
    }
    (&save("$GLOBE::historyname"))&&($val=1);
    ($val==1)&&($FLAG::noTemp=0);

    return $val;
}

1; 	# return true in case another program includes this one
