##############################
# Created by Peter Peterson on April 1, 1999
# Last modified on January 12, 2000
##############################
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

    body

=head1 OVERVIEW of body

    This is the majority of the GUI code that makes up the body of Xpdf. It 
    contains all of the entry fields and buttons as well as the error 
    checking.

=head2 Assumptions

    That the user can click on things.

=head2 Included files

    None.

=head2 Modifier flags

    None. This program is not intended to be run alone.

=head1 TECHNICAL DOCUMENTATION

=head2 Global Variables Used

=over 4

=back

=head2 Functions

=over 4

=cut

use Tk;
use strict;
package body;

my($table_f,$Ele,$AtN,$AtM,$CCS,$ICS,$ACS);
my $plotcommand="$GLOBE::binpath/PDFplot.pl";

#####
=pod

=item void makeFrame(widget)

    Make the body of Xpdf. This is the main function to be called by an 
    outside program.

=cut

sub makeFrame{
    my($widget)=@_;
    my $btext;
    my($startPanel)="$FLAG::startPanel";

    #################### Status bars at bottom of the main window

    ## Place to put <enter> <leave> bound messages in.
    $xpdf::mw->Label(-textvariable=>\$xpdf::status,
                     -font=>$colors::fontM
		     )->pack(-side=>'bottom',-fill=>'x',-expand=>1);

    ## Place to put processing messages. The label is a button that will 
    ## clear the message.
    my $mess_f=$xpdf::mw->Frame->pack(-side=>'bottom',-fill=>'x',-expand=>1);
    $mess_f->Button(-text=>'Message:',-relief=>'flat',
                    -highlightbackground=>"$colors::background",
                    -activebackground=>"$colors::background",
                    -font=>$colors::fontM,
                    -command=>sub{$xpdf::mess=''})
           ->pack(-side=>'left');
    $mess_f->Label(-textvariable=>\$xpdf::mess,
                   -font=>$colors::fontM)->pack(-side=>'left');
    my $stat_f=$xpdf::mw->Frame->pack(-side=>'bottom',-fill=>'x',-expand=>1);
    $stat_f->Label(-textvariable=>\$xpdf::process,
                   -font=>$colors::fontM)->pack(-side=>'top');

    #################### All the help messages are defined here

    my $user_msg ='Name of user who created the output files.';
       $user_msg.=' By default is the user running the program.';

    my $samRun_msg ='Name of file with sample data (without extension).';
       $samRun_msg.=' Use right mouse button to select files from list.';
    my $vanRun_msg ='File name of vanadium data (without extension).';
       $vanRun_msg.=' Use right mouse button to select files from list.';
    my $canRun_msg ='File name of container measurement (without extension).';
       $canRun_msg.=' Use right mouse button to select files from list.';

    my $samBack_msg ='Sample background data file (without extension) !';
       $samBack_msg.=' Use right mouse button to select files from list.';
    my $vanBack_msg ='Vanadium background data file (without extension)! ';
       $vanBack_msg.=' Use right mouse button to select files from list.';
    my $canBack_msg ='Container background data file (without extension)';
       $canBack_msg.=' Use right mouse button to select files from list.';

    my $workDir_msg='Current working directory! ';

    my $measDensity_msg ='Measured powder density is the measured';
       $measDensity_msg.=' mass divided by the container volume.';

    my $effDensity_msg  ="This parameter specifies the effective ";
       $effDensity_msg .="density used in the program corps. ";

    my $temp_msg='Temperature the data was taken at.';
    my $samRad_msg='Radius of sample in beam.';
    my $samHgt_msg='Height of sample in beam.';
    my $vanRad_msg='Radius of vanadium in beam.';
    my $vanHgt_msg='Height of vanadium in beam.';
    my $canThick_msg ='Thickness of sample container. Note that the ';
       $canThick_msg.=' defaults to vanadium containers. If you use a ';
       $canThick_msg.=' different container material EDIT the history file !';

    my $iparm_msg ='This is the name of the instrument parameter ';
       $iparm_msg.='file used to convert TOF to Q. This is currently ';
       $iparm_msg.='used for HIPD and NPDF and GSAS input files.';

    $bf1::msg='The button changes the number of banks. This also ';
    $bf1::msg.='changes the number of rows in the table.';

    $bf2::msg='The angle the detector is at. This should be taken ';
    $bf2::msg.='from the calibration of the machine.';

    $bf3::msg='Minimum momentum transfer for the bank. This number ';
    $bf3::msg.='is used by blend to determine where to begin the portion ';
    $bf3::msg.='of the structure function is used to make the combined ';
    $bf3::msg.='structure function. If you click on the button, a dialog ';
    $bf3::msg.='will appear which allows one to select the Q limit ';
    $bf3::msg.='corresponding to a fixed wavelength range for ALL ';
    $bf3::msg.='banks !';

    $bf4::msg='Maximum momentum transfer for the bank. This number is ';
    $bf4::msg.='used by blend to determine where to end the portion ';
    $bf4::msg.='of the structure function is used to make the combined ';
    $bf4::msg.='structure function. If you click on the button, a dialog ';
    $bf4::msg.='will appear which allows one to select the Q limit ';
    $bf4::msg.='corresponding to a fixed wavelength range for ALL ';
    $bf4::msg.='banks !';

    $bf5::msg='Whether bank should be included in the combined ';
    $bf5::msg.='structure function.';

    $bf6::msg='Constant to add to the data. This is used during prep.';

    $bf7::msg='Constant to add to the background. This is used during prep.';

    $bf8::msg='Factor to multiply the data by. This is used during prep.';

    $bf9::msg='Factor to multiply the background by. This is used during prep.';

    my $damp_msg  ='Qmin: The minimum momentum transfer for the combined';
       $damp_msg .=' structure function.';
       $damp_msg .='Qmax: The maximum momentum transfer for the combined';
       $damp_msg .=' structure function.';
       $damp_msg .='Grid size used to rebin data before merging. The';
       $damp_msg .=' default value is 0.02 A**-1.';
       $damp_msg .= 'This also sets the range and step size for G(r).';
       $damp_msg .= 'Rmax: Maximum R value (in A) for G(r).';
       $damp_msg .= 'Rnum: Number of points to be calculated up to Rmax.';

    my $smooth_msg ='This panel allows the user to select the smoothing ';
       $smooth_msg.='type and parameters. Note that for the sample, only ';
       $smooth_msg.='the background is smoothed, for vanadium and the ';
       $smooth_msg.='container the background (if specified) is subtracted ';
       $smooth_msg.='first and the resulting data are then smoothed. ';

    #################### General information

    my $gen_f=$widget->Frame()->pack(-fill=>'x');
    $body::gen_f=$gen_f;

    ##### Data file header

    my $gen_f2=$gen_f->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                     ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    my $gen_f2top=$gen_f2->Frame()->pack();

    if ($FLAG::debug) {
       $gen_f2top->Label(-text=>'-- DEBUG mode --',-font=>$colors::fontM,
   	              -bg=>"$colors::headerbg",-fg=>"$colors::highlight1")
                 ->pack(-side=>'left');
    }

    $gen_f2top->Label(-text=>"Data files",-font=>$colors::fontH,
	              -bg=>"$colors::headerbg",-fg=>"$colors::headerfg")
              ->pack(-side=>'left');
    $gen_f2top->Label(-text=>" ---- Extension: ",-font=>$colors::fontH,
	              -bg=>"$colors::headerbg")
              ->pack(-side=>'left');
    $gen_f2top->Label(-textvariable=>\$GLOBE::fileExt,-font=>$colors::fontH,
	              -bg=>"$colors::headerbg",-fg=>"$colors::headerfg")
              ->pack(-side=>'left');
    $gen_f2top->Label(-text=>" ---- Format: ",-font=>$colors::fontH,
	              -bg=>"$colors::headerbg")
              ->pack(-side=>'left');
    $gen_f2top->Label(-bg=>"$colors::headerbg",-fg=>"$colors::headerfg",
                      -textvariable=>\$GLOBE::prepName,
                      -font=>$colors::fontH)
              ->pack(-side=>'left');

    if ($FLAG::debug) {
       $gen_f2top->Label(-text=>'-- DEBUG mode --',-font=>$colors::fontH,
   	              -bg=>"$colors::headerbg",-fg=>"$colors::highlight1")
                 ->pack(-side=>'left');
    }

    ##### Data files

    my $gen_f3=$gen_f->Frame(-relief=>'ridge',-borderwidth=>2)
                     ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    ## sample
    my $samRun_f=$gen_f3->Frame->grid(-column=>0,-row=>0,-sticky=>'we');
    &help::click($samRun_f->Label(-text=>'Sample(s):',
                                  -font=>$colors::fontM)
                          ->pack(-side=>'left',-padx=>10),"$samRun_msg");
    my $samRun_e=$samRun_f->Entry(-textvariable=>\$GLOBE::runFile,-width=>16,
                                  -font=>$colors::fontM)
                          ->pack(-side=>'right');
    $samRun_e->bind('<Button-3>',
                    sub{&File::runList('Sample',$GLOBE::runFile,$gen_f3);});
    bind_name($samRun_f,'Sample data file');
    &help::click($samRun_f,"$samRun_msg");

    ## vanadium
    my $vanRun_f=$gen_f3->Frame->grid(-column=>1,-row=>0,-sticky=>'we');
    &help::click($vanRun_f->Label(-text=>'Vanadium:',
                                  -font=>$colors::fontM)
                          ->pack(-side=>'left',-padx=>10),"$vanRun_msg");
    my $vanRun_e=$vanRun_f->Entry(-textvariable=>\$GLOBE::vanFile,-width=>16,
                                  -font=>$colors::fontM)
                          ->pack(-side=>'right');
    $vanRun_e->bind('<Button-3>',
                    sub{&File::runList('Vanadium',$GLOBE::vanFile,$gen_f3);});
    bind_name($vanRun_f,'Vanadium rod data file');
    &help::click($vanRun_f,"$vanRun_msg");

    ## container
    my $canRun_f=$gen_f3->Frame->grid(-column=>2,-row=>0,-sticky=>'we');
    &help::click($canRun_f->Label(-text=>'Container:',
                                  -font=>$colors::fontM)
                          ->pack(-side=>'left',-padx=>10),"$canRun_msg");
    my $canRun_e=$canRun_f->Entry(-textvariable=>\$GLOBE::canFile,-width=>16,
                                  -font=>$colors::fontM)
                          ->pack(-side=>'right');
    $canRun_e->bind('<Button-3>',
                    sub{&File::runList('Container',$GLOBE::canFile,$gen_f3);});
    bind_name($canRun_f,'Container file');
    &help::click($canRun_f,"$canRun_msg");

    ## sample background
    my $samBack_f=$gen_f3->Frame->grid(-column=>0,-row=>1,-sticky=>'we');
    &help::click($samBack_f->Label(-text=>'Sam. backgr.:',
                                   -font=>$colors::fontM)
                           ->pack(-side=>'left',-padx=>10),"$samBack_msg");
    my $samBack_e=$samBack_f->Entry(-textvariable=>\$GLOBE::backFile,-width=>16,
                                    -font=>$colors::fontM)
                            ->pack(-side=>'right');
    $samBack_e->bind('<Button-3>',
        sub{&File::runList('Sample background',$GLOBE::backFile,$gen_f3);});
    bind_name($samBack_f,'Sample background data file');
    &help::click($samBack_f,"$samBack_msg");

    ## vanadium rod background
    my $vanBack_f=$gen_f3->Frame->grid(-column=>1,-row=>1,-sticky=>'we');
    &help::click($vanBack_f->Label(-text=>'Van. backgr.:',
                                   -font=>$colors::fontM)
                           ->pack(-side=>'left',-padx=>10),"$vanBack_msg");
    my $vanBack_e=$vanBack_f->Entry(-textvariable=>\$GLOBE::vBackFile,
                                    -width=>16,-font=>$colors::fontM)
                            ->pack(-side=>'right');
    $vanBack_e->bind('<Button-3>',
        sub{&File::runList('Vanadium background',$GLOBE::vBackFile,$gen_f3);});
    bind_name($vanBack_f,'Vanadium rod background data file');
    &help::click($vanBack_f,"$vanBack_msg");

    ## container background
    my $canBack_f=$gen_f3->Frame->grid(-column=>2,-row=>1,-sticky=>'we');
    &help::click($canBack_f->Label(-text=>'Cont. backgr.:',
                                   -font=>$colors::fontM)
                           ->pack(-side=>'left',-padx=>10),"$canBack_msg");
    my $canBack_e=$canBack_f->Entry(-textvariable=>\$GLOBE::cBackFile,
                                    -width=>16,-font=>$colors::fontM)
                            ->pack(-side=>'right');
    $canBack_e->bind('<Button-3>',
        sub{&File::runList('Container background',$GLOBE::cBackFile,$gen_f3);});
    bind_name($canBack_f,'Container background data file');
    &help::click($canBack_f,"$canBack_msg");

    ## directory
    my $workDir_f=$gen_f->Frame(-relief=>'ridge',-borderwidth=>2)
                        ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    &help::click($workDir_f->Label(-text=>'Working directory:',
                                   -font=>$colors::fontM)
                           ->pack(-side=>'left',-padx=>8),"$workDir_msg");
    &help::click($workDir_f->Label(-textvariable=>\$GLOBE::workdir,
                                   -font=>$colors::fontM)
	                   ->pack(-side=>'left', -fill=>'x'),"$workDir_msg");

    bind_name($workDir_f,'Current working directory');
    &help::click($workDir_f,"$workDir_msg");

    #################### This is the selector frame ...

    my $wd=19;
    my $select_f=$widget->Frame()->pack(-fill=>'x');
    $body::select_f=$select_f;

    my $sa=$select_f->Frame(-relief=>'ridge',-borderwidth=>2,
                            -bg=>"$colors::headerbg")
                    ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);

    $body::ssa=$sa->Button(-text=>'Sample',-bg=>"$colors::headerbg",
                           -relief=>'flat', -fg=>"$colors::headerfg",
                           -width=>$wd, -font=>$colors::fontH,
                           -command=>sub{&body::infoShow('s');})
                  ->pack(-fill=>'x');

    my $ex=$select_f->Frame(-relief=>'ridge',-borderwidth=>2,
                            -bg=>"$colors::headerbg")
                     ->pack(-side=>'left', -fill=>'x',-padx=>2,-pady=>2);
    $body::sex=$ex->Button(-text=>'Experimental',
                           -bg=>"$colors::headerbg",
                           -relief=>'flat',-fg=>"$colors::headerfg",
                           -width=>$wd, -font=>$colors::fontH,
                           -command=>sub{&body::infoShow('e');})
                  ->pack(-fill=>'x');

    my $ba=$select_f->Frame(-relief=>'ridge',-borderwidth=>2,
                            -bg=>"$colors::headerbg")
                    ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
    $body::sba=$ba->Button(-text=>'Detectors',-relief=>'flat',
                           -width=>$wd, -font=>$colors::fontH,
                           -bg=>"$colors::headerbg",-fg=>"$colors::headerfg",
                           -command=>sub{&body::infoShow('b');})
                  ->pack(-fill=>'x');

    my $pl=$select_f->Frame(-relief=>'ridge',-borderwidth=>2,
                            -bg=>"$colors::headerbg")
                    ->pack(-fill=>'x',-padx=>2,-pady=>2);
    $body::spl=$pl->Button(-text=>'Plotting',-relief=>'flat',
                           -width=>$wd, -font=>$colors::fontH,
                           -bg=>"$colors::headerbg",-fg=>"$colors::headerfg",
                           -command=>sub{&body::infoShow('p');})
                  ->pack(-fill=>'x');


    #################### Sample Details

    my $sample_f=$widget->Frame()->pack(-fill=>'x');
    $body::sample_f=$sample_f;

    ##### Frame of density

    my $sample_f2=$sample_f->Frame(-relief=>'ridge',-borderwidth=>2)
                           ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    ## measured powder density
    my $measDensity_f=$sample_f2->Frame()->pack(-side=>'left');
    &help::click($measDensity_f,"$measDensity_msg");
    &help::click($measDensity_f->Label(-text=>'Sample powder density:',
                                       -font=>$colors::fontM
			       )->pack(-side=>'left'),"$measDensity_msg");
    &help::click($measDensity_f->Entry(-width=>7,-font=>$colors::fontM,
		 -textvariable=>\$GLOBE::sampleDensity)
                               ->pack(-padx=>3,-side=>'right'),
		 "$measDensity_msg");
    &bind_name($measDensity_f,'Measured powder density');

    ## effective powder density
    my $effDensity_f=$sample_f2->Frame()->pack(-side=>'right');
    &help::click($effDensity_f,"$effDensity_msg");
    &help::click($effDensity_f->Label(-text=>'Sample effective density:',
                                      -font=>$colors::fontM)
                              ->pack(-side=>'left'),"$effDensity_msg");
    &help::click($effDensity_f->Entry(-width=>12,-font=>$colors::fontM,
		 -textvariable=>\$GLOBE::effSampleDensity)
                 ->pack(-padx=>3,-side=>'left'),"$effDensity_msg");
    &bind_name($effDensity_f,'Effective powder density');

    $effDensity_f->Button(-command=>sub{&body::autoNormalize();},
                          -font=>$colors::fontM,
                          -text=>'Automatic normalization')->pack(-side=>'left');

    ## material table
    &makeTable_f($sample_f);       # add table of element information

    #################### Experiment Details

    my $exp_f=$widget->Frame()->pack(-fill=>'x');
    $body::exp_f=$exp_f;

    ##### head has ouput filename and username

    my $exp_f1=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2)
                     ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    my $exp_f1top=$exp_f1->Frame()->pack(-side=>'top',-fill=>'x');

    ## name of user who analysed the data
    my $user_f=$exp_f1top->Frame->pack(-side=>'right');
    $GLOBE::user=getlogin() || getpwuid($<);
    chomp($GLOBE::user);
    &help::click($user_f->Label(-text=>'User:',-font=>$colors::fontM)
                        ->pack(-side=>'left'),"$user_msg");
    &help::click($user_f->Entry(-textvariable=>\$GLOBE::user,
	                        -font=>$colors::fontM,-width=>10)
                        ->pack(-side=>'right'),"$user_msg");
    bind_name($user_f,'Name of who created files');
    &help::click($user_f,"$user_msg");

    ## Title
    my $title_f=$exp_f1top->Frame->pack(-side=>'left');
    $title_f->Label(-text=>'Run Title:', -width=>15,
                    -font=>$colors::fontM)->pack(-side=>'left');
    $title_f->Entry(-textvariable=>\$GLOBE::title,-width=>50,
                    -font=>$colors::fontM)
            ->pack(-side=>'left');

    ## machine name and run title

    my $exp_f1bot=$exp_f1->Frame()->pack(-side=>'top',-fill=>'x');
    my $machine_f=$exp_f1bot->Frame->pack(-side=>'top',-fill=>'x');
    $machine_f->Label(-text=>'Instrument:', -width=>15, -font=>$colors::fontM)
              ->pack(-side=>'left');
    $machine_f->Entry(-textvariable=>\$GLOBE::machineName,-width=>15,
                      -font=>$colors::fontM)
              ->pack(-side=>'left');

    ##### Corrections
    my $exp_f2a=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2)
                      ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    $exp_f2a->Label(-text=>'Apply Plazcek correction to: ',
                    -font=>$colors::fontM)
            ->pack(-side=>'left', -padx=>15);
    $exp_f2a->Checkbutton(-variable=>\$GLOBE::samPlazcek,
             -selectcolor=>"$colors::selectcol",
             -font=>$colors::fontM,
             -text=>'Sample run(s)')->pack(-side=>'left');
    $exp_f2a->Checkbutton(-variable=>\$GLOBE::vanPlazcek,
             -selectcolor=>"$colors::selectcol",
             -font=>$colors::fontM,
             -text=>'Vanadium run(s)')->pack(-side=>'left');

    ##### Experimental details
    my $exp_f2=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2)
                     ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    $exp_f2->Label(-text=>'Sample in beam (cm) - ',-font=>$colors::fontM)
           ->grid(-row=>0, -column=>0, -sticky=>'e');
    ## sample radius of sample in beam
    &help::click($exp_f2->Label(-text=>'Radius:',-font=>$colors::fontM)
                        ->grid(-row=>0, -column=>1),"$samRad_msg");
    &help::click($exp_f2->Entry(-width=>10,-textvariable=>\$GLOBE::samRadius,
                                -font=>$colors::fontM)
	                ->grid(-row=>0, -column=>2),"$samRad_msg");

    ## height of sample in beam
    &help::click($exp_f2->Label(-text=>'Height:',-font=>$colors::fontM)
                        ->grid(-row=>0, -column=>3),"$samHgt_msg");
    &help::click($exp_f2->Entry(-width=>10,-textvariable=>\$GLOBE::samHeight,
                                -font=>$colors::fontM)
	                ->grid(-row=>0, -column=>4),"$samHgt_msg");

    ## temperature 
    &help::click($exp_f2->Label(-text=>'Temperature:',-font=>$colors::fontM)
                        ->grid(-row=>0, -column=>5),"$temp_msg");
    &help::click($exp_f2->Entry(-width=>6,-textvariable=>\$GLOBE::temp,
                                -font=>$colors::fontM)
			->grid(-row=>0, -column=>6),"$temp_msg");

    $exp_f2->Label(-text=>'Vanadium in beam (cm) - ',-font=>$colors::fontM)
           ->grid(-row=>1, -column=>0);

    ## vanadium radius of sample in beam
    &help::click($exp_f2->Label(-text=>'Radius:',-font=>$colors::fontM)
                        ->grid(-row=>1, -column=>1),"$vanRad_msg");
    &help::click($exp_f2->Entry(-width=>10,-textvariable=>\$GLOBE::vanRadius,
                                -font=>$colors::fontM)
	                ->grid(-row=>1, -column=>2),"$vanRad_msg");

    ## height of vanadium in beam
    &help::click($exp_f2->Label(-text=>'Height:',-font=>$colors::fontM)
                        ->grid(-row=>1, -column=>3),"$vanHgt_msg");
    &help::click($exp_f2->Entry(-width=>10,-textvariable=>\$GLOBE::vanHeight,
                                -font=>$colors::fontM)
	                ->grid(-row=>1, -column=>4),"$vanHgt_msg");

    $exp_f2->Label(-text=>'Container in beam (cm) - ',-font=>$colors::fontM)
           ->grid(-row=>2, -column=>0);

    ## wall thickness
    &help::click($exp_f2->Label(-text=>'Thickness:',-font=>$colors::fontM)
                        ->grid(-row=>2, -column=>1),"$canThick_msg");
    &help::click($exp_f2->Entry(-width=>10,-textvariable=>\$GLOBE::canWallThick,
                                -font=>$colors::fontM)
	                ->grid(-row=>2, -column=>2),"$canThick_msg");
    $exp_f2->Label(-text=>
     'Edit history file for NONE vanadium cont. !',
                   -font=>$colors::fontM,-fg=>"$colors::headerfg")
      ->grid(-row=>2, -column=>3, -columnspan=>4, -padx=>5);

    &bind_name($exp_f2,'Dimensions in beam and measurement temperature');
    
    ## Instrument parameter file (for prep=hipd or prep=gsas)

    $body::iparm_f=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2)
                         ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    $body::iparm_f->Label(-text=>'Instrument parameter file:',
                          -font=>$colors::fontM)
                  ->pack(-side=>'left',-padx=>10);
    my $iparm_e=
       $body::iparm_f->Entry(-textvariable=>\$GLOBE::instParamFile,
                             -font=>$colors::fontM,-width=>35)
                     ->pack(-side=>'left');
    $iparm_e->bind('<Button-3>',
                    sub{&File::getFile($GLOBE::instParamFile,$iparm_e,
                                       "Instrument parameter file","*.iparm");});
    $body::iparm_f->pack('forget');

    ##### Smoothing details
    my $exp_f3h=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2, 
                            -bg=>"$colors::headerbg")
                      ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    $exp_f3h->Label(-text=>'Smoothing parameters :',-font=>$colors::fontH, 
                    -fg=>"$colors::headerfg", -bg=>"$colors::headerbg")->pack();

    ## smoothing part
    my $exp_f3=$exp_f->Frame(-relief=>'ridge',-borderwidth=>2)
                     ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

    $exp_f3->Label(-text=>'Sample backgr.',-font=>$colors::fontM)
           ->grid(-row=>1, -column=>0, -sticky=>'e', -padx=>15);
    
    my $exp_f3a = $exp_f3->Frame()->grid(-row=>1, -column=>2, -sticky=>'w');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothSam, 
                          -text=>'None',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'0')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothSam, 
                          -text=>'2nd order poly.',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'1')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothSam, 
                          -text=>'Savitzky-Golay',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'2')->pack(-side=>'left');

    $exp_f3->Label(-text=>' - Param. :', -anchor=>'w',-font=>$colors::fontM)
           ->grid(-row=>1, -column=>3);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamSam[0],
                   -font=>$colors::fontM)
           ->grid(-row=>1, -column=>4);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamSam[1],
                   -font=>$colors::fontM)
           ->grid(-row=>1, -column=>5);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamSam[2],
                   -font=>$colors::fontM)
           ->grid(-row=>1, -column=>6);
    
    $exp_f3->Label(-text=>'Vanadium', -font=>$colors::fontM)
           ->grid(-row=>2, -column=>0, -sticky=>'e', -padx=>15);
    
    my $exp_f3a = $exp_f3->Frame()->grid(-row=>2, -column=>2, -sticky=>'w');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothVan, 
                          -text=>'None',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'0')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothVan, 
                          -text=>'2nd order poly.',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'1')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothVan, 
                          -text=>'Savitzky-Golay',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'2')->pack(-side=>'left');

    $exp_f3->Label(-text=>' - Param. :', -anchor=>'w',-font=>$colors::fontM)
           ->grid(-row=>2, -column=>3);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamVan[0],
                   -font=>$colors::fontM)
           ->grid(-row=>2, -column=>4);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamVan[1],
                   -font=>$colors::fontM)
           ->grid(-row=>2, -column=>5);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamVan[2],
                   -font=>$colors::fontM)
           ->grid(-row=>2, -column=>6);
    
    $exp_f3->Label(-text=>'Container',-font=>$colors::fontM)
           ->grid(-row=>3, -column=>0, -sticky=>'e', -padx=>15);
    
    my $exp_f3a = $exp_f3->Frame()->grid(-row=>3, -column=>2, -sticky=>'w');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothCan, 
                          -text=>'None',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'0')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothCan, 
                          -text=>'2nd order poly.',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'1')->pack(-side=>'left');
    $exp_f3a->Radiobutton(-variable=>\$GLOBE::smoothCan, 
                          -text=>'Savitzky-Golay',-font=>$colors::fontM,
                          -selectcolor=>"$colors::selectcol",
                          -value=>'2')->pack(-side=>'left');

    $exp_f3->Label(-text=>' - Param. :', -anchor=>'w',-font=>$colors::fontM)
           ->grid(-row=>3, -column=>3);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamCan[0],
                   -font=>$colors::fontM)
           ->grid(-row=>3, -column=>4);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamCan[1],
                   -font=>$colors::fontM)
           ->grid(-row=>3, -column=>5);
    $exp_f3->Entry(-width=>4,-textvariable=>\$GLOBE::smoothParamCan[2],
                   -font=>$colors::fontM)
           ->grid(-row=>3, -column=>6);
    
    &help::click($exp_f3,"$smooth_msg");
    &bind_name($exp_f3,'Smoothing parameters for data processing');

    #################### Detector Banks

    my $bank_f=$widget->Frame()->pack(-fill=>'x');
    $body::bank_f=$bank_f;

    ##### Head has column labels
    my $bank_ftab=$bank_f->Frame(-relief=>'ridge',-borderwidth=>2)
                         ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    $body::bank_ftab=$bank_ftab;

    ## button to change number of banks, column is bank number
    my $bf1=$bank_ftab->Button(-text=>"Bank\nNumber",-height=>2,-width=>9,
                               -bg=>"$colors::headerbg",
                               -font=>$colors::fontM,
			       -command=>sub{&bankDialog($bank_ftab);})
		      ->grid(-column=>0,-row=>0,-sticky=>'w',-pady=>2);

    &help::click($bf1,"$bf1::msg");
    bind_name($bf1,'Change the number of banks');

    ## angle bank is at
    my $bf2=$bank_ftab->Label(-text=>'Angle',-font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>8)
                      ->grid(-column=>1,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf2,'Angle the detector is at');
    &help::click($bf2,"$bf2::msg");

    ## Qmin for blend
    my $bf3=$bank_ftab->Button(-text=>'Qmin', -height=>2, -width=>8,
                               -bg=>"$colors::headerbg",-font=>$colors::fontM,
                               -command=>sub{&qlimitDialog($bank_ftab,'min');})
		      ->grid(-column=>2,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf3,'Minimum momentum transfer for blend');
    &help::click($bf3,"$bf3::msg");

    ## Qmax for blend
    my $bf4=$bank_ftab->Button(-text=>'Qmax', -height=>2, -width=>8,
                               -bg=>"$colors::headerbg",-font=>$colors::fontM,
                               -command=>sub{&qlimitDialog($bank_ftab,'max');})
		      ->grid(-column=>3,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf4,'Maximum momentum transfer for blend');
    &help::click($bf4,"$bf4::msg");

    ## should bank be blended
    my $bf5=$bank_ftab->Label(-text=>"Include\nBank",-font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>7)
                      ->grid(-column=>4,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf5,'Include bank in blend');
    &help::click($bf5,"$bf5::msg");

    ## constant to add to data
    my $bf6=$bank_ftab->Label(-text=>"Add\n   Data   ",-font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>11)
		      ->grid(-column=>5,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf6,'Constant to add to data');
    &help::click($bf6,"$bf6::msg");

    ## constant to add to background
    my $bf7=$bank_ftab->Label(-text=>"Add\nBackground",-font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>11)
		      ->grid(-column=>6,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf7,'Constant to add to background');
    &help::click($bf7,"$bf7::msg");

    ## constant to multiply data
    my $bf8=$bank_ftab->Label(-text=>"Multiply\n   Data   ",
                              -font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>11)
		      ->grid(-column=>7,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf8,'Factor to multiply data');
    &help::click($bf8,"$bf8::msg");

    ## contant to multiply background
    my $bf9=$bank_ftab->Label(-text=>"Multiply\nBackground",
                              -font=>$colors::fontM,
                              -bg=>"$colors::headerbg",-width=>11)
	              ->grid(-column=>8,-row=>0,-pady=>2,-sticky=>'nsew');
    bind_name($bf9,'Factor to multiply background');
    &help::click($bf9,"$bf9::msg");
    ## fill in columns
    &makeBankTable($bank_ftab);

    ## damp stuff
    my $bank_f2=$bank_f->Frame(-relief=>'ridge',-borderwidth=>2)
                       ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    &help::click($bank_f2,"$damp_msg");

    my $bank_f3=$bank_f2->Frame()->pack(-side=>'left');

    $bank_f3->Label(-text=>'Combined S(q) data: ',
                    -font=>$colors::fontM)
            ->grid(-column=>0, -row=>0, -sticky=>'w');

    $bank_f3->Label(-text=>'Qmin:',-font=>$colors::fontM)
            ->grid(-column=>1, -row=>0, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::dampQmin,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>2, -row=>0, -sticky=>'w');

    $bank_f3->Label(-text=>'Qmax:',-font=>$colors::fontM)
            ->grid(-column=>3, -row=>0, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::dampQmax,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>4, -row=>0, -sticky=>'w');

    $bank_f3->Label(-text=>'deltaQ:',-font=>$colors::fontM)
            ->grid(-column=>5, -row=>0, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::blendDQ,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>6, -row=>0, -sticky=>'w');

    $bank_f3->Label(-text=>'Autoscale to bank (0=off):',
                    -font=>$colors::fontM)
            ->grid(-column=>7, -row=>0, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::blendMatchBref,-width=>4,
                    -font=>$colors::fontM)
            ->grid(-column=>8, -row=>0, -sticky=>'w');

    ## ft stuff
    $bank_f3->Label(-text=>'Resulting G(r): ',-font=>$colors::fontM)
            ->grid(-column=>0, -row=>1, -sticky=>'w');

    $bank_f3->Label(-text=>'Rmax:',-font=>$colors::fontM)
            ->grid(-column=>1, -row=>1, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::ftMaxR,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>2, -row=>1, -sticky=>'w');

    $bank_f3->Label(-text=>'Points:',-font=>$colors::fontM)
            ->grid(-column=>3, -row=>1, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::ftNumPoint,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>4, -row=>1, -sticky=>'w');

    $bank_f3->Label(-text=>'rho0:',-font=>$colors::fontM)
            ->grid(-column=>5, -row=>1, -sticky=>'w');
    $bank_f3->Entry(-textvariable=>\$GLOBE::ftNumDensity,-width=>8,
                    -font=>$colors::fontM)
            ->grid(-column=>6, -row=>1, -sticky=>'w');
    $bank_f3->Label(-text=>'(to calculate rho(r) and N(r))',
                    -font=>$colors::fontM)
            ->grid(-column=>7, -columnspan=>1, -row=>1, -sticky=>'w');

    ## Correction file input
    my $bank_f4=$bank_f->Frame(-relief=>'ridge',-borderwidth=>2)
                       ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);
    my $corr_e=$bank_f4->Entry(-textvariable=>\$GLOBE::soqCorrFile,
                               -font=>$colors::fontM,-width=>65)
                       ->pack(-side=>'right');
    $corr_e->bind('<Button-3>',
                    sub{&File::getFile($GLOBE::soqCorrFile,$corr_e,
                                       "Correction file","*.fix");});
    $bank_f4->Label(-text=>'External corrections file:',
                          -font=>$colors::fontM)
                  ->pack(-side=>'right',-padx=>10);

    #################### Plotting panel (former PDFplot.pl ..)

    $body::plot_f=$widget->Frame()->pack(-fill=>'x');
    &Plot::plotPanel($body::plot_f);

    #################### Buttons to run wrappers

    my $but_f=$widget->Frame()->pack(-fill=>'x');
    $body::but_f=$but_f;

    ##### now we add the buttons
    my $but_f1=$but_f->Frame(-relief=>'ridge',-borderwidth=>2, 
                             -bg=>"$colors::headerbg")
                     ->pack(-fill=>'x',-padx=>2, -pady=>2);

    ## delete all intermediate files
    my $delete_b=$but_f1->Button(-text=>'Delete all',
                                 -font=>$colors::fontM,
                -command=>sub{&Hist::update(); 
                              whichOut('a');
                              &Plot::update_files();
                              $xpdf::mess="Intermediate files deleted ...";})
                ->pack(-side=>'left');

    &help::click($delete_b,'Delete all files created by the fortran code.');
    bind_name($delete_b,'Delete all intermediate files');

    ## run prep 
    $sqs::b=$but_f1->Button(-text=>'Create S(Q)',
                            -font=>$colors::fontM)->pack(-side=>'left');
    $sqs::b->configure(-command=>sub{&body::makeSOQ();});

    my $prephelp ="This command produces the S(Q) file with individual ";
       $prephelp.="banks still separated. First the prep-program for ";
       $prephelp.="the current file format is executes for the sample ";
       $prephelp.="and in case not already processed for the vanadium ";
       $prephelp.="and container runs (NOTE if you change settings ";
       $prephelp.="of vanadium or container parameters, you HAVE to ";
       $prephelp.="execute DELETE ALL first ! \n\n";
       $prephelp.="As a second step CORPS is executed to calculate ";
       $prephelp.="material dependent corrections and finally data ";
       $prephelp.="are corrected using vanadium and the container data ";
       $prephelp.="and S(Q) is generated. ";

    &help::click($sqs::b,$prephelp);
    bind_name($sqs::b,'Produces .int, .ain, .cor, .soq and .sqa file');

    ## run blend
    $sqb::b=$but_f1->Button(-text=>'Blend banks',
                            -font=>$colors::fontM)->pack(-side=>'left');
    $sqb::b->configure(-command=>sub{&body::makeBLEND();});

    my $blendhelp ="This commands blends the individual detector ";
       $blendhelp.="banks together and creates a single S(Q).\n\n";
       $blendhelp.="NOTE: If you change the additive or ";
       $blendhelp.="multiplicative corrections you have to go back ";
       $blendhelp.="all the way to the prep step and produce the ";
       $blendhelp.="initial S(Q) again ! ";

    &help::click($sqb::b,$blendhelp);
    bind_name($sqb::b,'Produces .bld and .sqb files');

    ## run ft
    $gr::b=$but_f1->Button(-text=>'Create G(r)',
                           -font=>$colors::fontM)->pack(-side=>'left');
    $gr::b->configure(-command=>sub{&body::makePDF();});

    my $pdfhelp ="This command finally creates the PDF G(r) as ";
       $pdfhelp.="Fourier transform of I(Q)=Q*[S(Q)-1]. \n\n ";
       $pdfhelp.="If you are only tuning the overall Q-range ";
       $pdfhelp.="than this is the only step to repeat. ";
    &help::click($gr::b,$pdfhelp);
    bind_name($gr::b,'Produces .asq, .sq and .pdf, .gr files');

    ## Here we have a drop down menu for other choices
    my $what;
    my $action_m;

    my $action_b=$but_f1->Button(-text=>'Do it',-width=>7,
                                 -font=>$colors::fontM,
                                 -command=>sub{&body::action($what);})
                        ->pack(-side=>'right');

    if ($xpdf::kuplot) {
      $what='Plot';
      $action_m=$but_f1->Optionmenu(-variable=>\$what, -width=>12,
                -font=>$colors::fontM,
                -options=>['Plot','Print plot','Show logs',
                           'Show inputs','Edit all','Analyze','Optimize'])
                ->pack(-side=>'right');
    } else {
      $what='Show logs';
      $action_m=$but_f1->Optionmenu(-variable=>\$what, -width=>12,
                -font=>$colors::fontM,
                -options=>['Show logs',
                           'Show inputs','Edit all','Analyze','Optimize'])
                ->pack(-side=>'right');
    }

    ## Finally select display 
    &body::infoShow($startPanel);
}

#####
=pod

=item void action()

    This runs commands selected in action menu

=cut

sub action{
	my ($what)=$_[0];

	if    ($what eq 'Plot')        {&Plot::kuplot_plot();}
	if    ($what eq 'Print plot')  {&Plot::kuplot_print();}
	elsif ($what eq 'Show logs')   {$FLAG::fileinp=0, &body::editLog()}
	elsif ($what eq 'Show inputs') {$FLAG::fileinp=1, &body::editLog()}
	elsif ($what eq 'Edit all')    {&body::editHistory()}
	elsif ($what eq 'Analyze')     {&body::startPDFqual()}
	elsif ($what eq 'Optimize')    {&body::startOptimize()}
}

#####
=pod

=item void makeSOQ()

    This runs all required programs to get S(Q), i.e. prep,
    corps and soqd.

=cut

sub makeSOQ{
	my $val;

	&Hist::update();

	## read out the bank information
	&readBank();

	my($fname)=(split /,/,$GLOBE::runFile)[0];
	if($GLOBE::numBankProcess){
	   unless (&errorCheck('scvrq')){return;}
	}else{
	   unless(&errorCheck('scv')){return;}
	}

	## Container if specified
	my $fname=(split /,/,$GLOBE::canFile)[0];
	my $val=body::whichOut('s');
	if ($fname =~/[A-Z,a-z,0-9]/) {
	   $GLOBE::canCorrect='T';
	   unless($val=~/c/){
	      $GLOBE::dataType='c';
	      $xpdf::mess="Producing $fname.int";
	      unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	      if(&body::prep_run()){
    	         $xpdf::mess='Done';
	         unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	      } else {
	         $xpdf::mess="Failed to make $fname.int";
	         unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	         return;
	      }
	   }
	} else {
	   $GLOBE::canCorrect='F';
	}
	
	## Vanadium
	my $fname=(split /,/,$GLOBE::vanFile)[0];
	if ($fname =~/[A-Z,a-z,0-9]/) {
	   $GLOBE::vanCorrect='T';
	   unless($val=~/v/){
	      $GLOBE::dataType='v';
	      $xpdf::mess="Producing $fname.int";
	      unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	      if(&body::prep_run()){
    	         $xpdf::mess='Done';
	         unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	      } else {
	         $xpdf::mess="Failed to make $fname.int";
	         unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	         return;
	      }
	   }
	} else {
	   $GLOBE::vanCorrect='F';
	}
	
	## Sample
	my $fname=(split /,/,$GLOBE::runFile)[0];
	$GLOBE::dataType='s';
	$xpdf::mess="Producing $fname.int";
	unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	if(&body::prep_run()){
    	   $xpdf::mess='Done';
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	} else {
	   $xpdf::mess="Failed to make $fname.int";
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	   return;
	}

	if($GLOBE::numBankProcess){
	   # do nothing
	}else{
	   $xpdf::mess='Intensity file with bank information created';
	   unless($FLAG::noGui>0){$xpdf::mw->update;}
	   unless($FLAG::noGui>0){&Plot::update_files();}
	   return;
	}

	## Run corps
	$xpdf::mess="Producing $fname.cor";
	unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	if(&rcorps::run()){
    	   $xpdf::mess='Done';
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	} else {
	   $xpdf::mess="Failed to make $fname.cor";
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	   return;
	}

	## Run soqd
	$xpdf::mess="Producing $fname.soq";
	unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	if(&rsoqd::run()){
    	   $xpdf::mess='Done';
	   unless ($FLAG::noGui>0) {
	       $sqs::b->configure(-bg=>"$colors::successbg");
	       $sqs::b->configure(-fg=>"$colors::successfg");
	   }
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	   unless ($FLAG::noGui>0) {&Plot::update_files();}
           return 1;
	} else {
	   $xpdf::mess="Failed to make $fname.soq";
	   unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	   return;
	}
}

#####
=pod

=item void makeBLEND()

    This command runs the BLEND step.

=cut

sub makeBLEND{

	&Hist::update();
	my $val;

	$val=whichOut('b');
	&readBank();

	if(&errorCheck('scvrqb')){
	    my $fname=(split /,/,$GLOBE::runFile)[0];
	    (&body::makeSOQ()) unless ($val=~/q/);
	    my $name="$fname.bld and ";
	    $name.="$fname.sqb";
	    if(&File::runOk('soqd')){
		$xpdf::mess="Producing $name";
	        unless ($FLAG::noGui>0) {$xpdf::mw->update;}
		if(&rblend::run()){
		    $xpdf::mess='Done';
	   	    unless ($FLAG::noGui>0) {
		          $sqs::b->configure(-bg=>"$colors::successbg");
    		          $sqs::b->configure(-fg=>"$colors::successfg");
	                  $sqb::b->configure(-bg=>"$colors::successbg");
	                  $sqb::b->configure(-fg=>"$colors::successfg");}
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	            unless ($FLAG::noGui>0) {&Plot::update_files();}
                    return 1;
		} else {
		    $xpdf::mess="Failed to make $name";
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
                    return;
		}
	    }
	}
}
#####
=pod

=item void makePDF()

    This commands cuts (damps) the final S(Q) and calculates
    the Fourier transform G(r).

=cut

sub makePDF{

	&Hist::update();
	my $val;

	$val=whichOut('d');

	if(&errorCheck('scvrqbdf')){

	    ## run damp 
	    my $fname=(split /,/,$GLOBE::runFile)[0];
	    (&body::makeBLEND()) unless ($val=~/b/);
	    my $name="$fname.asq and $fname.sq";
	    if(&File::runOk('blend')){
		$xpdf::mess="Producing $name";
	        unless ($FLAG::noGui>0) {$xpdf::mw->update;}
		if(&rdamp::run()){
		    $xpdf::mess='Done';
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
		} else {
		    $xpdf::mess="Failed to make $name";
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	            return;
		}
	    } else {
		$xpdf::mess="Failed to make $name";
	        unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	        return;
	    }

	    ## run pdf
	    $val=whichOut('f');
	    my $name="$fname.pdf and $fname.gr";
	    if(&File::runOk('damp')){
		$xpdf::mess="Producing $name";
	        unless ($FLAG::noGui>0) {$xpdf::mw->update;}
		if(&rft::run()){
		    $xpdf::mess='Done';
	   	    unless ($FLAG::noGui>0) {
		          $sqs::b->configure(-bg=>"$colors::successbg");
	                  $sqs::b->configure(-fg=>"$colors::successfg");
	                  $sqb::b->configure(-bg=>"$colors::successbg");
	                  $sqb::b->configure(-fg=>"$colors::successfg");
	                  $gr::b->configure(-bg=>"$colors::successbg");
	                  $gr::b->configure(-fg=>"$colors::successfg");}
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	            unless ($FLAG::noGui>0) {&Plot::update_files();}
                    return 1;
		} else {
		    $xpdf::mess="Failed to make $name";
	            unless ($FLAG::noGui>0) {$xpdf::mw->update;}
	            return;
		}
	    }
	}
}
#####
=pod

=item void infoShow(what)

    This command selects the info panel that is displayed. $what can
    be 's' for sample information, 'e' for experimental details and
    'b' for bank information.

=cut

sub infoShow{
    my ($what)=@_;

    $FLAG::startPanel=$what;

    $body::sample_f->pack('forget')   if (&Tk::Exists($body::sample_f));
    $body::exp_f->pack('forget')      if (&Tk::Exists($body::exp_f));
    $body::bank_f->pack('forget')     if (&Tk::Exists($body::bank_f));
    $body::but_f->pack('forget')      if (&Tk::Exists($body::but_f));
    $body::select_f->pack('forget')   if (&Tk::Exists($body::select_f));
    $body::gen_f->pack('forget')      if (&Tk::Exists($body::gen_f));
    $body::plot_f->pack('forget')     if (&Tk::Exists($body::plot_f));

    $body::gen_f->pack(-fill=>'x', -expand=>1)    
                  if (&Tk::Exists($body::gen_f));
    $body::select_f->pack(-fill=>'x', -expand=>1) 
                  if (&Tk::Exists($body::select_f));

    if ($what eq 's') {
	$body::ssa->configure(-fg=>"$colors::headerfg");
	$body::sex->configure(-fg=>'black');
	$body::sba->configure(-fg=>'black');
	$body::spl->configure(-fg=>'black');

        $body::sample_f->pack(-fill=>'x', -expand=>1);
    } 
    elsif ($what eq 'e') {
	$body::ssa->configure(-fg=>'black');
	$body::sex->configure(-fg=>"$colors::headerfg");
	$body::sba->configure(-fg=>'black');
	$body::spl->configure(-fg=>'black');

        $body::exp_f->pack(-fill=>'x', -expand=>1)      
                      if (&Tk::Exists($body::exp_f));
    }
    elsif ($what eq 'b') {
	$body::ssa->configure(-fg=>'black');
	$body::sex->configure(-fg=>'black');
	$body::sba->configure(-fg=>"$colors::headerfg");
	$body::spl->configure(-fg=>'black');

        $body::bank_f->pack(-fill=>'x', -expand=>1)
                       if (&Tk::Exists($body::bank_f));
    }
    elsif ($what eq 'p') {
	$body::ssa->configure(-fg=>'black');
	$body::sex->configure(-fg=>'black');
	$body::sba->configure(-fg=>'black');
	$body::spl->configure(-fg=>"$colors::headerfg");

        $body::plot_f->pack(-fill=>'x', -expand=>1)
                       if (&Tk::Exists($body::plot_f));
	unless($FLAG::noGui>0) {&Plot::update_files();}
    }

    $body::but_f->pack(-fill=>'x', -expand=>1)          
                  if (&Tk::Exists($body::but_f));

}
#####
=pod

=item string whichOut(remove)

    This checks which files exist and deletes all files after and including 
    the file chosen by remove. remove choses the file according to the 
    following key: s-sample, c-container, v-vanadium, r-corps, q-soqd, 
    b-blend, d-damp, f-ft, a-all. The string returned will be which files 
    exist before whichOut() is run.

=cut

sub whichOut{
    my ($remove)=@_;
    my $val='';
    my $color; 

    my $prefix_c=(split /,/,$GLOBE::canFile)[0];
    my $prefix_v=(split /,/,$GLOBE::vanFile)[0];
    my $prefix_s=(split /,/,$GLOBE::runFile)[0];

    ### remove the files that should be removed and change the color of 
    ### buttons if necessary

    unless ($FLAG::noGui>0) {$color=$xpdf::mw->cget(-bg);}
    if($remove=~/a/){
	&prep_clean($prefix_s);
	&prep_clean($prefix_v);
	&prep_clean($prefix_c);
	&rcorps::clean($prefix_s);
	&rsoqd::clean ($prefix_s);
	&rblend::clean($prefix_s);
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$sqs::b->configure(-bg=>"$color");
		 $sqs::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$sqb::b->configure(-bg=>"$color");
		 $sqb::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/s/){
	&prep_clean($prefix_s);
	&rcorps::clean($prefix_s);
	&rsoqd::clean ($prefix_s);
	&rblend::clean($prefix_s);
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$sqs::b->configure(-bg=>"$color");
		 $sqs::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$sqb::b->configure(-bg=>"$color");
		 $sqb::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/r/){
	&rcorps::clean($prefix_s);
	&rsoqd::clean ($prefix_s);
	&rblend::clean($prefix_s);
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$sqs::b->configure(-bg=>"$color");
		 $sqs::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$sqb::b->configure(-bg=>"$color");
		 $sqb::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/q/){
	&rsoqd::clean ($prefix_s);
	&rblend::clean($prefix_s);
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$sqs::b->configure(-bg=>"$color");
		 $sqs::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$sqb::b->configure(-bg=>"$color");
		 $sqb::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/b/){
	&rblend::clean($prefix_s);
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$sqb::b->configure(-bg=>"$color");
		 $sqb::b->configure(-fg=>"$colors::foreground");}
	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/d/){
	&rdamp::clean ($prefix_s);
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }
    if($remove=~/f/){
	&rft::clean   ($prefix_s);

	unless ($FLAG::noGui>0) {$gr::b->configure(-bg=>"$color");
		 $gr::b->configure(-fg=>"$colors::foreground");}
    }

    ## Check for existing files

    if ($FLAG::debug) {
      if(&File::checkExist("prep_s.inp"))    {$val.='scv';}  # sample
      if(&File::checkExist($rcorps::inpname)){$val.='r';}    # corps
      if(&File::checkExist($rsoqd::inpname)) {$val.='q';}    # soqd
      if(&File::checkExist($rblend::inpname)){$val.='b';}    # blend
      if(&File::checkExist($rdamp::inpname)) {$val.='d';}    # damp
      if(&File::checkExist($rft::inpname))   {$val.='f';}    # ft
    } else {
      if(&File::checkExist($prefix_s.".int")){$val.='s';}  # sample
      if(&File::checkExist($prefix_c.".int")){$val.='c';}  # container
      if(&File::checkExist($prefix_v.".int")){$val.='v';}  # vanadium
      if(&File::checkExist($prefix_s.".cor")){$val.='r';}  # corps
      if(&File::checkExist($prefix_s.".soq")){$val.='q';}  # soqd
      if(&File::checkExist($prefix_s.".bld")){$val.='b';}  # blend
      if(&File::checkExist($prefix_s.".asq")){$val.='d';}  # damp
      if(&File::checkExist($prefix_s.".pdf")){$val.='f';}  # ft
    }
    return $val;
}

#####
=pod

=item void bind_name(widget,msg)

    This displaying a msg in the xpdf::status variable which the cursor 
    enters widget and removing it when the cursor leaves.

=cut

sub bind_name{
    my($widget,$msg)=@_;

    $widget->bind('<Enter>', sub{$xpdf::status=$msg;});
    $widget->bind('<Leave>', sub{$xpdf::status='';});
}

#####
=pod

=item void prep_run()

    This routine calls the machine dependent prep program ..

=cut

sub prep_run{
    if ($GLOBE::prepName =~ /sepd/) {
      return &rprepsepd::run();
    }
    elsif ($GLOBE::prepName =~ /norm/) {
      return &rprepnorm::run();
    }
    elsif ($GLOBE::prepName =~ /ariel/) {
      return &rprepariel::run();
    }
    elsif ($GLOBE::prepName =~ /gsas/ ||
           $GLOBE::prepName =~ /hipd/) {
      return &rprepgsas::run();
    }
}

#####
=pod

=item void prep_getTitle(file)

    This routine calls the dialog for possible special inputs
    for the prep step of the data processing.

=cut

sub prep_getTitle{
    my ($file)=@_;

    if ($GLOBE::prepName =~ /sepd/) {
      return &rprepsepd::getTitle($file);
    }
    elsif ($GLOBE::prepName =~ /norm/) {
      return &rprepnorm::getTitle($file);
    }
    elsif ($GLOBE::prepName =~ /ariel/) {
      return &rprepariel::getTitle($file);
    }
    elsif ($GLOBE::prepName =~ /gsas/ ||
           $GLOBE::prepName =~ /hipd/) {
      return &rprepgsas::getTitle($file);
    }
}

#####
=pod

=item subroutine logFile()

    This routine returns the logfile or inputfile name.

=cut

sub logFile{
    my ($what,$linp) = @_;

    ## prep step
    if ($what eq 'prep') {
      if ($linp) {
        if ($GLOBE::prepName =~ /sepd/) {
          return $rprepsepd::inpname;
        }
        elsif ($GLOBE::prepName =~ /norm/) {
          return $rprepnorm::inpname;
        }
        elsif ($GLOBE::prepName =~ /ariel/) {
          return $rprepariel::inpname;
        }
        elsif ($GLOBE::prepName =~ /gsas/ ||
               $GLOBE::prepName =~ /hipd/) {
          return $rprepgsas::inpname;
        }
      } else {
        if ($GLOBE::prepName =~ /sepd/) {
          return $rprepsepd::logname;
        }
        elsif ($GLOBE::prepName =~ /norm/) {
          return $rprepnorm::logname;
        }
        elsif ($GLOBE::prepName =~ /ariel/) {
          return $rprepariel::logname;
        }
        elsif ($GLOBE::prepName =~ /gsas/ ||
               $GLOBE::prepName =~ /hipd/) {
          return $rprepgsas::logname;
        }
      }
    }

    ## corps
    elsif ($what eq 'corps') {
      if ($linp) {
        return $rcorps::inpname;
      } else {
        return $rcorps::logname;
      }
    }

    ## soqd
    elsif ($what eq 'soqd') {
      if ($linp) {
        return $rsoqd::inpname;
      } else {
        return $rsoqd::logname;
      }
    }

    ## blend
    elsif ($what eq 'blend') {
      if ($linp) {
        return $rblend::inpname;
      } else {
        return $rblend::logname;
      }
    }

    ## damp
    elsif ($what eq 'damp') {
      if ($linp) {
        return $rdamp::inpname;
      } else {
        return $rdamp::logname;
      }
    }

    ## ft
    elsif ($what eq 'ft') {
      if ($linp) {
        return $rft::inpname;
      } else {
        return $rft::logname;
      }
    }
}

#####
=pod

=item void prep_clean()

    This routine calls the machine dependent prep clean
    routine ..

=cut

sub prep_clean{
    if ($GLOBE::prepName =~ /sepd/) {
      &rprepsepd::clean($_[0]);
    }
    elsif ($GLOBE::prepName =~ /norm/) {
      &rprepnorm::clean($_[0]);
    }
    elsif ($GLOBE::prepName =~ /ariel/) {
      &rprepariel::clean($_[0]);
    }
    elsif ($GLOBE::prepName =~ /gsas/ ||
           $GLOBE::prepName =~ /hipd/) {
      &rprepgsas::clean($_[0]);
    }
}

#####
=pod

=item void makeBankTable(widget)

    Fill in the columns of the bank table. If the columns have already 
    been filled in destroy the previous version.

=cut

sub makeBankTable{
    my($widget)=@_;
    my($change)=0;
    ($change=1)if($FLAG::startPanel=~/b/);

    &body::infoShow('e') if ($change);

    ##### Destroy old version
    $bank::numb->destroy() if (&Tk::Exists($bank::numb));
    $bank::angl->destroy() if (&Tk::Exists($bank::angl));
    $bank::qmin->destroy() if (&Tk::Exists($bank::qmin));
    $bank::qmax->destroy() if (&Tk::Exists($bank::qmax));
    $bank::incl->destroy() if (&Tk::Exists($bank::incl));
    $bank::addD->destroy() if (&Tk::Exists($bank::addD));
    $bank::addB->destroy() if (&Tk::Exists($bank::addB));
    $bank::mulD->destroy() if (&Tk::Exists($bank::mulD));
    $bank::mulB->destroy() if (&Tk::Exists($bank::mulB));
    

    ##### Frames for data
    ## bank number
    $bank::numb=$widget->Frame()->grid(-column=>0,-row=>1,-sticky=>'nsew');
    &help::click($bank::numb,"$bf1::msg");
    ## angle of bank
    $bank::angl=$widget->Frame()->grid(-column=>1,-row=>1,-sticky=>'nsew');
    &help::click($bank::angl,"$bf2::msg");
    ## qmin for blend
    $bank::qmin=$widget->Frame()->grid(-column=>2,-row=>1,-sticky=>'nsew');
    &help::click($bank::qmin,"$bf3::msg");
    ## qmax for blend
    $bank::qmax=$widget->Frame()->grid(-column=>3,-row=>1,-sticky=>'nsew');
    &help::click($bank::qmax,"$bf4::msg");
    ## should bank be included in blend
    $bank::incl=$widget->Frame()->grid(-column=>4,-row=>1,-sticky=>'nsew');
    &help::click($bank::incl,"$bf5::msg");
    ## constant to add to data
    $bank::addD=$widget->Frame()->grid(-column=>5,-row=>1,-sticky=>'nsew');
    &help::click($bank::addD,"$bf6::msg");
    ## constant to add to background
    $bank::addB=$widget->Frame()->grid(-column=>6,-row=>1,-sticky=>'nsew');
    &help::click($bank::addB,"$bf7::msg");
    ## constant to multiply to data
    $bank::mulD=$widget->Frame()->grid(-column=>7,-row=>1,-sticky=>'nsew');
    &help::click($bank::mulD,"$bf8::msg");
    ## constant to multiply to background
    $bank::mulB=$widget->Frame()->grid(-column=>8,-row=>1,-sticky=>'nsew');
    &help::click($bank::mulB,"$bf9::msg");
    
    ##### Fill in the data
    my($int);
    ## if the bank number is labeled then create a bank number default 
    ## which is larger than any of the previous bank numbers
    for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	unless($GLOBE::bank[$int]){
	    my $max=0;
	    foreach (@GLOBE::bank){
		($max=$_)if($_>$max);
	    }
	    $GLOBE::bank[$int]=$max+1;
	}
    }
    for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	## bank number
	&help::click($bank::numb->Label(-textvariable=>\$GLOBE::bank[$int],
			   -width=>2,-justify=>'right',-font=>$colors::fontM,
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf1::msg");
	## bank angle
	&help::click($bank::angl->Label(
			   -textvariable=>\$GLOBE::angle[$int],
			   -width=>5,-justify=>'right',-font=>$colors::fontM,
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf2::msg");
	## Qmin for blend
	&help::click($bank::qmin->Entry(-width=>4,-justify=>'right',
			  -textvariable=>\$GLOBE::blendQmin[$int],
                          -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf3::msg");
	## Qmax for blend
	&help::click($bank::qmax->Entry(-width=>4,-justify=>'right',
			  -textvariable=>\$GLOBE::blendQmax[$int],
                          -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf4::msg");
	## include bank in blend
	&help::click($bank::incl->Checkbutton(
				 -variable=>\$GLOBE::cb[$int],
                                 -font=>$colors::fontM,
                                 -selectcolor=>"$colors::selectcol")
			        ->pack(-side=>'top',-expand=>1,
					 -fill=>'x',-padx=>2),"$bf5::msg");
	## constant to add to data
	&help::click($bank::addD->Entry(-width=>4,-justify=>'right',
			   -textvariable=>\$GLOBE::dataAdd[$int],
                           -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf6::msg");
	## constant to add to background
	&help::click($bank::addB->Entry(-width=>4,-justify=>'right',
			   -textvariable=>\$GLOBE::backAdd[$int],
                           -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf7::msg");
	## constant to multiply to data
	&help::click($bank::mulD->Entry(-width=>4,-justify=>'right',
			   -textvariable=>\$GLOBE::dataMult[$int],
                           -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf8::msg");
	## constant to multiply to background
	&help::click($bank::mulB->Entry(-width=>4,-justify=>'right',
			   -textvariable=>\$GLOBE::backMult[$int],
                           -font=>$colors::fontM
			   )->pack(-side=>'top',-expand=>1,
				   -fill=>'x',-padx=>2),"$bf9::msg");
    }
    $xpdf::mw->update() if (&Tk::Exists($body::but_f));
    sleep 0.1;
    &body::infoShow('b') if ($change);
}

#####
=pod

=item void makeTable_f(widget)

    Fill in the element table. Most of this is a database lookup after the 
    elements are selected. If the user wishes to use different values they 
    must edit the history file by hand.

=cut

sub makeTable_f{
    my($widget)=@_;
    my($change)=0;
    ($change=1)if($FLAG::startPanel=~/s/);

    &body::infoShow('e') if ($change);

	##### Messages
        my $atomNum_msg ='Once the elements have been selected this';
           $atomNum_msg.=' button will bring a window so the user can';
           $atomNum_msg.=' select the number of the elements in atomic';
           $atomNum_msg.=' formula units.';

        my $element_msg ='This button brings up a window to choose';
           $element_msg.=' the elements which make up the material.';
           $element_msg.=' Once the user clicks done another window';
           $element_msg.=' allows the user to choose the number of the';
           $element_msg.=' element in atomic formula units.';

	$eleTable::msg ='This table contains the information from';
        $eleTable::msg.=' the file neutron.table. This file has the nuclear';
        $eleTable::msg.=' information for the elements isotropically';
        $eleTable::msg.=' averaged according to their natural';
	$eleTable::msg.=' abundencies.\n\n'; 
        $eleTable::msg.='Note that elements with negative neutron'; 
        $eleTable::msg.=' scattering length have a negative coherent'; 
        $eleTable::msg.=' cross section because this is how CORPS'; 
        $eleTable::msg.=' expects the input for those elements.'; 

        my $ele_msg ='The material is composed of these elements. To';
           $ele_msg.=' change left click on the column label.';
	my $atn_msg ='The properly normalized number of each element';
           $atn_msg.=' in atomic formula units. To change either left';
           $atn_msg.=' click on column label, unless disabled, or left';
           $atn_msg.=' click on the Element column header.';

    	##### Destroy old version
    	$table_f->destroy() if (&Tk::Exists($table_f));
	$table_f=$widget->Frame(-relief=>'ridge',-borderwidth=>2)
                        ->pack(-side=>'top',-fill=>'x',-padx=>2,-pady=>2);

	##### Column labels 
	my $tf2=$table_f->Frame()->grid(-column=>1,-row=>0,-sticky=>'nesw'); 
	## number of element in material
	my $atomNum_b=$tf2->Button(-text=>"Number in Atomic\nFormula Unit", 
                                   -font=>$colors::fontM,
		                   -state=>'disabled',-width=>18,
                                   -bg=>"$colors::headerbg",
	                           -command=>sub{&table::makeAmt($table_f);})
                          ->pack(-fill=>'both',-expand=>1,-pady=>2,-padx=>0);
	if($GLOBE::numElements > 0){ 
		$atomNum_b->configure(-state=>'normal'); 
	}
	bind_name($atomNum_b,'Change atomic Number');
	&help::click($atomNum_b,$atomNum_msg);

	my $tf1=$table_f->Frame()->grid(-column=>0,-row=>0,-sticky=>'nsew'); 

	my $element_b=$tf1->Button(-text=>'Elements',-width=>14,
                                   -font=>$colors::fontM,
                                   -bg=>"$colors::headerbg",
	                           -command=>sub{&table::makeEle($table_f); })
                          ->pack(-fill=>'both',-pady=>2,-expand=>1);
	&help::click($element_b,$element_msg);
	bind_name($element_b,'Change material properties');
	&help::click($table_f->Label(-text=>"Atomic Mass",
                                     -font=>$colors::fontM,
                                     -bg=>"$colors::headerbg",
		   -width=>14)->grid(-column=>2,-row=>0,-pady=>2,-sticky=>'nsew'),
                   "$eleTable::msg");
	&help::click($table_f->Label(-text=>"Coherent\nCrossSection",
                                     -bg=>"$colors::headerbg",
                                     -font=>$colors::fontM,
				     -width=>14)->grid(-pady=>2,-column=>3,
				     -row=>0,-sticky=>'nsew'),"$eleTable::msg");
	&help::click($table_f->Label(-text=>"Incoherent\nCross Section", 
                                     -bg=>"$colors::headerbg",
                                     -font=>$colors::fontM,
				     -width=>14)->grid(-pady=>2,-column=>4,-row=>0,
				     -sticky=>'nsew'),"$eleTable::msg");
	&help::click($table_f->Label(-text=>"Absorption\nCross Section", 
                                     -bg=>"$colors::headerbg",
                                     -font=>$colors::fontM,
				     -width=>14)->grid(-pady=>2,-column=>5,-row=>0,
				     -sticky=>'nsew'),"$eleTable::msg"); 
    ##### Frames for data
    ## element names
    $Ele=$table_f->Frame()->grid(-column=>0,-row=>1,-sticky=>'nsew');
    &help::click($Ele,"$eleTable::msg");
    ## number in atomic formula unit
    $AtN=$table_f->Frame()->grid(-column=>1,-row=>1,-sticky=>'nsew');
    &help::click($AtN,"$eleTable::msg");
    ## atomic mass is amu
    $AtM=$table_f->Frame()->grid(-column=>2,-row=>1,-sticky=>'nsew');
    &help::click($AtM,"$eleTable::msg");
    ## coherent cross section
    $CCS=$table_f->Frame()->grid(-column=>3,-row=>1,-sticky=>'nsew');
    &help::click($CCS,"$eleTable::msg");
    ## incorherent cross section
    $ICS=$table_f->Frame()->grid(-column=>4,-row=>1,-sticky=>'nsew');
    &help::click($ICS,"$eleTable::msg");
    ## absorption cross section
    $ACS=$table_f->Frame()->grid(-column=>5,-row=>1,-sticky=>'nsew');
    &help::click($ACS,"$eleTable::msg"); 

    ##### Fill in the data

    my($int); 
    (print STDOUT "getStats() did not work\n")unless(&table::getStats()); 
    for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	## element
	&help::click($Ele->Label(-textvariable=>\$GLOBE::element[$int],
                                 -font=>$colors::fontM
				 )->pack(-side=>'top'),$ele_msg);
	## number in atomic formula unit
	&help::click($AtN->Label(
		   -textvariable=>\$GLOBE::samAtomNumOf{$GLOBE::element[$int]},
		   -justify=>'right',-font=>$colors::fontM)
                         ->pack(-side=>'top'),$atn_msg);
	## atomic mass
	&help::click($AtM->Label(
		    -textvariable=>\$GLOBE::samAtomMass{$GLOBE::element[$int]},
		    -justify=>'right',-font=>$colors::fontM)
                         ->pack(-side=>'top'),"$eleTable::msg");
	## coherent cross section
	&help::click($CCS->Label(
		 -textvariable=>\$GLOBE::samAtomCoherCS{$GLOBE::element[$int]},
		 -justify=>'right',-font=>$colors::fontM)
                         ->pack(-side=>'top'),"$eleTable::msg");
	## incoherent cross section
	&help::click($ICS->Label(
	       -textvariable=>\$GLOBE::samAtomIncoherCS{$GLOBE::element[$int]},
	       -justify=>'right',-font=>$colors::fontM)
                         ->pack(-side=>'top'),"$eleTable::msg");
	## absorption cross section
	&help::click($ACS->Label(
		-textvariable=>\$GLOBE::samAtomAbsorpCS{$GLOBE::element[$int]},
		-justify=>'right',-font=>$colors::fontM)
                         ->pack(-side=>'top'),"$eleTable::msg"); 
    }

    &bind_name($table_f,'Material information');
    $xpdf::mw->update() if (&Tk::Exists($body::but_f));
    sleep 0.1;
    &body::infoShow('s') if ($change);
}

#####
=pod

=item void fillBank(void)

    Set all banks to 'selected' as default ..

=cut

sub fillBank{
    my $int;
#    print STDOUT "fillBank: $GLOBE::numBankBlend ";
    for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
#	$GLOBE::blendBank[$int]="$GLOBE::bank[$int]";
	$GLOBE::cb[$int]=1;
    }
#    $GLOBE::numBankBlend=@GLOBE::blendBank;
#    print STDOUT "-> $GLOBE::numBankBlend \n";
}

#####
=pod

=item void readBank(void)

    Error checks the information in the bank data table and assign values 
    to some variables which are implied by the table.

=cut

sub readBank{
    my($int,$val);
    ## check that both data and background are added, multiplied or not and 
    ## that all banks have the same operation done

#    print STDOUT "in readBank() before $GLOBE::numBanksMult ";

    $GLOBE::numBanksAdd=0;
    $GLOBE::numBanksMult=0;

    my($addTrue)='1';
    my($multTrue)='1';

    for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	my $bank=$GLOBE::bank[$int];
	
	# if any of the additive stuff is done to one bank set it for all
	if(($GLOBE::dataAdd[$int] || $GLOBE::backAdd[$int])&&$addTrue){
	  if(($GLOBE::dataAdd[$int]!=0.0)||($GLOBE::backAdd[$int]!=0.0)){
#	    print STDOUT "addTrue($GLOBE::dataAdd[$int],$GLOBE::backAdd[$int]) ";
	    $addTrue='0';
	    $GLOBE::numBanksAdd="$GLOBE::numBankProcess";
	    for( my $cnt=0 ; $cnt<$GLOBE::numBankProcess ; $cnt++ ){
		$GLOBE::bankAdd[$cnt]=$GLOBE::bank[$cnt];
		($GLOBE::dataAdd[$cnt]='0.0')unless($GLOBE::dataAdd[$cnt]);
		($GLOBE::backAdd[$cnt]='0.0')unless($GLOBE::backAdd[$cnt]);
	   
 }
	  }
	}

	# if any of the multiplicitive stuff is done to one bank set it for all
	if(($GLOBE::dataMult[$int] || $GLOBE::backMult[$int])&&$multTrue){
	  if(($GLOBE::dataMult[$int]!=1.0)||($GLOBE::backMult[$int]!=1.0)){
#	    print STDOUT "multTrue ";
	    $multTrue='0';
            $GLOBE::numBanksMult="$GLOBE::numBankProcess";
	    for( my $cnt=0 ; $cnt<$GLOBE::numBankProcess ; $cnt++ ){
		$GLOBE::bankMult[$cnt]=$GLOBE::bank[$cnt];
	        ($GLOBE::dataMult[$cnt]='1.0')unless($GLOBE::dataMult[$cnt]);
	        ($GLOBE::backMult[$cnt]='1.0')unless($GLOBE::backMult[$cnt]);
	    }
	  }
	}
    }

    # set all the elements to blank if there is no info
    if($addTrue){
	$GLOBE::numBanksAdd='0';
	for( my $cnt=0 ; $cnt<$GLOBE::numBankProcess ; $cnt++ ){
	    $GLOBE::bankAdd[$cnt]='';
	    $GLOBE::dataAdd[$cnt]='';
	    $GLOBE::backAdd[$cnt]='';
	}
    }

    # set all the elements to blank if there is no info
    if($multTrue){
        $GLOBE::numBanksMult='0';
	for( my $cnt=0 ; $cnt<$GLOBE::numBankProcess ; $cnt++ ){
	    $GLOBE::bankMult[$cnt]='';
	    $GLOBE::dataMult[$cnt]='';
	    $GLOBE::backMult[$cnt]='';
	}
    }

    ## assign bank numbers
    $GLOBE::numBankBlend=0;
    for( $int=0 ; $int<$GLOBE::numBankProcess ; $int++ ){
	if($GLOBE::cb[$int]){
	    $GLOBE::blendBank[$GLOBE::numBankBlend]=$GLOBE::bank[$int];
	    $GLOBE::numBankBlend+=1;
	}
    }

#    print STDOUT "after $GLOBE::numBanksMult\n";
}

#####
=pod

=item void editHistory()

    This is the dialog allowing users to edit the temporary history file.

=cut

sub editHistory{
	my ($widget) = $xpdf::mw;
	my ($num, $val);

        my $edit_dia=$widget->DialogBox(-title=>'Expert edit mode ..',
                                        -buttons=>['Done','Cancel']);
	my $t = $edit_dia->Scrolled("Text", -font=>$colors::fontL)
	                 ->pack(-side=>'bottom',-fill=>'both',-expand=>1);

	# make the two widgets gobble <Return>
	$edit_dia->bind("<Return>",sub{});
	$t->bind("<Return>",sub{});

	&readBank();
	&Hist::update();

	$t->delete("1.0", "end");
	if (open(FH,"<$GLOBE::historyname")) {
		while (<FH>) { $t->insert("end", $_); }
		close (FH);
        	$val=$edit_dia->Show;
        	if($val eq 'Done'){
	        	if (open(FH,">$GLOBE::historyname")) {
				print FH $t->get("1.0", "end");
				close(FH);
				&Hist::read($GLOBE::historyname);
            			&body::makeTable_f($body::sample_f);
            			&body::makeBankTable($body::bank_ftab);
				$xpdf::mess="History updated ..";
			} else {
				$xpdf::mess="ERROR: Could not write history!";
			}
		}
	}
        $edit_dia->destroy();
}
#####
=pod

=item void editLog()

    This displays the current log files.

=cut

sub editLog{
	my ($widget) = $xpdf::mw;

        my $log_dia=$widget->DialogBox(-title=>'Log files ..',
                                       -buttons=>['Done']);

	## Create frame layout

	my $sf=$log_dia->Frame()->pack(-side=>'top', -fill=>'x');
	$body::text=$log_dia->Scrolled("Text",-font=>$colors::fontL)
	                    ->pack(-side=>'bottom',-fill=>'both',
                                   -padx=>2,-pady=>2,-expand=>1);

	$log_dia->bind("<Return>",sub{});
	$body::text->bind("<Return>",sub{});

	## Create buttons to select wanted log file

	my $sprep=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                     ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("prep",$FLAG::fileinp);
	$pname=~s/_[s,v,c]//;
	$body::bprep=$sprep->Button(-text=>"$pname", -bg=>"$colors::headerbg",
                        -relief=>'flat', -fg=>'black',-width=>10,
                        -font=>$colors::fontH,
                        -command=>sub{&body::showLog("prep");})
                        ->pack(-fill=>'x');

	my $scorps=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                      ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("corps",$FLAG::fileinp);
	$body::bcorps=$scorps->Button(-text=>"$pname", 
                             -bg=>"$colors::headerbg",
                         -font=>$colors::fontH,
                         -relief=>'flat', -fg=>'black',-width=>10,
                         -command=>sub{&body::showLog("corps");})
                         ->pack(-fill=>'x');

	my $ssoqd=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                      ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("soqd",$FLAG::fileinp);
	$body::bsoqd=$ssoqd->Button(-text=>"$pname", 
                             -bg=>"$colors::headerbg",
                         -font=>$colors::fontH,
                         -relief=>'flat', -fg=>'black',-width=>10,
                         -command=>sub{&body::showLog("soqd");})
                         ->pack(-fill=>'x');

	my $sblend=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                      ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("blend",$FLAG::fileinp);
	$body::bblend=$sblend->Button(-text=>"$pname", 
                             -bg=>"$colors::headerbg",
                         -font=>$colors::fontH,
                         -relief=>'flat', -fg=>'black',-width=>10,
                         -command=>sub{&body::showLog("blend");})
                         ->pack(-fill=>'x');

	my $sdamp=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                      ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("damp",$FLAG::fileinp);
	$body::bdamp=$sdamp->Button(-text=>"$pname", 
                             -bg=>"$colors::headerbg",
                         -font=>$colors::fontH,
                         -relief=>'flat', -fg=>'black',-width=>10,
                         -command=>sub{&body::showLog("damp");})
                         ->pack(-fill=>'x');

	my $sft=$sf->Frame(-relief=>'ridge',-borderwidth=>2,
                             -bg=>"$colors::headerbg")
                      ->pack(-side=>'left',-fill=>'x',-padx=>2,-pady=>2);
	my $pname=&body::logFile("ft",$FLAG::fileinp);
	$body::bft=$sft->Button(-text=>"$pname", -bg=>"$colors::headerbg",
                         -font=>$colors::fontH,
                         -relief=>'flat', -fg=>'black',-width=>10,
                         -command=>sub{&body::showLog("ft");})
                         ->pack(-fill=>'x');

	&body::showLog("prep");

        $log_dia->Show;
        $log_dia->destroy();
}
#####
=pod

=item void showLog(logtype)

    This routine puts the contents of the logfile/input file in 
    the text widget ..

=cut

sub showLog{
	my ($type) = $_[0];
        my $pname;

	$body::text->configure(-state=>'normal');
	$body::text->delete("1.0", "end");

	if ($type eq "prep" && $FLAG::fileinp) {
	  $pname = &body::logFile($type,$FLAG::fileinp);
          $pname =~s/_[s,c,v]/_s/;
	  if (open(FH,"<$pname")) {
	     $body::text->insert("end", "*** SAMPLE: prep_s.inp         ***\n");
	     while (<FH>) { $body::text->insert("end", $_); } close (FH);
	  } else {
	     $body::text->insert("end", "*** No sample input file found ***\n");
          }

          $pname =~s/_[s,c,v]/_v/;
	  if (open(FH,"<$pname")) {
	     $body::text->insert("end", "*** VANADIUM: prep_v.inp       ***\n");
	     while (<FH>) { $body::text->insert("end", $_); } close (FH);
	  } else {
	     $body::text->insert("end", "*** No van. input file found   ***\n");
          }

          $pname =~s/_[s,c,v]/_c/;
	  if (open(FH,"<$pname")) {
	     $body::text->insert("end", "*** CONTAINER: prep_c.inp      ***\n");
	     while (<FH>) { $body::text->insert("end", $_); } close (FH);
	  } else {
	     $body::text->insert("end", "*** No can input file found    ***\n");
          }

        } else {
	  $pname = &body::logFile($type,$FLAG::fileinp);
	  if (open(FH,"<$pname")) {
	     while (<FH>) { $body::text->insert("end", $_); } close (FH);
	  } else {
	     $body::text->insert("end", "*** No file found ***\n");
          }
	}

	## Type "p"
	if ($type eq "prep") {
	   $body::bprep->configure(-fg=>"$colors::headerfg");
	   $body::bcorps->configure(-fg=>'black');
	   $body::bsoqd->configure(-fg=>'black');
	   $body::bblend->configure(-fg=>'black');
	   $body::bdamp->configure(-fg=>'black');
	   $body::bft->configure(-fg=>'black');
	}

	## Type "c"
	if ($type eq "corps") {
	   $body::bprep->configure(-fg=>'black');
	   $body::bcorps->configure(-fg=>"$colors::headerfg");
	   $body::bsoqd->configure(-fg=>'black');
	   $body::bblend->configure(-fg=>'black');
	   $body::bdamp->configure(-fg=>'black');
	   $body::bft->configure(-fg=>'black');
	}

	## Type "s"
	if ($type eq "soqd") {
	   $body::bprep->configure(-fg=>'black');
	   $body::bcorps->configure(-fg=>'black');
	   $body::bsoqd->configure(-fg=>"$colors::headerfg");
	   $body::bblend->configure(-fg=>'black');
	   $body::bdamp->configure(-fg=>'black');
	   $body::bft->configure(-fg=>'black');
	}

	## Type "b"
	if ($type eq "blend") {
	   $body::bprep->configure(-fg=>'black');
	   $body::bcorps->configure(-fg=>'black');
	   $body::bsoqd->configure(-fg=>'black');
	   $body::bblend->configure(-fg=>"$colors::headerfg");
	   $body::bdamp->configure(-fg=>'black');
	   $body::bft->configure(-fg=>'black');
	}

	## Type "d"
	if ($type eq "damp") {
	   $body::bprep->configure(-fg=>'black');
	   $body::bcorps->configure(-fg=>'black');
	   $body::bsoqd->configure(-fg=>'black');
	   $body::bblend->configure(-fg=>'black');
	   $body::bdamp->configure(-fg=>"$colors::headerfg");
	   $body::bft->configure(-fg=>'black');
	}

	## Type "f"
	if ($type eq "ft") {
	   $body::bprep->configure(-fg=>'black');
	   $body::bcorps->configure(-fg=>'black');
	   $body::bsoqd->configure(-fg=>'black');
	   $body::bblend->configure(-fg=>'black');
	   $body::bdamp->configure(-fg=>'black');
	   $body::bft->configure(-fg=>"$colors::headerfg");
	}

	$body::text->configure(-state=>'disabled');

}
#####
=pod

=item void startOptimize()

    Opens a dialog to optimize PDF by varying additive constant

=cut

sub startOptimize{
  my($widget)=$xpdf::mw;
  my($iterations)=0;
  my($Glow);
  my($blabel)="Start";
  my($fname)=(split /,/,$GLOBE::runFile)[0];

  $Glow=&getGlow($fname);

  my($optDia)=$widget->DialogBox(-title=>'Optimize PDF',
				 -buttons=>['Done']);

  my($title)=$optDia->Frame(-relief=>'ridge',-borderwidth=>2,
                        -bg=>"$colors::headerbg")
                        ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
  $title->Label(-text=>"Optimize PDF: $fname.gr",-font=>$colors::fontM,
		-bg=>"$colors::headerbg",-fg=>"$colors::headerfg"
	       )->pack(-expand=>1,-fill=>'x');

  my($general)=$optDia->Frame()->pack(-side=>'top',-expand=>1,-fill=>'x');
  my($g1)=$general->Frame(-relief=>'ridge',-borderwidth=>2) 
                  ->pack(-side=>'top',-expand=>1,-fill=>'x');

  $g1->Label(-text=>'This will optimize the PDF by varying the',
             -font=>$colors::fontM)
     ->grid(-column=>0,-row=>0, -columnspan=>2, -sticky=>'wn');
  $g1->Label(-text=>'additive constant of the blended S(Q) to',
             -font=>$colors::fontM)
     ->grid(-column=>0,-row=>1, -columnspan=>2, -sticky=>'wn');
  $g1->Label(-text=>'minimize the value of Glow (see manual).',
             -font=>$colors::fontM)
     ->grid(-column=>0,-row=>2, -columnspan=>2, -sticky=>'wn');


  my($g2)=$general->Frame(-relief=>'ridge',-borderwidth=>2) 
                  ->pack(-side=>'top',-expand=>1,-fill=>'x');

  $g2->Label(-text=>'Additive constant BETA:',
             -font=>$colors::fontM)
     ->grid(-column=>0,-row=>0,-sticky=>'en');
  $g2->Label(-textvariable=>\$GLOBE::dampBx,-width=>10,-font=>$colors::fontM,
             -fg=>"$colors::headerfg")
     ->grid(-column=>1,-row=>0,-sticky=>'wn');

  $g2->Label(-text=>'Current iterations:',-font=>$colors::fontM)
     ->grid(-column=>0,-row=>1,-sticky=>'en');
  $g2->Label(-textvariable=>\$iterations,-width=>10,-font=>$colors::fontM,
             -fg=>"$colors::headerfg")
     ->grid(-column=>1,-row=>1,-sticky=>'wn');

  $g2->Label(-text=>'Current value Glow:',-font=>$colors::fontM)
     ->grid(-column=>0,-row=>2,-sticky=>'en');
  $g2->Label(-textvariable=>\$Glow,-width=>10,-font=>$colors::fontM,
             -fg=>"$colors::headerfg")
     ->grid(-column=>1,-row=>2,-sticky=>'wn');

  $g2->Label(-text=>'Maximum iterations:',-font=>$colors::fontM)
     ->grid(-column=>0,-row=>3,-sticky=>'en', -pady=>5);
  unless ($GLOBE::optMax) {$GLOBE::optMax=10;}
  $g2->Entry(-width=>8,-textvariable=>\$GLOBE::optMax,-font=>$colors::fontM)
     ->grid(-column=>1,-row=>3,-sticky=>'wn', -padx=>5, -pady=>5);
  $g2->Button(-textvariable=>\$blabel,-width=>8,-font=>$colors::fontM,
	      -command=>sub{&optimize($fname,\$Glow,\$iterations,\$blabel);},
              -fg=>"$colors::headerfg")
     ->grid(-column=>2,-row=>3,-sticky=>'e',-padx=>3);

  $optDia->Show;
  $optDia->destroy();
}

#####
=pod

=item void optimize()

  optimizes the PDF by varying beta in a way that minimizes Glow

=cut

sub optimize{
  my($fname,$glow,$iter,$blabel)=@_;
  $$iter=0;
  my $gui=$FLAG::noGui;
  $FLAG::noGui=1;

  $$blabel="Working";
  &body::makePDF();
  $$glow=&getGlow($fname);
  unless ($gui > 0) {$xpdf::mw->update();}
  my($Gmin,$Bmin,$direc,$dB)=("$$glow","$GLOBE::dampBx",1,.1);
  while($$iter<=$GLOBE::optMax){
    $GLOBE::dampBx+=$direc*$dB;
    $GLOBE::dampBx=sprintf("%.4f",$GLOBE::dampBx);
    &body::makePDF();
    $$glow=&getGlow($fname);
    (printf STDOUT "(%2d) %7.4f,%7.4f (%7.4f,%7.4f) (%2d*%5.4f)",
    $$iter,$$glow,$GLOBE::dampBx,$Gmin,$Bmin,$direc,$dB)if($FLAG::debug);
    if($$glow<$Gmin){
      (print "a\n")if($FLAG::debug);
      $Gmin="$$glow";
      $Bmin="$GLOBE::dampBx";
    }else{
      $$glow="$Gmin";
      $GLOBE::dampBx="$Bmin";
      if($direc==1){
	(print "b\n")if($FLAG::debug);
	$direc=-1;
      }else{
	(print "c\n")if($FLAG::debug);
	$direc=1;
	$dB*=.1;
	(last)if($dB<0.0001);
      }
    }
    $$iter++;
    unless ($gui > 0) {
      $xpdf::mw->update();
    } else {
      print "  Iteration: $$iter - additive B: $GLOBE::dampBx, ";
      print "Glow: $$glow ..\n";
    }
  }

  &body::makePDF();
  $FLAG::noGui=$gui;
  $$glow=&getGlow($fname);
  $$blabel="Start";
}

#####
=pod

=item float getGlow()

  finds the value of Glow as written in the ascii gr file

=cut

sub getGlow{
  my($fname)="$_[0].gr";
  my($Glow)=0;
  (return 'pdf not found')unless(-r $fname);
  open(FILE,$fname) || die "Could not open $fname: $!";
  while(<FILE>){
    if(/^\#P0\s+/){
      $Glow=(split /\s+/,$_)[3];
      $Glow=sprintf("%.4f",$Glow);
      last;
    }
  }
  close(FILE) || die "Could not close $fname: $!";
  return $Glow;
}

#####
=pod

=item void startPDFqual()

    Runs PDFqual and displays output in dialog

=cut

sub startPDFqual{
	my ($widget) = $xpdf::mw;

        my $fname=(split /,/,$GLOBE::runFile)[0];
        unless (-r $fname.".sq") {
          $xpdf::mess="Create S(Q) file first ...";
          return 0;
        }

	if($GLOBE::dampAveMin < 1) 
          {$GLOBE::dampAveMin=$GLOBE::dampAveMin*$GLOBE::dampQmax;}

        my $qual_dia=$widget->DialogBox(-title=>'How good is your PDF ..',
                                       -buttons=>['Done']);

	### title for the dialog box

	my $upper=$qual_dia->Frame(-relief=>'ridge', -borderwidth=>2,
                        -bg=>"$colors::headerbg")
                        ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
        $upper->Label(-text=>"How good is the PDF ?",
                      -font=>$colors::fontM,
                      -bg=>"$colors::headerbg",-fg=>"$colors::headerfg")
                      ->pack(-expand=>1,-fill=>'x');

	## Create text frame

	my $bottom=$qual_dia->Frame(-relief=>'ridge', -borderwidth=>2)
                   ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>1);
	$body::qualtext=$bottom->Scrolled("Text", -width=>63, -height=>23,
                                          -font=>$colors::fontL)
	                         ->pack(-side=>'bottom',-fill=>'both',
                                        -padx=>2,-pady=>2,-expand=>1);

	&body::runPDFqual($body::qualtext,
        "-r $GLOBE::ftNumDensity -Q $GLOBE::dampAveMin -R $GLOBE::ftIntMaxR");

	$qual_dia->bind("<Return>",sub{});
	$body::qualtext->bind("<Return>",sub{});

	## Show dialog 

        $qual_dia->Show;
        $qual_dia->destroy();

}
#####
=pod

=item void runPDFqual()

    This runs PDFqual and enters output in text frame

=cut

sub runPDFqual {

	my($text,$cmd)=@_;
        my($date,$fname,$out);

	$text->configure(-state=>'normal');
	$text->delete("1.0", "end");
	$date=&Stdio::getDate();
	$text->insert("end", "*** PDFqual data from $date  ***\n");

        $fname=(split /,/,$GLOBE::runFile)[0];
        $cmd=$fname." ".$cmd;
        $out=&Qual::pdfQual($cmd);
	$text->insert("end",$out);
}

#####
=pod

=item void qlimitDialog()

    This the the dialog allowing users to set Q limits according 
    to constant wavelength interval for different banks.

=cut

sub qlimitDialog{
	my ($widget) = $_[0];
	my ($limit)  = $_[1];

	my ($qe,$i,$iref,$val,$olimit,$bank,$angle,$qlim,$lambda);

	### Find reference bank
	$iref=-1;

	if ($limit eq 'max') {
	  $olimit = 'min';
	  $angle  = 0.0;
	  for ($i=0; $i<$GLOBE::numBankProcess; $i++) {
	    if ((abs($GLOBE::angle[$i]) > $angle) && $GLOBE::cb[$i]) 
                {$iref=$i; $angle=$GLOBE::angle[$i]}
	  } 
	  $bank=$GLOBE::bank[$iref];
	  $angle=$GLOBE::angle[$iref];
	  $qlim=$GLOBE::blendQmax[$iref];
	}

	if ($limit eq 'min') {
	  $olimit = 'max';
	  $angle  = 180.0;
	  for ($i=0; $i<$GLOBE::numBankProcess; $i++) {
	    if ((abs($GLOBE::angle[$i]) < $angle) && $GLOBE::cb[$i]) 
                {$iref=$i; $angle=$GLOBE::angle[$i]}
	  } 
	  $bank=$GLOBE::bank[$iref];
	  $angle=$GLOBE::angle[$iref];
	  $qlim=$GLOBE::blendQmin[$iref];
	}

	if ($iref < 0) {
          $xpdf::mess='Error: Select at least ONE bank ...';
	  return 0;
	}

	$lambda=&QtoLambda($qlim,$angle);
	
	### Dialog basic frame
        my $limit_dia=$widget->DialogBox(-title=>'Set Q range ..',
                      -buttons=>['Set','Cancel']);
	$limit_dia->bind("<Return>",sub{});
    	my $frame  = $limit_dia->Frame()->pack(-side=>'top', -fill=>'x');

	### title for the dialog box
	my $upper=$frame->Frame(-relief=>'ridge', -borderwidth=>2,
                        -bg=>"$colors::headerbg")
                        ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
        $upper->Label(-text=>"Set limits Q$limit using fixed wavelength ..",
                      -font=>$colors::fontM,
                      -bg=>"$colors::headerbg",-fg=>"$colors::headerfg")
                      ->pack(-expand=>1,-fill=>'x');

	### main frame
	my $main=$frame->Frame(-relief=>'ridge', -borderwidth=>2)
                       ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
	$main->Label(-text=>'Reference bank :',-font=>$colors::fontM)
	     ->grid(-column=>0, -row=>0, -sticky=>'w');
	$main->Label(-text=>"$bank", -fg=>"$colors::headerfg",
                     -font=>$colors::fontM)
	     ->grid(-column=>1, -row=>0, -sticky=>'w');

	$main->Label(-text=>'Bank at 2theta :',-font=>$colors::fontM)
	     ->grid(-column=>0, -row=>1, -sticky=>'w');
	$main->Label(-text=>"$angle deg.", -fg=>"$colors::headerfg",
                     -font=>$colors::fontM)
	     ->grid(-column=>1, -row=>1, -sticky=>'w');

	$main->Label(-text=>"Wavelength $olimit (A) :",-font=>$colors::fontM)
	     ->grid(-column=>0, -row=>2, -sticky=>'w');
    	$main->Label(-textvariable=>\$lambda, -fg=>"$colors::headerfg",
                     -font=>$colors::fontM)
             ->grid(-column=>1, -row=>2, -sticky=>'w');

	$main->Label(-text=>"Q $limit (A**-1) :",-font=>$colors::fontM)
	     ->grid(-column=>0, -row=>3, -sticky=>'w',-pady=>5);
    	$qe=$main->Entry(-textvariable=>\$qlim,-width=>10,
                         -font=>$colors::fontM)
                 ->grid(-column=>1, -row=>3, -sticky=>'w',-pady=>5);
    	$main->Button(-text=>"Convert",-font=>$colors::fontM,
             -command=>sub{$lambda=&QtoLambda($qlim,$angle);})
             ->grid(-column=>2,-row=>3,-sticky=>'w');
	$qe->bind("<Return>",sub{$lambda=&QtoLambda($qlim,$angle);});


        $val=$limit_dia->Show;
        $limit_dia->destroy();
        if($val ne 'Cancel'){
	    for ($i=0; $i<$GLOBE::numBankProcess; $i++) {
	      if ($limit eq 'min') {
	        $GLOBE::blendQmin[$i]=LambdatoQ($lambda,$GLOBE::angle[$i]);
	      }
	      if ($limit eq 'max') {
	        $GLOBE::blendQmax[$i]=LambdatoQ($lambda,$GLOBE::angle[$i]);
	      }
	    } 

	   ## update overall Q-range

           my $Qmin="$GLOBE::blendQmin[0]";
           my $Qmax="$GLOBE::blendQmax[0]";
           for($i=1 ; $i<$GLOBE::numBankProcess ; $i++ ){
             if($GLOBE::blendQmin[$i]<$Qmin){$Qmin="$GLOBE::blendQmin[$i]";}
             if($GLOBE::blendQmax[$i]>$Qmax){$Qmax="$GLOBE::blendQmax[$i]"; }
           }
           $GLOBE::dampQmin="$Qmin";
           $GLOBE::dampQmax="$Qmax";
           $GLOBE::dampQstart="$Qmax";
	}
}

#####
=pod

=item float QtoLambda()

    Converts Q at given TTH to wavelength Lambda.

=cut

sub QtoLambda{
	my ($q)   = $_[0];
	my ($tth) = $_[1];
	my ($pi)  = 3.141592654;

	if ($q == 0) {return 0;}

	my $th=($tth/2.0/180.0 * $pi);
	my $lambda=sprintf "%.6f",(4.0*$pi*sin(abs($th))/$q);

	return $lambda;
}

#####
=pod

=item float LambdatoQ()

    Converts wavelength Lambda at given TTH to Q.

=cut

sub LambdatoQ{
	my ($lambda) = $_[0];
	my ($tth)    = $_[1];
	my ($pi)     = 3.141592654;

	my $th=($tth/2.0/180.0 * $pi);
	my $q=sprintf "%.2f",(4.0*$pi*sin(abs($th))/$lambda);

	return $q;
}

#####
=pod

=item void bankDialog()

    This the the dialog allowing users to specify the number of banks

=cut

sub bankDialog{
	my ($widget) = $_[0];
	my ($num, $val, $what, $c, @line);

	$what='create';

        my $bank_dia=$widget->DialogBox(-title=>'Change number of banks',
                                        -buttons=>['OK','Cancel']);
    	my $frame  = $bank_dia->Frame()->pack(-side=>'top', -fill=>'x');
    	$frame->Radiobutton(-variable=>\$what,
                            -font=>$colors::fontM,
                            -text=>'Number of banks : ',
                            -selectcolor=>"$colors::selectcol",
                            -value=>'create')
                            ->grid(-column=>0, -row=>0, -sticky=>'w',-pady=>5);
    	$frame->Entry(-textvariable=>\$num,-width=>20,
                      -font=>$colors::fontM)
                      ->grid(-column=>1, -row=>0, -sticky=>'w',-pady=>5);

	my $fname=(split /,/,$GLOBE::runFile)[0];
    	if(&File::checkExist($fname.".ain")){
        	$frame->Radiobutton(-variable=>\$what,
                                    -font=>$colors::fontM,
                                    -text=>"Read information from",
                                    -selectcolor=>"$colors::selectcol",
                                    -value=>'read')
                      ->grid(-column=>0, -row=>1,-sticky=>'w',-pady=>5);
		$frame->Label(-text=>"$fname.ain", -fg=>"$colors::headerfg",
                              -font=>$colors::fontM)
	              ->grid(-column=>1, -row=>1,-sticky=>'w',-pady=>5);
		$what='read';
    	}

        $val=$bank_dia->Show;
        $bank_dia->destroy();
        if($val eq 'OK'){
	if ($what eq 'create' && $num=~/\d+/) {
       		$GLOBE::numBankProcess="$num";
               	&body::makeBankTable($body::bank_ftab);
 		&body::fillBank();
	}
	elsif ($what eq 'read') {
		if(&File::openRead($fname.".ain")){
			open (IFILE, "<$fname.ain");
			$c=-1;
			while (<IFILE>) {
			  if (/#S/) {
			    $c+=1;
			    @line=(split /\s+/,$_);    
			    $GLOBE::bank[$c]=@line[3];
			    ($GLOBE::angle[$c],
			     $GLOBE::blendQmin[$c],
			     $GLOBE::blendQmax[$c])=
			    (@line)[6,10,12];
			  }
			}
			close (IFILE);	
			$GLOBE::numBankProcess=$c+1;
			$GLOBE::numBanks=$c+1;
               		&body::makeBankTable($body::bank_ftab);
			&body::fillBank();
			{
			   my($Qmin,$Qmax);
			   $Qmin="$GLOBE::blendQmin[0]";
			   $Qmax="$GLOBE::blendQmax[0]";
			   for( my $int=1 ; $int<$GLOBE::numBanks ; $int++ ){
				if($GLOBE::blendQmin[$int]<$Qmin){
				   $Qmin="$GLOBE::blendQmin[$int]"; }
				if($GLOBE::blendQmax[$int]>$Qmax){
				   $Qmax="$GLOBE::blendQmax[$int]"; }
                           }
			   $GLOBE::dampQmin="$Qmin";
			   $GLOBE::dampQmax="$Qmax";
			   $GLOBE::dampQstart="$Qmax";
			}
		} else {
			$xpdf::mess="Could not open file $fname.ain !";
		}
	} }
}

#####
=pod

=item int cFloat(number)

    Checks that number has digits and neither spaces nor letters.

=cut

sub cFloat{
    my($val)=@_;
    my($test)="$val";

    $test=~s/\Q.\E//;
    $test=~s/\Q-\E//;
    if($test=~/\d+/){
	unless($test=~/\D+/){
	   return 1;
	}
    }
#    if($val=~/\d+/){
#	unless($val=~/\s+/){
#	    unless($val=~/[a-zA-Z]/){
#		return 1;
#	    }
#	}
#    }
    return 0;
}

#####
=pod

=item int cInt(number)

    Uses cFloat() to check number then makes sure that there is no decimal.

=cut

sub cInt{
    my($val)=@_;
    my($test)="$val";

    if(cFloat($test)){
	unless($test=~/\Q.\E/){
	    return 1;
	}
    }
    return 0;
}

#####
=pod

=item int errorCheck

    Checks that all parameters are of an acceptable form. If anything is 
    incorrect it displays a dialog box with a list of the errors and returns 
    false.

=cut

sub errorCheck{
    ############## key to parameters ################
    # s-sample # c-container # v-vanadium # r-corps #
    # q-soqd   # b-blend     # d-damp     # f-ft    #
    #################################################

    my($input)=@_;
    my($dialog,$val);
    
    ##### Check general information for errors
    ## machine name
    if(!$GLOBE::machineName){
	$FLAG::message.="\nMust specify instrument name.";
    }elsif($GLOBE::machineName=~/[A-Z]/){
	$GLOBE::machineName=~s/(\w+)/\L$1/;
    }
    ## run file names
    if(!$GLOBE::runFile){
	$FLAG::message.="\nMust specify run file(s).";
    }elsif($GLOBE::runFile=~/\s+/){
	$FLAG::message.="\nMust specify run files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::runFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }
    if(!$GLOBE::backFile){
	# do nothing
    }elsif($GLOBE::backFile=~/\s+/){
	$FLAG::message.="\nMust specify background run files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::backFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }
    ## container file names
    if(!$GLOBE::canFile){
	# do nothing
    }elsif($GLOBE::canFile=~/\s+/){
	$FLAG::message.="\nMust specify container files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::canFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }
    if(!$GLOBE::cBackFile){
	# do nothing
    }elsif($GLOBE::cBackFile=~/\s+/){
	$FLAG::message.="\nMust specify container background files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::cBackFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }
    ## vanadium file names
    if(!$GLOBE::vanFile){
	# do nothing
    }elsif($GLOBE::vanFile=~/\s+/){
	$FLAG::message.="\nMust specify vanadium files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::vanFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }
    if(!$GLOBE::vBackFile){
	# do nothing
    }elsif($GLOBE::vBackFile=~/\s+/){
	$FLAG::message.="\nMust specify vanadium background files as a comma delimited list";
    }else{
	my(@temp)=(split /,/,$GLOBE::vBackFile);
	my $file;
	foreach $file (@temp){
	   unless(-e "$file.$GLOBE::fileExt"){
 		$FLAG::message.="\n$file.$GLOBE::fileExt does not exist.";
	   }
 	}
    }

    ##### Check what nothing else does
    ## $GLOBE::sampleDensity
    if($GLOBE::sampleDensity){
	unless(cFloat($GLOBE::sampleDensity)){
	    $FLAG::message.="\nPowder density must be a number.";
	}
    }

    ##### Check according to the input parameters
    if($input=~/s/){
	&prepcheck('s');
    }	
    if($input=~/c/){
	&prepcheck('c');
    }	
    if($input=~/v/){
	&prepcheck('v');
    }	
    if($input=~/r/){
	my($val)=&rcorps::check();
	($FLAG::message.="$val")unless($val=~/corps/);
    }	
    if($input=~/q/){
	my($val)=&rsoqd::check();
	($FLAG::message.="$val")unless($val=~/soqd/);
    }	
    if($input=~/b/){
	my($val)=&rblend::check();
	($FLAG::message.="$val")unless($val=~/blend/);
    }	
    if($input=~/d/){
	my($val)=&rdamp::check();
	($FLAG::message.="$val")unless($val=~/damp/);
    }	
    if($input=~/f/){
	my($val)=&rft::check();
	($FLAG::message.="$val")unless($val=~/ft/);
    }	

    ##### Remove redundant messages
    &unredundant();

    ##### Display error dialog box and determine if the user hit OK or Cancel
    unless($FLAG::message){
	return 1;
	$xpdf::mess='Last Check Result: No Errors';
    }
    $xpdf::mess='Last Check Result: Errors';
    $FLAG::message='Error in entry: '."\n$FLAG::message";
    &help::displayHelp($xpdf::mw,"Error ...",$FLAG::message);
    $FLAG::message='';
    return 0;
	
}

#####
=pod

=item void prepcheck(string)

    This routine calls the machine dependent prep check

=cut

sub prepcheck{
    my($val)=@_;
    $GLOBE::dataType="$val";
    if($GLOBE::prepName=~/sepd/){
       $val=&rprepsepd::check();
    }elsif($GLOBE::prepName=~/norm/){
       $val=&rprepnorm::check();
    }elsif($GLOBE::prepName=~/ariel/){
       $val=&rprepariel::check();
    }elsif ($GLOBE::prepName =~ /gsas/ ||
            $GLOBE::prepName =~ /hipd/) {
       $val=&rprepgsas::check();
    }
    ($FLAG::message.="$val")unless($val=~/prep/);
}

#####
=pod

=item void unredundat(void)

    This routine removes all repetitions of error messages.

=cut

sub unredundant{
   (return)unless($FLAG::message);

   my(@message)=(split /\n/,$FLAG::message);
   $FLAG::message='';

   OUTER: for( my $int=0 ; $int<@message ; $int++ ){
	for( my $j=$int+1 ; $j<@message ; $j++ ){
	    (next OUTER)if("$message[$int]" eq "$message[$j]");
	}
	$FLAG::message.="$message[$int]\n";
   }
}

#####
=pod

=item void printStatus()

    This routine prints the status to screen or stdout

=cut

sub printStatus{

  my ($mess,$process)=@_;

  if ($FLAG::noGui > 0) {
    if ($mess)    {print "$mess\n";}
    if ($process) {print "$process\n";}
  } else {
    if ($mess)    {$xpdf::mess=$mess;}
    $xpdf::process=$process;
  }
}

#####
=pod

=item void autoNormalize()

    This routine executes the auto normalization request

=cut

sub autoNormalize{

  my $eps=0.01;
  my $delta_rho=2.00;
  my $mcyc=10;
  my $prec="%.4f";

  unless ($GLOBE::effSampleDensity) {$GLOBE::effSampleDensity=1.5;}
  $Qual::filename=(split(/,/,$GLOBE::runFile))[0];
  &Qual::QualInit();

  ## make PDF for current density

  &printStatus("","Starting normalization ..");
  &whichOut('s');
  unless (&makePDF()) {
    &printStatus("Error occurred - normalization aborted","");
    return;
  }
  &Qual::calcSQ();
  my $densf=sprintf ($prec,$GLOBE::effSampleDensity);
  my $savef=sprintf ($prec,$Qual::sq_avg);
  my $cyc=1;

  while (($cyc <= $mcyc) && (abs(1.0-$savef) > $eps)) {

    &printStatus("","Cycle $cyc: Density $densf - <S(Q)> is $savef");

    ## make a second PDF with different density

    $GLOBE::effSampleDensity=$GLOBE::effSampleDensity+(1.0/$cyc)*$delta_rho;
    &whichOut('s');
    &makePDF();
    &Qual::calcSQ();
    my $dens2=sprintf ($prec,$GLOBE::effSampleDensity);
    my $save2=sprintf ($prec,$Qual::sq_avg);

    ## calculate the correct density

    my $k0=($savef-$save2)/(1.0/$densf - 1.0/$dens2);
    my $k1=$k0/$densf - $savef;
    $GLOBE::effSampleDensity=sprintf($prec,$k0/(1.0+$k1));

    ## recalculate S(Q) with new density

    &whichOut('s');
    &makePDF();
    &Qual::calcSQ();
    $densf=sprintf ($prec,$GLOBE::effSampleDensity);
    $savef=sprintf ($prec,$Qual::sq_avg);

    $cyc++;
  }

  &printStatus("Final  : Density $densf - <S(Q)> is $savef","");
  $GLOBE::Savg_final=$savef;
}

#####
1;

