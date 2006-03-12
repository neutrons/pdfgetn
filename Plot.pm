#-----------------------------------------------------------------------------
# PDFplot version to be integrated into PDFgetN as a panel ..
#-----------------------------------------------------------------------------

package Plot;
use strict;

use IO::Handle;
use IPC::Open2;

STDOUT->autoflush();

#-----------------------------------------------------------------------------
# Create Plot panel
#-----------------------------------------------------------------------------

sub plotPanel {

  my ($body)=@_;

  $Plot::unit = 'q';
  $Plot::kuplot_status = 'NO KUPLOT !';
  $Plot::type = "ain";
  $Plot::current_plot=0;
  $Plot::load_banks=1;

#-----------------------------------------------------------------------------
# Frames for data type
#-----------------------------------------------------------------------------

  my $ff;
  my $wd="13"; my $wh="64";
  $Plot::ftype = $body->Frame()->pack(-side=>'left', -fill=>'both');

  my $lbf = $Plot::ftype->Frame(-relief=>'ridge', -borderwidth=>2)
            ->pack(-side=>'bottom', -fill=>'both', -padx=>2, -pady=>2);

  $lbf->Label(-text=>'Processing stage',-font=>$colors::fontH,
              -fg=>$colors::headerfg,
              -bg=>$colors::headerbg,
              -width=>$wh)->grid(-column=>0, -row=>0, -columnspan=>3);

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>0, -row=>1, -sticky=>'w');
  $Plot::bt[ 1]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'Prep (raw)',
                   -value=>'raw',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>0, -row=>2, -sticky=>'w');
  $Plot::bt[ 2]=$ff->Radiobutton(-variable=>\$Plot::type, 
                   -text=>'Prep (smoothed)',-font=>$colors::fontM,
                   -value=>'smo',
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>0, -row=>3, -sticky=>'w');
  $Plot::bt[ 3]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'I(Q) (int)',
                   -value=>'ain',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>1, -row=>1, -sticky=>'w');
  $Plot::bt[ 4]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'S(Q) (raw)',
                   -value=>'sqa',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>1, -row=>2, -sticky=>'w');
  $Plot::bt[ 5]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'S(Q) (blend)',
                   -value=>'sqb',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>1, -row=>3, -sticky=>'w');
  $Plot::bt[ 6]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'S(Q) (damp)',
                   -value=>'sq',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>2, -row=>1, -sticky=>'w');
  $Plot::bt[ 7]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'G(r)',
                   -value=>'gr',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>2, -row=>2, -sticky=>'w');
  $Plot::bt[ 8]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'rho(r)',
                   -value=>'rho',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>2, -row=>3, -sticky=>'w');
  $Plot::bt[ 9]=$ff->Radiobutton(-variable=>\$Plot::type, -text=>'N(r)',
                   -value=>'n',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $lbf->Label(-text=>'Corrections',
              -fg=>$colors::headerfg,
              -bg=>$colors::headerbg,-font=>$colors::fontH,
              -width=>$wh)->grid(-column=>0, -row=>4, -columnspan=>3);

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>0, -row=>5, -sticky=>'w');
  $Plot::bt[10]=$ff->Radiobutton(-variable=>\$Plot::type, 
                                 -text=>'Absorption Corr.',
                   -value=>'corr_abs',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>1, -row=>5, -sticky=>'w');
  $Plot::bt[11]=$ff->Radiobutton(-variable=>\$Plot::type,
                                 -text=>'Multiple Scat.',
                   -value=>'corr_msc',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>2, -row=>5, -sticky=>'w');
  $Plot::bt[12]=$ff->Radiobutton(-variable=>\$Plot::type, 
                                 -text=>'Placzek Corr.',
                   -value=>'corr_plc',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $ff = $lbf->Frame(-width=>$wd)->grid(-column=>0, -row=>6, -sticky=>'w');
  $Plot::bt[13]=$ff->Radiobutton(-variable=>\$Plot::type, 
                                 -text=>'Corrected S(Q)',
                   -value=>'corr_blen',-font=>$colors::fontM,
                   -width=>$wd,-anchor=>'w',
                   -selectcolor=>"$colors::selectcol",
                   -command=>sub{&button_update();})
     ->pack(-side=>'left');

  $lbf->Label(-text=>'Reference data set',
              -fg=>$colors::headerfg,-font=>$colors::fontH,
              -bg=>$colors::headerbg,
              -width=>$wh)->grid(-column=>0, -row=>7, -columnspan=>3);
  $Plot::refLabel=$lbf->Checkbutton(-text=>'Plot reference file',
                        -variable=>\$Plot::refPlot,-font=>$colors::fontM,
                        -selectcolor=>"$colors::selectcol")
                      ->grid(-column=>0, -row=>8, -sticky=>'wn');
  $Plot::refEntry=$lbf->Entry(-textvariable=>\$Plot::refFile,-width=>20,
                              -font=>$colors::fontM)
                      ->grid(-column=>1, -row=>8, -columnspan=>2, 
                             -sticky=>'w',-pady=>3);
  $Plot::refEntry->bind('<Button-3>',
                   sub{&File::getRefFile($Plot::refFile,
                       $Plot::refEntry);});

#-----------------------------------------------------------------------------
# Frames for data set 
#-----------------------------------------------------------------------------

  my $cbf = $Plot::ftype->Frame(-relief=>'ridge', -borderwidth=>2)
            ->pack(-side=>'top', -fill=>'both', -padx=>2, -pady=>2);

  $cbf->Label(-text=>'Data file',
              -fg=>$colors::headerfg,-font=>$colors::fontH,
              -bg=>$colors::headerbg,
              -width=>$wh)->grid(-column=>0, -row=>0, -columnspan=>3);

  $ff = $cbf->Frame(-width=>$wd)->grid(-column=>0, -row=>1, -sticky=>'w');
  $Plot::bset[1] = $ff->Radiobutton(-variable=>\$Plot::curr_file, 
                              -width=>$wd,-anchor=>'w',-font=>$colors::fontM,
                              -textvariable=>\$Plot::t1,-value=>$Plot::sfile,
                              -selectcolor=>"$colors::selectcol",
                              -command=>sub{$Plot::load_banks=1;})
                ->pack(-side=>'left',-anchor=>'w');

  $ff = $cbf->Frame(-width=>$wd)->grid(-column=>1, -row=>1, -sticky=>'w');
  $Plot::bset[2] = $ff->Radiobutton(-variable=>\$Plot::curr_file, 
                              -width=>$wd,-anchor=>'w',-font=>$colors::fontM,
                              -textvariable=>\$Plot::t2,-value=>$Plot::vfile,
                              -selectcolor=>"$colors::selectcol",
                              -command=>sub{$Plot::load_banks=1;})
                ->pack(-side=>'left');

  $ff = $cbf->Frame(-width=>$wd)->grid(-column=>2, -row=>1, -sticky=>'w');
  $Plot::bset[3] = $ff->Radiobutton(-variable=>\$Plot::curr_file, 
                              -width=>$wd,-anchor=>'w',-font=>$colors::fontM,
                              -textvariable=>\$Plot::t3,-value=>$Plot::cfile,
                              -selectcolor=>"$colors::selectcol",
                              -command=>sub{$Plot::load_banks=1;})
                ->pack(-side=>'left');

#-----------------------------------------------------------------------------
# Plot options
#-----------------------------------------------------------------------------

     $Plot::pbf = $body->Frame(-relief=>'ridge', -borderwidth=>2)
                 ->pack(-fill=>'both', -padx=>2, -pady=>2);

     $Plot::pbf->Label(-text=>'Plot options',
                 -fg=>$colors::headerfg,-font=>$colors::fontH,
                 -bg=>$colors::headerbg)
         ->pack(-side=>'top', -fill=>'x');

     $Plot::mcb = $Plot::pbf->Checkbutton(-variable=>\$Plot::markers,
                 -font=>$colors::fontM,
                 -selectcolor=>"$colors::selectcol",
                 -text=>'Plot markers')->pack(-anchor=>'w');
     $Plot::ecb = $Plot::pbf->Checkbutton(-variable=>\$Plot::error_bars,
                 -font=>$colors::fontM,
                 -selectcolor=>"$colors::selectcol",
                 -text=>'Plot error bars')->pack(-anchor=>'w');
     $Plot::pcb = $Plot::pbf->Checkbutton(-variable=>\$Plot::reduced_soq, 
                 -font=>$colors::fontM,
                 -state=>'disabled',
                 -selectcolor=>"$colors::selectcol",
                 -text=>'Plot Q[S(Q)-1]', -command=>sub{$Plot::load_banks=1;})
                ->pack(-anchor=>'w');
     $Plot::ccb = $Plot::pbf->Checkbutton(-variable=>\$Plot::cut_range,
                 -font=>$colors::fontM,
                 -selectcolor=>"$colors::selectcol",
                 -text=>"Plot 'blend' range only", 
                 -command=>sub{$Plot::load_banks=1;})
                ->pack(-anchor=>'w');
     $Plot::kcb = $Plot::pbf->Checkbutton(-variable=>\$Plot::keep_range,
                 -font=>$colors::fontM,
                 -selectcolor=>"$colors::selectcol",
                 -text=>"Keep plotting range")
                ->pack(-anchor=>'w');
  my $icb = $Plot::pbf->Checkbutton(-variable=>\$Plot::interactive,
                 -font=>$colors::fontM,
                 -selectcolor=>"$colors::selectcol",
                 -text=>'Interactive plot')->pack(-anchor=>'w');

  my $kbf = $body->Frame(-relief=>'ridge', -borderwidth=>2,
                         -bg=>$colors::headerbg)
                 ->pack(-fill=>'both', -padx=>2, -pady=>2);
     $kbf->Label(-textvariable=>\$Plot::kuplot_status,
                 -font=>$colors::fontH,
                 -height=>5, -fg=>$colors::headerfg,-bg=>$colors::headerbg)
         ->pack();

#-----------------------------------------------------------------------------
# Check for KUPLOT in given PATH and open connection
#-----------------------------------------------------------------------------

  &kuplot_open;
}

#-----------------------------------------------------------------------------
# Update plot information (called by button calls)
#-----------------------------------------------------------------------------
sub update_files{

    $Plot::sfile=(split /,/,$GLOBE::runFile)[0];
    $Plot::vfile=(split /,/,$GLOBE::vanFile)[0];
    $Plot::cfile=(split /,/,$GLOBE::canFile)[0];

    unless($Plot::sfile) {$Plot::sfile="no sample";}
    unless($Plot::vfile) {$Plot::vfile="no vanadium";}
    unless($Plot::cfile) {$Plot::cfile="no container";}

    $Plot::t1  = "$Plot::sfile (S)";
    $Plot::t2  = "$Plot::vfile (V)";
    $Plot::t3  = "$Plot::cfile (C)";

    if ($Plot::bset[1]) {$Plot::bset[1]->configure(-value=>$Plot::sfile);}
    if ($Plot::bset[2]) {$Plot::bset[2]->configure(-value=>$Plot::vfile);}
    if ($Plot::bset[3]) {$Plot::bset[3]->configure(-value=>$Plot::cfile);}

    $Plot::valid_type='none';

    unless ($Plot::curr_file eq $Plot::sfile ||
            $Plot::curr_file eq $Plot::vfile ||
            $Plot::curr_file eq $Plot::cfile ) {$Plot::curr_file=$Plot::sfile;}

    if ($Plot::curr_file eq 'no sample') {
      for (my $i=1; $i<=12; $i++) 
          {$Plot::bt[$i]->configure(-state=>'disabled');}
      return;
    }

    if ($Plot::bt[1] && $Plot::bset[1]) {
      &check_file("$Plot::sfile.braw",$Plot::bt[1],'raw');
      if ($Plot::type eq 'raw') {
        &check_file("$Plot::vfile.vraw",$Plot::bset[2],'raw');
        &check_file("$Plot::cfile.craw",$Plot::bset[3],'raw');
      }

      &check_file("$Plot::sfile.bsmo",$Plot::bt[2],'smo');
      if ($Plot::type eq 'smo') {
         &check_file("$Plot::vfile.vsmo",$Plot::bset[2],'smo');
         &check_file("$Plot::cfile.csmo",$Plot::bset[3],'smo');
      }

      &check_file("$Plot::sfile.ain",$Plot::bt[3],'ain');
      if ($Plot::type eq 'ain') {
         &check_file("$Plot::vfile.ain",$Plot::bset[2],'ain');
         &check_file("$Plot::cfile.ain",$Plot::bset[3],'ain');
      }

      &check_file("blen_bin.dat",$Plot::bt[13],'corr_blen');

      &check_file("soqd_sabs.dat",$Plot::bt[10],'corr_abs');
      if ($Plot::type eq 'corr_abs') {
        &check_file("soqd_vabs.dat",$Plot::bset[2],'corr_abs');
        &check_file("soqd_cabs.dat",$Plot::bset[3],'corr_abs');
      }

      &check_file("soqd_smsc.dat",$Plot::bt[11],'corr_msc');
      if ($Plot::type eq 'corr_msc') {
        &check_file("soqd_vmsc.dat",$Plot::bset[2],'corr_msc');
        &check_file("soqd_cmsc.dat",$Plot::bset[3],'corr_msc');
      }

      &check_file("soqd_splc.dat",$Plot::bt[12],'corr_plc');
      if ($Plot::type eq 'corr_plc') {
        &check_file("soqd_vplc.dat",$Plot::bset[2],'corr_plc');
        &check_file("soqd_cplc.dat",$Plot::bset[3],'corr_plc');
      }

      &check_file("$Plot::sfile.sqa",$Plot::bt[4],'sqa');
      &check_file("$Plot::sfile.sqb",$Plot::bt[5],'sqb');
      &check_file("$Plot::sfile.sq",$Plot::bt[6],'sq');
      &check_file("$Plot::sfile.gr",$Plot::bt[7],'gr');

      if ($GLOBE::ftNumDensity > 0.0) {
        &check_file("$Plot::sfile.gr",$Plot::bt[8],'gr');
        &check_file("$Plot::sfile.gr",$Plot::bt[9],'gr');
      } else {
        $Plot::bt[8]->configure(-state=>'disabled');
        $Plot::bt[9]->configure(-state=>'disabled');
      }

      if ($Plot::type eq 'raw' && 
        $Plot::bt[1]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'smo' && 
        $Plot::bt[2]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'ain' && 
        $Plot::bt[3]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'sqa' && 
        $Plot::bt[4]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'sqb' && 
        $Plot::bt[5]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'sq' && 
        $Plot::bt[6]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'gr' && 
        $Plot::bt[7]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'rho' && 
        $Plot::bt[8]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'n' && 
        $Plot::bt[9]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'corr_abs' && 
        $Plot::bt[10]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'corr_msc' && 
        $Plot::bt[11]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'corr_plc' && 
        $Plot::bt[12]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'corr_blen' && 
        $Plot::bt[13]->cget('-state') eq 'disabled') 
        {$Plot::type=$Plot::valid_type;}
      elsif ($Plot::type eq 'none')
        {$Plot::type=$Plot::valid_type;}

      if ($Plot::type eq 'sqa' || $Plot::type eq 'sqb' || $Plot::type eq 'sq' ||
          $Plot::type eq 'gr'  || $Plot::type eq 'rho' || $Plot::type eq 'n'  ||
          $Plot::type eq 'corr_blen') {
         $Plot::bset[2]->configure(-state=>'disabled');
         $Plot::bset[3]->configure(-state=>'disabled');
      } 
      elsif ($Plot::type eq 'corr_msc' || $Plot::type eq 'corr_plc') {
         $Plot::bset[2]->configure(-state=>'normal');
         $Plot::bset[3]->configure(-state=>'disabled');
      }
      else {
         $Plot::bset[2]->configure(-state=>'normal');
         $Plot::bset[3]->configure(-state=>'normal');
      }

      if ($Plot::curr_file eq $Plot::vfile && 
          $Plot::bset[2]->cget('-state') eq 'disabled') 
         {$Plot::curr_file=$Plot::sfile;}
      if ($Plot::curr_file eq $Plot::cfile && 
          $Plot::bset[3]->cget('-state') eq 'disabled') 
         {$Plot::curr_file=$Plot::sfile;}
    }

    $body::plot_f->update();
}

#-----------------------------------------------------------------------------
# Setting file buttons
#-----------------------------------------------------------------------------
sub check_file{

  my ($tfile,$widget,$ty) = @_;

  if (&File::checkExist($tfile)) {
     $widget->configure(-state=>'normal');
     $Plot::valid_type=$ty;
  } else {
     $widget->configure(-state=>'disabled');
  }
}

#-----------------------------------------------------------------------------
# Print dialog (stolen and modified from Xpdfplot ;-))
#-----------------------------------------------------------------------------
sub print_dialog{

    my $dialog = $body::plot_f->DialogBox(-title=>'Print current figure',
                                          -buttons=>['OK','Cancel']);

    my $wp = $dialog->Frame(-relief=>'ridge', -borderwidth=>2)
                    ->pack(-side=>'top', -fill=>'x', -padx=>2, -pady=>2);

    $wp->Radiobutton(-text=>"Printer command:", -value=>0,
                        -font=>$colors::fontM,
                        -variable=>\$Plot::prn_file,
                        -selectcolor=>"$colors::selectcol",
			-command=>sub{
			    $Plot::pr->configure(-fg=>'Black');
			    $Plot::pr->configure(-state=>'normal');
			    $Plot::pf->configure(-fg=>'DimGrey');
			    $Plot::pf->configure(-state=>'disabled');
			})
                        ->grid(-row=>0,-column=>0,-sticky=>'w');
    $wp->Radiobutton(-text=>'Filename:', -value=>1,
                        -font=>$colors::fontM,
                        -variable=>\$Plot::prn_file,
                        -selectcolor=>"$colors::selectcol",
			-command=>sub{
			    $Plot::pr->configure(-fg=>'DimGrey');
			    $Plot::pr->configure(-state=>'disabled');
			    $Plot::pf->configure(-fg=>'black');
			    $Plot::pf->configure(-state=>'normal');
			})
                        ->grid(-row=>1,-column=>0,-sticky=>'w');

    $Plot::pr=$wp->Entry(-textvariable=>\$Plot::prn_dev,-state=>'normal', 
                         -font=>$colors::fontM,
                         -width=>25, -fg=>'Black')
              ->grid(-row=>0,-column=>1,-sticky=>'w');
    $Plot::pf=$wp->Entry(-textvariable=>\$Plot::prn_name,-state=>'disabled', 
                         -font=>$colors::fontM,
                         -width=>25, -fg=>'DimGrey')
              ->grid(-row=>1,-column=>1,-sticky=>'w');

    if($Plot::prn_file==1){
	$Plot::pr->configure(-state=>'disabled',-fg=>'DimGrey');
	$Plot::pf->configure(-state=>'normal',-fg=>'black');
    }

    return $dialog->Show;

}

#-----------------------------------------------------------------------------
# Enable/disable possible selections
#-----------------------------------------------------------------------------

sub button_update {

  &update_files();

  if ($Plot::type eq 'gr' || $Plot::type eq 'rho' || $Plot::type eq 'n') {
     $Plot::pcb->configure(-state=>'disabled');
     $Plot::ccb->configure(-state=>'disabled');
     $Plot::ecb->configure(-state=>'normal');
     $Plot::refEntry->configure(-state=>'normal');
     $Plot::refLabel->configure(-state=>'normal');
     $Plot::unit = 'r';
  }
  elsif ($Plot::type eq 'ain' || $Plot::type eq 'raw' || $Plot::type eq 'smo') {
     $Plot::pcb->configure(-state=>'disabled');
     $Plot::ccb->configure(-state=>'normal');
     $Plot::ecb->configure(-state=>'normal');
     $Plot::refEntry->configure(-state=>'disabled');
     $Plot::refLabel->configure(-state=>'disabled');
     $Plot::unit='q';
  }
  elsif ($Plot::type eq 'sqa') {
     $Plot::pcb->configure(-state=>'normal');
     $Plot::ccb->configure(-state=>'normal');
     $Plot::ecb->configure(-state=>'normal');
     $Plot::refEntry->configure(-state=>'disabled');
     $Plot::refLabel->configure(-state=>'disabled');
     $Plot::unit='q';
  }
  elsif ($Plot::type eq 'sqb' || $Plot::type eq 'sq') {
     $Plot::pcb->configure(-state=>'normal');
     $Plot::ccb->configure(-state=>'disabled');
     $Plot::ecb->configure(-state=>'normal');
     $Plot::refEntry->configure(-state=>'normal');
     $Plot::refLabel->configure(-state=>'normal');
     $Plot::unit='q';
  }
  elsif ($Plot::type eq 'corr_blen') {
     $Plot::pcb->configure(-state=>'disabled');
     $Plot::ccb->configure(-state=>'normal');
     $Plot::ecb->configure(-state=>'disabled');
     $Plot::refEntry->configure(-state=>'disabled');
     $Plot::refLabel->configure(-state=>'disabled');
     $Plot::unit='q';
  }
  elsif ($Plot::type eq 'corr_msc' || $Plot::type eq 'corr_plc') {
     $Plot::pcb->configure(-state=>'disabled');
     $Plot::ccb->configure(-state=>'normal');
     $Plot::ecb->configure(-state=>'disabled');
     $Plot::refEntry->configure(-state=>'disabled');
     $Plot::refLabel->configure(-state=>'disabled');
     $Plot::unit='q';
  }
  elsif ($Plot::type eq 'corr_msc' || $Plot::type eq 'corr_plc') {
     $Plot::pcb->configure(-state=>'disabled');
     $Plot::ccb->configure(-state=>'normal');
     $Plot::ecb->configure(-state=>'disabled');
     $Plot::refEntry->configure(-state=>'disabled');
     $Plot::refLabel->configure(-state=>'disabled');
     $Plot::unit='q';
  }

  if ($Plot::type eq 'raw' || $Plot::type eq 'smo') {
    $Plot::t1 = "$Plot::sfile (B)"; 
  }
  else {
    $Plot::t1 = "$Plot::sfile (S)"; 
  }

  $Plot::load_banks=1;
  $Plot::keep_range=0;
}

#-----------------------------------------------------------------------------
# Sending command to KUPLOT and wait for answer ..
#-----------------------------------------------------------------------------

sub kuplot_send {
  my ($cmd)=@_;
  
  if ($FLAG::debug) {print "OUT: $cmd\n";}

  print kout "$cmd\n";
  print kout "echo done\n";

  while(<kin>){
      (print "IN : $_") if($FLAG::debug);
      (last) if(/echo done/);
  }
}

#-----------------------------------------------------------------------------
# Open pipe to KUPLOT
#-----------------------------------------------------------------------------

sub kuplot_open {
  
  my $cmd = File::Spec->canonpath("$GLOBE::execpath/kuplot");

  unless (-x $cmd || -x "$cmd.exe") {
    $Plot::ftype->pack('forget');
    $Plot::pbf->pack('forget');
    $Plot::xpdf::kuplot=0;
    return;
  }

  my $pid = open2(\*kin, \*kout, "$cmd") || die ('KUPLOT connection failed ..');
  if ($FLAG::debug) { print "Connection to KUPLOT opened, PID=$pid\n"; }

  $xpdf::kuplot=1;
  kout->autoflush();
  kin ->autoflush();

  while (<kin>){
    (print "IN : $_") if($FLAG::debug);
    (last) if (/Version/);
  }
  s/\s+//g; s/KUPLOTVersion//; s/\*//g;
  if ($_) {$Plot::kuplot_status="KUPLOT $_ running ..";}

  &kuplot_send ("set prompt,redirect,on");
  &kuplot_send ("size[1]=0.9");
  &kuplot_send ("fnam off");
  &kuplot_send ("font size,1.1");

  &kuplot_colors ();
} 

#-----------------------------------------------------------------------------
# Close pipe and wait for KUPLOT to finish
#-----------------------------------------------------------------------------

sub kuplot_close {
  print kout "exit\n\n";
  close(kin);
  close(kout);

  if ($FLAG::debug) {
    print "Connection to KUPLOT closed\n";
  }

  (unlink("kuplot.cor"))if(-e "kuplot.cor");
}

#-------------------------------------------------------------------------------
# Print subroutine
#-------------------------------------------------------------------------------

sub kuplot_print {
  my $message;

  if ($Plot::current_plot) {
    my $prn_val=&print_dialog();
    (return) if($prn_val eq 'Cancel');
  }
  else {
    &update_message("plotting error: You have to plot data first !");
    return;
  }

  if ($Plot::prn_file) {
    &kuplot_send ("save ps,$Plot::prn_name");
    $message = sprintf ("Saved figure in file %s", $Plot::prn_name);
  }
  else {
    &kuplot_send ("prin ps,\"$Plot::prn_dev\"");
    $message = sprintf ("Printed figure using \"%s \"", $Plot::prn_dev);
  }
  &update_message($message);

}

#-----------------------------------------------------------------------------
# Plot subroutine
#-----------------------------------------------------------------------------

sub kuplot_plot {
  my $message;

  if ($Plot::type eq 'gr' || $Plot::type eq 'rho' || $Plot::type eq 'n') {
    &kuplot_plot_gr;
  }
  elsif ($Plot::type eq 'sqb' || $Plot::type eq 'sq') {
    &kuplot_plot_sq;
  }
  elsif ($Plot::type eq 'sqa') {
    &kuplot_plot_banks_soq;
  }
  elsif ($Plot::type eq 'ain' || $Plot::type eq 'raw' || 
         $Plot::type eq 'smo') {
    &kuplot_plot_banks_int;
  }
  elsif ($Plot::type =~ /corr_/) {
    &kuplot_plot_banks_corr;
  }
  else {
    $message = sprintf ("plotting error: Data type \"%s\" unknown !", 
                        $Plot::type);
    &update_message($message);
  }
}

#-----------------------------------------------------------------------------
# Reset and change directory
#-----------------------------------------------------------------------------

sub kuplot_rese {

  &kuplot_send ("rese");
  &kuplot_send ("cd $GLOBE::workdir");

}

#-----------------------------------------------------------------------------
# Plot subroutine for G(r)
#-----------------------------------------------------------------------------

sub kuplot_plot_gr {

  my $fname = "$Plot::curr_file.gr";
  my $cmd;
  my $rfile="";

  if (!(-r $fname)) {
    my $message = sprintf("plotting error: File %s not found !",$fname);
    &update_message($message);
    return();
  }

  if ($Plot::refPlot) {
    $rfile="$Plot::refFile.gr";
    if (!(-r $rfile)) {
      my $message = sprintf("plotting error: File %s not found !",$rfile);
      &update_message($message);
      return();
    }
  }

  &kuplot_rese();

  if ($Plot::type eq 'gr') {
    &kuplot_send ("load st,$fname,1,1,2,0,3,4");
    if ($rfile) {&kuplot_send ("load st,$rfile,1,1,2,0,3,4");}
  } 
  elsif ($Plot::type eq 'rho') {
    &kuplot_send ("load st,$fname,1,1,5,0,3,6");
    if ($rfile) {&kuplot_send ("load st,$rfile,1,1,5,0,3,6");}
  }
  elsif ($Plot::type eq 'n') {
    &kuplot_send ("load st,$fname,1,1,7");
    if ($rfile) {&kuplot_send ("load st,$rfile,1,1,7");}
  }

  if ($rfile) {
    &kuplot_send ("sleg 1,Sample: $fname");
    &kuplot_send ("sleg 2,Ref.: $rfile");
  } else {
    &kuplot_send ("sleg 1,off");
  }
  &kuplot_error_bars(1);
  &kuplot_markers(1);
  
  my $title=$GLOBE::title;
  $title=~s/'/;/g;

  $cmd = sprintf ("tit1 %s", $title);
  &kuplot_send ("$cmd");
  my $ltime=localtime();
  $cmd = sprintf ("tit2 Created : %s", $ltime);
  &kuplot_send ("$cmd");

  &kuplot_output;
}

#-----------------------------------------------------------------------------
# Plot subroutine for S(q) (after blend)
#-----------------------------------------------------------------------------

sub kuplot_plot_sq {

  my $cmd;
  my $fname = "$Plot::curr_file.$Plot::type";
  my $rfile="";

  if (!(-r $fname)) {
    my $message = sprintf("plotting error: File %s not found !",$fname);
    &update_message($message);
    return();
  }

  if ($Plot::refPlot) {
    $rfile="$Plot::refFile.$Plot::type";
    if (!(-r $rfile)) {
      my $message = sprintf("plotting error: File %s not found !",$rfile);
      &update_message($message);
      return();
    }
  }

  &kuplot_rese();

  &kuplot_read_soq($fname,1,$Plot::reduced_soq);
  if ($rfile) {&kuplot_read_soq($rfile,1,$Plot::reduced_soq);}
  &kuplot_achx();
  &kuplot_achy_soq($Plot::reduced_soq);
  if ($rfile) {
    &kuplot_send ("sleg 1,Sample");
    &kuplot_send ("sleg 2,Ref. $rfile");
  } else {
    &kuplot_send ("sleg 1,off");
  }
  &kuplot_error_bars(1);
  &kuplot_markers(1);

  my $title=$GLOBE::title;
  $title=~s/'/;/g;

  $cmd = sprintf ("tit1 %s", $title);
  &kuplot_send ("$cmd");
  my $ltime=localtime();
  $cmd = sprintf ("tit2 Created : %s", $ltime);
  &kuplot_send ("$cmd");

  &kuplot_output;
}

#-----------------------------------------------------------------------------
# Plot subroutine for S(Q) - separate banks
#-----------------------------------------------------------------------------

sub kuplot_plot_banks_soq {

  my ($message, $fname, $cmd, $i);

  if ($Plot::load_banks) {
    $fname = "$Plot::curr_file.$Plot::type";
    if (!(-r $fname)) {
      $message = sprintf("plotting error: File %s not found !",$fname);
      &update_message($message);
      return();
    }

    &kuplot_rese();

    my $message ="Be patient, loading data ... ";
    &update_message("$message");
    &kuplot_read_soq($fname,$GLOBE::numBankProcess,$Plot::reduced_soq);
    for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
      $cmd = sprintf ("sleg %d,\"Bank %d\"", $i,$i);
      &kuplot_send ("$cmd");
    }
    $Plot::load_banks=0;
  }

  if ($Plot::cut_range) {&kuplot_exclude();}

  my $title=$GLOBE::title;
  $title=~s/'/;/g;

  &kuplot_achx();
  &kuplot_achy_soq($Plot::reduced_soq);
  $cmd = sprintf ("tit1 %s", $title);
  &kuplot_send ("$cmd");
  $cmd = sprintf ("tit2 Sample : %s", $fname);
  &kuplot_send ("$cmd");
  
  my $count = 0;
  my $kfra  = "kfra 1";

  for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
    if ($GLOBE::cb[$i-1]) { $kfra .= ","."$i"; $count++ }
    &kuplot_error_bars($i);
    &kuplot_markers($i);
  }
  unless ($Plot::reduced_soq) {$kfra .= ",".($GLOBE::numBankProcess+1).",".
                                      ($GLOBE::numBankProcess+2);}

  if ($count != 0) {
    &kuplot_send ("$kfra");
    &kuplot_output;
  }
  else {
    &update_message ("plotting error: Select at least ONE bank !");
  }
}

#-----------------------------------------------------------------------------
# Read SOQ file ..
#-----------------------------------------------------------------------------

sub kuplot_read_soq {

  my $fname = $_[0];
  my $nbank = $_[1];
  my $frsoq = $_[2];
  my $xcol=1;
  my $ycol;
  my $dycol;

  if   ($frsoq) { $ycol=4; $dycol=5; }
  else          { $ycol=2; $dycol=3; }

  &kuplot_send ("load st,$fname,all,$xcol,$ycol,0,0,$dycol");
}

#-----------------------------------------------------------------------------
# Plot subroutine for I(Q) - separate banks
#-----------------------------------------------------------------------------

sub kuplot_plot_banks_int {

  my ($message, $fname, $cmd, $i);

  if ($Plot::load_banks) {
    if ($Plot::type eq 'smo' || $Plot::type eq 'raw') {
      if ($Plot::curr_file eq $Plot::sfile) 
         {$fname="$Plot::curr_file.b$Plot::type";}
      if ($Plot::curr_file eq $Plot::vfile) 
         {$fname="$Plot::curr_file.v$Plot::type";}
      if ($Plot::curr_file eq $Plot::cfile) 
         {$fname="$Plot::curr_file.c$Plot::type";}
    } else {
      $fname = "$Plot::curr_file.$Plot::type";
    }
    if (!(-r $fname)) {
      $message = sprintf("plotting error: File %s not found !",$fname);
      &update_message($message);
      return();
    }

    &kuplot_rese();

    $message ="Be patient, loading data ... ";
    &update_message("$message");
    &kuplot_read_int($fname,$GLOBE::numBankProcess);
    for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
      $cmd = sprintf ("sleg %d,\"Bank %d\"", $i,$i);
      &kuplot_send ("$cmd");
    }

    my $title=$GLOBE::title;
    $title=~s/'/;/g;

    &kuplot_achx();
    &kuplot_send ("achy Normalised intensity");
    $cmd = sprintf ("tit1 %s", $title);
    &kuplot_send ("$cmd");
  
    if    ($Plot::curr_file eq $Plot::sfile) {
      if ($Plot::type eq 'smo' || $Plot::type eq 'raw') {
        $cmd = sprintf ("tit2 Background : %s", $fname);
        &kuplot_send ("$cmd");
      } else {
        $cmd = sprintf ("tit2 Sample : %s", $fname);
        &kuplot_send ("$cmd");
      }
    }
    elsif ($Plot::curr_file eq $Plot::vfile) {
      $cmd = sprintf ("tit2 Vanadium : %s", $fname);
      &kuplot_send ("$cmd");
    }
    elsif ($Plot::curr_file eq $Plot::cfile) {
      $cmd = sprintf ("tit2 Container : %s", $fname);
      &kuplot_send ("$cmd");
    }
    $Plot::load_banks=0;
  }

  if ($Plot::cut_range) {&kuplot_exclude();}

  my $count = 0;
  my $kfra  = "kfra 1";

  for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
    if ($GLOBE::cb[$i-1]) { $kfra .= ","."$i"; $count++ }
    &kuplot_error_bars($i);
    &kuplot_markers($i);
  }

  if ($count != 0) {
    &kuplot_send ("$kfra");
    &kuplot_output;
  }
  else {
    &update_message ("plotting error: Select at least ONE bank !");
  }
}

#-----------------------------------------------------------------------------
# Read INT file ..
#-----------------------------------------------------------------------------

sub kuplot_read_int {

  my $fname = $_[0];
  my $nbank = $_[1];
  my $frsoq = $_[2];

  my  $xcol=1;
  my  $ycol=2;
  my $dycol=3;

  &kuplot_send ("load st,$fname,all,$xcol,$ycol,0,0,$dycol");
}

#-----------------------------------------------------------------------------
# Plot subroutine for corrections - separate banks
#-----------------------------------------------------------------------------

sub kuplot_plot_banks_corr {

  my ($message, $fname, $cmd, $achy, $i);

  if ($Plot::load_banks) {
    if ($Plot::type eq 'corr_abs') {
      if ($Plot::curr_file eq $Plot::sfile) {$fname="soqd_sabs.dat";}
      if ($Plot::curr_file eq $Plot::vfile) {$fname="soqd_vabs.dat";}
      if ($Plot::curr_file eq $Plot::cfile) {$fname="soqd_cabs.dat";}
      $achy="Absorption correction factor";
    }
    elsif ($Plot::type eq 'corr_msc') {
      if ($Plot::curr_file eq $Plot::sfile) {$fname="soqd_smsc.dat";}
      if ($Plot::curr_file eq $Plot::vfile) {$fname="soqd_vmsc.dat";}
      $achy="Multiple scattering correction factor";
    }
    elsif ($Plot::type eq 'corr_plc') {
      if ($Plot::curr_file eq $Plot::sfile) {$fname="soqd_splc.dat";}
      if ($Plot::curr_file eq $Plot::vfile) {$fname="soqd_vplc.dat";}
      $achy="Plazcek correction factor";
    }
    elsif ($Plot::type eq 'corr_blen') {
      $fname="blen_bin.dat";
      $achy="Rebinned and corrected S(Q)";
    }
    if (!(-r $fname)) {
      $message = sprintf("plotting error: File %s not found !",$fname);
      &update_message($message);
      return();
    }

    &kuplot_rese();

    my($message)="Be patient, loading data ... ";
    &update_message("$message");
    &kuplot_read_corr($fname,$GLOBE::numBankProcess);
    for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
      $cmd = sprintf ("sleg %d,\"Bank %d\"", $i,$i);
      &kuplot_send ("$cmd");
    }

    my $title=$GLOBE::title;
    $title=~s/'/;/g;

    &kuplot_achx();
    &kuplot_send ("achy $achy");
    $cmd = sprintf ("tit1 %s", $title);
    &kuplot_send ("$cmd");
  
    if ($Plot::curr_file eq $Plot::sfile) {
      $cmd = sprintf ("tit2 Sample : %s", $fname);
      &kuplot_send ("$cmd");
    }
    elsif ($Plot::curr_file eq $Plot::vfile) {
      $cmd = sprintf ("tit2 Vanadium : %s", $fname);
      &kuplot_send ("$cmd");
    }
    elsif ($Plot::curr_file eq $Plot::cfile) {
      $cmd = sprintf ("tit2 Container : %s", $fname);
      &kuplot_send ("$cmd");
    }
    $Plot::load_banks=0;
  }

  if ($Plot::cut_range) {&kuplot_exclude();}

  my $count = 0;
  my $kfra  = "kfra 1";

  for ($i=1; $i<=$GLOBE::numBankProcess; $i++) {
    if ($GLOBE::cb[$i-1]) { $kfra .= ","."$i"; $count++ }
    &kuplot_markers($i);
  }

  if ($count != 0) {
    &kuplot_send ("$kfra");
    &kuplot_output;
  }
  else {
    &update_message ("plotting error: Select at least ONE bank !");
  }
}

#-----------------------------------------------------------------------------
# Read CORR file ..
#-----------------------------------------------------------------------------

sub kuplot_read_corr {

  my $fname = $_[0];
  my $nbank = $_[1];
  my $frsoq = $_[2];

  my  $xcol=1;
  my  $ycol=2;

  &kuplot_send ("load st,$fname,all,$xcol,$ycol");
}

#-----------------------------------------------------------------------------
# Plot line at y=1 and line at y=-(normalized laue term)
#-----------------------------------------------------------------------------

sub kuplot_line_one {

  &kuplot_send ("r[1]=xmax[1]");
  &kuplot_send ("do i[1]=2,n[1]\nr[1]=max(r[1],xmax[i[1]])\nenddo");

  # line at 1
  &kuplot_send ("alloc line,2");
  &kuplot_send ("x[n[1],1]=0.0");
  &kuplot_send ("y[n[1],1]=1.0");
  &kuplot_send ("x[n[1],2]=r[1]");
  &kuplot_send ("y[n[1],2]=1.0");
  &kuplot_send ("sleg n[1],off");

  # line at -(normalized laue term)
  &kuplot_send ("alloc line,2");
  &kuplot_send ("x[n[1],1]=0.0");
  &kuplot_send ("y[n[1],1]=-$GLOBE::laue");
  &kuplot_send ("x[n[1],2]=r[1]");
  &kuplot_send ("y[n[1],2]=-$GLOBE::laue");
  &kuplot_send ("sleg n[1],off");
}

#-----------------------------------------------------------------------------
# Toogle markers
#-----------------------------------------------------------------------------

sub kuplot_markers {

  my $ik = $_[0];

  if ($Plot::markers) { 
    &kuplot_send ("mtyp $ik,7"); 
    &kuplot_send ("mcol $ik,6"); 
  }
  else { 
    &kuplot_send ("mtyp $ik,0"); 
  }
}

#-----------------------------------------------------------------------------
# Toogle error bars
#-----------------------------------------------------------------------------

sub kuplot_error_bars {

  my $ik = $_[0];

  if ($Plot::error_bars) { 
    &kuplot_send ("etyp $ik,2"); 
    &kuplot_send ("ecol $ik,6"); 
  }
  else { 
    &kuplot_send ("etyp $ik,0"); 
  }
}

#-----------------------------------------------------------------------------
# Set excluded regions
#-----------------------------------------------------------------------------

sub kuplot_exclude {

  my ($qmax,$qmin);

  for (my $ik=1; $ik<=$GLOBE::numBankProcess; $ik++) {
    $qmin=$GLOBE::blendQmin[$ik-1];
    $qmax=$GLOBE::blendQmax[$ik-1];
    unless($qmin) {$qmin=0.0;}
    unless($qmax) {$qmax=0.0;}

    &kuplot_send ("excl $ik,xmin[$ik]-1.0,$qmin");
    &kuplot_send ("excl $ik,$qmax,xmax[$ik]+1.0");
  }
}

#-----------------------------------------------------------------------------
# Set x-axis label
#-----------------------------------------------------------------------------

sub kuplot_achx {

  my $achx;
 
  if    ($Plot::unit eq 'q') { $achx="Q (\\A\\u-1\\d)"; }
  elsif ($Plot::unit eq 'r') { $achx="r (\\A)"; }

  &kuplot_send ("achx $achx");
}

#-----------------------------------------------------------------------------
# Set y-axis label
#-----------------------------------------------------------------------------

sub kuplot_achy_soq {

  if ($_[0]) {
    &kuplot_send ("achy Q[S(Q)-1]");
  }
  else {
    &kuplot_send ("achy S(Q)");
    &kuplot_line_one;
  }
}

#-----------------------------------------------------------------------------
# Do the plotting ..
#-----------------------------------------------------------------------------

sub kuplot_output {

  if ($Plot::keep_range) {&kuplot_send("skal r[91],r[92],r[93],r[94]");}

  if ($Plot::interactive) {
    &kuplot_send ("mouse");
  }
  else {
    &kuplot_send ("plot");
  }
 
  &kuplot_send("r[91]=pwin[1]");
  &kuplot_send("r[92]=pwin[2]");
  &kuplot_send("r[93]=pwin[3]");
  &kuplot_send("r[94]=pwin[4]");

  $Plot::current_plot = 1;
  &update_message ("Done ..");
}

#-----------------------------------------------------------------------------
# Update message
#-----------------------------------------------------------------------------

sub update_message {
  $xpdf::mess=$_[0];
  $xpdf::mw->update();
}

#-----------------------------------------------------------------------------
# Setting colors
#-----------------------------------------------------------------------------

sub kuplot_colors {

  my ($r,$g,$b);

  for (my $i=0; $i<=15; $i++) {
    ($r,$g,$b)=&colors::convertColor($Plot::color[$i]);   
    &kuplot_send("color $i,$r,$g,$b");
  }
}

#-----------------------------------------------------------------------------
# Color defaults
#-----------------------------------------------------------------------------

sub kuplot_defaults {

  $Plot::color[ 0]="#ffffff";
  $Plot::color[ 1]="#ff0000";
  $Plot::color[ 2]="#00ff00";
  $Plot::color[ 3]="#0000ff";
  $Plot::color[ 4]="#ff00ff";
  $Plot::color[ 5]="#ffff00";
  $Plot::color[ 6]="#000000";
  $Plot::color[ 7]="#990000";
  $Plot::color[ 8]="#009900";
  $Plot::color[ 9]="#000099";
  $Plot::color[10]="#990099";
  $Plot::color[11]="#999900";
  $Plot::color[12]="#808080";
  $Plot::color[13]="#00ffff";
  $Plot::color[14]="#ff9999";
  $Plot::color[15]="#000000";

  $Plot::prn_file = 0;
  $Plot::prn_name = "pdfgetn.ps";
  $Plot::prn_dev = "print /d:'\\\\POPLER\\Dover' ";
  $Plot::error_bars=0;
  $Plot::markers=0;
  $Plot::interactive=0;
  $Plot::reduced_soq=0;
  $Plot::cut_range=0;
  $Plot::keep_range=0;

}

#-----------------------------------------------------------------------------
# Color selection dialog
#-----------------------------------------------------------------------------

sub kuplot_color_select {

  my $col_dia=$xpdf::mw->DialogBox(-title=>'Select plotting colors ..',
                      -buttons=>['Done']);
  $col_dia->bind("<Return>",sub{});
  my $frame  = $col_dia->Frame()->pack(-side=>'top', -fill=>'x');

  ## title for the dialog box
  my $upper=$frame->Frame(-relief=>'ridge', -borderwidth=>2,
                          -bg=>"$colors::headerbg")
                  ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
  $upper->Label(-text=>"Select KUPLOT colors\n(click to change colors)",
                -font=>$colors::fontH,
                -bg=>"$colors::headerbg",-fg=>"$colors::headerfg")
        ->pack(-side=>'top',-expand=>1,-fill=>'x');

  ## main frame
  my $main=$frame->Frame(-relief=>'ridge', -borderwidth=>2)
                 ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);

  $Plot::l_bg=$main->Button(-text=>'Plot background color',
                      -relief=>'flat',-font=>$colors::fontM,
                      -bg=>$Plot::color[0],
                      -fg=>$Plot::color[6],
                      -width=>46,
                      -command=>sub{&kuplot_color_pick(0);})
             ->grid(-column=>0, -columnspan=>4, -row=>0,-padx=>5,-pady=>5);

  $Plot::l_fg=$main->Button(-text=>'Plot foreground color (= line 6 !)',
                      -relief=>'flat',
                      -bg=>$Plot::color[0],-font=>$colors::fontM,
                      -fg=>$Plot::color[6],
                      -width=>46,
                      -command=>sub{&kuplot_color_pick(6);})
             ->grid(-column=>0, -columnspan=>4, -row=>1,-padx=>5,-pady=>5);

  $main->Label(-text=>"",-font=>$colors::fontM)->grid(-column=>0,-row=>2);

  for (my $i=1; $i<=15; $i++) {
    my $row=int(($i-1)/4);
    my $col=$i-$row*4-1;
    $Plot::ll[$i]=$main->Button(-text=>"Line $i ..",
                          -relief=>'flat',-font=>$colors::fontM,
                          -bg=>$Plot::color[0],
                          -fg=>$Plot::color[$i],
                          -width=>8)
             ->grid(-column=>$col,-row=>3+$row,
                    -padx=>5,-pady=>5);
  }

  $Plot::ll[ 1]->configure(-command=>sub{&kuplot_color_pick( 1);});
  $Plot::ll[ 2]->configure(-command=>sub{&kuplot_color_pick( 2);});
  $Plot::ll[ 3]->configure(-command=>sub{&kuplot_color_pick( 3);});
  $Plot::ll[ 4]->configure(-command=>sub{&kuplot_color_pick( 4);});
  $Plot::ll[ 5]->configure(-command=>sub{&kuplot_color_pick( 5);});
  $Plot::ll[ 6]->configure(-command=>sub{&kuplot_color_pick( 6);});
  $Plot::ll[ 7]->configure(-command=>sub{&kuplot_color_pick( 7);});
  $Plot::ll[ 8]->configure(-command=>sub{&kuplot_color_pick( 8);});
  $Plot::ll[ 9]->configure(-command=>sub{&kuplot_color_pick( 9);});
  $Plot::ll[10]->configure(-command=>sub{&kuplot_color_pick(10);});
  $Plot::ll[11]->configure(-command=>sub{&kuplot_color_pick(11);});
  $Plot::ll[12]->configure(-command=>sub{&kuplot_color_pick(12);});
  $Plot::ll[13]->configure(-command=>sub{&kuplot_color_pick(13);});
  $Plot::ll[14]->configure(-command=>sub{&kuplot_color_pick(14);});
  $Plot::ll[15]->configure(-command=>sub{&kuplot_color_pick(15);});

  ## Show dialog
  my $val=$col_dia->Show;
  $col_dia->destroy();

  ## Set colors in KUPLOT
  if ($xpdf::kuplot) {
    &kuplot_colors();
    &kuplot_send("plot");
  }
}

#-----------------------------------------------------------------------------
# Color picker 
#-----------------------------------------------------------------------------

sub kuplot_color_pick {
    my ($ic) = $_[0];

    my $sel_col = $xpdf::mw->chooseColor(-title=>'Select color',
                                         -initialcolor=>$Plot::color[$ic]);
    if ($sel_col) { 
      $Plot::color[$ic]=$sel_col; 

      $Plot::l_bg->configure(-bg=>$Plot::color[0],-fg=>$Plot::color[6]);
      $Plot::l_fg->configure(-bg=>$Plot::color[0],-fg=>$Plot::color[6]);
      for (my $i=1; $i<=15; $i++) {
        $Plot::ll[$i]->configure(-bg=>$Plot::color[0],-fg=>$Plot::color[$i]);
      }
    }
}
