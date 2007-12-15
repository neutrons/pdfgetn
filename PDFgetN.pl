#!/usr/bin/perl
##################################################
# Created by Peter Peterson on March 31, 1999
# Last modified on January 17, 2000
# Version 1.6.x
##################################################
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

    PDFgetN - version 1.6.x

head1 OVERVIEW of PDFgetN

    This program is intended to be a front end to data analysis code
    written in fortran from multiple sources. The idea is to make it
    easier for the user to perform analysis by removing the difficulty
    of creating input files and changing the same value in multiple
    input files. The approach is to use a graphic user interface, GUI,
    to create entry fields and buttons which perform the most common
    operations done in data analysis while leaving the less common
    options to be changed by other means.  Editing the history file
    directly will allow the user to change some things while the
    infrequently used options can be explored by using PDFgetN to
    create the input files and then editing them by hand. Invoking
    PDFgetN and looking at a history file will give a better idea as
    to what PDFgetN does.

    PDFgetN is also intended to make keeping track of data easier by
    prepending a history file to all output files that are saved. The
    other purpose to this is when a user wants to see an intermediate
    file for a given pair distribution function the file can be read
    in as a history and rerun.

    The supported data format are NORM (from e.g. GLAD, LAD, POLARIS,
    ..), SEPD (from SEPD and GPPD), ARIEL (from e.g. GEM) and
    HIPD. Since the HIPD prep-program is not yet rewritten some
    features such as the different smoothing algorithms and the
    display of *.?smo and *.?raw files are not supported.

=head2 Included files

    File.pm, Hist.pm, Stdio.pm, table, body, help, rprepnorm.pm, 
    rprepsepd.rm, rprepariel.pm, rprepgsas.pm, rcorps.pm, rsoqd.pm, 
    rblend.pm, rdamp.pm, rft.pm

=head2 Modifier flags

=over 4

=item -nogui

    Run PDFgetN using the specified history file and without the graphic user 
    interface. This is intended to quickly explore different options by 
    using a script produced by the user to modify particular values in
    the history file and rerun the analysis.

=item -keep

    Keep all intermediate files.

=back

=head1 TECHNICAL DOCUMENTATION

=head2 Global Variables Used

=over 4

=item $GLOBE::canFile

    The data file of the container measurement.

=item $GLOBE::historyname

    The name of the temporary history file. This is defined in Hist.pm.

=item $GLOBE::machineName

    The name of the machine where the data was taken.

=item $GLOBE::prepName

    The name of the prep program (i.e. data format)

=item @GLOBE::runFile

    The sample data file(s).

=item $GLOBE::vanFile

    The data file containing the vanadium measurement.

=back

=head2 Functions

=over 4

=cut

use Tk;
require Tk;
require Tk::Checkbutton;
require Tk::Dialog;
require Tk::DialogBox;
require Tk::Entry;
require Tk::Label;
require Tk::Listbox;
require Tk::Menu;
require Tk::Menubutton;
require Tk::Optionmenu;
require Tk::Radiobutton;
require Tk::Scrollbar;
require Tk::ProgressBar;
require Tk::Text;
require Tk::Font;

require IO::Handle;
require IO::Pipe;
require IPC::Open2;

require Fcntl;

use IO::Handle;
use FindBin qw($RealBin);
$GLOBE::binpath = "$RealBin";
$GLOBE::execpath = "$GLOBE::binpath/binary";

require "$GLOBE::binpath/File.pm";
require "$GLOBE::binpath/Hist.pm";
require "$GLOBE::binpath/colors.pm";
require "$GLOBE::binpath/Stdio.pm";
require "$GLOBE::binpath/Plot.pm";
require "$GLOBE::binpath/Qual.pm";
require "$GLOBE::binpath/table.pm";
require "$GLOBE::binpath/body.pm";
require "$GLOBE::binpath/help.pm";
require "$GLOBE::binpath/rcorps.pm";
require "$GLOBE::binpath/rsoqd.pm";
require "$GLOBE::binpath/rblend.pm";
require "$GLOBE::binpath/rdamp.pm";
require "$GLOBE::binpath/rft.pm";

# Some OS dependent things
if ($^O =~/win32/i) {
  require "$GLOBE::binpath/Win.pm";
} else {
  require "$GLOBE::binpath/Unix.pm";
}

use strict;

package xpdf;

$xpdf::version ="1.6.4";
$xpdf::authors ="Peter Peterson, Thomas Proffen, ";
$xpdf::authors.="Matthias Gutmann & Simon Billinge\n";
$xpdf::authors.="Michigan State University\n";
$xpdf::authors.="Department of Physics and Astronomy\n";
$xpdf::authors.="East Lansing, MI 48824-1116, USA";
$xpdf::www     ="http://www.totalscattering.org/programs/PDFgetN/";
$xpdf::bug     ="billinge\@pa.msu.edu";

$xpdf::status='';
$xpdf::process='';
$xpdf::exist=1;
$xpdf::tutorial="$GLOBE::binpath/tutorial/";
$xpdf::build="Wed Sep  5 09:00:05 MDT 2007";

if ($ENV{'HOME'}) {$GLOBE::localDefault = "$ENV{'HOME'}/.pdfgetn";}
else              {$GLOBE::localDefault = "$GLOBE::binpath/.pdfgetn";}

$GLOBE::globalDefault = "$GLOBE::bindir/defaults";
$GLOBE::fileid=101;

#################### Can we write in the current directory

if (not -W './') {
  print STDERR "PDFgetN must have write permission in current directory !\n";
#  exit 1;
}

#################### Preloading stuff

procCommand();

## No GUI version

if($FLAG::noGui>0) {
    print "PDFgetN $xpdf::version starting up ..\n\n";

    unless (&Hist::getHistName()) {exit -1;}
    unless (&Hist::read($xpdf::histfile)) {exit -1;}
    unless (&Hist::read($GLOBE::historyname)) {exit -1;}

    if ($FLAG::cmdParameters) {&procParameters();}

    &File::setpath($xpdf::histfile);
    if ($GLOBE::workdir) {
      print "Working directory : $GLOBE::workdir\n\n";
    }
    &table::getStats();

    if ($FLAG::autoNormalize) {
      &body::autoNormalize();
    } else {
      &body::whichOut('a');
      print "Creating S(Q) ...\n";
      &body::makeSOQ();
      print "Blending S(Q) ...\n";
      &body::makeBLEND();
      print "Creating G(r) ...\n";
      &body::makePDF();
    }

    my $runname=(split /,/,$GLOBE::runFile)[0];
    $xpdf::histfile =~s/.hst//;
    $xpdf::histfile =~s/.sq//;
    $xpdf::histfile =~s/.gr//;

    if ($FLAG::autoOptimize) {
      $GLOBE::optMax=20;
      my($glow,$iter,$label);
      print "Running optimization for G(r) ...\n";
      &body::optimize($xpdf::histfile,\$glow,\$iter,\$label);
    }

    unless ($xpdf::histfile eq $runname) {
        rename $runname.".gr",$xpdf::histfile.".gr";
        rename $runname.".sq",$xpdf::histfile.".sq";
    }

    print "Saving final S(Q) in file $xpdf::histfile.sq ...\n";
    print "Saving final G(r) in file $xpdf::histfile.gr ...\n";

    &xpdf::cleanup();
    exit;
}

#################### Open windows ..

$xpdf::mw=MainWindow->new;
&colors::Defaults();

$xpdf::mw->title("PDFgetN $xpdf::version");
$xpdf::mw->iconname("PDFgetN");
$xpdf::mw->resizable(0,0);

$xpdf::mw->setPalette("background"=>$colors::background,
                      "foreground"=>$colors::foreground);

#################### Menu bar at the top of the window
my $menu_f=$xpdf::mw->Frame(-relief=>'raised',
	       -borderwidth=>1)->pack(-side=>'top',-expand=>1,-fill=>'x');
# file menu
my $file_m=$menu_f->Menubutton(-text=>'File',
                               -font=>$colors::fontM,
			       -tearoff=>0)->pack(-side=>'left');
$file_m->command(-label=>'New', 
                 -command=>sub{&doSetup($menu_f,"");});
$file_m->separator();
$file_m->command(-label=>'Load', 
                 -command=>sub{&File::loadDialog($menu_f);});
$file_m->command(-label=>'Save', 
                 -command=>sub{
		  	   my $val;
	          	   my $fname=(split /,/,$GLOBE::runFile)[0];
		  	   if(&body::errorCheck('')){
		      	   if(($val=&File::saveDialog($menu_f,$fname))){
			  	   $FLAG::noTemp=1; } } });
$file_m->command(-label=>'Save GSAS',
                 -command=>sub{&File::saveGsasDialog($menu_f);});

$file_m->separator();
$file_m->command(-label=>'Exit', 
                 -command=>sub{$xpdf::mw->destroy();});

&body::bind_name($file_m,'File options');

##### option menu
my $opt_m=$menu_f->Menubutton(-text=>'Options',
                               -font=>$colors::fontM,
			      -tearoff=>0)->pack(-side=>'left');

##### keep  intermediate files cascading menu
$opt_m->checkbutton(-label=>'Delete input and log files',
		    -variable=>\$FLAG::noOut);

##### Set colors cascade

my $col_m=$opt_m->Menu(-tearoff=>0);
$col_m->command(-label=>'Foreground', 
                -command=>sub{&colors::setColor($colors::foreground);});
$col_m->command(-label=>'Background', 
                -command=>sub{&colors::setColor($colors::background);});
$col_m->separator();
$col_m->command(-label=>'Header foreground', 
                -command=>sub{&colors::setColor($colors::headerfg);});
$col_m->command(-label=>'Header background', 
                -command=>sub{&colors::setColor($colors::headerbg);});
$col_m->separator();
$col_m->command(-label=>'Button OK foreground', 
                -command=>sub{&colors::setColor($colors::successfg);});
$col_m->command(-label=>'Button OK background', 
                -command=>sub{&colors::setColor($colors::successbg);});
$col_m->separator();
$col_m->command(-label=>'Highlight one', 
                -command=>sub{&colors::setColor($colors::highlight1);});
$col_m->command(-label=>'Highlight two', 
                -command=>sub{&colors::setColor($colors::highlight2);});
$col_m->separator();
$col_m->command(-label=>'KUPLOT colors', 
                -command=>sub{&Plot::kuplot_color_select();});

##### Set colors
$opt_m->cascade(-label=>'Change colors ...');
$opt_m->entryconfigure('Change colors ...', -menu=>$col_m);

##### Save defaults
$opt_m->separator();
$opt_m->command(-label=>'Save defaults ...',
                -command=>sub{&colors::writeDefaults($GLOBE::localDefault);});

##### Help menu
my $help_m=$menu_f->Menubutton(-text=>'Help', -tearoff=>0,
                               -font=>$colors::fontM,
	    -menuitems=>[
		 ['command'=>'Using PDFgetN',
		  -command=>sub{&help::howto($xpdf::mw);}],
		 ['command'=>'Using the tutorial',
		  -command=>sub{&help::tutorial($xpdf::mw);}],
		 ['command'=>'View License',
		  -command=>sub{&help::vLicense($xpdf::mw)}],
                  '-',
		 ['command'=>'About',
                  -command=>sub{&help::aboutDialog($xpdf::mw)}]])
            ->pack(-side=>'right');

&body::bind_name($help_m,'Help information');

#################### Body resides in the file body

my $body_f=$xpdf::mw->Frame()->pack(-side=>'top');
&body::makeFrame($body_f);

unless (&doSetup($menu_f,$xpdf::histfile)) {
  &Plot::kuplot_close();
  exit;
}

#################### Now we are ready to go ........

&Tk::MainLoop();

&xpdf::cleanup();
&Plot::kuplot_close();

#####
=pod

=item void cleanup(void)

    Remove all temporary files ..

=cut

sub cleanup{
    if($FLAG::noOut){
        print "Cleaning up temporary files ... ";

        my $prefix_s=(split /,/,$GLOBE::runFile)[0];
        my $prefix_c=(split /,/,$GLOBE::vanFile)[0];
        my $prefix_v=(split /,/,$GLOBE::canFile)[0];

        &body::prep_clean($prefix_s);
        &body::prep_clean($prefix_c);
        &body::prep_clean($prefix_v);
        &rcorps::clean($prefix_s);
        &rsoqd::clean ($prefix_s);
        &rblend::clean($prefix_s);
        &rdamp::clean ($prefix_s);
        &rft::clean   ($prefix_s);

	&File::remove("soq2gsa.inp");
	&File::remove("soq2gsa.log");

        if($FLAG::noTemp){   # remove temporary history if recently saved
            &File::remove($GLOBE::historyname);
        }
        print "done \n";
    }
}
#####
=pod

=item void procParameters(void)

    This processes the parameters specified on command line

=cut

sub procParameters{

  my ($item);
  my @a=split(/,/,$FLAG::cmdParameters);

  foreach $item (@a) {
    my ($par,$val)=split(/=/,$item);
    if ($par eq "run") {
      $GLOBE::runFile=$val;
      print "Setting run file to $val ..\n";
    }
    elsif ($par eq "temp") {
      $GLOBE::temp=$val;
      print "Setting temperature to $val ..\n";
    }
    elsif ($par eq "dens") {
      $GLOBE::effSampleDensity=$val;
      print "Setting effective density to $val ..\n";
    }
    else {
      print "Parameter $val invalid - ignoring ..\n";
    }
  }

}

#####
=pod

=item void procCommand(void)

    This proccesses the command line arguments given upon invocation.

=cut

sub procCommand{
    my($item);
    $FLAG::noGui=0;
    $FLAG::numHist=0;
    $FLAG::debug=0;
    $FLAG::autoOptimize=0;
    $FLAG::autoNormalize=0;
    $FLAG::cmdParameters="";
    
    foreach $item (@ARGV){
	if($item eq "-nogui"){
	    $FLAG::noGui=1;
	} elsif($item eq "-debug"){
	    $FLAG::noOut=0;
    	    $FLAG::debug=1;
	    print STDOUT 
	          "DEBUG mode: no processing programs will we executed !\n";
	} elsif($item eq "-keep"){
	    $FLAG::noOut=0;
	} elsif($item eq "-nokeep"){
	    $FLAG::noOut=1;
	} elsif($item eq "-opt"){
	    $FLAG::autoOptimize=1;
	} elsif($item eq "-noopt"){
	    $FLAG::autoOptimize=0;
	} elsif($item eq "-norm"){
	    $FLAG::autoNormalize=1;
	} elsif($item eq "-nonorm"){
	    $FLAG::autoNormalize=0;
	} elsif($item=~/^-P/){
	    $FLAG::cmdParameters=$item;
	    $FLAG::cmdParameters=~s/^-P//;
	} elsif($item eq "-help"){
	    print "Usage: PDFgetN [-nogui] [-[no]keep] [-debug] [-[no]opt] \n";
            print "               [-[no]norm] [-Ppara=value,..] ";
            print "[history-file]\n\n";
            print "       Allowed parameters for -P (only with -nogui):\n\n";
            print "         run=sample file name\n";
            print "         temp=Temperature\n";
            print "         dens=effective density\n";
            exit;
	} else {
	    $FLAG::numHist+=1;
	    $xpdf::histfile=$item;
	}
    }
    if($FLAG::numHist>1){
	die "Too many history files specified.\n";
    }
}

#####
=pod

=item val doSetup(widget,histfile)

    Let user select file, data format or previous temporary history
    file if present.

=back

=cut

sub doSetup{
#################### Before we even start select data format or load history

    my($widget,$histfile)=@_;
    my($tname,$filename,$val,$what)=('','','','temp');

    unless (&Hist::read($histfile)) {

       my $dialog = $widget->DialogBox(-title=>'Setup !',
                                   -buttons=>['OK','Cancel']);
       my $frame  = $dialog->Frame()->pack(-side=>'top', -fill=>'x');

       ### change to working directory
       if (&File::checkDir($GLOBE::workdir)) 
          {chdir($GLOBE::workdir);}
       else
          {$GLOBE::workdir="$ENV{'PWD'}";}

       ### title for the dialog box
       my $upper=$frame->Frame(-relief=>'ridge', -borderwidth=>2, 
                       -bg=>"$colors::headerbg")
                       ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);

       my $version ="Version: $xpdf::version";
       my $build   ="Build: $xpdf::build";

       $upper->Label(-text=>"Welcome to PDFgetN"
                                    ,-font=>"$colors::fontH",
                                    ,-bg=>"$colors::headerbg",
                                     -fg=>"$colors::headerfg")
             ->pack(-expand=>1,-fill=>'x');

       $upper->Label(-text=>$version,-bg=>"$colors::headerbg",
                     -font=>$colors::fontM)
             ->pack(-expand=>1,-fill=>'x');
       $upper->Label(-text=>$build ,-bg=>"$colors::headerbg",
                     -font=>$colors::fontM)
             ->pack(-expand=>1,-fill=>'x');

       ### copyright information
       my $lower=$frame->Frame(-relief=>'ridge', -borderwidth=>2)
                       ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
       my $lowertext=$lower->Scrolled("Text",-scrollbars=>'e',
                     -font=>$colors::fontM)->pack();
       $lowertext->configure(-wrap=>'word',-width=>75,-height=>12);

       # configure some text tags
       $lowertext->tagConfigure('blue',-foreground=>"$colors::highlight1");
       $lowertext->tagConfigure('underline',-underline=>1);
       $lowertext->tagConfigure('red',-foreground=>"$colors::headerfg");
       $lowertext->tagConfigure('green',-foreground=>"$colors::successbg");

       # insert text
       $lowertext->insert('end',"A data analysis package for obtaining the atomic Pair Distribution Function (PDF) from neutron powder diffraction data.",'bold');
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"Please cite the following paper when publishing results that made use of this program:");
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"P.F.Peterson, M.Gutmann, Th.Proffen, and S.J.L.Billinge, ",'green');
       $lowertext->insert('end',"PDFgetN: A user-friendly program to extract the total scattering structure function and the pair distribution function from neutron powder diffraction data, ",'green');
       $lowertext->insert('end',"J. Appl. Cryst., 33, 1192 (2000)",'green');
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"Homepage ",'bold');
       $lowertext->insert('end',"$xpdf::www",'blue');
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"To report bugs please email ");
       $lowertext->insert('end',"$xpdf::bug",'blue');
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"COPYRIGHT 2000 Michigan State University Board of Trustees",'underline');
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"Use and distribution of this progam is subject to the terms laid out in the license in LICENSE.TXT included with this distribution. A copy of the license can be obtained from Michigan State University office of Libraries, Computing and Technology (517-353-0722).");
       $lowertext->insert('end',"\n\n");
       $lowertext->insert('end',"MSU MAKES NO WARRANTY, EXPRESS OR IMPLIED TO END USER OR TO ANY OTHER PERSON OR ENTITY.  SPECIFICALLY, MSU MAKES NO WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE OF THE SOFTWARE. MSU WILL NOT BE LIABLE FOR SPECIAL, INCIDENTAL, CONSEQUENTIAL, INDIRECT OR OTHER SIMILAR DAMAGES, EVEN IF MSU OR ITS EMPLOYEES HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES, REGARDLESS OF THE FORM OF THE CLAIM.",'red');
       ## disable the widget
       $lowertext->configure(-state=>'disabled');

       ## title for selection
       my $l_label=$frame->Frame(-relief=>'ridge', -borderwidth=>2, 
                         -bg=>"$colors::headerbg")
                         ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
       $l_label->Label(-text=>'Select a History File',
                         -bg=>"$colors::headerbg",-fg=>"$colors::headerfg",
                         -font=>$colors::fontM)
                         ->pack(-expand=>1,-fill=>'x');

       ### what type of history file to start with
       my $l_frame=$frame->Frame(-relief=>'ridge',-borderwidth=>2)
                         ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
       $l_frame->bind('<Return>',sub{});

       $l_frame->Radiobutton(-variable=>\$what, 
                           -selectcolor=>"$colors::selectcol",
   	                   -text=>'Load template for format ',
                           -font=>$colors::fontM,
                           -value=>'temp')
             ->grid(-column=>0, -row=>3, -sticky=>'w',-pady=>5);
       my @templates;
       &getTemplates(\@templates);
       $l_frame->Optionmenu(-variable=>\$tname, -width=>16,
                            -options=>\@templates,
                            -font=>$colors::fontM,
                            -command=>sub{$what='temp';})
             ->grid(-column=>1, -row=>3, -sticky=>'w',-pady=>5);

       # get it from a file
       $l_frame->Radiobutton(-variable=>\$what, 
                           -selectcolor=>"$colors::selectcol",
	                   -text=>'Load history from file ',
                           -font=>$colors::fontM,
                           -value=>'file')
             ->grid(-column=>0, -row=>4, -sticky=>'w',-pady=>5);
       $l_frame->Entry(-textvariable=>\$filename,-width=>20,-takefocus=>0,
                       -font=>$colors::fontM)
	   ->grid(-column=>1, -row=>4, -sticky=>'w',-pady=>5)
	       ->bind('<Return>',sub{});
       $l_frame->Button(-text=>"Browse ...",-font=>$colors::fontM,
               -command=>sub{$filename=File::fileDialog($xpdf::mw,"load","");
                             if ($filename) {
	                       $what="file";}
                              })
               ->grid(-column=>2, -row=>4, -sticky=>'w',-pady=>5,-padx=>5);
               

       # if a temporary history file exists give it as an option
       if(&File::checkExist($GLOBE::historyname)){
	   $what='hist';
           $l_frame->Radiobutton(-variable=>\$what, 
                                 -selectcolor=>"$colors::selectcol",
    	                         -text=>'Load temporary history file ',
                                 -font=>$colors::fontM,
                                 -value=>'hist')
                 ->grid(-column=>0, -row=>5, -sticky=>'w',-pady=>5);
       }

       ## title for current working directory
       unless ($GLOBE::workdir) {$GLOBE::workdir=$GLOBE::binpath;}
       my $x_frame=$frame->Frame(-relief=>'ridge',-borderwidth=>2)
                         ->pack(-side=>'top',-expand=>1,-fill=>'x',-pady=>5);
       $x_frame->bind('<Return>',sub{});
       $x_frame->Label(-text=>'Current working directory ',
                       -font=>$colors::fontM)
               ->grid(-column=>0, -row=>4, -sticky=>'w',-pady=>5);
       $x_frame->Entry(-textvariable=>\$GLOBE::workdir,-width=>55,
                       -font=>$colors::fontM,-takefocus=>0)
	       ->grid(-column=>1, -row=>4, -sticky=>'w',-pady=>5)
	       ->bind('<Return>',sub{});

       ## Now show the dialog .. 
       $val=$dialog->Show;
       if($val eq 'OK'){
           if (&File::checkDir($GLOBE::workdir)) {
               chdir "$GLOBE::workdir"; 
           } else {
               $xpdf::mess="Could not change to $GLOBE::workdir";
           }

	   ## use a template
   	   if ($what eq 'temp') {
	       # create the filename to load
	       $filename = "$GLOBE::binpath/templates/$tname.temp"
	   }
	   ## use the temporary history file
	   elsif ($what eq 'hist') {
	       $filename = $GLOBE::historyname
	   }
	   if(&Hist::read("$filename")){
   		&body::makeTable_f($body::sample_f);
		&body::makeBankTable($body::bank_ftab);
		$xpdf::mess="$filename loaded";
	   } else {
		$xpdf::mess="Could not load $filename";
   	   }
	   $dialog->destroy();
	   return 1;
       } else {
	   return 0;
       }
   } else {
       &File::setpath($histfile);

       &body::makeTable_f($body::sample_f);
       &body::makeBankTable($body::bank_ftab);
       $xpdf::mess="$histfile loaded";
       return 1;
   }
}

#####
=pod

=item getTemplates(array)

    Returns all *.temp files in the template directory.

=back

=cut

sub getTemplates{
    my($array)=@_;

    while(<$GLOBE::binpath/templates/*.temp>){
	my $temp="$_";
	$temp=~s/$GLOBE::binpath\/templates\///;
	$temp=~s/.temp//;
	push(@{$array},$temp);
    }

    sort(@{$array});

}

