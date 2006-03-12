##############################
# Created by Thomas Proffen on January, 18, 2000
# Last Modified on 
##############################
#
# This is part of the PDFgetN distribution written by Peter Peterson,
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
use Tk;
use strict;
package colors;

#####
=pod

=item void Defaults()

    This subroutine reads the default file either from the file
    .pdfgetn in the users home directory of the system defaults.

=cut

sub Defaults {

    ## hardwired defaults

    $FLAG::noOut = 1;
    $FLAG::startPanel = "e";

    $colors::foreground = "#000000";
    $colors::background = "#d2d2d2";
    $colors::headerfg   = "#c3063c";
    $colors::headerbg   = "#afafaf";
    $colors::successbg  = "#008040";
    $colors::successfg  = "#ffff80";
    $colors::highlight1 = "#3f75a2";
    $colors::highlight2 = "#daa520";

    &Plot::kuplot_defaults();
    &OS::defaults();

    ## now we read defaults files if available

    if (&File::checkRead($GLOBE::localDefault)) {
       &readDefaults($GLOBE::localDefault);
    } else {
       if (&File::checkRead($GLOBE::globalDefault)) {
         &readDefaults($GLOBE::globalDefault);
       }
    }

    ## Create fonts
                                                                                
    my @font=split /-/,$colors::mainfont;
    $colors::fontM=$xpdf::mw->Font(-family=>$font[0],-weight=>$font[1],
                                   -slant=>$font[2],-size=>$font[3]);
    my @font=split /-/,$colors::headfont;
    $colors::fontH=$xpdf::mw->Font(-family=>$font[0],-weight=>$font[1],
                                   -slant=>$font[2],-size=>$font[3]);
    my @font=split /-/,$colors::logfont;
    $colors::fontL=$xpdf::mw->Font(-family=>$font[0],-weight=>$font[1],
                                   -slant=>$font[2],-size=>$font[3]);

}

#####
=pod

=item void writeDefaults(file)

    This subroutine write the current settings to the users default
    file.

=cut

sub writeDefaults {

   my ($fname) = $_[0];

   open(DEF,">$fname") || die "Could not open $fname: $!";
   print DEF "##################################################\n";
   print DEF "# Defaults for PDFgetN\n";
   print DEF "#\n";
   print DEF "# The file is organized in pairs of keyword and\n";
   print DEF "# value on separate lines.\n";
   print DEF "##################################################\n";
   print DEF "\n";
   print DEF "## Removing intermediate files\n";
   print DEF "remove = $FLAG::noOut\n";
   print DEF "\n";
   print DEF "## Active panel\n";
   print DEF "panel = $FLAG::startPanel\n";
   print DEF "\n";
   print DEF "## Colour scheme\n";
   print DEF "foreground = $colors::foreground\n";
   print DEF "background = $colors::background\n";
   print DEF "headerfg   = $colors::headerfg\n";
   print DEF "headerbg   = $colors::headerbg\n";
   print DEF "successbg  = $colors::successbg\n";
   print DEF "successfg  = $colors::successfg\n";
   print DEF "highlight1 = $colors::highlight1\n";
   print DEF "highlight2 = $colors::highlight2\n";
   print DEF "mainfont   = $colors::mainfont\n";
   print DEF "headfont   = $colors::headfont\n";
   print DEF "logfont    = $colors::logfont\n";
   print DEF "\n";
   print DEF "## Working directory\n";
   print DEF "workdir    = $GLOBE::workdir\n"; 
   print DEF "\n";
   print DEF "## Plotting defaults\n";
   for (my $i=0; $i<=15; $i++) {
     print DEF "plotcol$i  ";
     if ($i<10) {print DEF " ";}
     print DEF "= $Plot::color[$i]\n"; 
   }
   print DEF "\n";
   print DEF "print_file = $Plot::prn_file\n";
   print DEF "print_fnam = $Plot::prn_name\n";
   print DEF "print_cmd  = $Plot::prn_dev \n";
   print DEF "error_bars = $Plot::error_bars\n";
   print DEF "markers    = $Plot::markers\n";
   print DEF "interactiv = $Plot::interactive\n";
   print DEF "plot_rsoq  = $Plot::reduced_soq\n";
   print DEF "plot_blend = $Plot::cut_range\n";
   print DEF "plot_keep  = $Plot::keep_range\n";

   close(DEF);

   $xpdf::mess = "Written defaults to $fname";
}

#####
=pod

=item void readDefaults(file)

    This subroutine reads the defaults from the file $HOME/.pdfgetn 
    or if that does not exist from the system default file.

=cut

sub readDefaults {

   my ($fname) = $_[0];
   my $line;
   my @item;

   open(DEF,"$fname") || die "Could not open $fname: $!";
   foreach $line (<DEF>){
      $line =~ s/\s+//g;
      @item = split (/=/,$line);

      if ($item[0] =~ /remove/) {
        $FLAG::noOut=$item[1];
      }
      elsif ($item[0] =~ /panel/) {
        $FLAG::startPanel = $item[1];
      }
      elsif ($item[0] =~ /background/) {
        $colors::background = $item[1];
      }
      elsif ($item[0] =~ /foreground/) {
        $colors::foreground = $item[1];
      }
      elsif ($item[0] =~ /headerfg/) {
        $colors::headerfg = $item[1];
      }
      elsif ($item[0] =~ /headerbg/) {
        $colors::headerbg = $item[1];
      }
      elsif ($item[0] =~ /successfg/) {
        $colors::successfg = $item[1];
      }
      elsif ($item[0] =~ /successbg/) {
        $colors::successbg = $item[1];
      }
      elsif ($item[0] =~ /highlight1/) {
        $colors::highlight1 = $item[1];
      }
      elsif ($item[0] =~ /highlight2/) {
        $colors::highlight2 = $item[1];
      }
      elsif ($item[0] =~ /mainfont/) {
        $colors::mainfont = $item[1];
      }
      elsif ($item[0] =~ /headfont/) {
        $colors::headfont = $item[1];
      }
      elsif ($item[0] =~ /logfont/) {
        $colors::logfont  = $item[1];
      }
      elsif ($item[0] =~ /workdir/) {
        $GLOBE::workdir = $item[1];
      }
      elsif ($item[0] =~ /plotcol/) {
        for (my $i=0; $i<=15; $i++) {
          if ($item[0] =~ /plotcol$i/) {
            $Plot::color[$i] = $item[1];
          } 
        }
      }
      elsif ($item[0] =~ /print_file/) {
        $Plot::prn_file = $item[1];
      }
      elsif ($item[0] =~ /print_fnam/) {
        $Plot::prn_name = $item[1];
      }
      elsif ($item[0] =~ /print_dev/) {
        $Plot::prn_dev = $item[1];
      }
      elsif ($item[0] =~ /error_bars/) {
        $Plot::error_bars = $item[1];
      }
      elsif ($item[0] =~ /markers/) {
        $Plot::markers = $item[1];
      }
      elsif ($item[0] =~ /interactiv/) {
        $Plot::interactive = $item[1];
      }
      elsif ($item[0] =~ /plot_rsoq/) {
        $Plot::reduced_soq = $item[1];
      }
      elsif ($item[0] =~ /plot_blend/) {
        $Plot::cut_range = $item[1];
      }
      elsif ($item[0] =~ /plot_keep/) {
        $Plot::keep_range = $item[1];
      }
   }
   close(DEF);
   $xpdf::mess = "Defaults read from $fname";
}

#####
=pod

=item void setColor(color)

    This subroutine opens color dialog and sets corresponding color.
    However PDFgetN needs to be restarted to have the changes take
    effect.

=cut

sub setColor {
    my ($color) = $_[0];

    my $sel_col = $xpdf::mw->chooseColor(-title=>'Select color',
                                         -initialcolor=>$color);

    if ($sel_col) {
      $_[0]=$sel_col;
      &colors::writeDefaults($GLOBE::localDefault);
      $xpdf::mess="You have to restart PDFgetN to activate the change";
    }
}

#####
=pod

=item string convertColor(color)

    This routine converts the color 'color' to a RGB triplet.

=cut

sub convertColor {
    my ($color) = $_[0];

    $color=~s/#//;
    my $value=hex($color);
    my $r=int($value/256/256);
    my $g=int(($value-$r*256*256)/256);
    my $b=int($value-$r*256*256-$g*256);

    return ($r/255.,$g/255.,$b/255.);
}

1;
