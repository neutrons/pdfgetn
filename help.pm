##############################
# Created by Peter Peterson on May 14, 1999
# Last Modified on December 13, 1999
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
use Tk;
use strict;
package help;

#####
=pod

=item void aboutDialog()

    This the dialog showing program information.

=cut

sub aboutDialog{
	my ($widget) = $_[0];

        my $about=$widget->DialogBox(-title=>'About PDFgetN',-buttons=>['OK']);

	$about->bind("<Return>",sub{});

	## Program name
	my $f1=$about->Frame(-relief=>'ridge', -borderwidth=>2,
                             -bg=>"$colors::headerbg")
	             ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    	$f1->Label(-text=>"PDFgetN", -fg=>"$colors::headerfg", 
                             -bg=>"$colors::headerbg",
                             -font=>$colors::fontH)->pack();
    	$f1->Label(-text=>"Version: $xpdf::version", 
                             -bg=>"$colors::headerbg",
                             -font=>$colors::fontM)->pack();
    	$f1->Label(-text=>"Build: $xpdf::build", 
                             -bg=>"$colors::headerbg",
                             -font=>$colors::fontM)->pack();

	## Authors
	my $f2=$about->Frame(-relief=>'ridge', -borderwidth=>2)
	             ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    	$f2->Label(-text=>"written by",-font=>$colors::fontM)->pack();
    	$f2->Label(-text=>"$xpdf::authors",-font=>$colors::fontM)->pack();

	## WWW Link
	my $f3=$about->Frame(-relief=>'ridge', -borderwidth=>2)
	             ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    	$f3->Label(-text=>"Homepage",-font=>$colors::fontM)->pack();
    	$f3->Label(-font=>$colors::fontM,
                   -text=>"$xpdf::www", 
                   -fg=>"$colors::highlight1")->pack();

        $about->Show;
        $about->destroy();
}

#####
=pod

=item void click()

    This routine pops up a help dialog if the right mouse button
    is clicked.

=cut

sub click{
    my($widget,$msg)=@_;
    
    $widget->bind('<Button-3>',sub{&help::displayHelp($widget,
                                    "Help on topic ..",$msg);});
}

#####
=pod

=item void displayHelp()

    This routine actually displays the help message.
    is clicked.

=cut

sub displayHelp{
    	my($widget,$help,$msg)=@_;
    
        if ($FLAG::noGui > 0) { print "$msg\n"; return 1; }

	my $dialog=$widget->Dialog(-title=>$help,-buttons=>['OK']);

	$dialog->bind("<Return>",sub{});

	## Header
	my $f1=$dialog->Frame(-relief=>'ridge', -borderwidth=>2,
                              -bg=>"$colors::headerbg")
	              ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    	$f1->Label(-text=>"PDFgetN", -fg=>"$colors::headerfg", 
                              -font=>$colors::fontH,
                              -bg=>"$colors::headerbg")->pack();
    	$f1->Label(-text=>"Online help", -bg=>"$colors::headerbg",
                   -font=>$colors::fontM)->pack();

	## Help text
	my $f2=$dialog->Frame(-relief=>'ridge', -borderwidth=>2, -width=>25)
	              ->pack(-side=>'top',-fill=>'x',-padx=>5,-pady=>5);
    	my $ft=$f2->Scrolled("Text",-scrollbars=>'oe',-font=>$colors::fontM)
                  ->pack();
	   $ft->configure(-wrap=>'word',-width=>55, -height=>18);

	$ft->bind("<Return>",sub{});
	$ft->insert("end",$msg);
	$ft->configure(-state=>'disabled');

	$dialog->Show();
	$dialog->destroy();
}

#####
=pod

=item void tutorial()

    This is the dialog showing tutorial information.

=cut

sub tutorial{
    my($widget)=@_;

	my $msg ="The distribution of PDFgetN contains the files ";
           $msg.="needed to run the tutorial described in the manual. ";
           $msg.="To run it, simply copy the files from the directory ";
           $msg.="given below to your working area and start PDFgetN ";
           $msg.="and follow the instructions given in the manual.\n\n";
           $msg.="The tutorial files are located in the directory ";
           $msg.="\n\n     $xpdf::tutorial \n\non your machine.";

	&help::displayHelp($widget,"How to use the tutorial ...",$msg);
}

#####
=pod

=item void howto()

    This is the dialog showing program information.

=cut

sub howto{
    my($widget)=@_;

	my $msg ="This program is designed to be self explanatory. ";
           $msg.="Most of what you need to know is in the layout of ";
           $msg.="the program. If there is a question about what a ";
           $msg.="particular field is or how it affects the program ";
           $msg.="right click on it and a dialog box will pop up with ";
           $msg.="a definition of the item.\n\n";
           $msg.="Questions, comments and bug reports email: ";
           $msg.="$xpdf::bug.";

	&help::displayHelp($widget,"How to use PDFgetN ...",$msg);
}

#####
=pod

=item void vLicense()

    This is the dialog showing the license in 'LICENSE.txt'.

=cut

sub vLicense{
    my($widget)=@_;

    my $filename="$GLOBE::binpath/LICENSE.txt";

    my $msg ='';
    
    open(FILE,$filename) || die "Could not open $filename: $!";
    while(<FILE>){
	if (/\w/) {s/\n/ /;} else {s/\n/\n\n/;} 
        $msg.="$_";
    }
    close(FILE) || die "Could not close $filename: $!";

    &help::displayHelp($widget,"PDFgetN License: LICENSE.txt",$msg);
}

1;
