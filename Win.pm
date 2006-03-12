#-----------------------------------------------------------------------------
# Here live Windows related subroutines ..
#-----------------------------------------------------------------------------

package OS;
use strict;

#-----------------------------------------------------------------------------
# OS specific defaults
#-----------------------------------------------------------------------------

sub openFile {

  my ($name)=@_;

  my $err=open(GLOBE::FILE,$name);
  binmode(GLOBE::FILE);
  return $err;
}

#-----------------------------------------------------------------------------
# OS specific defaults
#-----------------------------------------------------------------------------

sub defaults {
                                                                                
   $colors::mainfont   = "Arial-normal-roman-9";
   $colors::headfont   = "Arial-normal-italic-10";
   $colors::logfont    = "Courier-normal-roman-8";

   $colors::selectcol=$colors::background;
}

1;
