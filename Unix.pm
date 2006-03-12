#-----------------------------------------------------------------------------
# Here live Unix related subroutines ..
#-----------------------------------------------------------------------------

package OS;
use strict;


#-----------------------------------------------------------------------------
# OS specific file opening
#-----------------------------------------------------------------------------

sub openFile {

  my ($name)=@_;

  my $err=open(GLOBE::FILE,$name);
  return $err;
}

#-----------------------------------------------------------------------------
# OS specific defaults
#-----------------------------------------------------------------------------

sub defaults {
  $colors::selectcol=$colors::headerfg;  # Box checked by color !

  $colors::mainfont   = "Arial-normal-roman-8";
  $colors::headfont   = "Arial-normal-roman-8";
  $colors::logfont    = "Courier-normal-roman-8";

}

1;
