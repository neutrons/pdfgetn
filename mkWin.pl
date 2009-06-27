#!/usr/bin/perl
#----------------------------------------------------------------------------
# Prepares PDFgetN distribution for making Windows installer
#----------------------------------------------------------------------------

my $val;
my $line;
my $date;

print "Building Windows binaries ..\n";

####################### Step 1: Make all programs ###########################

chdir "fortran";
&makebin();
chdir "../bin2asc";
&makebin();
chdir "../raw2asc/ipns";
&makebin();
chdir "../..";

####################### Step 2: Insert build date ###########################

rename ("PDFgetN.pl", "PDFgetN.pl.old");
open(OLD,"PDFgetN.pl.old") || die "Could not open PDFgetN.pl.old: $!";
open(NEW,">PDFgetN.pl") || die "Could not open PDFgetN.pl: $!";

$date = `date`; chomp($date);

foreach $line (<OLD>) {
   if($line=~/\$xpdf\:\:build=/) {
      $line="\$xpdf\:\:build=\"$date\";\n";
   } 
   print NEW "$line";
}
close(NEW) || die "Could not close PDFgetN.pl: $!";
close(OLD) || die "Could not close PDFgetN.pl.old: $!";
unlink("PDFgetN.pl.old");

####################### Step 4: Convert CR/LF for DOS/Windows ###############

#system ("unix2dos 00INSTALL LICENSE.txt CHANGES.LOG PDFgetN.htm");
#system ("unix2dos defaults neutron.table");
#system ("unix2dos templates/GSAS.temp");
#system ("unix2dos templates/IPNS_Ascii.temp");
#system ("unix2dos templates/ISIS_Ariel.temp");
#system ("unix2dos templates/ISIS_Norm.temp");
#system ("unix2dos tutorial/SEPD/*.hst");
#system ("unix2dos tutorial/NPDF/*.gr");
#system ("unix2dos tutorial/NPDF/*.sq");

print "##############################################################\n";
print "# You are now ready to build the PDFgetN binary ..           #\n";
print "# You are now ready to build the Windows installer !!        #\n";
print "##############################################################\n";

sub makebin 
{
  $val=system("make clean");
  $val=$val/256; if ($val){ print STDOUT "Build failed - $!\n"; exit; }
  $val=system("make install");
  $val=$val/256; if ($val){ print STDOUT "Build failed - $!\n"; exit; }
  $val=system("make clean");
  $val=$val/256; if ($val){ print STDOUT "Build failed - $!\n"; exit; }
}
