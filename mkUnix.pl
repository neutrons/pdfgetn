#!/bin/perl
#----------------------------------------------------------------------------
# Prepares PDFgetN-x.x.x.tar.gz for UNIX distribution
#----------------------------------------------------------------------------

use strict;
my $line;
my $date;
my $version;

####################### Step 0: Get version #################################

open(FILE,"PDFgetN.pl") || die "Could not open PDFgetN.pl: $!";
        PDF: while(<FILE>){
            if(/\$xpdf::version/){ $version="$_";
            $version =~ s/\$xpdf::version//;
            $version =~ s/"//g;
            $version =~ s/;//g;
            $version =~ s/=//g;
            $version =~ s/\s//g;
            last PDF; }
        }
        close(FILE) || die "Could not close PDFgetN: $!";

####################### Step 1: Clean all program subdirectories ############

system("cd fortran ; make clean ; cd .. ;");
system("cd bin2asc ; make clean ; cd .. ;"); 
system("cd raw2asc/ipns ; make clean ; cd ../.. ;"); 
system("rm -f binary/*");
system("rm -f PDFgetN.exe");
system("rm -f .temp.hst");
system("rm -f tutorial/SEPD/.temp.hst");
system("rm -f tutorial/NPDF/.temp.hst");
system("rm -f *~");

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

####################### Step 3: Convert CR/LF for UNIX ######################

system ("dos2unix 00INSTALL LICENSE.txt CHANGES.LOG PDFgetN.htm");
system ("dos2unix defaults neutron.table *.pm *.pl templates/*");
system ("dos2unix tutorial/batch/* tutorial/SEPD/* tutorial/NPDF/*");

####################### Step 4: Create TAR file #############################

system ("cd .. ; tar --exclude='*CVS*' -cvzf ../downloads/PDFgetN-$version.tar.gz PDFgetN/");

print "##############################################################\n";
print "# Archive PDFgetN-$version.tar.gz successfully build ...        #\n";
print "##############################################################\n";
