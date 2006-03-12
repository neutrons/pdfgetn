#!/usr/bin/perl
#!c:/perl/bin/perl.exe
##################################################
# Created by Peter Peterson on December 15, 1999
# Last modified on January 10, 1999
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

  PDFqual

=head1 OVERVIEW of PDFqual

  This is a program to calculate PDF quality factors.

=head2 Included files

  Qual.pm

=head2 Modifier flags

  none

=head1 TECHNICAL DOCUMENTATION

=head2 Usage

  The program is to be called with the name base of the files being
  produced.

=cut

use FindBin qw($RealBin);
$binpath = "$RealBin";
require "$binpath/Qual.pm";

$out=&Qual::pdfQual(@ARGV);
print $out;

