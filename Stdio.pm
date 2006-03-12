####################
# Created by Peter Peterson on February 10, 1999
# Last Modified on February 18, 1999
####################
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

    Stdio.pm - version 1

=head1 OVERVIEW of Stdio

    This program is meant to be a set of tools for input/output from/to 
    STDIN/STDOUT.

=head2 Included files

    none

=head2 Modifier flags

=over 4

=item -help

    Displays the help information.

=back

=head1 FUNCTIONS

=over 4

=cut

package Stdio;
use strict;

#####
=pod

=item int askNo(string)

    Returns true if a word starting with n or nothing is entered. Returns 
    false if a word starting with y is entered. Iterates for all other 
    responses.

=cut

sub askNo{
    my($question,$answer,$val);
    chomp($question=$_[0]);
    $question=$question."     ";
    $answer=&readIn($question);
    if($answer=~/^y/i){
	$val=0;
    } elsif($answer=~/^n/i){
	$val=1;
    } elsif($answer eq "") {
	$val=1;
    } else {
	$val=2;
	until(($val==0)||($val==1)){
	    $val=&askNo("Answer (Y)es or (N)o: ");
	}
    }
    return $val;
}

#####
=pod

=item int askYes(string)

    Returns true if a word starting with y or nothing is entered. Returns 
    false if a word starting with n is entered. Iterates for all other 
    responses.

=cut

sub askYes{
    my($question,$answer,$val);
    chomp($question=$_[0]);
    $question=$question."     ";
    $answer=&readIn($question);
    if($answer=~/^n/i){
	$val=0;
    } elsif($answer=~/^y/i){
	$val=1;
    } elsif($answer eq ""){
	$val=1;
    } else {
	$val=2;
	until(($val==0)||($val==1)){
	    $val=&askYes("Answer (Y)es or (N)o: ");
	}
    }
    return $val;
}

#####
=pod

=item string getDate(void)

    Uses the system's date command to find out the current date and time in 
    wkday mnth date time zone year format.

=cut

sub getDate{
    my($val);
    chomp($val=localtime);
    return $val;
}

#####
=pod

=item string getFilename(void)

    Uses getDate to create a filename that is unique.

=cut

sub getFilename{
    my($val,@field);
    $val=getDate();
    @field=split(/\s+/,$val);
    chop($field[0]);
    chop($field[3]);
    chop($field[4]);
    $val=$field[0].$field[2].$field[1].$field[3].$field[4].$field[5];
    return $val;
}

#####
=pod

=item string readIn(string)

    Prints the string parameter to screen and captures all keystrokes up to 
    the end of line and returns the result. Remember, perl does not care 
    whether what is read in is a string or number until it is used and then 
    it determines from context.

=cut

sub readIn{
    my($question,$answer);
    $question=$_[0];
    print STDOUT "$question";
    chomp($answer=<STDIN>);
    return $answer;
}

1;				# return true if loaded by another program
