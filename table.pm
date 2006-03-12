####################
# Created by Peter Peterson on March 5, 1999
# Last Modified on May 26, 1999
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

    table

=head1 OVERVIEW of table

    This program is a set of subroutines to allow the user to select
    the elements which a material is composed of and in what
    quantities. Once the user has exited the windows that appear this
    program will search a data file for the isotopically averaged
    values of mass and cross section. If other values are desired the
    user should edit the history file by hand.

=head2 Assumptions

    This is invoked by Xpdf. It is not intended for stand-alone use.

=head1 TECHNICAL DOCUMENTATION

=head2 Global Variables Used

=over 4

=back

=head2 Functions

=over 4

=cut

use Tk;
require "$GLOBE::binpath/Hist.pm";
use strict;

package table;

my($show)=("");
my($H,$He);
my($Li,$Be,$B,$C,$N,$O,$F,$Ne);
my($Na,$Mg,$Al,$Si,$P,$S,$Cl,$Ar);
my($K,$Ca,$Sc,$Ti,$V,$Cr,$Mn,$Fe,$Co,$Ni,$Cu,$Zn,$Ga,$Ge,$As,$Se,$Br,$Kr);
my($Rb,$Sr,$Y,$Zr,$Nb,$Mo,$Tc,$Ru,$Rh,$Pd,$Ag,$Cd,$In,$Sn,$Sb,$Te,$I,$Xe);
my($Cs,$Ba,$La,$Hf,$Ta,$W,$Re,$Os,$Ir,$Pt,$Au,$Hg,$Tl,$Pb,$Bi,$Po,$At,$Rn);
my($Fr,$Ra,$Ac);
my($Ce,$Pr,$Nd,$Pm,$Sm,$Eu,$Gd,$Tb,$Dy,$Ho,$Er,$Tm,$Yb,$Lu);
my($Th,$Pa,$U,$Np,$Pu,$Am,$Cm,$Bk,$Cf,$Es,$Fm,$Md,$No,$Lr);
my(%cbState);
my(@elements)=qw(H He Li Be B C N O F Ne Na Mg Al Si P S Cl Ar K Ca Sc Ti V Cr Mn Fe Co Ni Cu Zn Ga Ge As Se Br Kr Rb Sr Y Zr Nb Mo Tc Ru Rh Pd Ag Cd In Sn Sb Te I Xe Cs Ba La Hf Ta W Re Os Ir Pt Au Hg Tl Pb Bi Po At Rn Fr Ra Ac Ce Pr Nd Pm Sm Eu Gd Tb Dy Ho Er Tm Yb Lu Th Pa U Np Pu Am Cm Bk Cf Es Fm Md No Lr);
my(%ScattLength);

#####
=pod

=item void defineFrames(widget)

    This function defines the main window of table and fills it with
    checkbuttons labeled by the different elements.

=cut

# toplevel definition
sub defineFrames{
    my($widget)=@_;
    $table::ele_f=$widget->Frame();

    # Frame which contains table of elements
    $table::ele_f->Label(
		  -textvariable=>\$show,-font=>$colors::fontM,
		  )->grid(
			  -column=>3,
			  -row=>0,
			  -columnspan=>7,
			  -sticky=>'nsew'
			  );
    $H =newCButton($table::ele_f,'H' ,0 ,0);
    &bind_name($H,'Hydrogen');
    
    $He=newCButton($table::ele_f,'He',17,0);
    &bind_name($He,'Helium');
    
    $Li=newCButton($table::ele_f,'Li',0 ,1);
    &bind_name($Li,'Lithium');

    $Be=newCButton($table::ele_f,'Be',1 ,1);
    &bind_name($Be,'Beryllium');

    $B =newCButton($table::ele_f,'B' ,12,1);
    &bind_name($B,'Boron');

    $C =newCButton($table::ele_f,'C' ,13,1);
    &bind_name($C,'Carbon');

    $N =newCButton($table::ele_f,'N' ,14,1);
    &bind_name($N,'Nitrogen');
    
    $O =newCButton($table::ele_f,'O' ,15,1);
    &bind_name($O,'Oxygen');
    
    $F =newCButton($table::ele_f,'F' ,16,1);
    &bind_name($F,'Flourine');
    
    $Ne=newCButton($table::ele_f,'Ne',17,1);
    &bind_name($Ne,'Neon');
    
    $Na=newCButton($table::ele_f,'Na',0 ,2);
    &bind_name($Na,'Sodium');
    
    $Mg=newCButton($table::ele_f,'Mg',1 ,2);
    &bind_name($Mg,'Magnesium');
    
    $Al=newCButton($table::ele_f,'Al',12,2);
    &bind_name($Al,'Aluminum');
    
    $Si=newCButton($table::ele_f,'Si',13,2);
    &bind_name($Si,'Silicon');
    
    $P =newCButton($table::ele_f,'P' ,14,2);
    &bind_name($P,'Phosphorus');
    
    $S =newCButton($table::ele_f,'S' ,15,2);
    &bind_name($S,'Sulfur');
    
    $Cl=newCButton($table::ele_f,'Cl',16,2);
    &bind_name($Cl,'Clorine');
    
    $Ar=newCButton($table::ele_f,'Ar',17,2);
    &bind_name($Ar,'Argon');
    
    $K =newCButton($table::ele_f,'K' ,0 ,3);
    &bind_name($K,'Potasium');
    
    $Ca=newCButton($table::ele_f,'Ca',1 ,3);
    &bind_name($Ca,'Calcium');
    
    $Sc=newCButton($table::ele_f,'Sc',2 ,3);
    &bind_name($Sc,'Scandium');
    
    $Ti=newCButton($table::ele_f,'Ti',3 ,3);
    &bind_name($Ti,'Titanium');
    
    $V =newCButton($table::ele_f,'V' ,4 ,3);
    &bind_name($V,'Vanadium');
    
    $Cr=newCButton($table::ele_f,'Cr',5 ,3);
    &bind_name($Cr,'Chromium');
    
    $Mn=newCButton($table::ele_f,'Mn',6 ,3);
    &bind_name($Mn,'Manganese');
    
    $Fe=newCButton($table::ele_f,'Fe',7 ,3);
    &bind_name($Fe,'Iron');
    
    $Co=newCButton($table::ele_f,'Co',8 ,3);
    &bind_name($Co,'Cobalt');
    
    $Ni=newCButton($table::ele_f,'Ni',9 ,3);
    &bind_name($Ni,'Nickel');
    
    $Cu=newCButton($table::ele_f,'Cu',10,3);
    &bind_name($Cu,'Copper');
    
    $Zn=newCButton($table::ele_f,'Zn',11,3);
    &bind_name($Zn,'Zinc');
    
    $Ga=newCButton($table::ele_f,'Ga',12,3);
    &bind_name($Ga,'Gallium');
    
    $Ge=newCButton($table::ele_f,'Ge',13,3);
    &bind_name($Ge,'Germanium');
    
    $As=newCButton($table::ele_f,'As',14,3);
    &bind_name($As,'Arsenic');
    
    $Se=newCButton($table::ele_f,'Se',15,3);
    &bind_name($Se,'Selenium');
    
    $Br=newCButton($table::ele_f,'Br',16,3);
    &bind_name($Br,'Bromine');
    
    $Kr=newCButton($table::ele_f,'Kr',17,3);
    &bind_name($Kr,'Krypton');
    
    $Rb=newCButton($table::ele_f,'Rb',0 ,4);
    &bind_name($Rb,'Rubidium');
    
    $Sr=newCButton($table::ele_f,'Sr',1 ,4);
    &bind_name($Sr,'Strontium');
    
    $Y =newCButton($table::ele_f,'Y' ,2 ,4);
    &bind_name($Y,'Yttrium');
    
    $Zr=newCButton($table::ele_f,'Zr',3 ,4);
    &bind_name($Zr,'Zirconium');
    
    $Nb=newCButton($table::ele_f,'Nb',4 ,4);
    &bind_name($Nb,'Niobium');
    
    $Mo=newCButton($table::ele_f,'Mo',5 ,4);
    &bind_name($Mo,'Molybdenum');
    
    $Tc=newCButton($table::ele_f,'Tc',6 ,4);
    &bind_name($Tc,'Technetium');
    
    $Ru=newCButton($table::ele_f,'Ru',7 ,4);
    &bind_name($Ru,'Ruthenium');
    
    $Rh=newCButton($table::ele_f,'Rh',8 ,4);
    &bind_name($Rh,'Rhodium');
    
    $Pd=newCButton($table::ele_f,'Pd',9 ,4);
    &bind_name($Pd,'Palladium');
    
    $Ag=newCButton($table::ele_f,'Ag',10,4);
    &bind_name($Ag,'Silver');
    
    $Cd=newCButton($table::ele_f,'Cd',11,4);
    &bind_name($Cd,'Cadmium');
    
    $In=newCButton($table::ele_f,'In',12,4);
    &bind_name($In,'Indium');
    
    $Sn=newCButton($table::ele_f,'Sn',13,4);
    &bind_name($Sn,'Tin');
    
    $Sb=newCButton($table::ele_f,'Sb',14,4);
    &bind_name($Sb,'Antimony');
    
    $Te=newCButton($table::ele_f,'Te',15,4);
    &bind_name($Te,'Tellurium');
    
    $I =newCButton($table::ele_f,'I' ,16,4);
    &bind_name($I,'Iodine');
    
    $Xe=newCButton($table::ele_f,'Xe',17,4);
    &bind_name($Xe,'Xenon');
    
    $Cs=newCButton($table::ele_f,'Cs',0 ,5);
    &bind_name($Cs,'Cesium');
    
    $Ba=newCButton($table::ele_f,'Ba',1 ,5);
    &bind_name($Ba,'Barium');
    
    $La=newCButton($table::ele_f,'La',2 ,5);
    &bind_name($La,'Lanthanum');
    
    $Hf=newCButton($table::ele_f,'Hf',3 ,5);
    &bind_name($Hf,'Hafnium');
    
    $Ta=newCButton($table::ele_f,'Ta',4 ,5);
    &bind_name($Ta,'Tantalum');
    
    $W =newCButton($table::ele_f,'W' ,5 ,5);
    &bind_name($W,'Tungsten');
    
    $Re=newCButton($table::ele_f,'Re',6 ,5);
    &bind_name($Re,'Rhenium');

    $Os=newCButton($table::ele_f,'Os',7 ,5);
    &bind_name($Os,'Osmium');

    $Ir=newCButton($table::ele_f,'Ir',8 ,5);
    &bind_name($Ir,'Iridium');

    $Pt=newCButton($table::ele_f,'Pt',9 ,5);
    &bind_name($Pt,'Platinum');

    $Au=newCButton($table::ele_f,'Au',10,5);
    &bind_name($Au,'Gold');

    $Hg=newCButton($table::ele_f,'Hg',11,5);
    &bind_name($Hg,'Mercury');
    
    $Bi=newCButton($table::ele_f,'Bi',12,5);
    &bind_name($Bi,'Bismuth');
    
    $Pb=newCButton($table::ele_f,'Pb',13,5);
    &bind_name($Pb,'Lead');
    
    $Tl=newCButton($table::ele_f,'Tl',14,5);
    &bind_name($Tl,'Thallium');
    
    $Po=newCButton($table::ele_f,'Po',15,5);
    &bind_name($Po,'Polonium');
    
    $At=newCButton($table::ele_f,'At',16,5);
    &bind_name($At,'Astatine');
    
    $Rn=newCButton($table::ele_f,'Rn',17,5);
    &bind_name($Rn,'Radon');
    
    $Fr=newCButton($table::ele_f,'Fr',0 ,6);
    &bind_name($Fr,'Francium');
    
    $Ra=newCButton($table::ele_f,'Ra',1 ,6);
    &bind_name($Ra,'Radium');
    
    $Ac=newCButton($table::ele_f,'Ac',2 ,6);
    &bind_name($Ac,'Actinium');
    
    $Ce=newCButton($table::ele_f,'Ce',4 ,7);
    &bind_name($Ce,'Cerium');
    
    $Pr=newCButton($table::ele_f,'Pr',5 ,7);
    &bind_name($Pr,'Praseodymium');
    
    $Nd=newCButton($table::ele_f,'Nd',6 ,7);
    &bind_name($Nd,'Neodymium');
    
    $Pm=newCButton($table::ele_f,'Pm',7 ,7);
    &bind_name($Pm,'Promethium');
    
    $Sm=newCButton($table::ele_f,'Sm',8 ,7);
    &bind_name($Sm,'Samarium');
    
    $Eu=newCButton($table::ele_f,'Eu',9 ,7);
    &bind_name($Eu,'Europium');
    
    $Gd=newCButton($table::ele_f,'Gd',10,7);
    &bind_name($Gd,'Gadolinum');
    
    $Tb=newCButton($table::ele_f,'Tb',11,7);
    &bind_name($Tb,'Terbium');
    
    $Dy=newCButton($table::ele_f,'Dy',12,7);
    &bind_name($Dy,'Dysprosium');
    
    $Ho=newCButton($table::ele_f,'Ho',13,7);
    &bind_name($Ho,'Holmium');
    
    $Er=newCButton($table::ele_f,'Er',14,7);
    &bind_name($Er,'Erbium');
    
    $Tm=newCButton($table::ele_f,'Tm',15,7);
    &bind_name($Tm,'Thulium');
    
    $Yb=newCButton($table::ele_f,'Yb',16,7);
    &bind_name($Yb,'Ytterbium');
    
    $Lu=newCButton($table::ele_f,'Lu',17,7);
    &bind_name($Lu,'Lutetium');
    
    $Th=newCButton($table::ele_f,'Th',4 ,8);
    &bind_name($Th,'Thorium');
    
    $Pa=newCButton($table::ele_f,'Pa',5 ,8);
    &bind_name($Pa,'Protactiunium');
    
    $U =newCButton($table::ele_f,'U' ,6 ,8);
    &bind_name($U,'Uranium');
    
    $Np=newCButton($table::ele_f,'Np',7 ,8);
    &bind_name($Np,'Neptunium');
    
    $Pu=newCButton($table::ele_f,'Pu',8 ,8);
    &bind_name($Pu,'Plutonium');
    
    $Am=newCButton($table::ele_f,'Am',9 ,8);
    &bind_name($Am,'Americium');
    
    $Cm=newCButton($table::ele_f,'Cm',10,8);
    &bind_name($Cm,'Curium');
    
    $Bk=newCButton($table::ele_f,'Bk',11,8);
    &bind_name($Bk,'Berkelium');
    
    $Cf=newCButton($table::ele_f,'Cf',12,8);
    &bind_name($Cf,'Californium');
    
    $Es=newCButton($table::ele_f,'Es',13,8);
    &bind_name($Es,'Einsteinium');
    
    $Fm=newCButton($table::ele_f,'Fm',14,8);
    &bind_name($Fm,'Fermium');
    
    $Md=newCButton($table::ele_f,'Md',15,8);
    &bind_name($Md,'Mendelevium');
    
    $No=newCButton($table::ele_f,'No',16,8);
    &bind_name($No,'Nobelium');
    
    $Lr=newCButton($table::ele_f,'Lr',17,8);
    &bind_name($Lr,'Lawrencium');

    $table::ele_f->pack(-fill=>'both',-expand=>1);

    &fillButtons();

}

#####
=pod

=item CheckButton newCButton(widget,label,x,y)

    This function creates a checkbutton with label marking it grided
    at (x,y).

=cut

sub newCButton{
    my($widget,$label,$x,$y)=@_;
    my($button);

    $button=$widget->Checkbutton(
			       -text=>"$label",
			       -variable=>\$cbState{"$label"},
			       -indicatoron=>0,
			       -padx=>8,
			       -pady=>8,
                               -font=>$colors::fontM
			       )->grid(
				       -column=>"$x",
				       -row=>"$y",
				       -sticky=>'nsew'
			      );

    return $button;
}

#####
=pod

=item void bind_name(widget,msg)

    This function displays msg when the cursor enters widget and
    displays nothing when the cursor leaves.

=cut

sub bind_name{
    my($widget,$msg)=@_;

    $widget->bind('<Enter>', sub{$show=$msg;});
    $widget->bind('<Leave>', sub{$show='';});
}

#####
=pod

=item void fillButtons(void)

    This function sets the buttons which are previously selected items
    to true.

=cut

sub fillButtons{
    my($int);
    for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	$cbState{$GLOBE::element[$int]}=1;
    }
}

#####
=pod

=item void checkButtons(void)

    This function checks which buttons are true and fills the
    @GLOBE::element array with those element names.

=cut

sub checkButtons{
    my($int);
    for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	pop(@GLOBE::element);
    }
    $GLOBE::numElements=0;
    foreach (@elements){
	if($cbState{$_}==1){
	    $GLOBE::element[$GLOBE::numElements]=$_;
	    $GLOBE::numElements+=1;
	}
    }
}

#####
=pod

=item void makeEle()

    This function created the periodic table dialog box and
    allows the user to select the materials elements ..

=cut

sub makeEle{

	my ($widget)=$_[0];

	my $ele_dialog = $widget->DialogBox(-title=>'Select elements ..',
                                            -buttons=>['Done']);

	&defineFrames($ele_dialog);
	$ele_dialog->Show;
	&checkButtons();
	$ele_dialog->destroy();
	&makeAmt($widget);
}

#####
=pod

=item void makeAmt(widget)

    This function finds out the amount of the different elements which
    make up the material and then normalize the total to one.

=cut

sub makeAmt{
    	my($int,$f,$total,$int,$val);
	my ($widget)=$_[0];

	my $amt_dialog = $widget->DialogBox(-title=>'Specify amounts ..',
                                            -buttons=>['Done','Cancel']);
    	$f=$amt_dialog->Frame()->pack(-side=>'top');
    	$f->Label(-text=>'Element',-font=>$colors::fontM)
          ->grid(-column=>0,-row=>0,-sticky=>'nsew');
    	$f->Label(-text=>' : ',-font=>$colors::fontM)
          ->grid(-column=>1,-row=>0,-sticky=>'nsew');
    	$f->Label(-text=>'Number in atomic formula unit',-font=>$colors::fontM)
          ->grid(-column=>2,-row=>0,-sticky=>'nsew');
    	for( $int=1 ; $int<$GLOBE::numElements+1 ; $int++ ){
		$f->Label(-textvariable=>\$GLOBE::element[$int-1],
                          -font=>$colors::fontM)
	          ->grid(-column=>0,-row=>"$int",-sticky=>'nse');
		$f->Label(-text=>' : ',
                          -font=>$colors::fontM)
	          ->grid(-column=>1,-row=>"$int",-sticky=>'nsew');
		if($GLOBE::samAtomNumOf{$GLOBE::element[$int-1]} eq ''){
	    		$GLOBE::samAtomNumOf{$GLOBE::element[$int-1]}=0.0;
		}
		$f->Entry(-textvariable=>
	                   \$GLOBE::samAtomNumOf{$GLOBE::element[$int-1]},
                          -font=>$colors::fontM)
	          ->grid(-column=>2,-row=>"$int",-sticky=>'nsw');
    	}
	$val=$amt_dialog->Show;
	$amt_dialog->destroy();
	if ($val eq 'Done') {
	       $total=0;
	       &body::makeTable_f($body::sample_f);
	       for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
		   $total+=$GLOBE::samAtomNumOf{$GLOBE::element[$int]};
	       }
	       for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
		   my $temp=$GLOBE::samAtomNumOf{$GLOBE::element[$int]};
		   $temp=sprintf "%8.6f",$temp/$total;
		   $GLOBE::samAtomNumOf{$GLOBE::element[$int]}=$temp;
	       }
	       &getLaue();
	       &Hist::update();
	}
}

#####
=pod

=item int getStats(void)

    This function does a data file search for the atomic information
    of the elements and fills in the appropriate arrays.

=back

=cut

sub getStats{
    my $filename="$GLOBE::binpath/neutron.table";
    my($line,$int);
    (return 0) unless (&File::openRead("$filename"));
    for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
	open(FILE,$filename) || warn "Could not open $filename: $!";
	while($line=<FILE>){
	    chomp($line);
	    if($line=~/$GLOBE::element[$int],/){
		($GLOBE::samAtomMass{$GLOBE::element[$int]},
                 $GLOBE::samAtomCoherCS{$GLOBE::element[$int]},
                 $GLOBE::samAtomIncoherCS{$GLOBE::element[$int]},
                 $GLOBE::samAtomAbsorpCS{$GLOBE::element[$int]})=
                 (split /,/, $line)[1,2,3,5,6];
	    }
	}
	close(FILE) || warn "Could not close $filename: $!";
    }

    return 1;
}

#####
=pod

=item int getLaue(void)

    This function does a data file search for the atomic information
    of the elements and calculates the normalized laue term.

=back

=cut

sub getLaue{
  my $filename="$GLOBE::binpath/neutron.table";
  my($line,$int);
  (return 0) unless (&File::openRead("$filename"));
  for( $int=0 ; $int<$GLOBE::numElements ; $int++ ){
    open(FILE,$filename) || warn "Could not open $filename: $!";
    while($line=<FILE>){
      chomp($line);
      if($line=~/$GLOBE::element[$int],/){
	$ScattLength{$GLOBE::element[$int]}=(split /,/, $line)[6];
      }
    }
  }

  # calculate the normalized Laue term
  {
    my($b_avg_sq,$b_sq_avg)=(0,0);
    
    for( my($int)=0 ; $int<$GLOBE::numElements ; $int++ ){
      my($ele)=$GLOBE::element[$int];
      my($amt,$b)=($GLOBE::samAtomNumOf{$ele},$ScattLength{$ele});
      $b_avg_sq+=$amt*$b;
      $b_sq_avg+=$amt*$b*$b;
    }
    $b_avg_sq=$b_avg_sq*$b_avg_sq;
    if($b_sq_avg){
      $GLOBE::laue=sprintf("%.5f",($b_sq_avg-$b_avg_sq)/$b_sq_avg);
    }else{
      $GLOBE::laue='';
    }
  }
}

1;
