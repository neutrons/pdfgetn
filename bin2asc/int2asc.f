c****************************************************************************
c	INTCONV - INT conversion program
c
c	This program converts binary INT (Simons format) to ASCII
c	SPEC files that can be used by KUPLOT.
c
c	Version: 1.0
c	Author : Thomas Proffen
c
c****************************************************************************
c
c This is part of the PDFgetN distribution written by Peter Peterson,
c Matthias Gutmann, Thomas Proffen, and Simon Billinge.
c 
c Copyright 2000 Michigan State University Board of Trustees
c 
c Use and distribution of this program is subject to the terms laid out
c in the license in LICENSE.txt included with this distribution.  A copy
c of the license  can be obtained from Michigan  State University office
c of Libraries, Computing and Technology (517-353-0722).  
c
c****************************************************************************
	program intconv
c
	parameter	(pi      = 3.141592654)
	parameter	(maxdat  = 50000)
c
	character*80    fname,aname,title,hist
	character*6	err
	real*4		rint(maxdat)
	real*4		dint(maxdat)
	real*4		x(maxdat)
c
	narg = iargc()
	if (narg.eq.2) then
	  call getarg (1,fname)
	  call getarg (2,aname)
	else
	  write (*,1000)
	  read(*,5000) fname
	  write (*,1100)
	  read(*,5000) aname
	endif
c
	nb  = 0
	err = 'readon'
c
	open (unit=77, file=aname, status='unknown')
	do while (err.ne.'endfil')
	  nb = nb+1
	  call intread(fname,nb,ib,twotht,nchan,x,rint,dint,err,maxdat)
	  if (err.ne.'endfil') then
	    rad    = pi/360.
	    factor = 2.0*sin(rad*abs(twotht))
	    q1=2.0*pi*factor/x(nchan-1)
	    q2=2.0*pi*factor/x(1)
	    qmax=max(q1,q2)
	    qmin=min(q1,q2)
	    write(77,2000) nb,nb,twotht,qmin,qmax
	    do i=nchan-1,1,-1
              d = x(i)/factor
	      q = 2.0*pi/d
	      write(77,*) q,rint(i),dint(i)
	    enddo
	  endif
	enddo
	close(77)
c
1000	format ('Give name of input binary INT file  : ',$)
1100	format ('Give name of output ascii INT file  : ',$)
2000	format ('#S ',i2,' Bank ',i2,' - Angle: ',f8.2,' deg',
     &	        ' - Q: ',f6.2,' to ',f6.2,' A**-1',/,
     &	        '#L Q Int sigmaInt')
5000	format (a)
	end
C****************************************************************************
	SUBROUTINE INTREAD(FILROOT,NBANK,IBANK,TWOTHT,NCHAN,X,RINT,
     1                     DELINT,ERR,NN)
C****************************************************************************

	REAL*4 RINT(NN),DELINT(NN),X(NN)
	CHARACTER*(*) FILROOT,ERR
	CHARACTER CHARHOLD*80,RTITL*80,FIL*4

        OPEN(UNIT=2,FILE=FILROOT,STATUS='OLD',FORM='UNFORMATTED',
     1       ACCESS='SEQUENTIAL')

	READ(2) FIL,CHARHOLD,RTITL,NBANKS
	DO J = 1,NBANK-1
	   READ(2,end=20) IB,T,D,XF,NCH,CWD,ANGLE
	   DO I=1,NCH
		READ(2) XL,S,DS
	   ENDDO
	ENDDO
	  READ(2,end=20) IBANK,TWOTHT,DIST,XF,NCHAN,CHWDT,ANGLE
	  DO J=1,NCHAN
		READ(2,end=30) X(J),RINT(J),DELINT(J)
	  ENDDO
	CLOSE(2)

	RETURN
20	ERR = 'endfil'	
	CLOSE(2)
	RETURN
30	STOP ' error during read of .int file'
	END
