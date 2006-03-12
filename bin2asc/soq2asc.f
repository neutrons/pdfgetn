c****************************************************************************
c	SOQCONV - SOQ conversion program
c
c	This program converts binary SOQ (Simon's format) to ASCII
c	SPEC files that can be used by KUPLOT.
c
c	Version: 1.1
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
	program soqconv
c
	parameter	(pi      = 3.141592654)
	parameter	(maxdat  = 50000)
c
	character*80    fname,aname,title,hist
	character*6	err
	real*4		soq(maxdat)
	real*4		dsoq(maxdat)
	real*4		x(maxdat)
c
	narg = iargc()
	if (narg.ge.2) then
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
	write(77,1500)
	do while (err.ne.'endfil')
	  nb = nb+1
	  call soqread(fname,nb,x,soq,dsoq,dr,j,n,title,hist,
     &	               angle,err,maxdat)
	  if (err.ne.'endfil') then
	    call soqave(x,soq,dsoq,n,j,maxdat,narg,qmin,qmax,sqave,dsqave)
	    call soqint(x,soq,dsoq,n,j,maxdat,narg,qfint,dqfint)
c
	    write(77,2000) nb,angle
	    write(77,2100) sqave,dsqave,1.0/sqave,qmin,qmax,qfint,dqfint
	    write(77,2200) 
	    do i=n-1,j,-1
	        q = 2.0*pi/x(i)
	       rq = q*(soq(i)-1)
	      drq = q*dsoq(i)
	      write(77,*) q,soq(i),dsoq(i),rq,drq
	    enddo
	  endif
	enddo
	close(77)
c
1000	format ('Give name of input binary SOQ file  : ',$)
1100	format ('Give name of output ascii SOQ file  : ',$)
1500	format ('#O0 S_ave  sig_S_ave  1/S_ave q_ave_min q_ave_max',/,
     &          '#O1 qF_int sig_qF_int')
2000	format ('#S ',i3,' Bank at ',f7.2,' degrees')
2100	format ('#P0 ',5(f8.5,2x),/,'#P1 ',2(f12.4,2x))
2200	format ('#L Q   S   sigmaS   F   sigmaF')
5000	format (a)
	end
C****************************************************************************
	subroutine soqint(x,soq,dsoq,n,j,maxdat,narg,qfint,dqfint)
c+
c	Calculates integral Q**2(S(Q)-1) ..
c-
	parameter	(pi = 3.141592654)
c
	character*40	cdummy
	real*4		x(maxdat),soq(maxdat),dsoq(maxdat)
c
	 qfint = 0.0
	dqfint = 0.0
c
	do i=j,n-2,1
	  q0   = 2.0*pi/x(i+1)
	  q    = 2.0*pi/x(i)
	  qf0  = q0**2*(soq(i+1)-1)
	  qf   = q**2*(soq(i)-1)
	  dqf0 = q0**2*dsoq(i+1)
	  dqf  = q**2*dsoq(i)
c
	  qfint  = qfint + abs(q-q0)*(qf+qf0)/2.0
	  dqfint = dqfint + abs(q-q0)**2*(dqf**2+dqf0**2)/4.0
	enddo
c
	dqfint = sqrt(dqfint)
c
	end
C****************************************************************************
	subroutine soqave(x,soq,dsoq,n,j,maxdat,narg,qmin,qmax,sqave,dsqave)
c+
c	Calculates average S(Q) in high Q range
c-
	parameter	(pi = 3.141592654)
c
	character*40	cdummy
	real*4		x(maxdat),soq(maxdat),dsoq(maxdat)
c
	fac=0.60
	qmax=2.0*pi/x(j)
c
	if (narg.eq.3) then
	   call getarg (3,cdummy) 
	   read(cdummy,*) fac
	endif
c
	if (fac.lt.1.0) qmin = fac*qmax
c
	sqave  = 0.0
	dsqave = 0.0
	npkt   = 0
c
	do i=n-1,j,-1
	  q = 2.0*pi/x(i)
	  if (q.ge.qmin .and. q.le.qmax) then
	    npkt   = npkt+1
	    sqave  = sqave + soq(i)
	    dsqave = dsqave + dsoq(i)**2
	  endif
	enddo
c
	if (npkt.gt.0) then
	  sqave  = sqave/float(npkt)
	  dsqave = sqrt(dsqave/float(npkt))
	endif
c
	end
C****************************************************************************
	SUBROUTINE SOQREAD(FILROOT,NBANK,X,SOQ,DELSOQ,DELX,NMIN,NMAX,
     1                     TITLE,HISTRY,ANGLE,ERR,N)
C***************************************************************************

	REAL*4        SOQ(N),DELSOQ(N),X(N)
	CHARACTER*(*) FILROOT,HISTRY,TITLE,ERR

        OPEN(UNIT=2,FILE=FILROOT,STATUS='OLD',FORM='UNFORMATTED',
     1      ACCESS='SEQUENTIAL')

	READ(2) TITLE
	READ(2) HISTRY
	DO J = 1,NBANK-1
	   READ(2,end=20) NMIN,NMAX,DELX,RHO,ANGLE
	   READ(2) (rdummy,rdummy,rdummy,I=NMIN,NMAX)
	ENDDO
	   READ(2,end=20) NMIN,NMAX,DELX,RHO,ANGLE
	   READ(2,end=30) (X(I),SOQ(I),DELSOQ(I),I=NMIN,NMAX)
	CLOSE(2)

	RETURN

20	ERR = 'endfil'	
	CLOSE(2)
	RETURN
30	STOP ' error during read of .SOQ file'
	END
