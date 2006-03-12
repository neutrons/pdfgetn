	SUBROUTINE uSmoothThis1(SLAM,Y,DY,NCHD,twotht,DQFIT,FITREG)
C******************************************************************************
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
C******************************************************************************
C*---Subroutine called from CANPROC which was written by B.H. Toby.
C*  This subroutine is designed specifically to smooth background,
C*  vanadium and empty can data from the HIPD instrument at LANSCE;
C*  however, hopefully it will be general enough to be used more generally.
C*
C* $Id: uSmoothThis1.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C*
C* $Log: uSmoothThis1.f,v $
C* Revision 1.1  2006/03/12 09:59:25  tproffen
C* Initial revision
C*
c Revision 1.2  1997/04/11  19:54:28  billinge
c removed DEC fortran
C*
C*
C* $Log: uSmoothThis1.f,v $
C* Revision 1.1  2006/03/12 09:59:25  tproffen
C* Initial revision
C*
C* Revision 1.3 1999/05/26  12:46:30  gutmann
C* program worked only when Q array was decreasing. Extended
C* the capabilities to accept also an increasing Q array.
C*
C*****************************************************************************
	INCLUDE 'parameters.inc'
c	Initialisation file for smoothing in HIPDPREP
C input:
C   SLAM 	wavelength in A
C   Y	        intensity 
C   DY		estimated errors on Y
C   NCHD	number of data points
C   QLAM 	conversion factor for wavelength to Q [4Pi*sin(theta)]
C   NPRINT	if > 1, print out intermediate results
C   DQFIT	width of interval to be fit in units of deltaQ/Q 
C   FITREG	ratio of extrapolated region to fit region 
C		e.g. if 1000 points are fit and FITREG=.05, then 
C		the polynomial from the fit will be used to smooth 50 points
C		FITREG=0.0 is slow, FITREG=.1 will produce discontinuities
C output:
C   results are returned in Y and DY
C
C
C 8/23/1999 SJB and MG
C
C subroutine adapted to work with usepdprep1
C
C----------------------------------------------------------------------
C temporary array

	parameter (Pi = 3.14159265359)

	real   y(n_chn),
     1         dy(n_chn),
     1         slam(n_chn),
     1         dyhold(n_chn),
     1         S(n_chn),
     1         Q(n_chn),
     1         twotht,
     1         errint,
     1         rat,
     1         sigscale         

	integer nreg,
     1          nchd,
     1          i1, i2, iA, iB, NC,
     1          nfitpoints

	write(6, *) '%%info: smoothing with overlapping',
     1       ' second-order polynomials'

C---NREG counts the number of regions used in the smoothing
	NREG = 0
C make a temporary copy of the input intensity array
	DO N=1,nchd
	  S(N)=Y(N)
C convert lambda to Q (in inverse Angstroms)
	  Q(N) = 4.*Pi*sin(twotht*2.*Pi/360.) / SLAM(N)
	  DYHOLD(N) = DY(N)
C	  PRINT *, Q(N),S(N),DYHOLD(N),"here"
	ENDDO

C
C	SMOOTH S(Q) 
C
	i1 = 1
	i2 = 1
	IA = 1

C range of data to fit for extrapolation is from IA to IB, we have IA so
C now find IB
120	DO I=IA,NCHD
	  IB = I
	  IF (ABS(Q(IA)-Q(I))/Q(IA) .GT. DQFIT*FITREG) GOTO 121
	ENDDO
C find the central value of Q	
121	NC = (IA + IB)/ 2
C find the first point to fit.
	DO I=IA,1,-1
	  I1 = I
	  IF (ABS(Q(I)-Q(NC))/Q(NC) .GT. DQFIT .AND. DY(I) .GT. 0.) GOTO 123
	ENDDO
C find the last point to fit.
123	DO I=IB+1,NCHD
	  I2 = I
	  IF (ABS(Q(NC)-Q(I))/Q(NC) .GT. DQFIT .AND. DY(I) .GT. 0.) GOTO 124
	ENDDO
c
c---Q(NC) is the Q value of the central point of the region to  be fit.
c  Q and S contain the dependent and independent variables.
c  The data are fit over the range I1 to I2

124	CALL FIT(Q,S,DY,Q(NC),I1,I2,A0,A1,A2)

c---Fit N points with 3 coefficients, thus sig**2 = SUM(sig**2(i))/(N-3)
	NFITPOINTS = I2 - I1
	ERRINT = 0.0
	DO N=I1,I2
	   ERRINT = SQRT(ERRINT**2 + DY(N)**2)
	ENDDO
	DO N=I1,I2
c---For DQFIT = 0.2, NFITPOINTS approx= 800
	   IF(NFITPOINTS .GT. 3) THEN 
	      DYHOLD(N) = ERRINT/(NFITPOINTS - 3)
	   ELSE
	      DYHOLD(N) = ERRINT	      
	   ENDIF
	ENDDO	   

C now use the polynomial to smooth the data
	DO N=IA,IB
	  Y(N) = SM0(Q(N),Q(NC))
	  DY(N) = DYHOLD(N)
	ENDDO
C	PRINT *, Q(IA),Y(IA),S(IA),DY(IA)
C	PRINT *, Q(NC),Y(NC),S(NC),DY(NC)
C	PRINT *, Q(IB),Y(IB),S(IB),DY(IB)
	iA = IB + 1
	if (IA .le. NCHD) THEN
	   NREG = NREG + 1
	   goto 120
	ENDIF

	RAT = NREG/NCHD
	SIGSCALE = SQRT(RAT)

C	IF (IPRLVL .GT. 0) WRITE(*,'(A,I4,A,/)') 
C     1        ' data were smoothed using ',NREG,' sections. '

C Print summary of smoothed data
C	IF (IPRLVL .LE. 2) RETURN
C	MULT=nchd/11
C	WRITE(*,101)
C101	FORMAT('     N       SLAM(N)       Q(N)         S(N)(bef)   ',
C     2	'SS(N)(aft)   DSS(N)',/)
C	DO M=1,10
C	  N=MULT*M
C	  WRITE(*,100)  N,SLAM(N),Q(N),S(N),Y(N),DY(N)
C100	  FORMAT(I6,2F13.5,3F13.3)
C	ENDDO
	RETURN
	end
