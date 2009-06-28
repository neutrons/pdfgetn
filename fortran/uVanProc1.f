	SUBROUTINE uVanProc1(SLAM,Y,DY,NCHD,twotht,DQFIT,FITREG, dqpeak)
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
C* $Id: uVanProc1.f,v 1.2 2009/06/28 00:17:49 tproffen Exp $
C*
C* $Log: uVanProc1.f,v $
C* Revision 1.2  2009/06/28 00:17:49  tproffen
C* Fixing up 1.6.6
C*
C* Revision 1.1.1.1  2006/03/12 09:59:25  tproffen
C* Initial import
C*
c Revision 1.2  1997/04/11  19:54:28  billinge
c removed DEC fortran
C*
C*
C* $Log: uVanProc1.f,v $
C* Revision 1.2  2009/06/28 00:17:49  tproffen
C* Fixing up 1.6.6
C*
C* Revision 1.1.1.1  2006/03/12 09:59:25  tproffen
C* Initial import
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
C----------------------------------------------------------------------
C
C
C 8/23/1999 SJB and MG
C
C Subroutine taken from original HIPDPREP suite of programs and adapted
C to work with usepdprep1.
C
C temporary array

	parameter (Pi = 3.14159265359)

	real   y(n_chn),
     1         dy(n_chn),
     1         slam(n_chn),
     1         dyhold(n_chn),
     1         S(n_chn),
     1         dsdat(n_chn),
     1         Q(n_chn),
     1         fitreg,
     1         dqfit,
     1         dqpeak,
     1         twotht,
     1         qv, qvan,
     1         q2van, q2,
     1         mq2v, dq,
     1         errint,
     1         a0, a1, a2,
     1         rat, sigscale

	integer nreg,
     1          nchd,
     1          i1, i2, iA, iB, NC,
     1          nfitpoints

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

C	SMOOTH OUT V PEAKS
C vanadium has peaks at Q = sqrt(2N)*2 pi/a where n=1,2,3,... and a=3.0274A

	write(6, *) '%%info: removing vanadium peaks...'
	qvan = 2. * 3.141592654 / 3.0274
	Q2VAN = 2*qvan*qvan
	DO 10 N=1,nchd
	  IF(Q(N) .GT. 15.0) GO TO 10
	  Q2 = Q(N)*Q(N)
C find position of nearest peak
	  MQ2V = NINT((Q2/Q2VAN))
	  IF (MQ2V.LE.0)GO TO 10
	  QV = sqrt(2.*MQ2V) * QVAN
C damp away intensity for dQ/Q +- 0.01
	  DQ = abs(Q(N)-QV)/QV
	  IF (DQ .LT. DQPEAK) dsdat(N)= -dsdat(N)
 10	CONTINUE
C
C	SMOOTH S(Q) -- don't fit points where a peak is present
C

	write(6, *) '%%info: smoothing vanadium using',
     1    ' overlapping second-order polynomials...'
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
C**************************************************************************

C----------------------------------------------------------------------
	SUBROUTINE FIT(X,Y,SIGY,XC,I1,I2,A0,A1,A2)
C----------------------------------------------------------------------
C  Fit 2nd polynomial to a set of data points
C----------------------------------------------------------------------
C input:
C	X(*)	Array containing the independant variable
C	Y(*)	Array containing the dependant variable
C	SIGY(*)	Estimated standard deviations on the Y values 
C	        -- values <= 0 cause point to be ignored
C	XC	central point for polynomial fit
C	I1,I2	range of data points to be fitted (I2>I1)
C output:
C	A0,A1,A2  polynomial coefficents
C----------------------------------------------------------------------
	IMPLICIT NONE
	REAL X(*),Y(*),SIGY(*),XC
	INTEGER I1,I2
	REAL A0,A1,A2

	COMMON /SMOOTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 X11,X12,X22,X23,X33,Y1,Y2,Y3
	REAL DXJ,DXJ2,DXJ3,DXJ4,SIGYJ2I
	INTEGER I,J

C zero out matrix terms
	Y1 = 0.0
	Y2 = 0.0
	Y3 = 0.0
	X11 = 0.0
	X12 = 0.0
	X22 = 0.0
	X23 = 0.0
	X33 = 0.0
	I = 0
	DO J=I1,I2
	  IF (SIGY(J) .GT. 0.) THEN
	    I = I + 1
C compute intermediate results to speed computation
	    DXJ = X(J)-XC
	    DXJ2 = DXJ*DXJ
	    DXJ3 = DXJ2*DXJ
	    DXJ4 = DXJ2*DXJ2
	    SIGYJ2I = 1. / SIGY(J)**2
C sum the matrix elements
	    Y1 = Y1 + Y(J) * SIGYJ2I
	    Y2 = Y2 + Y(J) * DXJ * SIGYJ2I
	    Y3 = Y3 + Y(J) * DXJ2 * SIGYJ2I
	    X11 = X11 + SIGYJ2I
	    X12 = X12 + DXJ * SIGYJ2I
	    X22 = X22 + DXJ2 * SIGYJ2I
	    X23 = X23 + DXJ3 * SIGYJ2I
	    X33 = X33 + DXJ4 * SIGYJ2I
	  ENDIF
	ENDDO
C compute determinant
	detX = X11 * X22 * X33 + 2.*X12 * X22 * X23 - X22**3 - 
     1	X11 * X23**2 - X12**2 * X33
C check for singlar matrix)
	IF (I .eq. 0 .or. detX .eq. 0) THEN
	  PRINT *,'Singular matrix error'
	  PRINT *,I,' Points were fit between points ',I1,I2
	  STOP
	ENDIF
C invert the matrix
	Z11 = X22 * X33 - X23**2 
	Z12 = X22 * X23 - X12 * X33
	Z13 = X12 * X23 - X22**2
	Z22 = X11 * X33 - X22**2
	Z23 = X12 * X22 - X11 * X23
	Z33 = X11 * X22 - X12**2
C compute the polynomial coefficients
	A0C = (Y1 * Z11 + Y2 * Z12 + Y3 * Z13)/detX
	A1C = (Y1 * Z12 + Y2 * Z22 + Y3 * Z23)/detX
	A2C = (Y1 * Z13 + Y2 * Z23 + Y3 * Z33)/detX
C make copies of polynomial coefficients to return COMMON block
	A0 = A0C
	A1 = A1C
	A2 = A2C
	XCC = XC
	RETURN
	END
C----------------------------------------------------------------------
	REAL FUNCTION SM0(XI,XC)
C----------------------------------------------------------------------
C Evaluate the smoothed polynomial at point XI
C----------------------------------------------------------------------
C input:
C	XI	value of the independant variable
C	XC	central point for polynomial fit
C output:
C	SM0	= A0 + A1 * (XI-XC) + A2 * (XI-XC)**2
C----------------------------------------------------------------------
	IMPLICIT NONE
	REAL XI,XC

	COMMON /SMOOTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL DX

	IF (XC .ne. XCC) THEN
	  PRINT *,'ERROR: XC does not agree with number used for fit'
	  PRINT *,'XC = ',XC
	  PRINT *,'XCC = ',XCC
	  STOP
	ENDIF
	DX = XI-XC
	SM0 = A0C + DX*(A1C + (A2C*DX))
	RETURN
	END
C----------------------------------------------------------------------
	REAL FUNCTION SM1(XI,XC)
C----------------------------------------------------------------------
C Evaluate the first derivative of the smoothed polynomial at point XI
C----------------------------------------------------------------------
C input:
C	XI	value of the independant variable
C	XC	central point for polynomial fit
C output:
C	SM1	= A1 + 2. * A2 * (XI-XC)
C----------------------------------------------------------------------
	IMPLICIT NONE
	REAL XI,XC

	COMMON /SMOOTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33

	IF (XC .ne. XCC) THEN
	  PRINT *,'ERROR: XC does not agree with number used for fit'
	  PRINT *,'XC = ',XC
	  PRINT *,'XCC = ',XCC
	  STOP
	ENDIF
	SM1 = A1C + 2.*A2C*(XI-XC)
	RETURN
	END
C----------------------------------------------------------------------
	SUBROUTINE SMERR(X,Y,SIGY,XC,I1,I2,XI,SM0ERR,SM1ERR,SM2ERR)
C----------------------------------------------------------------------
C  Estimate error on value computed by SM0, SM1, and SM2
C----------------------------------------------------------------------
C input:
C	X(*)	Array containing the independant variable
C	Y(*)	Array containing the dependant variable
C	SIGY(*)	Estimated standard deviations on the Y values 
C	        -- values <= 0 cause point to be ignored
C	XC	central point for polynomial fit
C	I1,I2	range of data points to be fitted (I2>I1)
C	XI	input X value
C output:
C	SM0ERR	estimated error on SM0 value
C	SM1ERR	estimated error on SM1 value
C	SM2ERR	estimated error on SM2 value
C----------------------------------------------------------------------
	IMPLICIT NONE
	REAL X(*),Y(*),SIGY(*),XC,XI,SM0ERR,SM1ERR,SM2ERR
	INTEGER I1,I2

	COMMON /SMOOTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL SIGYJI,DXJ,D1,D2,D3,da0SIGYJ,da1SIGYJ,da2SIGYJ,dxi
	real*8 sigma0,sigma1,sigma2
	INTEGER J

	DO 2 J=I1,I2
	  IF (SIGY(J) .LT. 0.) GOTO 2
	  SIGYJI = 1. / SIGY(J)
	  DXJ = X(J)-XC
	  D1 = DXJ * SIGYJI
	  D2 = D1 * DXJ
	  D3 = D2 * DXJ
	  da0SIGYJ = D1 * Z11 + D2 * Z12 + D3 * Z13
	  da1SIGYJ = D1 * Z12 + D2 * Z22 + D3 * Z23
	  da2SIGYJ = D1 * Z13 + D2 * Z23 + D3 * Z33
	  DXI = XI-XC
	  sigma0 = sigma0 + (da0SIGYJ + DXI*(da1SIGYJ + DXI*da2SIGYJ))**2
	  sigma1 = sigma1 + (da1SIGYJ + 2.*DXI*da2SIGYJ)**2
	  sigma2 = sigma2 + (2.*da2SIGYJ)**2
2	CONTINUE
	SM0ERR = SQRT(sigma0) / detX
	SM1ERR = SQRT(sigma1) / detX
	SM2ERR = SQRT(sigma2) / detX
	RETURN
	END

