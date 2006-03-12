C----------------------------------------------------------------------
	SUBROUTINE FIT(X,Y,SIGY,XC,I1,I2,A0,A1,A2)
C----------------------------------------------------------------------
C  Fit 2nd polynomial to a set of data points
C
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
C
C $Id: poly2fit.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: poly2fit.f,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
c Revision 1.2  1997/04/13  17:02:44  billinge
c removed DEC fortran
c
C
C-----------------------------------------------------------------------
	IMPLICIT NONE
	REAL X(*),Y(*),SIGY(*),XC
	INTEGER I1,I2
	REAL A0,A1,A2

	COMMON /SMTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
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
	DO 1 J=I1,I2
	  IF (SIGY(J) .LT. 0.) GOTO 1
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
1	CONTINUE
C compute determinant
	detX = X11 * X22 * X33 + 2.*X12 * X22 * X23 - X22**3 - 
     1	X11 * X23**2 - X12**2 * X33
C check for singlar matrix)
	IF (I .eq. 0 .or. detX .eq. 0) THEN
	  print *,'Singular matrix error on '
	  print *,I,' Points were fit between points ',I1,I2
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

	COMMON /SMTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL DX

	IF (XC .ne. XCC) THEN
	  print *,'ERROR: XC does not agree with number used for fit'
	  print *,'XC = ',XC
	  print *,'XCC = ',XCC
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

	COMMON /SMTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33

	IF (XC .ne. XCC) THEN
	  print *,'ERROR: XC does not agree with number used for fit'
	  print *,'XC = ',XC
	  print *,'XCC = ',XCC
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

	COMMON /SMTH/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
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















