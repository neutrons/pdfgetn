!     ------------------------------------------------------------------
!
!     soqread.f
!
!
!     Created on Wed Nov 26 15:34:48 2003
!     Copyright (c) 2003 MyCompany. All rights reserved.
!
!
!     ------------------------------------------------------------------

C***************************************************************************
	SUBROUTINE soqread(FILROOT,NBANK,X,SOQ,DELSOQ,DELX,NMIN,NMAX,
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
	
