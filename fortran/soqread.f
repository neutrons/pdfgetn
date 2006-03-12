C****************************************************************************
C*
c*   SUBROUTINE TO READ S(Q) FILES
C*---Read will read .SOQ and .ASQ, ETC.  files. 
C*
C* $Id: soqread.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C*
C* $Log: soqread.f,v $
C* Revision 1.1  2006/03/12 09:59:25  tproffen
C* Initial revision
C*
c Revision 1.2  1997/04/13  16:12:31  billinge
c removed DEC fortran
c
C*
C***************************************************************************

	SUBROUTINE SOQREAD(FILROOT,NBANK,X,SOQ,DELSOQ,DELX,EXT,RHO,NMIN
     1                        ,NMAX,TITLE,HISTRY,angle,err)

	include 'parameters.inc'

	REAL*4 SOQ(*),DELSOQ(*),X(*)
	CHARACTER FILROOT*80,HISTRY*80,TITLE*80,FIL*50,EXT*4,ERR*6

        OPEN(UNIT=2,FILE=FILROOT(1:index(FILROOT,'  ')-1)//EXT,
     1	STATUS='OLD',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')


	READ(2) TITLE
	if (nbank .eq. 1) PRINT *, TITLE
	READ(2) HISTRY
	if (nbank .eq. 1) PRINT *, HISTRY
	DO J = 1,NBANK-1
	   READ(2,end=20) NMIN,NMAX,DELX,RHO,ANGLE
	   READ(2) (rdummy,rdummy,rdummy,I=NMIN,NMAX)
	ENDDO
	   READ(2,end=20) NMIN,NMAX,DELX,RHO,ANGLE
CTEP	   WRITE(*,1000) NBANK,NMIN,NMAX,DELX,RHO,ANGLE
1000	   FORMAT(1x,'Bank ',i3,' > Range : ',i4,' to ',i4,
     &	         '  dQ : ',f8.5, ' Rho : ',f8.5,' Angle : ',f8.3)
	   READ(2,end=30) (X(I),SOQ(I),DELSOQ(I),I=NMIN,NMAX)

	CLOSE(2)

	return
C-----if end of file detected, send error in EXT
20	err = 'endfil'	
	return
30	STOP ' error during read of .SOQ file'
	END
