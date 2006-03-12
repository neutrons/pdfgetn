C----------------------------------------------------------------------
C PROGRAM DAMP - CUTS TOGETHER AND DAMPS S(Q) 
C
C Originally written by H.D. Rosenfeld, I think (SJB)
C
C $Id: damp.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: damp.f,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
c Revision 1.4  1997/04/15  18:55:01  billinge
c corrected typo introduced by last revision.
c
c Revision 1.3  1997/04/15  18:53:32  billinge
c removed file-read bug
c
c Revision 1.2  1997/04/13  16:18:48  billinge
c removed DEC fortran
c
C
C----------------------------------------------------------------------

C CREATES FILE *.ASQ  {D,S(Q),DSS(Q)} After damping -- Unformatted 

	INCLUDE 'const.inc'
      	CHARACTER RTITL*80,namin*80,HISTRY*80
C allowed error in D-space:
	PARAMETER (EPS=0.00001)

C number of points in reciprocal space
	PARAMETER (NQ=15000)
        REAL*4 SS(NQ),D(NQ),DSS(NQ),DS(NQ),DDSS(NQ)
        REAL*4 SSUM(NQ),dsum(NQ),DSSUM(NQ)
	INTEGER*2 NSUM(NQ)

C number of groups 
	PARAMETER (NGR=10)
	REAL*4 dx(NQ,NGR),ST(NQ,NGR),DST(NQ,NGR)
	REAL*4 RMNC(NGR),RMXC(NGR)
	INTEGER*4 MNC(NGR),MXC(NGR)
	INTEGER*4 NX(NGR)
	DATA NX/NGR*0/
	DATA MNC/NGR*0/MXC/NGR*NQ/

	PRINT *,'ENTER INPUT FILE'
	READ(5,'(A)') NAMIN
C If an input file name is given, open it. Otherwise use logical name
C GLASS_ROOT: to point to the file
c    this will not work now.  If the program cannot find the file it will
c    bomb.  Changed the code to reflect this  
c	IF (NAMIN .ne. ' ') then
	  OPEN (UNIT=10,FILE=NAMIN,
     1	STATUS='OLD',
     2	FORM='UNFORMATTED',ACCESS='SEQUENTIAL', err=57)
c	ELSE
c	  OPEN (UNIT=10,FILE='GLASS_ROOT:'//'.bld',
c     1	STATUS='OLD',
c     2	FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
c	ENDIF

	inquire (UNIT=10,NAME=NAMIN)
	PRINT 990,'Reading file: '//NAMIN
	READ(10) RTITL
	PRINT 990,RTITL
990	FORMAT(1X,A)
	READ(10) HISTRY
	PRINT 990,HISTRY
C-----------------------------------------------------------------------
C---- THERE ARE ONLY TWO OPTIONS FOR OUTPUT FILES
C-----------------------------------------------------------------------
C      PRINT *,' Input lambda minimum and maximum for weighting.'
C               IF input is zero, maximum values are used.
       PRINT *,' A, B, and C, where S(Q)--> A*S(Q)+B+C*Q.'
       PRINT *,' If A is input as 0 data are not modified'
C              
	NGTOT=0
	PRINT *,' A,B,C'
	READ(5,*) AX,BX,CX
	nstep = 0
	DO N=1,NGR
	  READ(10,END=225) MNC(N),MXC(N),DELQ,den,ANGLE
	  READ(10,END=225) (dx(NPT,N),ST(NPT,N),DST(NPT,N),NPT=MNC(N),MXC(N))
	  WRITE(6,'(a,3I5,3F10.4)') ' Group:',N,MNC(N),MXC(N)
     1	     ,DELQ,den,ANGLE
	    WRITE(6,997) N
997	    FORMAT(4X,'Input NX, QMIN and QMAX for group',I3)

c--------read in the limits
	    READ(5,*) NX(N),RMNC(N),RMXC(N)
	    IF (NX(N) .GT. 0) THEN
	      NGTOT=NGTOT+1
	      IF (RMNC(N).LE.0.) THEN
	        TRMXC=dx(MXC(N),N)
	      ELSE
	        TRMXC=2.*PI/RMNC(N)
	      ENDIF
	      IF (RMXC(N).LE.0.) THEN
	        TRMNC=dx(MNC(N),N)
	      ELSE
	        TRMNC=2.*PI/RMXC(N)
	      ENDIF
cc	PRINT *, ' input Qmax = ',rmxc(n),' min d is therefore ',trmnc
c	PRINT *, ' input Qmin = ',rmnc(n),' min d is therefore ',trmxc

	      RMNC(N)=TRMNC
	      RMXC(N)=TRMXC

	    WRITE(6,'(4x,a,I2,2F10.4)') 'Process as step #',
     1 			NX(N),RMNC(N),RMXC(N)
	    nstep = max(nstep,NX(N))
	  ENDIF
	ENDDO

C----------------------------------------------------------------------
225	PRINT*,'Total number of groups used:',NGTOT

C SUM/JOIN GROUPS
	NL=1
	NH=0

C process each group in the order selected:
	DO 33 KX=1,nstep
	  write(*,'(/A,i3)') ' **** Processing step ',KX
C look for 1st KXth group to process
	  DO 32 I=1,NGR
	    IF (NX(I).NE.KX) GOTO 32
C found 1st matching group
	    DO J=1,NQ
		NSUM(J) = 0
	    ENDDO
C     find range of points to use
	PRINT *, ' passing dx(1,I) = ',dx(1,I),' to range.'
	PRINT *, ' passing mnc,mxc,rmnc,rmxc = ',
     1         MNC(I),MXC(I),RMNC(I),RMXC(I)

	    CALL NRANGE(1,i,dx,mnc,mxc,rmnc,rmxc,nq,ngr)
	    PRINT *,'    Using Group# ',I,' range of channels=',MNC(I),MXC(I)
	PRINT *, ' returning dx(1,I) = ',dx(1,I),' from range.'
	PRINT *, ' returning mnc,mxc,rmnc,rmxc = ',
     1         MNC(I),MXC(I),RMNC(I),RMXC(I)

C     load data into sum array
	    DO K=MNC(I),MXC(I)
	      dsum(K) = dx(K,I)
	      SSUM(K) = ST(K,I)
	      DSSUM(K) = DST(K,I)**2
	      NSUM(K) = 1
	    ENDDO
	    PRINT *,'     (d-space range = ',dsum(MNC(I)),dsum(MXC(I)),')'
C     look for other groups to sum with previous
	    DO J=I+1,NGR
	      IF(NX(J).EQ.KX) THEN
C     are the ranges to be summed identical?
	        IF (ABS(RMNC(J)-RMNC(I)).GT.EPS .or. 
     1		ABS(RMXC(J)-RMXC(I)).GT.EPS) goto 92
C     find range of points to use
	PRINT *, ' second call to range'
	PRINT *, ' passing dx(1,I) = ',dx(1,I),' to range.'
	        CALL NRANGE(1,j,dx,mnc,mxc,rmnc,rmxc,nq,ngr)
		PRINT *,'    Summing Group#',J,'  points=',MNC(J),MXC(J)
	    PRINT *,'     (d-space range = ',dsum(MNC(J)),dsum(MXC(J)),')'
C     do the ranges have the same numbers of points?
		IF (MNC(J).ne.MNC(I) .or. MXC(J).ne.MXC(I)) goto 92
		DO K=MNC(J),MXC(J)
		  dsum(K) = dx(K,J) + dsum(K)
		  SSUM(K) = ST(K,J) + SSUM(K)
		  DSSUM(K) = DST(K,J)**2 + DSSUM(K)
		  NSUM(K) = NSUM(K) + 1
		ENDDO
	      ENDIF
	    ENDDO
	    goto 31
C  all groups to be processed in step I have now been handled
32	  CONTINUE
	  write(*,'(/A,i3/)') ' Warning no groups to process for step ',I
	  goto 33
C     all groups have been sumd, add them into the master array
31	  PRINT *,'  Joining',1+MXC(I)-MNC(I),' points, starting at',NH+1
	  DO K=MNC(I),MXC(I)
            NH = NH + 1
	    IF (NSUM(K) .le. 0) stop 'NSUM is 0!'
	    D(NH) = dsum(K)/NSUM(K)
	    IF (NH .gt. 2 .AND. D(NH-1) .GE. D(NH)) THEN
	      PRINT *,'Points are out of sequence:',NH-1,D(NH-1),NH,D(NH)
	      stop 'Overlap/sequence error'
	    ENDIF
	    SS(NH) = SSUM(K)/NSUM(K)
	    DSS(NH) = SQRT(DSSUM(K))/NSUM(K)
	  ENDDO
33	CONTINUE

	PRINT*,'NL,NH',NL,NH
C-----------------------------------------------------------------------------
	PRINT *,' QMUC, NMOD?'
	READ(5,*) QMAX,NMOD
!1000	FORMAT(I5,F10.5,f10.5,I5,F10.5)
C----THE MEANING OF QMAX IS NOW DIFFERENT:
C		IF NMOD=1 DAMPING STARTS AT QMAX AND ENDS AT D(NL)
	IF(NMOD.EQ.1) then
	  write(rtitl(56:),'(A,f5.1,A,f5.1)') 'Mod1',qmax,' to',2.*pi/D(nl)
	ELSEIF(NMOD.EQ.2) THEN
	  write(rtitl(56:),'(A,f5.1,A,f5.1)') 'Mod2',qmax,' to',2.*pi/D(nl)
	ELSEIF(NMOD.EQ.3) THEN
	  write(rtitl(56:),'(A,f5.1,A,f5.1)') 'Mod3',qmax,' to',2.*pi/D(nl)
	ELSEIF(NMOD.EQ.4) THEN
	  write(rtitl(56:),'(A,f5.1,A,f5.1)') 'Mod4',qmax,' to',2.*pi/D(nl)
	ENDIF

	IF(NMOD.GT.0)PRINT 1001,nmod
1001	FORMAT(/'  MODIFICATION FUNCTION #',i3,' USED'/)
	IF(NMOD.EQ.1) THEN
	  PRINT *,'Damping function: '
	  PRINT *,'       SIN[PI*delQ/delQmax]'
	  PRINT *,'       --------------------'
	  PRINT *,'          PI*delQ/delQmax'
	  PRINT *,' '
	  PRINT *,'                                   DelQ = Q(i) - Qmax'
	  PRINT *,'                                   DelQmax = Qlast - Qmax'
	ELSEIF (NMOD.EQ.2) THEN
	  PRINT *,'Damping function: '
	  PRINT *,'       SIN[PI*delQ/delQmax]              delQ'
	  PRINT *,'       --------------------  * EXP {-[2 -------]**2}'
	  PRINT *,'          PI*delQ/delQmax               DelQmax'
	  PRINT *,' '
	  PRINT *,'                                   DelQ = Q(i) - Qmax'
	  PRINT *,'                                   DelQmax = Qlast - Qmax'
	ELSEIF (NMOD.EQ.4) THEN
	  PRINT *,'Damping function: '
	  PRINT *,'              0.5*PI*delQ'
	  PRINT *,'       COS**2[-----------]'
	  PRINT *,'                delQmax'
	  PRINT *,' '
	  PRINT *,'                                   DelQ = Q(i) - Qmax'
	  PRINT *,'                                   DelQmax = Qlast - Qmax'
	ENDIF
C
C-----------------------------------------------------------------------------
C apply damping function -- compute modified S(Q) 
	A = PI /(2.*PI/D(nl) - QMAX)
	DO N=nl,nh
	  IF(NMOD.EQ.1 .and. 2.*PI/D(N).GT.QMAX) THEN
	    Q0 = 2.*PI/D(N)-QMAX
	    SMOD = SIN(Q0*A)/(Q0*A)
	  ELSEIF(NMOD.EQ.2 .and. 2.*PI/D(N).GT.QMAX) THEN
	    Q0 = 2.*PI/D(N)-QMAX
	    SMOD = SIN(Q0*A)/(Q0*A)
	    SMOD = SMOD*EXP(-4.*(Q0/(2.*PI/D(nl)-QMAX))**2)
	  ELSEIF(NMOD.EQ.4 .and. 2.*PI/D(N).GT.QMAX) THEN
	    Q0 = 2.*PI/D(N)-QMAX
	    SMOD = (COS(0.5*Q0*A))**2
	  ELSE
	    SMOD=1.0
	  ENDIF
	  DS(N) = 1. + SMOD*(SS(N)-1.)
	  DDSS(N) = SMOD*DSS(N)
	ENDDO
C---- extrapolate to S(q=0) -------------------------------------------------
	PRINT *,' S(q=0) = (<<b>>^2 - <<b^2>>)/<<b>>^2 ? '
	READ (5,*) S0
	if (S0.gt.0.) goto 100
	Qnh=2.*PI/D(nh)
	DQ=(2.*PI/D(nh-1))-Qnh
	NF=INT(Qnh/DQ)
	DSINC=(DS(nh)-S0)/(Qnh**2)
	QE=Qnh
	DO N=nh+1,nh+NF
	  QE=QE-DQ
	  D(N)=2.*PI/QE
	  DS(N)=S0+(DSINC*(QE**2))
	  DDSS(N)=DDSS(nh)*((Qe/Qnh)**2)
	ENDDO
	nh=nh+NF
	PRINT *, ' S(q=',QE,')=',DS(nh)
100	if (s0.gt.0.) PRINT *,' no extrapolation to S(q=0)'
C---- Modify data  and errors by AX,BX,CX -----------------------------------
	IF (AX.GT.0.0) THEN
	  PRINT *, 'Modifying S(Q) with AX=',AX,' BX=',BX,' CX=',CX
	  DO NP=NL,NH
	    DS(NP)=AX*DS(NP)+BX+CX*2.*PI/D(NP)
	    DDSS(NP)=AX*DDSS(NP)
	  ENDDO
	ENDIF
C---- write the .ASQ file ---------------------------------------------------
	ANGLE=0.0
	OPEN(UNIT=4,FILE=NAMIN(1:index(NAMIN,'.bld')-1)//'.asq',FORM=
     1  	'UNFORMATTED',ACCESS='SEQUENTIAL',STATUS='unknown')
	inquire (UNIT=4,NAME=NAMIN)
	PRINT 990,'Writing file: '//NAMIN
	  WRITE(4) RTITL
	  WRITE(4) HISTRY
	  WRITE(4) NL,NH,DELQ,den,ANGLE
	  WRITE(4) (D(NP),DS(NP),DDSS(NP),NP=NL,NH)
	  CLOSE(UNIT=4)

  	print *, 'successful termination of damp'	
	STOP
92	PRINT *,I,MNC(J),MXC(J),RMNC(J),RMXC(J)
	PRINT *,J,MNC(J),MXC(J),RMNC(J),RMXC(J)
	STOP 'ERROR IN SUM -- SETS/RANGES are inequivalent'
 57     STOP '***** file error read, can''t find file **********'
	END
C----------------------------------------------------------------------
	SUBROUTINE NRANGE(IQ,IGR,Q,IMIN,IMAX,RMIN,RMAX,NQ,NGR)
C find first and last point in array Q inside range RMIN-RMAX
	REAL*4    Q(NQ,NGR),RMIN(NGR),RMAX(NGR)
	INTEGER*4 IMIN(NGR),IMAX(NGR)

	DO I=IMIN(IGR),IMAX(IGR)
	  IF(Q(I,IGR).GT.RMIN(IGR)) THEN
	    IMIN(IGR) = I
!	    PRINT*,'RMIN FOUND',I
	    GOTO 11
	  ENDIF
	ENDDO
	RETURN

11	DO I=IMAX(IGR),IMIN(IGR),-1
	  IF (Q(I,IGR).LT.RMAX(IGR)) THEN
	    IMAX(IGR) = I
!	    PRINT *,'RMAX FOUND',I
	    RETURN
	  ENDIF
	ENDDO 
	RETURN
	END





