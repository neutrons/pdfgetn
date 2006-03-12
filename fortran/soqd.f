C******************************************************************************
C*
C* PROGRAM SOQ (LIQUID & AMORPHOUS PROGRAM)
C* USES SUBROUTINE REDUCT (GLASSRED) WRITTEN BY JMC, MODIFIED BY DLP,
C* AND BY WSH AND YET AGAIN BY DGM (7 July 1985)
C* CALLED FOR EACH SUBGROUP 
C*---->  FORTRAN/CRO/LIS SOQ  [creates a listing file SOQ.LIS]
C*---->  LINK SOQ,VIDEO/OPTC 
C* MODIFIED FOR NEW VANADIUM CROSS SECTIONS AND METHANE MODERATOR
C* PARAMETERS BY DLP 28 July 1986
C* program changes with !! apparently made by W. Dmowski to remove DISPLA
C*	and to avoid interpolation to even steps
C* changes made by B. Toby/S. Billinge to clean up DO loop @ 200 to read
C*	in .INT banks, skipping banks where CORPS has not 
C*	been run for that bank					(11/88)
C*
C* changes made by B. Toby for improved error analysis           (9/89)
C*
C* ---S. Billinge going through to clean & check   (11/92)
C* unit 5 is the output from CORPS
C*
C*---Modified for UNIX by W. C. Tonjes 1995
C*
C*
C* $Id: soqd.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C*
C* $Log: soqd.f,v $
C* Revision 1.1  2006/03/12 09:59:25  tproffen
C* Initial revision
C*
c Revision 1.5  1999/05/24  16:59:48  billinge
c term message -> standard output
c
c Revision 1.4  1999/05/24  15:23:21  billinge
c added "successful termination" message
c
c Revision 1.3  1997/04/15  18:46:52  billinge
c removed file open error for corps file
c
c Revision 1.2  1997/04/13  16:41:28  billinge
c removed DEC fortran.
c commented out an ENCODE statement which writes header into the .soq
c file.  This change has not been tested.
c
C*
C****************************************************************************
C* local variables
	INCLUDE 'parameters.inc'

c---variables for fitting subroutine
	COMMON /SMOOTHVAR/ A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 A0C,A1C,A2C,XCC,detX,Z11,Z12,Z13,Z22,Z23,Z33
	REAL*8 X11,X12,X22,X23,X33,Y1,Y2,Y3
	REAL DXJ,DXJ2,DXJ3,DXJ4,SIGYJ2I

	REAL*4 SSUM(N_CHN),DSUM(N_CHN),XLAM(N_CHN)
	REAL*4 TI(5),AX(20),BX(20),NAMOUT(5),AX1(20),BX1(20)
	CHARACTER*4 XNAM(5),RNAM(5),FTITL(20),RUNNAM(5),HIST(7)
	CHARACTER*80 HISTINFO,SFILE,VFILE,SCFILE,VCFILE
	REAL*4 RTITLE(20),LABEL3(15),NAMX(2),LABEL4(15)
	INTEGER*4 NMULT(N_CHN),I1(20),I2(20),NCODE(20)
	CHARACTER*40 CORPSIN,SOQOUT*70,INAME*4,FNAM*4

	INTEGER*4 NX(20),IMIN(20),IMAX(20)
C
	COMMON/SOFQ/Q,SS,DSS,RTITL,SMAX1,OFFSET
	REAL*4 Q(N_CHN),SS(N_CHN),DSS(N_CHN)
	CHARACTER*80 RTITL
	COMMON/PARAS/NCH,TWOTHT,PI,NSRANG,NPRINT,DOMEGA,DELQ,IQMIN,IQMAX,JSG1
	COMMON/PASSRUNNAM/RUNNAM,INAME
	REAL*4 TWOTHT,PI
	COMMON/CSVAN/SIGSR,SIGAR,VOVAN,DEVAN
	COMMON/CSSAM/SIGCS,SIGSS,SIGAS,VOSAM,DESAM
	COMMON/PLACZ/MBYM,MBYM2,KE
	REAL*4 MBYM,MBYM2,KE
	COMMON/DETPAR/DENDET,RDET,XF,SN,C1,C2,C3
	COMMON/LOGS/LPR,LPS,LCAN,LVAN,LCOR,NOVAN
	LOGICAL     LPR,LPS,LCAN,LVAN,LCOR,NOVAN
	DATA NX/20*1/,IMAX/20*N_CHN/,IMIN/20*0/
	DATA AX/20*1.0/,BX/20*0.0/

C
C---initial data specified here.
      SMAX=0.
      OFFSET=0.
      DANGLE=38.1*1.112/150.0/150.0
      PI=4.0*ATAN(1.0D0)
      NMIN=1000
      NMAX=0
C	DETECTOR CONSTANTS - INSTRUMENT DEPENDENT
	PDET=10.
	RDET=0.556
	DENDET=PDET*0.602252*(273.15/293.15)/22413.6
	DENDET=DENDET*5327./1.8
C
      DELQ=0.01
C
      PRINT *,'TYPE TITLE OF THIS ANALYSIS RUN'
      READ(5,'(A)') RTITL

      SFILE=' '
      VFILE=' '
      SCFILE=' '
      VCFILE=' '
C
      PRINT*,'TYPE FOUR-LETTER INSTRUMENT NAME'
      READ(5,'(A)')INAME
      PRINT*,'TYPE SAMPLE FILE NAME'
      READ(5,'(A)')SFILE
      PRINT*,'TYPE VANADIUM FILE NAME'
      READ(5,'(A)')VFILE
      PRINT*,'TYPE CONTAINER FILE NAME'
      READ(5,'(A)')SCFILE
      PRINT*,'TYPE VANADIUM CONTAINER FILE NAME'
      READ(5,'(A)')SVFILE
C
	CALL HOW_LONG(ILS,SFILE)
	NOVAN=.FALSE.
	CALL HOW_LONG(ILV,VFILE)
	IF(ILV .LT. 1) NOVAN=.TRUE.
	LCAN=.TRUE.
	CALL HOW_LONG(ILSC,SCFILE)
	IF(ILSC .LT. 1) LCAN=.FALSE.
	LVAN=.TRUE.
	CALL HOW_LONG(ILVC,VCFILE)
	IF(ILVC .LT. 1) LVAN=.FALSE.
C
C open the Intensity (.INT) file for the sample
	WRITE(*,'(''LIQUID DIFFRACTION DATA''/''SAMPLE : '',A)')SFILE(1:ILS)
	OPEN (UNIT=13,FILE=SFILE(1:ILS)//'.int'
     1    ,STATUS='OLD', FORM='UNFORMATTED',
     2    ACCESS='SEQUENTIAL')
	READ(13,END=888)FNAM,RNAM,RTITLE,NSGD
	PRINT '(1X,20A4)',RNAM
C open the Intensity (.INT) file for the Vanadium
	IF(.not. NOVAN) THEN
	  PRINT 1004,VFILE(1:ILV)
1004	  FORMAT(' REFERENCE : ',A)
	  OPEN (UNIT=12,FILE=VFILE(1:ILV)//'.int',STATUS='OLD'
     1      ,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
	  READ(12,END=888)FNAM,RNAM,FTITL,NSG1
	ENDIF
C open the Intensity (.INT) file for the Can file
	IF (LCAN) THEN
	  PRINT 1005,SCFILE(1:ILSC)
1005	  FORMAT(' CONTAINER : ',A)
	  OPEN (UNIT=14,FILE=SCFILE(1:ILSC)//'.int',STATUS='OLD'
     1      ,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
	  READ(14,END=888)FNAM,RNAM,FTITL,NSG2
	ENDIF
C open the Intensity (.INT) file for the Can-Vanadium file
	IF (LVAN) THEN
	  PRINT 1006,VCFILE(1:ILVC)
1006	  FORMAT(' VAN CONT : ',A)
	  OPEN (UNIT=15,FILE=VCFILE(1:ILVC)//'.int',STATUS='OLD'
     1      ,FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
	  READ(15,END=888)FNAM,RNAM,FTITL,NSG3
	ENDIF
c-sjb # subgroups, print-level,nsrang=0 for no smoothing of data,
c   nadd = 1, if nadd.>1 then data will be modified, see below.
	PRINT *,'TYPE NSGD,NPRINT,NSRANG,NADD'
	READ(5,*)NSGD,NPRINT,NSRANG,NADD
	PRINT 1008,NSGD,NPC,NPRINT,NSRANG,NADD
1008	FORMAT('NSGD=',I2,' NPC=',I2,
     2	' NPRINT=',I2,' NSRANG=',I2,' NADD=',I2) 
c 
c modify the data using NADD paramameters. 
c
	IF(NADD.GT.1) THEN
      	  PRINT*,' TYPE NADD SETS OF(NX,MULT,ADD)'
	  PRINT*,' FOR EACH DETECTOR SUBGROUP COMBINED IN AVERAGE'
c sjb, if NADD>1, S(Q) becomes MULT*(S(Q) + ADD) 
c
	  READ(5,*) (NCODE(M),AX1(M),BX1(M),M=1,NADD)
	  DO M=1,NADD
	    NM=NCODE(M)
	    AX(NM)=AX1(M)
	    BX(NM)=BX1(M)
	  ENDDO
	ENDIF
C
c
	LCOR=.FALSE.
	CORPSIN=' '
	PRINT *,'CORRECTIONS FILE FROM CORPS (Default extension .cor)?'
	READ(5,'(A)')CORPSIN
	IF(CORPSIN.NE.' ')LCOR=.TRUE.
c
c----if there is no CORPS file then take these values
c
	IF(.NOT. LCOR) THEN
	  SIGSR=0.375
	  SIGAR=0.204
	  DEVAN=0.60233*6.1/50.95
	  PRINT *,'TYPE VAN RADIUS & HEIGHT'
	  READ(5,*) RREF,HREF
	  PRINT 1013,SIGSR,SIGAR,RREF,HREF
1013	  FORMAT('0REFERENCE'/14X,' SIGS=',F8.5,' SIGA=',F8.5,
     1	' RADIUS=',F8.5,' HEIGHT=',F8.5)
	  VOVAN = RREF*RREF*HREF
C
	  PRINT*,'TYPE COHERENT,SCATTERING,ABSORPTION CS (CM-1 AT 1 ANG)',
     1	'SAMPLE RADIUS,HEIGHT (CM), NUMBER DENSITY (CM-3)'
 	  READ(5,*) SIGCS,SIGSS,SIGAS,RSAMP,HSAMP,DESAM
	  PRINT 1012,SIGCS,SIGSS,SIGAS,RSAMP,HSAMP,DESAM
1012	  FORMAT('0  SAMPLE'/' SIGC=',F8.5,' SIGS=',F8.5,' SIGA=',
     1	F8.5,' RADIUS=',F8.5,' HEIGHT=',F8.5,' RHO=',F8.5)
	  VOSAM=RSAMP*RSAMP*HSAMP
	ELSE
C
C Open the output file from CORPS (Now .COR)
	  OPEN(UNIT=16,STATUS='OLD',
     1 FORM='FORMATTED',FILE=CORPSIN)
	  READ(16,'(1X,20A4)',END=888)RINST,RNUM
	  READ(16,'(20A4)',END=888)RBSH
	  READ(16,'(20A4)',END=888)RBSH
	  READ(16,1014,END=888)SIGSR,SIGAR,VOVAN,DEVAN
1014	  FORMAT(14X,2(6X,F8.5),2(3X,F8.5))
	  READ(16,1011,END=888)SIGCS,SIGSS,SIGAS,VOSAM,DESAM
1011	  FORMAT(6X,F8.5,2(6X,F8.5),2(3X,F8.5))
	  READ(16,1034) MBYM,MBYM2,KE
1034	  FORMAT(6X,F10.6,7X,F10.6,6X,F10.2)
	ENDIF
C
20    ZERO=0.
!!      PRINT 1031
!!1031    FORMAT('0REFERENCE DATA')
!!      PRINT 1032,ZERO,SIGSR,SIGAR,VOVAN,DEVAN
!!      PRINT 1033
!!1033    FORMAT('0SAMPLE DATA')
!!      PRINT 1032,SIGCS,SIGSS,SIGAS,VOSAM,DESAM
!!1032    FORMAT(' SIGC=',F10.5,' SIGS=',F10.5,' SIGA=',F10.5,
!!     1   ' VO=',F10.5,' DE=',F10.5)
21	PRINT *,'PLACZEK (V&S) ? TT, TF, OR FF '
	READ(5,'(2L1)')LPR,LPS
1022	FORMAT(2L1)
	IF (LPS) THEN
	  PRINT*,'TYPE [M/M],[M2/M2],[KE]'
c- if no corps file, read more input
	  IF(.NOT.LCOR) READ(5,*)MBYM,MBYM2,KE
	  WRITE (*,'(''[M/M]='',F10.6,'' [M2/M2]='',F10.6,'' KE='',F10.2)')
     1	       MBYM,MBYM2,KE 
	ENDIF
c
c---open the output file
c
 	OPEN (UNIT=10,FILE=SFILE(1:ILS)//'.soq',STATUS='UNKNOWN',
     1	FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
 	INQUIRE (UNIT=10,NAME=SOQOUT)
	WRITE (*,'(''OUTPUT S(Q) FILE: '',A)')SOQOUT
c
c------- Open files for corrections
c
	call kuplotOpenCorrFiles(novan,lcan)
c
c--------write in some header information
c
	WRITE(10) RTITL
	HIST(1)=SFILE(1:4)
	HIST(2)=SFILE(5:8)
	HIST(3)=' NO '
	HIST(4)=' CAN'
	HIST(5)=' NO '
	HIST(6)=' VAN'
	IF(LCAN) HIST(3)=SCFILE(1:4)
	IF(LCAN) HIST(4)=SCFILE(5:8)
	IF(.NOT.NOVAN) HIST(5)=VFILE(1:4)
	IF(.NOT.NOVAN) HIST(6)=VFILE(5:8)
	IF(.NOT.LPR) HIST(7)='=FF '
	IF(LPR.AND.LPS) HIST(7)='=TT '
	IF(LPR.AND..NOT.LPS) HIST(7)='=TF '
	WRITE(HISTINFO,333) (HIST(JJ),JJ=1,7)
333	FORMAT(' SAMP=',2A4,'  CAN=',2A4,'  V=',2A4,' PLACZEK',A4,18X)
	WRITE(10) HISTINFO
c
c---initialise arrays
c
	DO NPT=1,N_CHN
	  NMULT(NPT)=0
	  DSUM(NPT)=0.0
	  SSUM(NPT)=0.0
	ENDDO
C
C --- Loop for each detector bank.  Loop goes almost to the end of the prog.
C
	DO 200 J=1,NSGD
	  IF(LCOR) READ(16,'(9X,I2,8x,F8.3,4X,F10.5)',END=888)JSG1,TWOTH1,XF1
	  IF(TWOTH1 .LT. 0.0) TWOTH1 = -1.*TWOTH1

C read sample bank, skip bank(s) if no data present in corps file for that
c   bank.
c
80	  READ(13,END=888)JSG,TWOTHT,DIST,XF,NCH,TZERO,CHNWID
	  NCHD=NCH
	  PRINT 1010,'SAMPLE',JSG,TWOTHT,XF,NCH
	  twothtsgn = twotht
	  IF(TWOTHT .LT. 0.0) TWOTHT = -1.*TWOTHT
1010	  FORMAT(' READING ',a10,' SUBGROUP',I2,' 2THT=',F8.3,' XF=',
     1	F8.3,' NCH=',I5)
	  IF(LCOR) THEN
	    IF(JSG.NE.JSG1) THEN
	      PRINT *,'  Skipping SAMPLE bank #',jsg
	      do n=1,nch
	        read(13,end=888)
	      enddo
	      goto 80
	    ENDIF
	    IF(ABS(TWOTHT-TWOTH1).GT.0.001)STOP 'CORR 2THETA DISAGREES'
	    IF(ABS(XF1-XF).GT.0.001)STOP 'CORR XF DISAGREES'
	  ENDIF

C read vanadium bank, skip over data of banks to ignore.

	  IF(.not. NOVAN) THEN
81	    READ(12,END=888)JSG1,TWOTH1,DIST,XF1,NCH,TZERO,CHNWID
	    PRINT 1010,'VANADIUM',JSG1,TWOTH1,XF1,NCH
	  IF(TWOTH1 .LT. 0.0) TWOTH1 = -1.*TWOTH1
	    IF(JSG.NE.JSG1) THEN
	      PRINT *,'  Skipping VANADIUM bank #',jsg1
	      do n=1,nch
		read(12,end=888)
	      enddo
	      goto 81
	    ENDIF
	    IF(ABS(TWOTHT-TWOTH1).GT.0.001)STOP 'VANADIUM 2THETA DISAGREES'
	    IF(ABS(XF1-XF).GT.0.001)STOP 'VANADIUM XF DISAGREES'
	    IF(NCH.NE.NCHD) STOP 'VANADIUM CHANNELS DISAGREE'
	  ENDIF

C read can bank, skip over data of banks to ignore

	  IF (LCAN) THEN
82	    READ(14,END=888)JSG1,TWOTH1,DIST,XF1,NCH,TZERO,CHNWID
	    PRINT 1010,'SAMPLE CAN',JSG1,TWOTH1,XF1,NCH
	  IF(TWOTH1 .LT. 0.0) TWOTH1 = -1.*TWOTH1
	    IF(JSG.NE.JSG1) THEN
	      PRINT *,'  Skipping CAN bank #',jsg1
	      do n=1,nch
	        read(14,end=888)
	      enddo
	      goto 82
	    ENDIF
	    IF(ABS(TWOTHT-TWOTH1).GT.0.001)STOP 'CAN 2THETA DISAGREES'
	    IF(ABS(XF1-XF).GT.0.001)STOP 'CAN XF DISAGREES'
	    IF(NCH.NE.NCHD) STOP 'CAN CHANNELS DISAGREE'
	  ENDIF

C read vanadium can bank, skip over data of banks to ignore

	  IF (LVAN) THEN
83	    READ(15,END=888)JSG1,TWOTH1,DIST,XF1,NCH,TZERO,CHNWID
	    PRINT 1010,'VAN. CAN',JSG1,TWOTH1,XF1,NCH
	  IF(TWOTH1 .LT. 0.0) TWOTH1 = -1.*TWOTH1
	    IF(JSG.NE.JSG1) THEN
	      PRINT *,'  Skipping CAN-VANADIUM bank #',jsg1
	      do n=1,nch
	        read(15,end=888)
	      enddo
	      goto 83
	    ENDIF
	    IF(ABS(TWOTHT-TWOTH1).GT.0.001)STOP 'VAN/CAN 2THETA DISAGREES'
	    IF(ABS(XF1-XF).GT.0.001)STOP 'VAN/CAN XF DISAGREES'
	    IF(NCH.NE.NCHD) STOP 'VAN/CAN CHANNELS DISAGREE'
	  ENDIF

c-------------------------------------------------------------------
C this is where the data are corrected
c   NX(J) = NRS = NADD = 1 in usual circumstances
c-------------------------------------------------------------------
	  NRS=NX(J)
	  CALL REDUCT(NRS,SOQOUT)
C
c---if minimum corrections, then jump ahead to 200, otherwise
c
	  IF(NX(J).EQ.0) GO TO 200
	  IF(NADD.NE.0) THEN
	    WRITE(10) 1,NCH,DELQ,DESAM,TWOTHTSGN
c---this factor is 2sin(theta)
	    factor=2.*sin(twotht*pi/360.)
	    DO i=1,nch
	      q(i)=Q(i)/factor
	    ENDDO
	PRINT *, 'RANGE OF D-SPACINGS IS ',Q(1),' TO ',Q(NCH)
c	PRINT *, 'range of d-spacings in xlam is',xlam(1),xlam(nch)
c
c---now Q is in units of d-spacing, i.e. Q(i)=lambda/2sin(theta)
c---no analysis appears to have been done specifically in these 
ci--units, but the data are written to the output file in units of d.
c
c---here we apply AX and BX to the data
c---NOTE we use J and not JSG, so bank numbers are supposed to be
c---1,2,3,4,... 
c
	IF (AX(J).NE. 1.0 .OR. BX(J).NE.0.0) THEN
	  PRINT *, 'MODIFYING BANK : ',J, ' S''(Q)=',AX(J),
     1	           ' * (S(Q) + ',BX(J),')'
	  DO I=1,NCH
	     SS(I)=AX(J)*(SS(I)+BX(J))
	    DSS(I)=AX(J)*DSS(I)
	  ENDDO
	ENDIF
c
c---write data to .soq file
c
	    WRITE(10) (Q(N),SS(N),DSS(N),N=1,NCH)

c---fit a polynomial to the region of the S(Q) from Q=20 onward to check the
c  normalisation.  The array q is, contrary to its name, in units of
c  d-spacing. The program fits a 2nd order polynomial to the region
c  of S(Q) beyond 15 A**-1.  In this region the scattering should be
c  dominated by incoherent scattering and should approximate to 
c  P = 1 + 0*x + 0*x**2.

c---convert the Q array to units of Q to avoid confusion.  Find the
c  start and finish channel numbers (IONE and ITWO) for the fit.
	TWOPI = 2*PI
	DO I=1,NCH
	   Q(I) = TWOPI/Q(I)
	ENDDO
	PRINT *, Q(1),Q(2),Q(50),Q(100),Q(1000)

	QMINI = 20.
	QMAXI = Q(1)

	IONE = 1
	QMIDD = (QMAXI - QMINI)/2. + QMINI	

	DO I=1,NCH
	   ITWO = I
	   IF (Q(I) .LE. QMINI) GOTO 536
	ENDDO
536	CONTINUE

	if(itwo -  ione .ge. 10) then
	   WRITE(*,'(/,A)') ' A polynomial will be fit over the range of S(Q)'
	   WRITE(*,'(A6,F8.2,A9,I4,A6,F8.2,A9,I4,A2)') ' from ',Q(ITWO),
     1                 ' (ICHN = ',ITWO,')  to ',Q(IONE),
     2                 ' (ICHN = ',IONE,') '
	   WRITE(*,'(A)') ' to check the normalisation. '

	   CALL FIT(Q,SS,DSS,QMIDD,IONE,ITWO,A0,A1,A2)

	   WRITE(*,'(A)') 'The coefficients of the fitted polynomial are as '
     1        //'follows:'
	   WRITE(*,'(A)') ' P = A0 + A1*X + A2*X**2 '
	   WRITE(*,'(A6,I2,A10,F11.4,2(A15,F11.4))') ' Bank ',JSG,
     1           '     A0 = ',A0,'          A1 = ',A1,
     1                         '          A2 = ',A2
	endif

C-----Change Q array back, just in case.
	DO I=1,ITWO
	   Q(I) = TWOPI/Q(I)
	ENDDO
	  ENDIF	  
200	CONTINUE

c----------------------------------------------------------------------------
c--------------end of the loop, go back for next channel---------------------
c----------------------------------------------------------------------------
        print *, 'successful termination of soqd: NMAX ZERO'
	IF(NMAX.EQ.0)STOP 
	print *, 'successful termination of soqd: NO AVERAGE'
	IF(NADD.EQ.1) STOP 
c----------------------------------------------------------------------------
c-------------------under normal circumstances, end here---------------------
c----------------------------------------------------------------------------

	NMINP=NMIN
	NMAXP=NMAX
	MULT=(NMAXP-NMINP+1)/50
	DO NPT=NMINP,NMAXP
	  IF(NMULT(NPT).GT.0) THEN
	   NP=NPT/MULT
	   NP=NP*MULT
	   Q(NPT)=NPT*DELQ
	   DSS(NPT)=SQRT(DSUM(NPT))/NMULT(NPT)
	   SS(NPT)=SSUM(NPT)/NMULT(NPT)
	   IF (NPT.EQ.NP) WRITE (*, '(I5,3F10.5,I5)')NPT,Q(NPT),
     1		SS(NPT),DSS(NPT),NMULT(NPT)
          ENDIF
	ENDDO
	TT=0.0
	WRITE(10)NMINP,NMAXP,DELQ,DESAM,TT
	WRITE(10)(Q(N),SS(N),DSS(N),N=NMINP,NMAXP)

c---close out cleanly

889	IF(.NOT.NOVAN) CLOSE (UNIT=12)
	IF(LCAN) CLOSE (UNIT=14)
	IF(LVAN) CLOSE (UNIT=15)
	CLOSE (UNIT=13)
	CLOSE (UNIT=10)
c
c------- Close files for corrections
c
	call kuplotCloseCorrFiles(novan,lcan)
c
        print *, 'successful termination of soqd'
	STOP 
888	PRINT *,' END OF FILE READ'
	GO TO 889
	END



