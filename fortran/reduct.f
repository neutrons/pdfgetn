	SUBROUTINE REDUCT(NRS,SOQOUT)
C		     *****GLASSRED*****
C     
C     GLASSRED IS A ROUTINE FOR FIRST-CUT ANALYSIS OF
C     GLASS DIFFRACTION DATA.  IT ASSUMES THAT "SIGNAL",
C     "BACKGROUND", AND VANADIUM "REFERENCE" SPECTRA HAVE BEEN
C     COLLECTED.  ("BACKGROUND" IS ASSUMED TO BE APPROPRIATE
C     FOR BOTH "SIGNAL" AND "REFERENCE" SPECTRA.)
C     THE PROGRAM SUBTRACTS A SPLINE-SMOOTHED BACKGROUND FROM
C     SIGNAL AND SPLINE-SMOOTHED REFERENCE SPECTRA, COMPUTES	
C     AND APPLIES AN ATTENUATION CORRECTION FOR THE VANADIUM 	
C     SCATTERING, AND COMPUTES THE NET SIGNAL SPECTRUM NORMALIZED 
C     TO THE NET, SMOOTHED, CORRECTED REFERENCE SPECTRUM. 
C		     JMC 31/8/81
C     THIS VERSION MODIFIED BY DLP NOVEMBER 1981
C     AND BY WSH JUNE 1982
C
C $Id: reduct.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: reduct.f,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
c Revision 1.2  1997/04/13  16:53:10  billinge
c removed DEC fortran
c
C
	INCLUDE 'parameters.inc'
	CHARACTER SOQOUT*70

C local variables
	CHARACTER*70 SOQDMP
	REAL*4 XLAM(N_CHN),INAME
	REAL*4 AS(N_CHN),DSDAT(N_CHN),SAS(N_CHN)
	REAL*4 AR(N_CHN),DRDAT(N_CHN),SAR(N_CHN) 
	REAL*4 AC(N_CHN),DCDAT(N_CHN)
	REAL*4 AV(N_CHN),DVDAT(N_CHN)
	REAL*4 SFAC(N_CHN),RFAC(N_CHN)

	REAL*4 VREF(3),BREF(3)
	REAL*4 COEFF(4),RUNNAM(5)
	REAL*4 PVA(4),PVMS(4)
	REAL*4 P1(4),P2(4),P3(4)
C temp storage for smoothing
	REAL*4 XP(50),SSP(50),DSP(50),AAS(50),BBS(50),CCS(50),
     1       DDS(50)
	COMMON/SOFQ/Q,SS,DSS,RTITL,SMAX1
	REAL*4 Q(N_CHN),SS(N_CHN),DSS(N_CHN),RTITL(20),
     1      TEMPARRAY(N_CHN)
	COMMON/PARAS/NCH,TWOTHT,PI,NSRANG,NPRINT,DOMEGA,DELQ,
     1       IQMIN,IQMAX,JSG1
	COMMON/PASSRUNNAM/RUNNAM,INAME
	COMMON/CSVAN/SIGSR,SIGAR,VOVAN,DEVAN
	COMMON/CSSAM/SIGCS,SIGSS,SIGAS,VOSAM,DESAM
	COMMON/PLACZ/MBYM,MBYM2,KE
	REAL*4 MBYM,MBYM2,KE
	COMMON/DETPAR/DENDET,RDET,XF,SN,C1,C2,C3

	COMMON/LOGS/LPR,LPS,LCAN,LVAN,LCOR,NOVAN
	LOGICAL LPR,LPS,LCAN,LVAN,LCOR,NOVAN
	DATA PVA/1.,3*0./,PVMS/1.,3*0./
	DATA P1/1.,3*0./,P2/1.,3*0./,P3/1.,3*0./

      common /unit/ isabs,icabs,ivabs,ivplc,isplc,ivmsc,ismsc
C
C		CONVERT TO ANGLE THETA IN RADIANS
C
	THETAB=TWOTHT*PI/180./2.
	SN=SIN(THETAB)**2
	QLAM=4.*PI*SIN(THETAB)

c----read the sample data

	DO N=1,NCH
	  READ(13,END=888)XLAM(N),AS(N),DSDAT(N)
	ENDDO

c----read the vanadium data

	IF(.not. NOVAN) THEN
	  DO N=1,NCH
	    READ(12,END=888)XLAM(N),AR(N),DRDAT(N)
	  ENDDO
	ELSE

c--if no vanadium data, estimate spectrum using these parameters.

	  PRINT *,'Enter Vanadium Coeff'
	  READ(5,'(10F10.5)')COEFF
	  WRITE (*,'('' VANADIUM COEFF : '',4F10.5)')COEFF
	  DO N=1,NCH
	    AR(N)=POLY4(XLAM(N),COEFF)
	    DRDAT(N)=0.01
	  ENDDO
	ENDIF

c---read the can data

	IF(LCAN) THEN
	  DO N=1,NCH
	    READ(14,END=888)XLAM(N),AC(N),DCDAT(N)
	  ENDDO
	ENDIF

c---read the vanadium can data

	IF (LVAN) THEN
	  DO N=1,NCH
	    READ(15,END=888) XLAM(N),AV(N),DVDAT(N)
	  ENDDO
	ENDIF

C  Assignment of arrays at this point:
C		   SAMPLE	VANADIUM	CAN		VAN/CAN
C                  ------	--------	---		-------
C  DATA ARRAY	   AS		AR		AC		AV
C  ESD ARRAY	   DSDAT	DRDAT		DCDAT		DVDAT
C  Wavelengths	   XLAM		(same)		(same)		(same)
C
c---print out a load of info

	IF(NPRINT.GE.1)THEN
	  IF (LCAN) THEN
	    PRINT 1002
1002	FORMAT(/'    N      XLAM        AS     DSDAT        AR     DRDAT',
     1	'        AC     DCDAT'/)
	  ELSE
	    PRINT 1001
1001	FORMAT(/'    N      XLAM        AS     DSDAT        AR     DRDAT'/)
	  ENDIF
	  MULT=NCH/50
	  DO M=1,50
	    N=MULT*M
	    IF (LCAN) THEN
	      WRITE (*,'(I5,7F10.3)')N,XLAM(N),AS(N),DSDAT(N),AR(N)
     1	 ,DRDAT(N),AC(N),DCDAT(N)
	    ELSE
	      WRITE (*,'(I5,7F10.3)')N,XLAM(N),AS(N),DSDAT(N),AR(N),DRDAT(N)
	    ENDIF
	  ENDDO
	ENDIF

C---read CORPS corrections

	IF(LCOR)THEN
	  READ(16,'(5X,I2,3X,4F10.6)',END=888)KOR,PVA
	  READ(16,'(5X,I2,3X,4F10.6)',END=888)KOR,PVMS
	  READ(16,'(5X,I2,3X,4F10.6)',END=888)KOR,P1
	  IF(LCAN)READ(16,'(5X,I2,3X,4F10.6)',END=888)KOR,P2
	  READ(16,'(5X,I2,3X,4F10.6)',END=888)KOR,P3
	write(*,*) 'I was here ...'
	ENDIF

c------------------------------------------------------------------------
c---CORRECTIONS TO DATA BEGIN HERE---
c------------------------------------------------------------------------
C subtract can from sample/vanadium

	IF(LCAN .or. LVAN) THEN
	  DO N=1,NCH
c I think that coef may be the absorption correction for the can data
	    IF(LCAN .OR. LVAN) COEF = POLY4(XLAM(N),P2)
	    IF(LCAN) then
	      AS(N) = AS(N) - COEF*AC(N)
C adjust for errors in CAN 
	      DSDAT(N) = SQRT(DSDAT(N)**2 + (COEF*DCDAT(N))**2)
	    ENDIF

c---if there is a vanadium can, adjust for errors in Vanadium CAN

	    IF(LVAN) then
	      AR(N) = AR(N) - COEF*AV(N)
	      DRDAT(N) = SQRT(DRDAT(N)**2 + (COEF*DVDAT(N))**2)
	    ENDIF
	  ENDDO

C-----write an output file so that the corrections can be plotted
	   DO COUNT = 1,NCH,4
	     TEMPARRAY(COUNT) = POLY4(XLAM(COUNT),P2)
	   ENDDO
	   call kuplotCorrWrite(icabs,'Can absorption correction factor',
     1	                        'CAbs',jsg1,nch,qlam,xlam,temparray)
	ENDIF

C  Assignment of arrays at this point:
C		   SAMPLE	VANADIUM
C                  ------	--------
C  DATA ARRAY	   AS		AR
C  ESD ARRAY	   DSDAT	DRDAT
C  Wavelengths	   XLAM		(same)

	IF(NRS.EQ.0)RETURN

C		COMPUTE ABSORPTION-CORRECTED  SPECTRA,
C		AND MONITOR SPECTRA CORRECTED FOR DETECTOR
C		EFFICIENCY (THE FLUX SPECTRUM)
	
	VREF(2)=VOVAN*DEVAN
	BREF(2)=VREF(2)*SIGSR
	VREF(3)=VOSAM*DESAM
	BREF(3)=VREF(3)*SIGSS

C		CONSTANTS DESCRIBING REFERENCE SCATTERER

	PRINT *,'PROCESSING VANADIUM REFERENCE SPECTRUM'
	WRITE (*,'('' VREF='',F10.5,'' BREF='',F10.5)')VREF(2),BREF(2)

 
C	LOOP OVER REFERENCE CHANNELS STARTS HERE

	DO N=1,NCH
	  Q(N)=QLAM/XLAM(N)
 	  X=POLY4(XLAM(N),PVA)*BREF(2)
c sjb catch divide by zero when there is an accidental zero in ref sample
	  if(ar(n) .eq. 0.0) ar(n)=ar(n-1)
	  AR(N)=AR(N)/X
	  DRDAT(N)=DRDAT(N)/X
	ENDDO

C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = POLY4(XLAM(COUNT),PVA)*BREF(2)
	ENDDO
	call kuplotCorrWrite(ivabs,
     1	               'Vanadium absorption correction factor',
     1	               'VAbs',jsg1,nch,qlam,xlam,temparray)

C
c---print normalization factors
C
	IF (NPRINT.GE.1) THEN
	  MULT=NCH/50
	  PRINT *,'   N      LAMDA       ATT     AR(N)  DRDAT(N)'
	  MULT=NCH/50
	  DO M=1,50
	    N=M*MULT
	    X=POLY4(XLAM(N),PVA)*BREF(2)
	    WRITE (*,'(I5,5F10.3)') N,XLAM(N),X,AR(N),DRDAT(N)
	  ENDDO
	ENDIF

C  	END OF LOOP OVER REFERENCE CHANNELS

	IF(NPRINT.GT.1) PRINT *,'1'

c	smooth vanadium reference s(q) (ar()) using local sliding spline-function fit
c	number of points fitted is (2*nsrang+1).
c	choose smoothing criterion that is statistically defensible
C
c---sjb---N.B., it is recommended to give NSRANG = 0.0 now.  In that case
c  no smoothing is carried out in  S(Q) on either the data or reference
c  samples. Instead, smoothing routines have been incorporated into
c  the data preparation programs, NEWINT for IPNS data   and 
c  HIPD_GLASSPREP for LANSCE data.


	IF (NSRANG .GT. 0) THEN
C smooth reference
	  NSTOTL=(2*NSRANG+1)
	  SMTHS=NSTOTL
	  NSMIN=NSRANG+1
	  NSMAX=NCH-NSRANG-1
	  DO N=NSMIN,NSMAX
	    DO NS=1,NSTOTL
	      NP=N-NSRANG-1+NS
	      XP(NS)=XLAM(NP)*100.
	      SSP(NS)=AR(NP)
	      DSP(NS)=DRDAT(NP)
	    ENDDO
	    CALL SMOOTH(XP,SSP,DSP,NSTOTL,SMTHS,AAS,BBS,CCS,DDS)
	    SAR(N)=AAS(NSRANG+1)
	  ENDDO
C     Clean up ends by putting  unsmoothed S(Q) into smoothed array 
	  DO N=1,NSMIN
	    SAR(N) = AR(N)
	    SAR(NCH+1-N) = AR(NCH+1-N)
	  ENDDO
	ELSE
C don not smooth reference
	  DO N=1,NCH
	    SAR(N) = AR(N)
	  ENDDO
	ENDIF

C		PRINT RESULTS
C
	IF (NPRINT.GE.1 .AND. NSRANG .gt. 0) THEN
	  MULT=NCH/50
	  PRINT *,'VANDADIUM S(Q) BEFORE & AFTER SMOOTHING'
	  PRINT *,'    N      XLAM(N)         Q(N)        AR(N)'
     1	    //'       SAR(N)     DRDAT(N)'
	  DO M=1,50
	    N=MULT*M
	    WRITE (*,'(I6,5F13.3)') N,XLAM(N),Q(N),AR(N),SAR(N),DRDAT(N)
	  ENDDO
	ENDIF
c----------------------------------------------------------------------------
c-------this is the end of the reference data smoothing---
c-------------------------------------------------------------------------
C
C		 CONSTANTS DESCRIBING SAMPLE
C
	PRINT *,'PROCESSING SAMPLE SPECTRUM'
	WRITE (*,'('' VSAMP='',F10.5,'' BSAMP='',F10.5)') VREF(3),BREF(3)  

 
C	LOOP OVER SAMPLE CHANNELS STARTS HERE

	DO N=1,NCH
	  X=POLY4(XLAM(N),P1)*BREF(3)
	  AS(N)=AS(N)/X
	  DSDAT(N)=DSDAT(N)/X
	ENDDO
C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = POLY4(XLAM(COUNT),P1)*BREF(3)
	ENDDO
	call kuplotCorrWrite(isabs,
     1	               'Sample absorption correction factor',
     1	               'SAbs',jsg1,nch,qlam,xlam,temparray)
C
C---print normalization factors
C
	IF (NPRINT.GE.1) THEN
	  PRINT *,'   N      LAMDA       ATT     AS(N)  DSDAT(N)'
	  MULT=NCH/50
	  DO M=1,50
	    N=M*MULT
	    X=POLY4(XLAM(N),P1)*BREF(3)
	    WRITE (*,'(I5,5F10.3)')N,XLAM(N),X,AS(N),DSDAT(N)
	  ENDDO
	ENDIF
C
C  	END OF LOOP OVER CHANNELS
C
	IF(NPRINT.GT.1)PRINT *,'1'
C
C	SMOOTH SAMPLE S(Q) (AS()) USING LOCAL SLIDING SPLINE-FUNCTION FIT
C	NUMBER OF POINTS FITTED IS (2*NSRANG+1).
C	CHOOSE SMOOTHING CRITERION THAT IS STATISTICALLY DEFENSIBLE
C
c---sjb---N.B., it is recommended to give NSRANG = 0.0 now.  In that case
c  no smoothing is carried out in  S(Q) on either the data or reference
c  samples. Instead, smoothing routines have been incorporated into
c  the data preparation programs, NEWINT for IPNS data   and 
c  HIPD_GLASSPREP for LANSCE data. NO SMOOTHING is recommended for the
c  sample, only for the background, vanadium and can data.
c
	IF (NSRANG .GT. 0) THEN
C smooth reference
	  NSTOTL=(2*NSRANG+1)
	  SMTHS=NSTOTL
	  NSMIN=NSRANG+1
	  NSMAX=NCH-NSRANG-1
	  DO N=NSMIN,NSMAX
	    DO NS=1,NSTOTL
	      NP=N-NSRANG-1+NS
	      XP(NS)=XLAM(NP)*100.
	      SSP(NS)=AS(NP)
	      DSP(NS)=DSDAT(NP)
	    ENDDO
	    CALL SMOOTH(XP,SSP,DSP,NSTOTL,SMTHS,AAS,BBS,CCS,DDS)
	    SAS(N)=AAS(NSRANG+1)
	  ENDDO
C     Clean up ends by putting  unsmoothed S(Q) into smoothed array 
	  DO N=1,NSMIN
	    SAS(N)=AS(N)
	    SAS(NCH+1-N)=AS(NCH+1-N)
	  ENDDO
	ELSE
C don not smooth reference
	  DO N=1,NCH
	    SAS(N) = AS(N)
	  ENDDO
	ENDIF

C		PRINT RESULTS

	IF (NPRINT.GE.1 .AND. NSRANG .gt. 0) THEN
	  MULT=NCH/50
	  PRINT *,'SAMPLE S(Q) BEFORE & AFTER SMOOTHING'
	  PRINT *,'    N      XLAM(N)         Q(N)        AS(N)  '//
     1	  	'     SAS(N)     DSDAT(N)'
	  DO M=1,50
	    N=MULT*M
	    WRITE(*,'(I6,5F13.3)')N,XLAM(N),Q(N),AS(N),SAS(N),DSDAT(N)
	  ENDDO
	ENDIF
c-------------------------------------------------------------------
c-------end of sample smoothing-----
c-----------------------------------------------------------------
****************************************************************************
C  COMPUTE MULTIPLE SCATTERING AND PLACZEK CORRECTIONS
c
c---LPS and LPR are .TRUE. if Placzek corrections are desired on
c  sample and reference data respectively.
c
c	reset the rfac arrays
	  DO N=1,NCH
	    RFAC(N) = 0.0
	    SFAC(N) = 0.0
	  ENDDO

140	IF(LPS .or. LPR) then
	  CALL CORPL(NCH,XLAM,RFAC,SFAC)
	ENDIF

C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = RFAC(COUNT)
	ENDDO
	call kuplotCorrWrite(ivplc,'Vanadium Placzek correction',
     1	               'VPlc',jsg1,nch,qlam,xlam,temparray)

C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = SFAC(COUNT)
	ENDDO
	call kuplotCorrWrite(isplc,'Sample Placzek correction',
     1	               'SPlc',jsg1,nch,qlam,xlam,temparray)

C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = POLY4(XLAM(COUNT),PVMS)
	ENDDO

	call kuplotCorrWrite(ivmsc,
     1	               'Vanadium multiple scattering correction',
     1	               'VMsc',jsg1,nch,qlam,xlam,temparray)
C-----write an output file so that the corrections can be plotted
	DO COUNT = 1,NCH,4
	   TEMPARRAY(COUNT) = POLY4(XLAM(COUNT),P3)
	ENDDO

	call kuplotCorrWrite(ismsc,
     1	               'Sample multiple scattering correction',
     1	               'SMsc',jsg1,nch,qlam,xlam,temparray)

	DO N=1,NCH
	  RFAC(N) = RFAC(N) + POLY4(XLAM(N),PVMS)
	  SFAC(N) = SFAC(N) + POLY4(XLAM(N),P3)
	ENDDO

C DIVIDE SAMPLE FUNCTION BY REFERENCE FUNCTION AND 
C CORRECT FOR INCOHERENT SCATTERING, MULTIPLE SCATTERING AND INELASTIC EFFECTS
	IF(NPRINT.GT.1)PRINT *,'1'
	PRINT *,'SAMPLE FUNCTION DIVIDED BY REFERENCE FUNCTION & CORRECTED FOR'
	PRINT *,'INCOHERENT AND MULTIPLE SCATTERING & INELASTIC EFFECTS'

C  Assignment of arrays at this point:
C		   SAMPLE	smoothed	VANADIUM   smoothed
C                  ------	--------	--------   --------
C  DATA ARRAY	   AS		SAS		AR	   SAR
C  ESD ARRAY	   DSDAT	--		DRDAT	   --
C  Wavelengths	   XLAM				(same)
C  Q		   Q				(same)
C
	PRINT 1232,SIGSS/SIGCS
1232	FORMAT('0Adjusting S(Q) by a factor of (SigSS/SigCS): ',g16.5)

C----divide unsmoothed by unsmoothed
	DO N=1,NCH
	  AC(N) = (AS(N)/AR(N) * RFAC(N) - SFAC(N)) * SIGSS/SIGCS + 1.0

C--- if data were smoothed divide smoothed by smoothed

	  IF (NSRANG .gt. 0) then 
	    SS(N) = (SAS(N)/SAR(N) * RFAC(N) - SFAC(N)) * SIGSS/SIGCS + 1.0
	  ELSE
	    SS(N) = AC(N)
	  ENDIF
C estimate errors on S(Q)
C The following incorporates errors on vanadium -- it increases
C the error values by 20-50% -- not applied since vanadium is heavily smoothed
C	  DSS(N) = SQRT((DSDAT(N)/SAS(N))**2 + (DRDAT(N)/SAR(N))**2)
C	1		* SAS(N)/SAR(N) * SIGSS/SIGCS*RFAC(N)
C
C Do not incorporate error values -- just rescale
	  DSS(N) = DSDAT(N)/SAR(N) * SIGSS/SIGCS * RFAC(N)
	ENDDO

c-------alot of printout here

	IF (NPRINT.GE.3 .AND. NSRANG .gt. 0) THEN
	  open(unit=99,file=soqout//'.soqdeb',status='unknown')
	  INQUIRE (UNIT=99,NAME=SOQDMP)
	  WRITE (*,'(''OUTPUT S(Q) INTERMEDIATE FILE: '',A)') SOQDMP
	  DO N=1,NCH
	    write(99,'(1p11g12.3)') xlam(N),Q(N),AS(N),AR(N),
     1	rfac(n),sfac(N),SAS(N),SAR(N)
	  ENDDO
	  close(unit=99)
	 ELSEIF (NPRINT.GE.3) THEN
	  open(unit=99,file=soqout//'.soqdeb;',status='unknown')
	  INQUIRE (UNIT=99,NAME=SOQDMP)
	  WRITE (*,'(''OUTPUT S(Q) INTERMEDIATE FILE: '',A)') SOQDMP

	  DO N=1,NCH
	    write(99,'(1p8g12.3)') xlam(N),Q(N),AS(N),AR(N),rfac(n),sfac(N)
	  ENDDO
	  close(unit=99)
	 ENDIF
C 
	IF (NPRINT.GE.1) THEN
	  PRINT *,'   N      XLAM(N)         Q(N)        AC(N)     '//
     1		'   SS(N)       DSS(N)'
	  MULT=NCH/50
	  DO N=MULT,NCH,MULT
	    WRITE(*,'(I6,5F13.3)')N,XLAM(N),Q(N),AC(N),SS(N),DSS(N)
	  ENDDO
	ENDIF
c---printout ends here
C
C---reset the Q array back to wavelengths

	DO IQ=1,NCH
	  Q(IQ)=XLAM(IQ)
	ENDDO

	RETURN
888	STOP 'END OF FILE READ'
	END



