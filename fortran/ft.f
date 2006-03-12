C----------------------------------------------------------------------
C PROGRAM FT - Fourier TRANSFORMS S(Q) TO D(R) \ AVERAGING AND GROUPING
C               S(Q) FROM DIFFERENT DETECTOR BANKS IS AVAILABLE\
C
C $Id: ft.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: ft.f,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
c Revision 1.2  1997/04/13  16:21:51  billinge
c removed DEC fortran
c
C
C----------------------------------------------------------------------

C CREATES FILE  *.PDF  { R,N(R),G(R),A(R) } Unformatted
C
C In:
C Q      - scattering vector
C SS(Q)   - structure factor
C DSS(Q) - error
C
C N(R)   - number density function
C G(R)   - radial density function=4*pi*r*(n(r)-n0)
C A(R)   - number of atoms between 0 and R
	
	INCLUDE 'const.inc'
	CHARACTER RTITL*80,namin*80,HISTRY*80
C allowed error in D-space:
	PARAMETER (EPS=0.00001)

C number of points in reciprocal space
	PARAMETER (NQ=15000)
        REAL*4 D(NQ),SS(NQ),DSS(NQ)

C number of points in real space
	PARAMETER (NR=15000)
	REAL*4 RR(NR),BS(NR),CS(NR),DOFRS(NR),rho(NR),drho(NR)


	PRINT *,'ENTER INPUT FILE? (DEFAULT EXTENSION=.asq)? '
	READ(5,'(A)') NAMIN
C If an input file name is given, open it. Otherwise use logical name
C GLASS_ROOT: to point to the file
	IF (NAMIN .ne. ' ') then
	  OPEN (UNIT=10,FILE=NAMIN,STATUS='OLD',
     1       FORM='UNFORMATTED',
     2       ACCESS='SEQUENTIAL')
	ELSE
	  OPEN (UNIT=10,FILE='GLASS_ROOT:',
     1	STATUS='OLD',
     2	FORM='UNFORMATTED',ACCESS='SEQUENTIAL')
	ENDIF
	inquire (UNIT=10,NAME=NAMIN)
	PRINT '(1X,A)','Reading file: '//NAMIN

C---- read the .ASQ file ---------------------------------------------------
	READ(10) RTITL
	PRINT '(1X,A)',RTITL
	READ(10) HISTRY
	PRINT '(1X,A)',HISTRY
	read(10) NL,NH,DELQ,den,ANGLE
	read(10) (D(NP),SS(NP),DSS(NP),NP=NL,NH)
	CLOSE (UNIT=10)
	PRINT *, 'FILE READ FINISHED '
C-----------------------------------------------------------------------
C---- THERE ARE ONLY TWO OPTIONS FOR OUTPUT FILES
C     IFILE=0 - ONLY SEPD*.RDF
C---- IFILE=1 -  SEPD*.RDF also SEPD*.ASQ
C-----------------------------------------------------------------------
C      PRINT *,' Error code (0=no error, 1=yes)'
	PRINT *,' TYPE Ifile  (Format 3I2)' 
	READ(5,*) IFILE
C-----------------------------------------------------------------------------
	PRINT *,' NPTS(=0 calls EXIT),RMAX?'
	READ(5,*)NRPTS,RMAX,RHO0
	NRPTS = MIN(NR,NRPTS)
	IF (NRPTS.EQ.0) THEN 
	   STOP 'Zero data points'
	ENDIF

c density has been taken out of the ft program so that [rho(r)-rho0] is
c   output instead of rho(r) as before.  This is so that the density can
c   appear as a parameter in the modelling rather than as a parameter in the
c   analysis.
c	IF(RHOP.GT.0.001) den=RHOP

C		FORM GRID OF EQUALLY-SPACED R'S AT WHICH THE  FOURIER
C		TRANSFORM IS TO BE COMPUTED (RMAX IN ANGSTROMS), AND THE
C		NUMBER OF R-POINTS.
C
	DO I=1,NRPTS
	  RR(I)=(RMAX/NRPTS)*I
 	ENDDO
C
C	 THE NUMBER OF POINTS IN THE RANGE OF DATA TO
C		BE TRANSFORMED.
C
C-----------------------------------------------------------------------------
C		COMPUTE FOURIER TRANSFORM OF THE DATA
C  IF IFILE = 1 also COMPUTE ERROR ON FOURIER TRANSFORM
	QLAM=2.*PI
	AFACT=2.0/PI
	DO I=1,NRPTS
	  FS = 0.0
	  ERR = 0.0
	  RP = RR(I)
	  DO N=nl+1,nh
	    XLAM = QLAM**2 / D(N)**3
	    DELD = D(N) - D(N-1)
	    SINUS = SIN((QLAM/D(N))*RP) * XLAM * DELD
	    FS = FS + SINUS * (SS(N)-1.)
	    IF (IFILE.eq.1) ERR = ERR+(SINUS*DSS(N))**2 
	  ENDDO
	  DOFRS(I) = FS*AFACT
	  rho(I)   = DOFRS(I) / (4.0*PI*RP)
	  IF (IFILE.eq.1) drho(I)=sqrt(err)*AFACT/(4.*PI*RP)
	ENDDO
C-----------------------------------------------------------------------------
C CALCULATE G(0)
	  NGY=12
	  NGZ=100
	  DZERO=0.0
	  DENOM=0.0  
	  DO I=NGY,NGZ
	    DENOM=DENOM+4.0*PI*RR(I)
	    DZERO=DZERO+DOFRS(I)
	  ENDDO
	  GZERO=-DZERO/DENOM
C	  PRINT 361,GZERO,NGY,NGZ
C 361    FORMAT('1GZERO=',F10.5,' AVERAGED FROM ',
C     1   I5,' TO',I5,' POINTS,  RHO=',F10.5) 
C	  PRINT 361,GZERO,NGY,NGZ
	  BS(1)=4.0*PI*RR(1)*RR(1)*rho(1)
	  CS(1)=0.5*BS(1)*RR(1)
	  DO I=2,NRPTS
	    BS(I)=4.0*PI*RR(I)*RR(I)*rho(I)
	    CS(I)=0.5*(BS(I)+BS(I-1))*(RR(I)-RR(I-1))+CS(I-1)
	  ENDDO
   
C
	NMINP=1
	NMAXP=NRPTS
C
	OPEN (UNIT=2,FILE=NAMIN(1:index(NAMIN,'.asq')-1)//'.pdf',
     1	STATUS='unknown',FORM='UNFORMATTED',
     2       access='sequential')
	WRITE(2)RTITL
	WRITE(2)HISTRY
	DELR=RMAX/NRPTS
	WRITE(2)NMINP,NMAXP,DELR,RHO0
	WRITE(2) (RR(I),rho(I),Drho(I),1.0,I=nminp,nmaxp)
	CLOSE (UNIT=2)


c---old write statement
c	WRITE(2)(RR(I),rho(I),DOFRS(I),CS(I),I=NMINP,NMAXP)


	print *, 'successful termination of ft'
	END
