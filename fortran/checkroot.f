        SUBROUTINE CHECKROOT(FILROOT,EXT)
C******************************************************************
C	A Subroutine for finding the Root of a filename on UNIX
C
C $Id: checkroot.f,v 1.1 2006/03/12 09:59:24 tproffen Exp $
C
C******************************************************************

	CHARACTER*80 FILROOT,FILEIN
	CHARACTER EXT*4
	LOGICAL EX
	INTEGER ifind,ifind2,nfind

C-------Set place counters to zero	
	ifind = 0
	ifind2 = 0
        nfind = 0

C-------Call for file name
CTEP    PRINT *,'Enter file to use'
	READ(*,'(A)'), FILEIN
	FILROOT = FILEIN

C-------Check input and find filename root
	CALL HOW_LONG(ILONG,FILROOT)
	IF(ILONG .NE. 0) THEN
	  inquire(file=filein,exist=ex)
	  if (ex) then                         	
            length = len(FILROOT(1:index(FILROOT,' ')))-1
            DO k=1,length
                 nfind = index(FILROOT(ifind+1:length),'/')
                 ifind = ifind + nfind
            ENDDO
            ifind2 = index(FILROOT(ifind+1:length),'.')
	    IF (ifind2 .NE. 0)
     1          EXT = FILROOT(ifind2+ifind:index(FILROOT,' ')-1) 	
            FILROOT = FILROOT(ifind+1:ifind2+ifind-1)
          endif
	endif

	RETURN
	END
