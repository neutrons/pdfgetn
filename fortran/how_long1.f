	SUBROUTINE how_long1(ILONG,STRING)

!PURPOSE: subroutine to find the length of a useful characters within a string

!PROGRAMMED by Simon Billinge 1992

! $Id: how_long1.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $

!CALLING ARGUMENTS:
	INTEGER  	IDIM 	!dimensioned length of the string, 
	INTEGER  	ILONG	!number of useful characters in the string.

!INCLUDE STATEMENTS:

	CHARACTER STRING*(*)

	IDIM=LEN(STRING)
	DO I=IDIM,1,-1
	   IF(STRING(I:I) .NE. ' ') GOTO 10
	   ILONG = I-1
	ENDDO


10	CONTINUE
	IF(I .EQ. 255) ILONG = 255
	IF (I .EQ. LEN(STRING)) ILONG = IDIM
	RETURN
	END
c
	SUBROUTINE how_long(ILONG,STRING)
	INTEGER*4 ILONG
	CHARACTER*(*) STRING
c
	CALL HOW_LONG1(ILONG,STRING)
	RETURN
	END
