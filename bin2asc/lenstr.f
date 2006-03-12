!     ------------------------------------------------------------------
!
!     lenstr.f
!
!
!     Created on Wed Nov 26 15:47:27 2003
!     Copyright (c) 2003 MyCompany. All rights reserved.
!
!
!     ------------------------------------------------------------------

c*****7***************************************************************
	integer function len_str(string)
c-
c	 determines the position of the last non-blank character
c+
	implicit       none
c
	character*(*)  string
	integer        laenge,i
c
	laenge=len(string)
	i=laenge
	do while (i.gt.0 .and. string(i:i).eq.' ')
	  i=i-1
	enddo
	if (i.ge.1) then
	  if (ichar(string(i:i)).eq.13) i=i-1
	endif
	len_str=i
	end
	
c*****7***************************************************************
	logical function check_star(string)
c
	implicit	none
c
	character*(*)	string
	integer		i
	logical		lfound
c
	lfound=.false.
	do i=1,len(string)
	  lfound = lfound .or. (string(i:i).eq.char(42))
	enddo
c
	check_star=lfound
	end
