!     ------------------------------------------------------------------
!
!     maketmap.f
!
!
!     Created on Wed Nov 26 16:54:52 2003
!     Copyright (c) 2003 MyCompany. All rights reserved.
!
!
!     ------------------------------------------------------------------

c*****7****************************************************************
	subroutine make_timemap(tof,ntof,tmap,nt,maxdat)
c-
c	Creates timemap for GSAS file
c	NOTE: We need to go back from musec to 100ns for this !!!
c+
	implicit	none
c
	integer		maxdat
c
	real		tof(maxdat)
	integer		tmap(maxdat)
	integer		ntof,nt,dt,j
c
	tmap(1) = 1
	tmap(2) = nint(10.* tof(1))
	tmap(3) = nint(10.*(tof(2)-tof(1)))
c
	nt = 1
	dt = tmap(3)
c
	do j=2,ntof-1
	  if(nint(10.*(tof(j+1)-tof(j))).ne.dt) then
	    nt=nt+3
	    tmap(nt)=j
	    tmap(nt+1)=nint(10.*tof(j))
	    dt=nint(10.*(tof(j+1)-tof(j)))
	    tmap(nt+2)=dt
	  endif
	enddo
c
	nt = nt+3
	tmap(nt) = nint(10.*tof(ntof))
c
	end
