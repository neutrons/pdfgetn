!     ------------------------------------------------------------------
!
!     invnorm.f
!
!
!     Created on Wed Nov 26 22:21:31 2003
!     Copyright (c) 2003 MyCompany. All rights reserved.
!
!
!     ------------------------------------------------------------------

c*****7****************************************************************
	subroutine inv_norm(xval,yval,dyval,ndat,ib,s,o,czero,ascal,
     &	                    vtype,vrange,vp,nbank,maxarray)
c+
c	Inversly normalizes by incident spectrum
c-                                
	implicit	none
c
	integer		nbank
	integer		maxarray
c
	character*1	czero,ascal
	real		xval(maxarray)
	real		yval(maxarray)
	real		dyval(maxarray)
	real		vrange(nbank,2),vp(nbank,12)
	real		cof(12)
	real		i0,tof
	real		s,o,ymax,ymin
	integer		vtype(nbank)
	integer		ndat,i,j,ib
c
        do i=1,ndat
          tof=xval(i)/1000.
          if (vtype(ib).eq.1) then
            i0=vp(ib,1)
            i0=i0+vp(ib, 2)*exp(-1.0*vp(ib, 3)*tof)
            i0=i0+vp(ib, 4)*exp(-1.0*vp(ib, 5)*tof**2)
            i0=i0+vp(ib, 6)*exp(-1.0*vp(ib, 7)*tof**3)
            i0=i0+vp(ib, 8)*exp(-1.0*vp(ib, 9)*tof**4)
            i0=i0+vp(ib,10)*exp(-1.0*vp(ib,11)*tof**5)
c
          elseif (vtype(ib).eq.2) then
            i0=vp(ib,1)
            i0=i0+(vp(ib,2)/tof**5) *
     &                           exp(-1.0*vp(ib, 3)/tof**2)
            i0=i0+vp(ib, 4)*exp(-1.0*vp(ib, 5)*tof**2)
            i0=i0+vp(ib, 6)*exp(-1.0*vp(ib, 7)*tof**3)
            i0=i0+vp(ib, 8)*exp(-1.0*vp(ib, 9)*tof**4)
            i0=i0+vp(ib,10)*exp(-1.0*vp(ib,11)*tof**5)
c
          elseif (vtype(ib).eq.3) then
            call calc_cheb(tof,cof)
            i0=vp(ib,1)
            do j=2,12
              i0=i0+vp(ib,j)*cof(j)
            enddo
c
          elseif (vtype(ib).eq.4) then
            call calc_cheb(tof,cof)
            i0=vp(ib,1)
            i0=i0+(vp(ib,2)/tof**5) *
     &                           exp(-1.0*vp(ib, 3)/tof**2)
            do j=2,10
              i0=i0+vp(ib,j+2)*cof(j)
            enddo
          endif
           yval(i)= yval(i)*i0
          dyval(i)=dyval(i)*i0
	enddo
c
c------	Deal with scaling and negative numbers
c
	if (ascal.eq.'y' .or. ascal.eq.'Y') then
	  ymin=yval(1)
	  ymax=yval(1)
	  do i=2,ndat
	    ymax=max(ymax,yval(i))
	    ymin=min(ymin,yval(i))  
	  enddo
	  s=99999.9/(ymax-ymin)
	  o=-ymin*s
	endif
c
	do i=1,ndat
	  if (yval(i).ne.0.0) then
	     yval(i)=s* yval(i)+o
	    dyval(i)=s*dyval(i)
	  endif
	enddo
c
c------	Set negative numbers to zero if selected
c
	do i=1,ndat
	  if (czero.eq.'Y' .or. czero.eq.'y') then
	    if (yval(i).lt.0.0) then
	      yval(i)=0.0
	      dyval(i)=0.0
	    endif
	  endif
	enddo
c
	end
c*****7****************************************************************
        subroutine calc_cheb(tof,cof)
c+
c       Calculated Chebyschev polynominal coefficients
c-
	implicit	none
c
        real            tof,cof(12)
        real            t
        integer         i
c
        t=1.0/tof
        t=2.0*t-1.0
c
        cof(1)=1.0
        cof(2)=t
c
        do i=3,12
          cof(i)=2.0*t*cof(i-1)-cof(i-2)
        enddo
c
        end
c*****7****************************************************************
        subroutine fix_value(y,dy)
c+
c       Set NaN and Inf to 0.0
c-
	implicit	none
c
        real            y,dy
	character*40	cstr
c
	write(cstr,*) y
	if (index(cstr,'INF').ne.0 .or. index(cstr,'NAN').ne.0) then
	   y=0.0
	  dy=0.0
	endif
c
	end

