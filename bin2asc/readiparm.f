!     ------------------------------------------------------------------
!
!     readiparm.f
!
!
!     Created on Wed Nov 26 15:38:17 2003
!     Copyright (c) 2003 MyCompany. All rights reserved.
!
!
!     ------------------------------------------------------------------

c*****7****************************************************************
	subroutine read_iparm(inam,nb,difc,difa,tzero,
     &	                      twotheta,bname,fpath,nbank,
     &                        vrange,vp,vtype)
c+
c	Read GSAS instrument parameter file
c-                                
	implicit	none
c
	integer		nb
c
	character*(*)	inam
	character*80	line
	character*30	bname(nb)
	character*6	search
	real		difc(nb)
	real		difa(nb)
	real		tzero(nb)
	real		twotheta(nb)
	real		vrange(nb,2),vp(nb,12)
	real		fpath
	real		dummy
	integer		vtype(nb)
	integer		ibank,nbank,ll,i
c
	integer		len_str
c
	write(*,*) 'Reading instrument parameter file ..'
c
	open(unit=12,file=inam,status='old')
c
c------ Read instrument parameter file information
c
20      continue
	read (12,'(a)',end=40) line
	ll=len_str(line)
	read (line(1:ll),'(4x,i2,a6)',err=20) ibank,search
c
c------ Number of banks
c
	if (search.eq.'BANK  ') then
	  read (line(13:ll),*) nbank
c
c------ Primary flight path
c
	elseif (search.eq.'FPATH1') then
	  read (line(13:ll),*) fpath
c
c------ Diffractometer constants
c
	elseif (search.eq.' ICONS') then
	  read (line(13:ll),*) difc(ibank),difa(ibank),tzero(ibank)
c
c------ Bank parameters
c
	elseif (search.eq.'BNKPAR') then
	  read (line(13:ll),*) dummy,twotheta(ibank)
	elseif (search.eq.'BNKNAM') then
	  bname(ibank)=line(13:ll)
c
c------ Incident spectrum information
c
	elseif (search.eq.'I ITYP') then
	  read (line(13:ll),*) vtype(ibank),vrange(ibank,1),
     &	                      vrange(ibank,2),dummy
	elseif (search.eq.'ICOFF1') then
	  read (line(13:ll),*) (vp(ibank,i),i=1,4)
	elseif (search.eq.'ICOFF2') then
	  read (line(13:ll),*) (vp(ibank,i),i=5,8)
	elseif (search.eq.'ICOFF3') then
	  read (line(13:ll),*) (vp(ibank,i),i=9,12)
c
	endif
	goto 20
c
40	continue
c
        close(12)
        return
        end
