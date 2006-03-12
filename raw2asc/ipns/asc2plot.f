c****************************************************************************
c	ASC2PLOT - SEPD ASC to SPEC plot files
c
c	This program converts SEPS ASCII files to a SPEC file format
c	that can be plotted using KUPLOT.
c
c	Version: 1.0
c	Author : Thomas Proffen
c
c****************************************************************************
c
c This is part of the PDFgetN distribution written by Peter Peterson,
c Matthias Gutmann, Thomas Proffen, and Simon Billinge.
c 
c Copyright 2000 Michigan State University Board of Trustees
c 
c Use and distribution of this program is subject to the terms laid out
c in the license in LICENSE.txt included with this distribution.  A copy
c of the license  can be obtained from Michigan  State University office
c of Libraries, Computing and Technology (517-353-0722).  
c
c****************************************************************************
	program asc2plot
c
	parameter	(maxdat  = 50000)
	parameter	(maxbank = 20)
c
	character*80    fname,aname,line
	integer*4	counts(maxdat)
	real		tmin(maxbank),tstep(maxbank)
c
	narg = iargc()
	if (narg.eq.2) then
	  call getarg (1,fname)
	  call getarg (2,aname)
	else
	  write (*,1000)
	  read(*,5000) fname
	  write (*,1100)
	  read(*,5000) aname
	endif
c
	open (unit=77, file=fname, status='unknown')
	open (unit=78, file=aname, status='unknown')
c
c------ read timing information
c
	line = "nothing here yet ..."
	do while (line(1:27).ne.'##### Number of time fields')
	  read(77,5000) line
	enddo
c
	read(77,*) num_t
	read(77,5000) line
c
	do i=1,num_t
	  read(77,*) ib,t_min,t_max,t_step
	  tmin(ib) =t_min
	  tstep(ib)=t_step
	enddo
c
c------ now we read data
c
10	continue
	  line = "nothing here yet ..."
	  do while (line(1:10).ne.'##### Data')
	    read(77,5000,end=20) line
	  enddo
c
	  read(77,*,end=20) ib,ihist,idat
	  read(77,5000) line
	  read(77,*,end=20) (counts(i),i=1,idat)
c
	  write(78,2000) ib
	  do i=1,idat
	    write(78,*) (i-1)*tstep(ib)+tmin(ib),counts(i)
	  enddo
c
	goto 10
c
20	continue
	close(77)
	close(78)
c
1000	format ('Give name of input ascii data file  : ',$)
1100	format ('Give name of output SPEC plot file  : ',$)
2000	format ('#S ',i3,' (Bank)',/,'#L ToF Counts')
5000	format (a)
	end
