!     ------------------------------------------------------------------
!
!     SOQtoGSA.f
!
!     Program to convert PDFgetN S(Q) files (raw stage) back to
!     GSAS files for Rietveld refinement.
!
!
!     Created on Wed Nov 26 15:15:00 2003
!     Copyright (c) 2003 Thomas Proffen, LANL. All rights reserved.
!
!
!     ------------------------------------------------------------------


	program SOQtoGSA
c
	character*4	version
	parameter	(version='1.11')
c
	parameter	(nbb=20)
	parameter	(maxdat=12000)
c
	character*80	soqfile,ifile,gfile,title,hist
	character*80	outstr
	character*30	bname(nbb)
	character*6	err
	character*2	eol
	character*1	chr,czero,ascal
	real*4		soq(maxdat),dsoq(maxdat)
	real*4		x(maxdat),xx(maxdat)
	real*4		gdat(maxdat),dgdat(maxdat)
	real		tof(maxdat),tof_map(maxdat)
	real		difc(nbb),difa(nbb),tzero(nbb)
	real		twotheta(nbb)
	real		vrange(nbb,2),vp(nbb,12)
	real		qmin(nbb),qmax(nbb)
	real		fpath
	integer		brec(nbb)
	integer		tmap(maxdat)
	integer		vtype(nbb)
	integer		nbank,nb,irec
	logical		lfile,lmap
c
	logical		check_star
c
	write(*,*) '***         SOQtoGSAS '//version//'     ***'
	write(*,*) '*** (c) Thomas Proffen, LANL   ***'
	write(*,*)
	write(*,*) 'This program converts the binary SOQ file from'
	write(*,*) 'PDFgetN back to a GSAS file. You will need the'
	write(*,*) 'file xxxx.soq which is produced by PDFgetN as'
	write(*,*) 'well as the corresponding instrument parameter'
	write(*,*) 'file. The output file will be xxxx.soq.gsa ...'
	write(*,*)
c
c------	Read user input
c
10	continue
	write(*,1000)
	read (*,9999) soqfile
	gfile=soqfile(1:len_str(soqfile))//'.gsa'
	write(*,1100)
	read (*,9999) ifile
	write(*,1150)
	read (*,9999) ascal
	if (ascal.eq.'n' .or. ascal.eq.'N') then
	  write(*,1200)
	  read (*,*) s,o
	  write(*,1300)
	  read (*,9999) czero
	endif
	write(*,1400)
	read(*,*) ibb
	do i=1,ibb
	  write(*,1410) i
	  read (*,*) qmin(i),qmax(i)
	enddo
	write(*,*)
c
	eol=char(13)//char(10)
	irec=1
c
c------	Read instrument parameter file
c
	call read_iparm(ifile,nbb,difc,difa,tzero,
     &	                twotheta,bname,fpath,nbank,vrange,vp,vtype)
	write(*,3000)
	do k=1,nbank
	  write(*,3100) k,bname(k),difc(k),difa(k),tzero(k),twotheta(k)
	enddo
	write(*,*)
c
c------ Read data file and convert
c
	inquire(file=gfile,exist=lfile)
	if(lfile) then
	  open(unit=77,file=gfile,status='old')
	  close (unit=77,status='delete')
	endif
c
	open(unit=77,file=gfile,status='new',access='direct',
     &	     form='formatted',recl=82)
c
	nb   = 0
	imap = 0
c
	err = 'readon'
	do while (err.ne.'endfil')
	  nb = nb+1
	  call soqread(soqfile,nb,x,soq,dsoq,dr,jj,n,title,hist,
     &	               angle,err,maxdat)
	  if (err.ne.'endfil') then
c
c------	--- If it is the first bank, write header & timemap
c
	    if (nb.eq.1) then
	      write(outstr,9999) title(1:len_str(title))
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      write(outstr,5040) ifile(1:len_str(ifile))
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      write(outstr,5050) 1
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
c
	      write(outstr,8000)
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      write(outstr,8100) soqfile(1:len_str(soqfile))
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      write(outstr,8000)
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
c
	      do i=1,nbank
	        write(outstr,8000)
	        write(77,9999,rec=irec) outstr(1:80)//eol
	        brec(i)=irec
	        irec=irec+1
	      enddo
c
	      write(outstr,8000)
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	    endif
c
c------	--- Convert d back to TOF
c
	    do i=1,n
	      xx(i)=x(i)*difc(nb)+x(i)**2*difa(nb)+tzero(nb)
	    enddo
c
	    tof(1)=xx(1)-(xx(2)-xx(1))*0.5
	    do i=1,n
	      tof(i+1)=xx(i)+(xx(i+1)-xx(i))*0.5
	    enddo
c
	    if (nb.eq.1) then
	      lmap=.true.
	    else
	      lmap=.false.
	      do i=1,n
	        lmap=lmap.or.(abs(tof(i)-tof_map(i)).gt.0.5)
	      enddo
	    endif
c
	    do i=1,n
	      tof_map(i)=tof(i)
	    enddo
c
	    if (lmap) then
	      write(*,4100)
	      imap=imap+1
	      call make_timemap(tof,n,tmap,ntmap,maxdat)
	      nrec=(ntmap-1)/10+1
	      write(outstr,5000) 'TIME_MAP',imap,ntmap,nrec,
     &	                         ' TIME_MAP',100
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      ibeg=1
	      do j=1,nrec
	        ifin=min(ibeg+9,ntmap)
	        write(outstr,5100) (tmap(k),k=ibeg,ifin)
	        write(77,9999,rec=irec) outstr(1:80)//eol
	        irec=irec+1
	        ibeg=ibeg+10
	      enddo
	    endif
c
c------	--- Now write data
c
	    dmin=2.0*3.1415927/qmax(nb)
	    dmax=2.0*3.1415927/qmin(nb)
	    n=n-1
	    do i=1,n
	      if (x(i).le.dmin .or. x(i).ge.dmax) then
	        gdat(i)=0.0
	        dgdat(i)=0.0
	      else
	        gdat(i) = soq(i)*(tof(i+1)-tof(i))
	        dgdat(i)=dsoq(i)*(tof(i+1)-tof(i))
	        call fix_value(gdat(i),dgdat(i))
	      endif
	    enddo
c
c------	--- Fold incident spectrum back in (we all love GSAS)
c
	    call inv_norm(tof,gdat,dgdat,n,nb,s,o,czero,ascal,
     &	                  vtype,vrange,vp,nbb,maxdat)
c
	    nrec=(n-1)/5+1  
	    write(outstr,5200) 'BANK ',nb,n,nrec,' TIME_MAP',
     &	                       imap,' ESD'
	    write(77,9999,rec=irec) outstr(1:80)//eol
	    irec=irec+1
c
	    ibeg=1
	    do j=1,nrec
	      ifin=min(n,ibeg+4)
	      write(outstr,5300) (gdat(k),dgdat(k),k=ibeg,ifin)
	      if (check_star(outstr)) goto 999
	      write(77,9999,rec=irec) outstr(1:80)//eol
	      irec=irec+1
	      ibeg=ibeg+5
	    enddo
c
	    write(*,4000) nb,n,s,o
	    write(outstr,4010) nb,n,s,o
	    write(77,9999,rec=brec(nb)) outstr(1:80)//eol
c
	  endif
	enddo
	write(*,*)
	write(*,6000) gfile(1:len_str(gfile))
c
c------ One more ?
c
30	continue
	close(77)
	write(*,2000)
	read (*,9999) chr
	if (chr.eq.'Y' .or. chr.eq.'y') goto 10
c
	write(*,*)
	write(*,*) 'Bye ..'
	stop
c
999	write (*,*) 'Error writing GSAS file - aborting ..'
	goto 30
c
1000	format (1x,'Give name of binary S(Q) file           : ',$)
1100	format (1x,'Give name of instrument parameter file  : ',$)
1150	format (1x,'Automatic scaling for GSAS format (Y/N) : ',$)
1200	format (1x,'Give scale and offset for GSAS data     : ',$)
1300	format (1x,'Set negative intensities to zero (Y/N)  : ',$)
1400	format (1x,'Give number of banks                    : ',$)
1410	format (1x,'  Give Qmin and Qmax for bank ',i2,'        : ',$)
2000	format (1x,'Process another file (Y/N) ? ',$)
3000	format (1x,'Bank',5x,'Name',10x,'DIFC',9x,'DIFA',7x,'TZERO',4x,
     &          'Bank angle',/,1x,70('-'))
3100	format (1x,i3,4x,a12,1x,f9.3,3x,f9.5,2x,f9.5,5x,f7.2) 
4000	format (1x,'Done bank ',i2,' (',i5,' channels, scale=',
     &	           f8.4,', offset=',f12.4,') ..')
4010	format ('# Bank ',i2,': ',i5,' channels, scale=',
     &	           f8.4,', offset=',f12.4)
4100	format (1x,'Writing new TIME_MAP ..')
5000	format (a,3i5,a,i5)
5040	format ('Instrument parameter file: ',a)
5050	format ('Monitor: ',i15)
5100	format (10i8)
5200	format (a,i3,i6,i5,a,i3,a)
5300	format (10f8.1)
6000	format (1x,'Output written to file ',a,' ..')
8000	format ('#',79('-'))
8100	format ('# Converted from PDFgetN file : ',a)
9999	format (a)
	end
