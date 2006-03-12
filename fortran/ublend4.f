      program ublend4

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Bins data on an evenly spaced Q-grid and merges data
C from different banks using 'flux-weighting' which is nothing
C else than a weighted average.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'
      include 'const.inc'

      real soq(n_chn),
     1     dsoq(n_chn),
     1     q(n_chn),
     1     cq(n_chn),
     1     cadd(n_chn),
     1     cmul(n_chn),
     1     merge_q(n_chn),
     1     merge_soq(n_chn),
     1     merge_dsoq(n_chn),
     1     new_soq(n_banks, n_chn),
     1     new_dsoq(n_banks, n_chn),
     1     new_q(n_banks, n_chn),
     1     swap(n_chn),
     1     swap_err(n_chn),
     1     swap_d(n_chn),
     1     twotheta(n_banks),
     1     qmin(n_banks),
     1     qmax(n_banks),
     1     qlow,
     1     qbig,
     1     gridstep,
     1     d_space(n_chn),
     1     dspace(n_chn),
     1     delx,
     1     rho,
     1     angle,
     1     bank_angle(n_chn),
     1     sumvalue,
     1     fraction

      integer ibankbit(n_banks),
     1     ibankproc(n_banks),
     1     no_of_banks,
     1     nmin,
     1     nmax,
     1     num_of_channels,
     1     upperbound,
     1     no_of_points,
     1     dfl,
     1     local_points

      character instrcode*4,
     1     outfile*80,
     1     filroot*80,
     1     corrfile*80,
     1     title*80,
     1     histry*80,
     1     ext*4,
     1     err*6

      logical lex,
     1     lescape,
     1     lmatch,
     1     lback,
     1     lskal

      data ext/'.soq'/,err/'  '/

C Some presets

      qbig = 0.0
      qlow = 10000.

      do i=1,n_banks
         ibankbit(i)=0
      enddo

C Read stuff

      read(*,'(a)') instrcode
      read(*,'(a)') outfile
      call checkroot(filroot,ext)

      read(*,*) no_of_banks
      read(*,*) (ibankproc(j), j=1,no_of_banks)

      do i=1,no_of_banks
         ibankbit(ibankproc(i))=1
      enddo

      do i=1,no_of_banks
         read(*,*) twotheta(ibankproc(i)), qmin(ibankproc(i)), 
     1             qmax(ibankproc(i))
         qlow = min(qlow, qmin(ibankproc(i)))
         qbig = max(qbig, qmax(ibankproc(i)))
      enddo

      read (*,*) gridstep

      read (*,*) iref
      lmatch=(iref.gt.0)
      if (lmatch) read (*,*) lskal,lback

      read (*,'(a)') corrfile

C Check for the existence of the .soq-file

      inquire(file=filroot(:index(filroot,' ')-1)//ext, name=title,
     1        exist=lex)
      if (lex) then
	 call how_long(il,title)
         write(*,500) title(1:il)
500	 format (' %%Info: Using S(Q) file: ',a)
      else
         write(*,'(a)') 'STOP *** cannot find input file ***'
         stop
      endif

C Set a default binning step in case the step is too fine

      if (gridstep .lt. 0.005) gridstep = 0.005

C Write output

      write (*,*)
      do i=1,no_of_banks
         write (*,1001) twotheta(ibankproc(i)), 
     1                  qmin(ibankproc(i)), qmax(ibankproc(i))
      enddo

 1001 format(' %%Info: ',f7.2,' degree bank from Qmin = ',
     1         f6.2,' to Qmax = ', f6.2,' A**-1')

C Read now the data form .soq file. The data is in units of d-spacing.
C Big loop over all banks. Only the desired banks are being processed.

      if (corrfile.ne.'') then
        open (96,file='blen_corr.dat',status='unknown')
      endif
      lescape = .false.
      do i=1, n_banks
         if (ibankbit(i) .ne. 0) then
            call soqread(filroot, i, d_space, soq, dsoq, delx, ext,
     1                   rho, nmin, nmax, title, histry, angle, err)
            if (err .eq. 'endfil') then
               write(6,5432) no_of_banks
5432	       format (1x,i2,' banks were read and will be processed')
               lescape = .true.
            endif
            bank_angle(i) = angle

C Convert data from d-spacing into units of Q.

            do j=nmin, nmax
               q(j) = 2.*pi/d_space(j)
            enddo

C Write some output

            write(*,1005) i,angle,nmin,nmax
1005	    format (/,1x,79('-'),/,' %%Bank: ',i3,' at ',f8.3,
     1              ' degrees - Range: ',i5,' to ',i5,/,1x,79('-'),/)

C Rebin the data here. The data arrays will be reversed in that 
C subroutine such that Q is ascending. It will only be reversed
C in the end of the program.

            call uRebinQ2(q, soq, dsoq, nmin, nmax, gridstep, 
     1                    qbig, qmin(i), qmax(i), .true.)

            num_of_channels = nmax - nmin +1

C I think we need to fill zeros BEFORE merging - Thomas

            call fill_array(q,soq,dsoq,n_chn,num_of_channels,
     1                      qmin(i),qmax(i))

C If specified read file with corrections, rebin to the same grid
C and apply

            if (corrfile.ne.'') then
7000          format('#S ',i3,' Rebinned corrections')
7100          format('#L  Q   SQ   MUL    ADD   CORR')
	      write(96,7000) i
	      write(96,7100)
              write(*,1006) i
1006          format(' %%Info: Bank ',i3,
     1               ' applying correction from external file')
	      call corrread(corrfile,cadd,cmul,i,
     1                      gridstep,qbig,qmin(i),qmax(i))
              do j=nmin,nmax
	         write(96,*) q(j),soq(j),cmul(j),cadd(j),
     1                       cmul(j)*soq(j)+cadd(j)
                 soq(j) =cmul(j)*soq(j)+cadd(j)
                 dsoq(j)=dsoq(j)*cmul(j)
              enddo
            endif

C Assign the rebinned data to a 2D-array. Note: nmin and nmax were
C adjusted in uRebinQ2. All data arrays should have the same length
C now, namely extending from Q = 0.0 to Q = qbig.

            do j=nmin, nmax
               new_soq(i, j) = soq(j)
               new_dsoq(i, j) = dsoq(j)
               new_q(i, j) = q(j) 
            enddo
         endif

         if (lescape) goto 58
      enddo

 58   lescape = .false.

      if (corrfile.ne.'') then
        close (96)
      endif

C The arrays are still in ascending order, i.e. they go from Qmin to Qmax.
C The data are in new_soq, the error in new_dsoq and Q in new_q.
C End of big loop
C End of big loop to erase data and set limits.

C Merge the data here. Another big loop. The of all banks to be merged
C are already on identical Qgrids with identical limits so they can
C simply be merged together.

C Now we call the optional bank matching (from KUPLOT)

      if (lmatch) then
        call bank_match(new_soq,new_dsoq,num_of_channels,no_of_banks,
     &                  iref,lback,lskal,ibankproc)
      endif
      call bank_output(new_q,new_soq,new_dsoq,
     &                 num_of_channels,no_of_banks,ibankproc)

C Initialize merged array. merge_soq will contain the merge S(Q) and
C merge_dsoq its error.

      do i=1, num_of_channels
         merge_q(i) = new_q(ibankproc(1),i)
         merge_soq(i) = 0.0
         merge_dsoq(i) = 0.0
      enddo

      do i=1, num_of_channels
         local_points = 0
         do ibank = 1, n_banks
            if (ibankbit(ibank) .ne. 0) then
               if (new_dsoq(ibank,i) .gt. 0) then
                  merge_soq(i) = merge_soq(i) + new_soq(ibank, i)
     1                 /new_dsoq(ibank, i)**2
                  merge_dsoq(i) = merge_dsoq(i) + 
     1                 1/new_dsoq(ibank, i)**2
                  local_points = local_points + 1
               endif
            endif
         enddo
         if (merge_dsoq(i) .ne. 0.0) then
            merge_dsoq(i) = sqrt(1/merge_dsoq(i))
            merge_soq(i) = merge_soq(i)*merge_dsoq(i)**2
         endif
      enddo

C Reverse the array again and convert back to units of d-spacing
C and leave out Q=0 (point 1) !!!

      do i=1, num_of_channels-1
         swap(i) = merge_soq(num_of_channels-i+1)
         swap_err(i) = merge_dsoq(num_of_channels-i+1)
         swap_d(i) = 2.*pi/merge_q(num_of_channels-i+1)
      enddo

      num_of_channels = num_of_channels-1

      do i=1, num_of_channels
         merge_soq(i) = swap(i)
         merge_dsoq(i) = swap_err(i)
         dspace(i) = swap_d(i)
      enddo

C Calculate mean S(Q) over the data range.

      sumvalue = 0.0
      no_of_points = 0
      do i=1, num_of_channels
         if (merge_dsoq(i) .gt. 0.0) then
            sumvalue = sumvalue + merge_soq(i)
            no_of_points = no_of_points + 1
         endif
      enddo
      sumvalue = sumvalue/float(no_of_points)
      
      write(*,510) sumvalue
510   format (/,' %%Info: Average S(Q) over the data range : ', f12.7)

C Write data to .bld file

      open(unit=10, file=outfile,status='unknown',
     1     form='unformatted', access='sequential')
      write(6,'(a)') ' %%Info: Output s(q) file: '//
     1               filroot(1:index(filroot,' ')-1)//'.bld'

      write(10) title
      write(10) histry
      write(10) 1, num_of_channels, deltaq, rho, 0.0
      write(10) (dspace(n), merge_soq(n), merge_dsoq(n), 
     1           n=1, num_of_channels)
      close(10)

      write(6, *) 'successful termination of blend'
      stop
      end
c*******************************************************************
	subroutine corrread(corrfile,cadd,cmul,ibank,
     1                      gs,qb,qmi,qma)
c
	include 'parameters.inc'
c
	character*(*)	corrfile
	real            cadd(n_chn)
	real            cmul(n_chn)
	real            csig(n_chn)
	real            gs,qb,qmi,qma
	integer		nmi,nma
	integer		ibank
c
	character*80	line
	real            local_dummy(n_chn)
	real		local_cq(n_chn)
	real		local_gs,local_qb,local_qmi,local_qma
	integer		local_nmi,local_nma
	integer		ib,i,j
c
	open(91,file=corrfile,status='old',err=999)

c Find correct bank first

10	continue
	  read(91,'(a)',end=998,err=997) line
	  if (line(1:2).eq.'#S') then
	    read(line(3:80),*) ib
	    if (ib.eq.ibank) goto 20
	  endif
	goto 10
20	continue

c Now read data

30	continue
	  read(91,'(a)',end=998,err=997) line
	  if (line(1:1).ne.'#') then
	    backspace(91)
	    goto 40
	  endif
	  goto 30
40	continue

	j=1
50	continue
	  read(91,*,err=60,end=60) local_dummy(j),cmul(j),cadd(j)
	  j=j+1
	  goto 50
60	continue

	close (91)
c
	nmi=1
	nma=j-1
c
	do i=nmi,nma
          local_cq(i)=local_dummy(i)
	  csig(i)=1.0
	enddo
	local_nmi=nmi
	local_nma=nma
	local_gs=gs
	local_qb=qb
	local_qmi=qmi
	local_qma=qma
c
        call uRebinQ2(local_cq,cmul,csig,local_nmi,local_nma,
     1                local_gs,local_qb,local_qmi,local_qma,.false.)
        call fill_array(local_cq,cmul,csig,n_chn,local_nma,
     1                  local_qmi,local_qma)
c
	do i=nmi,nma
          local_cq(i)=local_dummy(i)
	  csig(i)=1.0
	enddo
	local_nmi=nmi
	local_nma=nma
	local_gs=gs
	local_qb=qb
	local_qmi=qmi
	local_qma=qma
        call uRebinQ2(local_cq,cadd,csig,local_nmi,local_nma,
     1                local_gs,local_qb,local_qmi,local_qma,.false.)
        call fill_array(local_cq,cadd,csig,n_chn,local_nma,
     1                  local_qmi,local_qma)
c
	return
c
997	stop '%% Error: Error reading correction file'
998	stop '%% Error: Could not find bank in correction file'
999	stop '%% Error: Could not open correction file'
c
	end
c*******************************************************************
	subroutine bank_output(q,soq,dsoq,nc,nb,iproc)
c
	include 'parameters.inc'
c
	integer iproc(n_banks)
	real    q(n_banks,n_chn)
	real    soq(n_banks,n_chn)
	real    dsoq(n_banks,n_chn)
c
	write(*,500)
c
	open(91,file='blen_bin.dat',status='unknown')
	do ii=1,nb
	  ib=iproc(ii)
	  write(91,1000) ii,ib
	  write(91,1100)
	  do ic=1,nc
	    if (dsoq(ib,ic).gt.0.0)
     &	       write(91,*) q(ib,ic),soq(ib,ic),dsoq(ib,ic)
	  enddo
	enddo
c
	close(91)
c
500	format(/,' %%Info: Writing rebinned bank data ',
     &	         'to blen_bin.dat ..')
1000	format('#S ',i3,' rebinned data from blend (Bank ',i3,')')
1100	format('#L  Q   SOQ    DSOQ')
	end
c*******************************************************************
	subroutine bank_match(soq,dsoq,nc,nb,iref,lback,lskal,iproc)
c
	include 'parameters.inc'
c
	real    soq(n_banks,n_chn)
	real    dsoq(n_banks,n_chn)
	integer iproc(n_banks)
	logical lback,lskal
c
	write(*,500)
c
	do ii=1,nb
	  ib=iproc(ii)
	  if (ib.ne.iref) then
	   wtot = 0.0
	   e    = 0.0
	   c    = 0.0
	   ee   = 0.0
	   cc   = 0.0
	   ce   = 0.0
c
	   do ic=1,nc
	     if (dsoq(ib,ic).gt.0.0 .and. dsoq(iref,ic).gt.0.0) then
	       wi   = 1.0
	       wtot = wtot+wi
	       e    = e +wi*soq(iref,ic)
	       ee   = ee+wi*soq(iref,ic)**2
	       c    = c +wi*soq(ib,ic)
	       cc   = cc+wi*soq(ib,ic)**2
	       ce   = ce+wi*soq(ib,ic)*soq(iref,ic)
	     endif
	   enddo 
c
	   if (lskal .and. lback) then
	     sk = (wtot*ce - e*c) / (wtot*cc - c*c)
	     ba = (e-sk*c) / wtot
       	   elseif (lskal .and. .not. lback) then
	     sk = ce / cc
	     ba = 0.0
	   elseif (.not. lskal .and. lback) then
	     sk = 1.0
	     ba = (e-c) / wtot
	   else
	     sk = 1.0
	     ba = 0.0
	   endif                         
c
	   write (*,1000) ib,iref,sk,ba
c
	   do ic=1,nc
	     soq(ib,ic)=sk*soq(ib,ic)+ba
	     dsoq(ib,ic)=sk*dsoq(ib,ic)
	   enddo
	  endif
	enddo
c
 500	format(/,' %%Info: Automatic bank matching ..')
1000	format(' %%Info: Matching bank ',i3,' to ',i3,
     &	       ': a = ',f7.4,', b = ',f7.4)
      end
c*******************************************************************
      subroutine fill_array(ax,ay,day,nmax,npts,alow,ahigh)
c
      real ax(nmax),ay(nmax),day(nmax)
      integer upperbound
c
      ipts_l = 0     
      ipts_m = 0     
      ipts_h = 0     

      ibound_max = 0

      do i=1, npts
         if ((ax(i) .ge. alow) .and. (ax(i) .le. ahigh)) then
            if (day(i) .eq. 0.0) then
               upperbound = 0
               do k = i+1, npts
                  if (ay(k) .ne. 0.0) then
                     upperbound = k
                     goto 71
                  endif
               enddo
               goto 72

 71	       continue

               if     (ax(i).gt.20.0) then
                 ipts_h  = ipts_h + upperbound - i
               elseif (ax(i).gt.10.0) then
                 ipts_m  = ipts_m + upperbound - i
               else
                 ipts_l  = ipts_l + upperbound - i
               endif

	       ibound_max = max(upperbound-i,ibound_max)

               do j=i, upperbound-1
                  fraction = (ax(j)-ax(i-1))/(ax(upperbound)-ax(i-1))
                  ay(j)  =  ay(i-1)+fraction*( ay(upperbound)- ay(i-1))
                  day(j) = day(i-1)+fraction*(day(upperbound)-day(i-1))
               enddo
            endif
         endif
 72      continue
      enddo

      write(*,500) ipts_l,ipts_m,ipts_h,ibound_max

500   format (' %%Info: Interpolated  (low/ >10 / >20)  : ',
     1        i7,'/',i7,'/',i7,/,
     1        ' %%Info: Max. number of interpolated pts : ',i7)

      end   
