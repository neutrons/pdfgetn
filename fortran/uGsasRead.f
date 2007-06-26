c------------------------------------------------------------------------
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
c------------------------------------------------------------------------
 
      subroutine uGsasGetNumberOfBanks(local_run_title,
     1                                 local_inputfile, 
     1	                               local_n_banks,local_bank_id)
c------------------------------------------------------------------------
c Purpose: Run through data file and determine number of banks.
c------------------------------------------------------------------------

      include       'parameters.inc'
      include       'gsasprep.inc'

      character*(*) local_run_title
      character*(*) local_inputfile
      character*80  local_line
      integer       local_bank_id(n_banks)
      integer       local_n_banks

      call how_long1(local_dfl, local_inputfile)
      open(unit=1,file=local_inputfile(1:local_dfl),status="old")

c Very first line is the title

      read (1,'(a)', end=20) local_run_title
      read (1,'(a)', end=20) local_line
      call how_long1(local_ipa, local_inputfile)
      if (local_line(1:10).ne.'Instrument') then
        write (*,*) '%%warn: No instrument parameter file entry'
      else
        if (local_line(27:local_ipa).ne.iparm_file) then
          write (*,*) 
     1       '%%warn: Instr. parameter file does not match input'
        endif
      endif

c Now get number of banks

      local_n_banks=0
10    continue
        read (1,'(a)', end=20) local_line
        call how_long1(ilen,local_line)
        if (local_line(1:5).eq.'BANK ') then
	  read(local_line(6:ilen),*,err=998) ib
	  if (ib.ne.0) then
	    local_n_banks=local_n_banks+1
	    local_bank_id(local_n_banks)=ib
	  endif
	endif
      goto 10
20    continue

      write(6,*) '%%info: Number of banks found : ',local_n_banks
      close(1)
      return
c
998   stop '%%%severe: Error reading data file'
      end

c------------------------------------------------------------------------

      subroutine uGsasReadBank1(local_inputfile, local_bank_no,
     1     local_tof, local_data, local_data_err, local_add_to_data,
     1     local_mult_data, local_num_of_channels, 
     1     local_twotht, local_dist, local_xf, local_difc,
     1     local_difa, local_tzero, local_print_level,
     1     local_minindex, local_maxindex, local_monitor,local_bank_id,
     1     local_weight)

c------------------------------------------------------------------------
c Purpose: Reads data from one bank from a GSAS data file
c------------------------------------------------------------------------

      include 'parameters.inc'
      include 'gsasprep.inc'

      real     local_tof(n_chn),
     1         local_data(n_chn),
     1         local_data_err(n_chn),
     1         local_temp(n_chn),
     1         local_twotht,
     1         local_dist,
     1         local_xf,
     1         local_l0,
     1         local_l1,
     1         local_monitor,
     1         local_add_to_data,
     1         local_mult_data,
     1         local_weight

      real*8   local_difc,
     1         local_difa,
     1         local_tzero 

      integer  local_bank_id(n_banks)

      integer  local_bank_no,
     1         local_print_level,
     1         local_dfl,
     1         local_num_of_channels,
     1         local_minindex,
     1         local_maxindex

      character*80  local_inputfile

      character*80  local_line,
     1              local_line2

      logical   determine_size,llt0,langle

      call how_long1(local_dfl, local_inputfile)
      write(6,*) '%%info: reading: ',local_inputfile(1:local_dfl)

c Some defaults

      determine_size = .false.
      local_minindex = 1
      local_maxindex = local_num_of_channels
      
c Read information for bank from instrument parameter file

      call parm_read(iparm_file,local_bank_no,local_difc,
     1               local_difa,local_tzero,
     1               local_l0,local_l1,local_twotht,
     1               local_print_level,local_bank_id,n_banks)

c Calculate some length

      local_dist = local_l0+local_l1
      local_xf   = local_l1/local_dist

c Read data for bank

      call data_read(local_inputfile, local_num_of_channels,
     1               local_tof, local_data, local_data_err, 
     1               local_monitor, local_bank_no, local_bank_id,
     1               local_print_level)

c Apply here multiplicative and additive corrections.

      do i=1,local_num_of_channels
         local_data(i)=local_weight*local_data(i)
         local_data(i)=local_mult_data*local_data(i)+local_add_to_data
c
         local_data_err(i)=local_weight**2*local_data_err(i)**2
         local_data_err(i)=local_mult_data*local_data_err(i)
      enddo

c Only pass the real data to gsasprep and its subprograms, so remove the
c artificial zeros. This is only done for DATA otherwise DATA and
c background might not match !

c Find out where the data begins and ends by scanning the data for the
c first non-zero value (with non-zero error). First find out the lower
c boundary by scanning the array starting from index=1:

      if (determine_size) then
        local_minindex = 1
        do j=1,local_num_of_channels
           if ((local_data(j) .eq. 0.0) .and. 
     1          (local_data_err(j) .eq. 0.0)) then
              local_minindex = local_minindex+1
           else
              goto 120
           endif
        enddo
      endif

c and now the upper boundary by scanning the array from the highest
c index backwards until it hits non-zero data:

 120  continue
      if (determine_size) then
        local_maxindex = local_num_of_channels
        do j=local_num_of_channels,1,-1
           if ((local_data(j) .eq. 0.0) .and. 
     1          (local_data_err(j) .eq. 0.0)) then
              local_maxindex = local_maxindex-1
           else
              goto 130
           endif
        enddo
      endif

 130  continue
      if (local_maxindex .eq. 0 .and. determine_size) then
         write(6, *), '%%warn: Bank ',local_bank_no,
     1        ' contains no data and should be skipped'
         
c---now determine the length of the real data array:

      else
ctep         local_num_of_channels=local_maxindex-local_minindex+1

c---construct the real data by using a temporary array

         do i=1,local_num_of_channels
            local_temp(i)=local_tof(local_minindex+i-1)
            local_tof(i) = local_temp(i)
         enddo

         do i=1,local_num_of_channels
            local_temp(i)=local_data(local_minindex+i-1)
            local_data(i)=local_temp(i)
         enddo
            
         do i=1,local_num_of_channels
            local_temp(i)=local_data_err(local_minindex+i-1)
            local_data_err(i)=local_temp(i)
         enddo
            
      endif
      return
      end
c------------------------------------------------------------------------

      subroutine data_read(local_inputfile, local_num_of_channels,
     1                     local_tof, local_data, local_data_err, 
     1                     local_monitor,local_bank_no,local_bank_id,
     1                     local_print_level)
c------------------------------------------------------------------------
c Purpose: Read data for given Bank
c------------------------------------------------------------------------

       include 'parameters.inc'

       character*(*) local_inputfile
       real          local_tof(n_chn)
       real          local_data(n_chn)
       real          local_data_err(n_chn)
       real          local_monitor
       integer       local_bank_id(n_banks)
       integer       local_num_of_channels
       integer       local_bank_no
       integer       local_print_level
c
       character*80  line,bank_line
       integer       tmap(3,n_chn),ntmax
       integer       ll,ib,nc,nr
       logical       lmon,ltmap,lbank
c
       lmon=.false.
       ltmap=.false.
       lbank=.false.
c
       open(unit=1,file=local_inputfile,status="old",err=999)
c
c Find monitor count
c
10     continue
         read(1,'(a)',end=500,err=998) line
         call how_long1(ll,line)
         if (line(1:8).eq.'Monitor:' .or.
     1       line(1:8).eq.'MONITOR:'     ) then
           read(line(9:ll),*,err=998) local_monitor
           lmon=.true.
         endif
c
c HDFBIN normalizes by monitor, so in this case local_monitor=1
c
         if (line(1:14).eq.'#monitor_scale') then
           local_monitor=1.0
           lmon=.true.
         endif
       if (.not.lmon) goto 10
c
c Now try to find the bank we are looking for
c
       rewind(1)
       scal=1.0
20     continue
         read(1,'(a)',end=510,err=998) line
         call how_long1(ll,line)
         if (line(1:15).eq.'#scale_factor =') then
           read(line(16:ll),*,err=998) scal
         endif
         if (line(1:4).eq.'BANK') then
           itmap=index(line(5:ll),'TIME_MAP')
           icons=index(line(5:ll),'CONST')
           ilog =index(line(5:ll),'SLOG')

c Type TIME_MAP

           if (itmap.gt.0) then
             read(line(5:itmap+3),*,err=998) ib,nc,nr
             lbank=(ib.eq.local_bank_id(local_bank_no))
             if (lbank) then
               bank_line=line
               read(line(itmap+12:ll),*,err=998) i_tmap
               if (local_print_level.ge.2) then
                 write(*,*)'%%info: TOF binning TIME_MAP number ',i_tmap
                 write(*,*)'%%info: Number of channels :',nc
                 write(*,*)'%%info: Number of records  :',nr
               endif
             endif

c Type CONST

           elseif (icons.gt.0) then
             read(line(5:icons+3),*,err=998) ib,nc,nr
             lbank=(ib.eq.local_bank_id(local_bank_no))
             if (lbank) then
               bank_line=line
               read(line(icons+9:ll),*,err=998) tof_min,tof_d
               if (local_print_level.ge.2) then
                 write(*,*)'%%info: TOF binning type CONST found'
                 write(*,*)'%%info: TOF minimum, step  :',tof_min,tof_d
                 write(*,*)'%%info: Number of channels :',nc
                 write(*,*)'%%info: Number of records  :',nr
               endif
             endif
c Type SLOG

           elseif (ilog.gt.0) then
             read(line(5:icons+3),*,err=998) ib,nc,nr
             lbank=(ib.eq.local_bank_id(local_bank_no))
             if (lbank) then
               bank_line=line
               read(line(icons+9:ll),*,err=998) gtmin,gtmax,gtlog
               if (local_print_level.ge.2) then
                 write(*,*)'%%info: TOF binning type SLOG found'
                 write(*,*)'%%info: TOF min, max, log  :',
     &	                   gtmin,gtmax,gtlog
                 write(*,*)'%%info: Number of channels :',nc
                 write(*,*)'%%info: Number of records  :',nr
               endif
             endif

c UNKNOWN

           else
             stop '%%%severe: Unsupported TOF binning type found'
           endif
         endif
       if (.not.lbank) goto 20
c
c Now we can read the data
c
       if (scal.ne.1.0) then
         write(*,*)'%%info: Rescaling bank by  :',scal
       endif
c - STD
       if (index(bank_line,'STD').ne.0) then
         read(1,'(10(i2,f6.0))') (idummy,local_data(i),i=1,nc)
         do i=1,nc
           local_data_err(i)=sqrt(local_data(i))
           local_data(i)=scal*local_data(i)
           local_data_err(i)=scal*local_data_err(i)
         enddo
         local_num_of_channels = nc
         if (local_print_level.ge.2) then
           write(*,*)'%%info: Date type STD found'
         endif
c - ESD
       elseif (index(bank_line,'ESD').ne.0) then
         read(1,'(10f8))') (local_data(i),local_data_err(i),i=1,nc)
         do i=1,nc
           local_data(i)=scal*local_data(i)
           local_data_err(i)=scal*local_data_err(i)
         enddo
         local_num_of_channels = nc
         if (local_print_level.ge.2) then
           write(*,*)'%%info: Date type ESD found'
         endif
c - FXYE
       elseif (index(bank_line,'FXYE').ne.0) then
         do i=1,nc
           read(1,*) local_tof(i),local_data(i),
     &	             local_data_err(i)
           local_data(i)=scal*local_data(i)
           local_data_err(i)=scal*local_data_err(i)
         enddo
         local_num_of_channels = nc
         if (local_print_level.ge.2) then
           write(*,*)'%%info: Date type FXYE found'
         endif
c - DEFAULT (STD)
       else
         write(*,*) '%%warn: No data file type entry found - assume STD'
         read(1,'(10(i2,f6.0))') (idummy,local_data(i),i=1,nc)
         do i=1,nc
           local_data_err(i)=sqrt(local_data(i))
           local_data(i)=scal*local_data(i)
           local_data_err(i)=scal*local_data_err(i)
         enddo
         local_num_of_channels = nc
       endif      
 
c Now we generate the TOF entry if needed 
c ICONS

       if (icons.gt.0) then
         if (local_print_level.ge.2) then
            write(*,*)'%%info: Generating TIME_MAP for CONST binning'
         endif
         do i=1,local_num_of_channels
           local_tof(i)      = tof_min + (i-1)*tof_d
           local_data(i)     = local_data(i)/tof_d
           local_data_err(i) = local_data_err(i)/tof_d
         enddo

c TIME_MAP

       elseif (itmap.gt.0) then

c we read the first time_map anyway (due to a bug in FSTBUSBIN)
c then try finding the matching one (HIPPO will have mult. TIME_MAPs)

         rewind (1)
50       continue
           read(1,'(a)',end=60,err=998) line
           call how_long1(ll,line)
           if (line(1:8).eq.'TIME_MAP') then
             lt=index(line(9:ll),'TIME_MAP')+8
             read(line(9:lt),*,err=998) it_no,it_nval,it_nrec
             read(line(lt+9:ll),*,err=998) clckwdt

             maxmapu=(it_nval-1)/3
             read(1,'(10i8)',err=998,end=998) 
     1           ((tmap(j,i),j=1,3),i=1,maxmapu),ntmax
             ltmap=.true.
           endif
         if (.not.ltmap .or. it_no.ne.i_tmap) goto 50
60       continue

         if (ltmap) then
           if (local_print_level.ge.2) then
             write(*,*)'%%info: Found TIME_MAP in data file'
             write(*,*)'%%info: TIME_MAP no, nval, nrec :',
     1                  it_no,it_nval,it_nrec
             write(*,*)'%%info: TIME_MAP clock width    :',clckwdt
           endif
           if (it_no.ne.i_tmap) then
             write(*,*) 
     1            '%%warn: No matching TIME_MAP found, taking first one'
           endif
         else
           stop '%%%severe: No TIME_MAP entry found'
         endif

c Now generate the tof entry from the tmap array

         maxmapu=(it_nval-1)/3
         
         iq=0
         do i=1,maxmapu-1
           itmap3i = tmap(3,i)
            tmap3i = tmap(3,i)
           itmap2i = tmap(2,i)
           itmap1i = tmap(1,i)
           itmap1iplus1 = tmap(1,i+1)
           do j = itmap1i,itmap1iplus1-1
             iq = iq + 1
             local_tof(iq) = itmap2i + itmap3i*(j-itmap1i) +  tmap3i/2.0
             local_data(iq) = local_data(iq)/itmap3i
             local_data_err(iq) = local_data_err(iq)/itmap3i
           enddo
         enddo

         itmap3i = tmap(3,maxmapu)
          tmap3i = tmap(3,maxmapu)
         itmap2i = tmap(2,maxmapu)
         itmap1i = tmap(1,maxmapu)
         imax = (ntmax-itmap2i)/itmap3i + itmap1i
         do j = itmap1i,imax
           iq = iq + 1
           local_tof(iq) = itmap2i + itmap3i*(j-itmap1i) +  tmap3i/2.0
           local_data(iq) = local_data(iq)/itmap3i
           local_data_err(iq) = local_data_err(iq)/itmap3i
         enddo
         onetick = clckwdt/1000.
         do iq = 1,imax
           local_tof(iq) = local_tof(iq)*onetick
         enddo
       endif
c
       close(1)
       return
c
500    stop '%%%severe: File contains no MONITOR entry'
510    stop '%%%severe: Requested bank not found in file'
998    stop '%%%severe: Error reading data file'
999    stop '%%%severe: Error opening data file'
       end
c------------------------------------------------------------------------
 
      subroutine uGsasTofToLambda(local_tof,local_lambda,local_num_chan,
     1                            local_twotht,difc,difa,tzero)
c------------------------------------------------------------------------
c Purpose: Convert from TOF to wavelength
c------------------------------------------------------------------------

      parameter (pi=3.141592654)

      include 'parameters.inc'

      real    local_tof(n_chn)
      real    local_lambda(n_chn)
      real    local_twotht
      real*8  difc,difa,tzero,tsint
      real*8  secondterm,d,t
      integer local_num_chan

c Use d = [-DIFC +/- sqrt(DIFC**2 - 4*DIFA*(TOF-TZERO))]/2*DIFA and
c     l = 2d sin(theta)

      tsint = 2.*sin(abs(local_twotht*pi)/360.)

c In case DIFA is 0.0 use d=(T-T0)/DIFC

      if (difa.ne.0.0) then
        do j=1,local_num_chan
          t = local_tof(j) - tzero
          secondterm = dsqrt(4.0*difa*t + difc*difc)
          d = (-difc + secondterm)/2./difa
          local_lambda(j) = tsint*d
        enddo
      else
        do j=1,local_num_chan
          d = (local_tof(j) - tzero)/difc
          local_lambda(j) = tsint*d
        enddo
      endif

      return
      end
c------------------------------------------------------------------------
 
      subroutine parm_read(parmnam,nbank,difc,difa,tzero,
     1                     el0,el1,twotheta,plvl,bankid,nb)
c------------------------------------------------------------------------
c Purpose: Read information from instrument parameter file
c------------------------------------------------------------------------

        character*(*)  parmnam
        character*80   line
        character*6    search
        integer        bankid(nb)
        integer        nbank,plvl
        real           el0,el1,twotheta
        real*8         difc,difa,tzero

        open(unit=12,file=parmnam,status='old',err=999)

c Find primary flight path

10      read (12,'(a)',err=998,end=998) line
        if (line(7:12) .ne. 'FPATH1') goto 10
        call how_long1(ll,line)
        read (line(13:ll),*,err=998,end=998), el0

c Find diffractometer constants for given bank

20      read (12,'(4x,i2,a6)',err=998,end=997) ibank, search
        if (search .ne. ' ICONS') goto 20
        if (ibank .ne. bankid(nbank)) goto 20
        backspace(12)
        read (12,'(a)',err=998,end=998) line
        call how_long1(ll,line)
        read (line(13:ll),*,err=998,end=998) difc,difa,tzero

30      read (12,'(4x,i2,a6)',err=998,end=998) ibank, search
        if (search .ne. 'BNKPAR') goto 30
        backspace(12)
        read (12,'(a)',err=998,end=998) line
        call how_long1(ll,line)
        read (line(13:ll),*,err=998,end=998) el1,twotheta

        if (plvl.ge.2) then
          write (*,*) '%%info: Diffract. constants used for bank',
     1          nbank
          write (*,*) '        TWOTHETA, L1 : ',twotheta,el1
          write (*,*) '        DIFC         : ',difc
          write (*,*) '        DIFA         : ',difa
          write (*,*) '        TZERO        : ',tzero
        endif

        close(12)
        return
c
997     stop '%%%severe: Required bank entry not found in iparm. file'
998     stop '%%%severe: Error reading instrument parameter file'
999     stop '%%%severe: Error opening instrument parameter file'
        end

