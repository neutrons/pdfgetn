      subroutine uArielGetNumberOfBanks(local_inputfile, local_n_banks)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Run through data file and determine number of banks.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      character*(*) local_inputfile
      character*80  local_line
      integer       local_n_banks

      call how_long1(local_dfl, local_inputfile)
      open(unit=1,file=local_inputfile(1:local_dfl),status="old")

      local_n_banks=0
10    continue
        read (1,'(a)', end=20) local_line
        if (local_line(1:2).eq.'#S') local_n_banks=local_n_banks+1
      goto 10
20    continue

      write(6,*) '%%info: Number of banks found : ',local_n_banks
      close(1)
      end
C----------------------------------------------------------------------
      subroutine uArielReadBank1(local_inputfile, local_bank_no,
     1     local_lambda, local_data, local_data_err, local_add_to_data,
     1     local_mult_data, local_num_of_channels, 
     1     local_twotht, local_dist, local_xf, local_print_level,
     1     local_minindex, local_maxindex)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Reads data from one bank from a ARIEL-data file
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      parameter (eps=1e-19)

      include 'parameters.inc'

      real     local_lambda(n_chn),
     1         local_data(n_chn),
     1         local_data_err(n_chn),
     1         local_temp(n_chn),
     1         local_twotht,
     1         local_dist,
     1         local_xf,
     1         local_l0,
     1         local_l1,
     1	       local_lstep,
     1         local_fpath,
     1         local_add_to_data,
     1         local_mult_data

      integer  local_bank_no,
     1         local_group,
     1         local_nlines,
     1         local_print_level,
     1         local_dfl,
     1         local_dfi,
     1         local_num_of_channels,
     1         local_minindex,
     1         local_maxindex

      character*80  local_inputfile

      character*80  local_line,
     1              local_line2

      logical   determine_size,llt0,langle

      call how_long1(local_dfl, local_inputfile)

      write(6,*) '%%info: reading: ',local_inputfile(1:local_dfl)

      open(unit=1,file=local_inputfile(1:local_dfl),status="old")
      rewind(1)

C---Now start to read the file. 

C search for the bank that should be read. Do this by looking for the line
C '#S' which indicates the beginning of a new bank.

      local_group = 0

c     determine_size = (local_minindex .lt. 0)
      determine_size = .false.

      local_minindex = 1
      local_maxindex = local_num_of_channels


      do while(.true.)
         read(1, '(A)') local_line
	 if (local_line(1:23) .eq. '#C  Primary flight path') then
	    read(local_line(24:80),*) local_l0  
	 endif
         if (local_line(1:2) .eq. '#S') then
            local_group = local_group+1
         endif
         if (local_group .eq. local_bank_no) then
            goto 10
         endif
      enddo

C OK, when the bank is found read in the lambda, data and error arrays.

C First we get the bank angle ..

 10   continue

      langle = .false.
      llt0   = .false.

      do while (.true.)
         read(1, '(A)') local_line
	 if (local_line(1:3) .eq. '#P0') then
	    read(local_line(4:80),*) local_l1, local_lstep, 
     &	                             local_twotht, local_fpath
	    goto 20
         endif
      enddo
 20   continue

C Calculate some length

      local_dist = local_l0+local_l1
      local_xf   = local_l1/local_dist

C Now go to the start of the data

 30   do while (.true.)
         read(1, '(A)') local_line
         if (local_line(1:2) .eq. '#L') then
	    if (local_line(5:5) .ne. 'W') then 
	      write(*,*) '*** Units need to be wavelength ! ***'
	      stop
	    endif
            goto 40
         endif
      enddo
 40   continue

C Now read the data 

      i=0
 50   continue
        i=i+1
        read (1,*,end=60,err=60) local_lambda(i),local_data(i),
     1                           local_data_err(i)
      goto 50
 60   continue
      local_num_of_channels=i

C---Apply here multiplicative and additive corrections.

      do i=1,local_num_of_channels
         local_data(i)=local_mult_data*local_data(i)+local_add_to_data
      enddo

C---Only pass the real data to arielprep and its subprograms, so remove the
C   artificial zeros. This is only done for DATA otherwise DATA and
C   background might not match !

C---Find out where the data begins and ends by scanning the data for the
C   first non-zero value (with non-zero error). First find out the lower
C   boundary by scanning the array starting from index=1:

      if (determine_size) then
        local_minindex = 1
        do j=1,local_num_of_channels
           if ((local_data(j) .lt. eps) .and. 
     1          (local_data_err(j) .lt. eps)) then
              local_minindex = local_minindex+1
           else
              goto 120
           endif
        enddo
      endif


C...and now the upper boundary by scanning the array from the highest
C   index backwards until it hits non-zero data:


 120  continue
      if (determine_size) then
        local_maxindex = local_num_of_channels
        do j=local_num_of_channels,1,-1
           if ((local_data(j) .lt. eps) .and. 
     1          (local_data_err(j) .lt. eps)) then
              local_maxindex = local_maxindex-1
           else
              goto 130
           endif
        enddo
      endif

 130  continue
      if (local_maxindex .eq. 0) then
         write(6, *), '%%warn: Bank ',local_bank_no,
     1        ' contains no data and should be skipped'
         
c---now determine the length of the real data array:

      else
ctep         local_num_of_channels=local_maxindex-local_minindex+1

c---construct the real data by using a temporary array

         do i=1,local_num_of_channels
            local_temp(i)=local_lambda(local_minindex+i-1)
            local_lambda(i) = local_temp(i)
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

      close(1)
      return
      end
