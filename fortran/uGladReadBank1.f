      subroutine uGladGetNumberOfBanks(local_inputfile, local_n_banks)

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
        if (local_line.eq.'# no_label') local_n_banks=local_n_banks+1
      goto 10
20    continue

      write(6,*) ' %%info: Number of banks found : ',local_n_banks
      close(1)
      end
C----------------------------------------------------------------------
      subroutine uGladReadBank1(local_inputfile, local_bank_no,
     1     local_Q, local_data, local_data_err, local_add_to_data,
     1     local_mult_data, local_num_of_channels, 
     1     local_twotht, local_dist, local_xf, 
     1     local_source_to_sample, local_print_level,
     1     local_minindex, local_maxindex)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Reads data from one bank from a GLAD-data file that
C was converted to ASCII format using OPEN GENIE.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      parameter (eps=1e-6)

      include 'parameters.inc'

      real     local_Q(n_chn),
     1         local_data(n_chn),
     1         local_data_err(n_chn),
     1         local_temp(n_chn),
     1         local_twotht,
     1         local_l0,
     1         local_l1,
     1         local_dist,
     1         local_xf,
     1         local_add_to_data,
     1         local_mult_data,
     1         local_source_to_sample

      integer  local_bank_no,
     1         local_group,
     1         local_nlines,
     1         local_print_level,
     1         local_dfl,
     1         local_dfi,
     1         local_num_of_channels,
     1         local_minindex,
     1         local_maxindex,
     1         local_infocount

      character*80  local_inputfile

      character*80  local_line,
     1              local_line2

      logical   determine_size

      call how_long1(local_dfl, local_inputfile)

      write(6,*) '%%info: reading: ',local_inputfile(1:local_dfl)

      open(unit=1,file=local_inputfile(1:local_dfl),status="old")

      rewind(1)

C---Now start to read the file. A *.nrm file converted to an ASCII file using
C   OPEN GENIE contains 19 blocks with three subblocks per blockseparated by 
C   lines containing some other information: First subblock is intensity, 
C   second subblock is Q-scale, and third subblock is error on intensity

C search for the bank that should be read. Do this by looking for the line
C '# no_label' which indicates the beginning of a new bank.

      local_group = 0
      local_infocount = 0
      local_line2 = '# no_label'

      determine_size = (local_minindex .lt. 0)

      do while(.true.)
         read(1, '(A80)') local_line
         if (local_line .eq. local_line2) then
            local_group = local_group+1
         endif
         if (local_group .eq. local_bank_no) then
            goto 10
         endif
      enddo

C OK, when the bank is found read in the Q, data and error arrays.

C Search first the start of the data.

 10   do while (.true.)
         if (local_infocount .eq. 6) then
            goto 20
         endif
         read(1, '(A80)') local_line
C         write(6, *) local_line
         if (local_line .eq. '  "y"') then
            local_infocount = local_infocount + 1
C Found the intensity block. Read the data.
            do i=1,2
               read(1, *)
            enddo
            read (1,*) local_num_of_channels
            local_nlines=local_num_of_channels/10
            do j=1,local_nlines
               read (1,*) (local_data(j*10-9+i),i=0,9)
            enddo
            read (1,*) (local_data(local_nlines*10+i),i=1,
     1           local_num_of_channels-local_nlines*10)

C---Reverse the intensity array here otherwise fancy things are going to happen
C   later! Apply here multiplicative and additive corrections.

            do i=1,local_num_of_channels
               local_temp(local_num_of_channels-i+1)=local_data(i)
            enddo

            do i=1,local_num_of_channels
               local_data(i)=local_mult_data*local_temp(i)+
     1              local_add_to_data
            enddo
         endif

C---Array is now reversed.

         if (local_line .eq. '  "x"') then
            local_infocount = local_infocount + 1
C Found the Q block. Read it.
            do i=1,2
               read(1,*)
            enddo
            read(1, *) local_num_of_channels
            local_nlines = local_num_of_channels/10
            do j=1,local_nlines
               read (1,*) (local_Q(j*10-9+i),i=0,9)
            enddo

            read (1,*) (local_Q(local_nlines*10+i),i=1, 
     1           local_num_of_channels-local_nlines*10)

C---Reverse the Q array here otherwise fancy things are going to happen
C   later!

            do i=1,local_num_of_channels
               local_temp(local_num_of_channels-i+1) = local_Q(i)
            enddo

            do i=1,local_num_of_channels
               local_Q(i)=local_temp(i)
            enddo

C---Array is now reversed, so let's go on reading.
         endif


         if (local_line .eq. '  "l1"') then
            local_infocount = local_infocount + 1
C Found the sample-detector flight path. Read it.
            read (1, *)
            read (1,*) local_l1
         endif
         if (local_line .eq. '  "l2"') then
            local_infocount = local_infocount + 1
C Found the incident flight path. Read it.
            read (1, *)
            read (1,*) local_l0
         endif
         if (local_line .eq. '  "twotheta"') then
            local_infocount = local_infocount + 1
C Found the bank angle twotheta.
            read(1, *)
            read(1, *) local_twotht
         endif

C Look now for the beginning of the error array, marked by an "e".

         if (local_line .eq. '  "e"') then
            local_infocount = local_infocount + 1
C Found error block. Read error bars.
            do i=1,2
               read(1, *)
            enddo
            read(1,*) local_num_of_channels
            local_nlines = local_num_of_channels/10
            do j=1,local_nlines
               read (1,*) (local_data_err(j*10-9+i),i=0,9)
            enddo
            read (1,*) (local_data_err(local_nlines*10+i),i=1,
     1           local_num_of_channels-local_nlines*10)


C---Reverse the error array here otherwise fancy things are going to happen
C   later! Apply here multiplicative corrections.

            do i=1,local_num_of_channels
               local_temp(local_num_of_channels-i+1)=local_data_err(i)
            enddo

            do i=1,local_num_of_channels
               local_data_err(i)=local_mult_data*local_temp(i)
C     print *,local_Q(i),local_data(i),local_data_err(i)
            enddo
C Array is now reversed
         endif
      enddo


C---Calculate some geometric quantities

 20   local_dist = local_l0+local_l1
      local_xf = local_l1/local_dist
      local_source_to_sample = local_l0

C---Only pass the real data to gladprep and its subprograms, so remove the
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
         local_num_of_channels=local_maxindex-local_minindex+1

c---construct the real data by using a temporary array

         do i=1,local_num_of_channels
            local_temp(i)=local_Q(local_minindex+i-1)
            local_Q(i) = local_temp(i)
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
