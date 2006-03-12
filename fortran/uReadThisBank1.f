c---------------------------------------------------------------------------
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
c---------------------------------------------------------------------------
      subroutine uReadThisBank1(local_data_file, local_num_of_bank,
     1     local_data, local_data_err, local_add_corr, 
     1     local_mult_corr, local_num_of_channels, local_nd, 
     1     local_dangle, local_tstep, local_print_level)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine just reads the data from one bank. The bank
C and data file are specified by the user. The error array for the
C data is created and multiplicative and additive corrections to
C the data are applied as specified by the user.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      character*(*)  local_data_file
      character*80   local_line1,
     1               local_line2,
     1               local_dummy

      integer   local_histogram,
     1     local_num_of_channels,
     1     local_num_of_bank,
     1     local_num_of_lines,
     1     local_group,
     1     local_rest,
     1     local_dfl,
     1     local_print_level,
     1     local_nd
     
      real local_data(n_chn), 
     1     local_data_err(n_chn),
     1     local_add_corr(n_banks),
     1     local_mult_corr(n_banks),
     1     local_dangle,
     1     local_tstep,
     1     local_nd1


C Open the data file here

      call how_long1(local_dfl,local_data_file)
      open(23,file=local_data_file(1:local_dfl),status='old',
     1     err=999)


      write(6,*) '%%info: reading file : ',
     1     local_data_file(1:local_dfl)

C Rewind first, so you are sure to start reading from the beginning
C of the file

      rewind(unit=23)

C We are looking for this kind of line, that indicates the beginning
C of data:

      local_line2='##### Data: group, histogram, # channels'
 10   continue

C Read now line after line from the data file and compare it to 
C local_line2

      read(23, '(A)') local_line1
      do while (local_line1 .ne. local_line2)
         read(23,'(A)') local_line1
      enddo

      read(23, *) local_group, local_histogram, local_num_of_channels
      read(23,'(a80)') local_dummy

C In case you found the bank you want to read in, read it here.
C Read first all lines which contain 10 intensities per line

      if (local_group .eq. local_num_of_bank) then
         if (local_print_level .eq. 2) then
            write(6, *) '      Reading data from bank:', 
     1           local_num_of_bank
            write(6, *) '      This bank has',
     1           local_num_of_channels,' channels'
         endif
         local_num_of_lines=int(local_num_of_channels/10)
         do i=1, local_num_of_lines
            read(23, *) (local_data((i-1)*10+j), j=1, 10)
         enddo

C-If a bank has an additional line with less then 10 intensities on it, 
C read it here. The variable local_rest is just the number of intensities
C to be read from this last line.

         local_rest=int(local_num_of_channels-
     1        local_num_of_lines*10)
         if (local_rest .gt. 0) then
            read(23, *) (local_data(local_num_of_lines*10+i), 
     1           i=1, local_rest)
            goto 20
         endif
      else
         goto 10
      endif

 20   continue

C Apply additive and multiplicative corrections to the data here
C and create the error array:

      do i=1, local_num_of_channels
         local_data_err(i)=local_mult_corr(local_group)*
     1        sqrt(local_data(i))/(local_nd*local_dangle*local_tstep)
         local_data(i)=local_mult_corr(local_group)*
     1        local_data(i)/(local_nd*local_dangle*local_tstep)+
     1        local_add_corr(local_group)
      enddo

      close(23)

      return

 999  stop '%%%severe: error opening  data file'
      end
