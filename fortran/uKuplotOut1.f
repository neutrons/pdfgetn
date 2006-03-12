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
      subroutine uKuplotOut1(local_unit, local_out_file, local_ending,
     1     local_lambda, local_data, local_data_err, 
     1     local_num_of_channels, local_isg, local_twotht)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine writes data from a single bank to a kuplot-
C compatible output file. The output-file format is similar to
C a SPEC-file.
C
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      character*(*) local_out_file,
     1              local_ending

      character*80  local_run_title,
     1              local_fields,
     1              local_dummy1,
     1              local_filename

      integer   local_num_of_channels,
     1          local_isg,
     1          local_unit,
     1          local_dfl,
     1          local_dfo
     
      real   local_lambda(n_chn),
     1       local_data(n_chn),
     1       local_data_err(n_chn),
     1       local_dummy_xerr(n_chn),
     1       local_twotht

      logical local_exist


      call how_long1(local_dfl,local_out_file)
      call how_long1(local_dfo,local_ending)
C Check whether the file exists. If it does not exists open
C it as 'new'. If it exists open it as 'old' and search for
C the end.

      local_filename = local_out_file(1:local_dfl)
      local_filename = local_filename(1:local_dfl)//
     1                 local_ending(1:local_dfo)
      local_exist = .false.
      inquire(file=local_filename,exist=local_exist)
      if (local_exist .eqv. .false.) then
         open(local_unit,file=local_filename,status='new',err=999)
      else
         open(local_unit, file=local_filename,status='old')
         do while (.true.)
            read(local_unit, *, end = 10)
         enddo
      endif
 10   write(6,*) '%%info: writing intermediate graphics',
     1     ' to  file : ', local_filename(1:local_dfl+local_dfo)

C-Initialize local_dummy_xerr(n_chn):

      local_run_title ='#T Created by PDFgetN ...'
      local_fields ='#L   Q      I  Qerr  Ierr'
      local_dummy1 ='#S'
      do i=1, local_num_of_channels
         local_dummy_xerr(i)=0.0
      enddo

C-write the kuplot output file here

 100  format(A2,1X,I8,1X,A5,1X,F8.4,1X,F8.4,1X,A1,1X,A1,1X,I5)
 101  format(A27)
 102  format(A25)
      call how_long1(local_dfl, local_dummy1)
      write(local_unit, 100) local_dummy1(1:local_dfl), local_isg,
     1     ' scan', local_lambda(1), 
     1     local_lambda(local_num_of_channels), 
     1     '0', '0', local_num_of_channels
      call how_long1(local_dfl, local_fields)
      write(local_unit, 102) local_fields(1:local_dfl)
      do i=1, local_num_of_channels
         write(local_unit, *) local_lambda(i), local_data(i), 
     1        local_dummy_xerr(i), local_data_err(i)
      enddo
      write(local_unit, *) ' '
      close(local_unit)
      return

 999  stop '%%%severe: error opening KUPLOT file'

      end

