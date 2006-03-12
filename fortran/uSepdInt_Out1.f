      subroutine uSepdInt_OutOpen1(io,local_out_file, local_user_name,
     1     local_run_title, local_num_grps_to_go)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Writes header of old-fashioned .int-file. 
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      character*80   local_out_file,local_user_name,
     1               local_run_title

      integer        local_dfl,local_num_grps_to_go,io


      call how_long1(local_dfl, local_out_file)

      write(6, *) '%%info: output is being written on ', 
     1     local_out_file(1:local_dfl)//'.int'
      open(unit=io, status = 'unknown', file=local_out_file
     1    (1:local_dfl)//'.int', form='unformatted', 
     1     access='sequential')
      write(io) local_out_file(1:4), local_user_name, 
     1          local_run_title, local_num_grps_to_go

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine uSepdInt_Out1(io, local_bank, local_twotht,
     1     local_dist, local_xf, local_num_of_channels, local_t_step,
     1     local_lambda, local_data, local_data_err)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Writes an old-fashioned .int-file. Hopefully one day, this
C can be replaced by uKuplotOut1 or uSepdNormOut1.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      integer        local_bank,
     1               local_num_of_channels

      real           local_twotht,
     1               local_dist,
     1               local_xf,
     1               local_t_step,
     1               local_lambda(n_chn),
     1               local_data(n_chn),
     1               local_data_err(n_chn)


      logical        local_exist

      local_xf=(local_dist-local_xf)/local_dist

      write(io) local_bank, local_twotht, local_dist, local_xf,
     1     local_num_of_channels, local_t_step, 7.
      do i=1, local_num_of_channels
         write(io) local_lambda(i), local_data(i), local_data_err(i)
      enddo

      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine uSepdInt_OutClose1(io)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Close output file ..
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      write(6, *) '%%info: Closing output file '
      close(io)

      return
      end
