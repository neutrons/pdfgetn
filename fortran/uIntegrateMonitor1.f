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
      subroutine uIntegrateMonitor1(local_data, local_data_err, 
     1     local_num_of_channels, local_result, local_result_err,
     1     local_print_level)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine integrates the data from the monitor.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      integer   local_num_of_channels,
     1          local_print_level
     
      real      local_data(n_chn),
     1          local_data_err(n_chn),
     1          local_result,
     1          local_result_err


      write(6,*) '%%info: integrating monitor 1 '

C-Initialization

      local_result=0
      local_result_err=0

C-Sum up the monitor here

      do i=1, local_num_of_channels
         local_result=local_result+local_data(i)*0.00001
         local_result_err=local_result_err+
     1        (local_data_err(i)*0.00001)**2
      enddo
      local_result_err = sqrt(local_result_err)

      if (local_print_level .eq. 2) then
         write(6, *) '      Integrated monitor is: ' 
         write(6, *) local_result, ' (',local_result_err,')'
      endif

      return

      end
