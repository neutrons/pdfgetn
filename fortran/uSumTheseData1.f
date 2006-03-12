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
      subroutine uSumTheseData1(local_data1, local_data1_err,
     1     local_data2, local_data2_err, local_sum, local_sum_err, 
     1     local_num_of_channels)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine sums up the data from different files. The sum
C is stored in the variable local_sum and returned to the main program.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'


      integer local_num_of_channels
     
      real   local_data1(n_chn),
     1       local_data1_err(n_chn),
     1       local_data2(n_chn),
     1       local_data2_err(n_chn),
     1       local_sum_err(n_chn),
     1       local_sum(n_chn)


      write(6,*) '%%info: Summing data '

      do i=1, local_num_of_channels
         local_sum(i)=local_data1(i)+local_data2(i)
         local_sum_err(i)=sqrt(local_data1_err(i)**2+
     1        local_data2_err(i)**2)
      enddo

      return

      end

      
