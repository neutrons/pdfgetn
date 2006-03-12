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
      subroutine uSubtractData1(local_data, local_data_err, 
     1     local_bkg_data, local_bkg_data_err,
     1     local_num_of_channels)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine subtracts the background data from the sample
C data (sample = your sample, empty can or vanadium rod).
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'


      integer local_num_of_channels
     
      real    local_data(n_chn),
     1        local_data_err(n_chn),
     1        local_bkg_data(n_chn),
     1        local_bkg_data_err(n_chn)

      write(6,*) '%%info: subtracting background from sample '


C-Subtract the data here and make a proper error propagation:

      do i=1, local_num_of_channels
         local_data_err(i)=sqrt(local_data_err(i)**2+
     1        local_bkg_data_err(i)**2)
         local_data(i)=local_data(i)-local_bkg_data(i)
      enddo

      return

      end
