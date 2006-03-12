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
      subroutine uNormalizeThisBank1(local_data, local_data_err, 
     1     local_norm_factor, local_norm_factor_err,
     1     local_num_of_channels)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine divides the data by the integrated monitor.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'


      integer local_num_of_channels
     
      real    local_data(n_chn),
     1        local_data_err(n_chn),
     1        local_norm_factor,
     1        local_norm_factor_err

      write(6,*) '%%info: Normalizing this bank '

C-Ok, let's normalize the data here

      do i=1, local_num_of_channels
         local_data(i)=local_data(i)/local_norm_factor
      enddo

C-Ok, let's do proper error propagation:

      if (local_norm_factor_err.gt.0.0) then
        do i=1, local_num_of_channels
           local_data_err(i)=sqrt((local_data_err(i)/
     1          local_norm_factor)**2+
     1          ((local_data(i)*local_norm_factor_err)/
     1          (local_norm_factor**2))**2)
        enddo
      endif

      return

      end
