      subroutine uDetermineDataRange1(local_lambda, local_data, 
     1     local_data_err, local_num_of_channels, local_lambda_min, 
     1     local_lambda_max)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This subroutine determines the range of non-zero (and there-
C fore useful) data. It accepts the user-supplied data limits 
C local_lambda_min and local_lambda_max if useful data is contained
C within those limits. If this is not the case the user input is over-
C ridden. Otherwise you will get a divide-by-zero error in soqd.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      real     local_data(n_chn),
     1         local_data_err(n_chn),
     1         local_lambda(n_chn),
     1         local_lambda_min,
     1         local_lambda_max

      integer  local_num_of_channels,
     1         local_min_index,
     1         local_max_index

C Start here determine first the lower bound of useful data. Jump out
C of the loop if either the data is 0.0 or if the user-supplied local_
C lambda_min is encountered, whatever occurs first.

      local_min_index = 1
      local_max_index = local_num_of_channels

      do i=1, local_num_of_channels
         if (local_data_err(i) .gt. 0.0 .and.
     &	     local_data(i)     .gt. 0.0      ) then
            local_min_index = i+1
            goto 10
         endif
         if (local_lambda(i) .gt. local_lambda_min) then
            local_min_index = i
            goto 10
         endif
      enddo

 10   continue

C Check now whether the user-supplied minimum wavelength is less
C than local_lambda_min. If this is so, override the user-supplied
C minimum wavelength.

      if (local_lambda(local_min_index) .gt. local_lambda_min) then
         local_lambda_min = local_lambda(local_min_index)
      endif


C Check the data further for 0.0 starting from local_min_index. Jump
C out of the loop if you discover 0.0 data or if the user-supplied
C maximum wavelength is reached, whatever occurs first.

      do i=local_min_index, local_num_of_channels
         if (local_data_err(i) .le. 0.0 .and.
     &	     local_data(i)     .le. 0.0      ) then
            local_max_index = i-1
            goto 20
         endif
         if (local_lambda(i) .gt. local_lambda_max) then
            local_max_index = i-1
            goto 20
         endif
      enddo


 20   local_num_of_channels = local_max_index-local_min_index+1

      do i=local_min_index, local_num_of_channels
         local_lambda(i-local_min_index+1) = local_lambda(i)
         local_data(i-local_min_index+1) = local_data(i)
         local_data_err(i-local_min_index+1) = local_data_err(i)
      enddo

      return
      end
