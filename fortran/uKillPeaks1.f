      subroutine uKillPeaks1(local_data, local_data_err, 
     1     local_data_Q, num_of_channels, print_level, threshold) 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Kills peaks in the data. The first derivative is calculated
C using the Savitzky-Golay filter. Between Q=1 A^-1 and Q=15 A^-1
C peaks are searched. If peaks are found then the intensity between
C near points left and right of the peak is linearly interpolated.
C So is the error...
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      include 'parameters.inc'

      real     local_data(n_chn),
     1         local_data_err(n_chn),
     1         local_data_Q(n_chn),
     1         local_swap_data(n_chn),
     1         local_swap_Q(n_chn),
     1         local_swap_err(n_chn),
     1         coeff(n_chn),
     1         swap_coeff(n_chn),
     1         local_data1(n_chn),
     1         local_data1_err(n_chn),
     1         local_average,
     1         local_fluct,
     1         local_diff(n_chn),
     1         local_diff1,
     1         local_step,
     1         sigmaleft,
     1         sigmaright,
     1         threshold,
     1         percent

      integer  num_of_channels,
     1         left_points1,
     1         right_points1,
     1         no_of_points1,
     1         points,
     1         order_of_polynom1,
     1         order_of_polynom2,
     1         index,
     1         left_points2,
     1         right_points2,
     1         local_isg,
     1         leftindex,
     1         rightindex,
     1         lowerindex,
     1         upperindex,
     1         print_level,
     1         total_killed_points,
     1         killed_points

      character local_out_file*80


      if (threshold .eq. 0.0) then
         write(6, *) '%%info: Full smoothing and peak removal'
      endif

      if (threshold .lt. 0.0) then
         write(6, *) '%%info: Peak removal deselected by user.'
         return
      endif

      if (threshold .gt. 0.0) then
         write(6, *) '%%info: Removing peaks'
      endif

C Initalize some basic values for the Savitzky-Golay filter.

      order_of_polynom1 = 4
      left_points1 = nint(0.025*float(num_of_channels))
      right_points1 = left_points1

C Initialize the smoothed array data1.

      do i=1, num_of_channels
         local_data1(i) = 0.0
         local_data1_err(i) = 0.0
         local_swap_Q(i) = 0.0
         local_swap_data(i) = 0.0
         local_swap_err(i) = 0.0
         local_diff(i) = 0.0
      enddo

C Reverse the arrays first

      do i=1, num_of_channels
         local_swap_Q(i) = local_data_Q(num_of_channels-i+1)
         local_swap_data(i) = local_data(num_of_channels-i+1)
         local_swap_err(i) = local_data_err(num_of_channels-i+1)
      enddo

      do i=1, num_of_channels
         local_data_Q(i) = local_swap_Q(i)
         local_data(i) = local_swap_data(i)
         local_data_err(i) = local_swap_err(i)
      enddo

      no_of_points1 = left_points1+right_points1+1

C Check whether the parameters for the Savitzky-Golay filter are
C valid.

      if ((left_points1 .lt. 0) .or. (right_points1 .lt. 0) .or.
     1     (order_of_polynom1 .lt. 0)) then
         write(6, *) '%%warn: negative value Savitzky-Golay input'
         stop
      endif

      if (order_of_polynom1 .gt. 6) then
         write(6, *) '%%warn: maximum order of polynom is 6.'
         stop
      endif

      if ((left_points1+right_points1) .lt. order_of_polynom1) then
         write(6, *) '%%warn: the sum of number of points > order !'
         stop
      endif

      if (no_of_points1 .gt. num_of_channels) then
         write(6, *) '%%warn: filter is > number of data points.'
         stop
      endif

C If you are at the beginning of the data there are no points to
C the left. Do not smooth the first point at all. Move to the next
C point and increase the size of the filter (meaning number of points
C left and right of the point to be smoothed) such that the filter is
C always symmetrical. Note: the coefficients in the Savitzky-Golay filter
C are c0 is in c(1), c-1 is in c(2), c-2 is in c(3) and so on. If np is
C the length of the smoothing filter, i.e. left_points + right_points +1
C then  c(1) is at c(np), c2 is at c(np-1) etc. Well, that's the way it is...


      local_data1(1) = local_data(1)
      local_data1_err(1) = local_data_err(1)
      local_data1(num_of_channels) = local_data(num_of_channels)
      local_data1_err(num_of_channels) = local_data_err(num_of_channels)
      do i=2, left_points1
         left_points2 = i-1
         if (order_of_polynom1 .gt. 2*left_points2) then
            order_of_polynom2 = 2*left_points2
         else
            order_of_polynom2 = order_of_polynom1
         endif
         points = 2*left_points2+1
         call savgol(coeff, points, left_points2, 
     1        left_points2, 0, order_of_polynom2)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, left_points2+1
            swap_coeff(k) = coeff(left_points2+2-k)
         enddo
         do k=1, left_points2+1
            coeff(k) = swap_coeff(k)
         enddo
         do k=1, left_points2
            swap_coeff(k) = coeff(2*left_points2+2-k)
         enddo
         do k=1, left_points2
            coeff(k+left_points2+1) = swap_coeff(k)
         enddo
C Smooth the data now
         do k = i-left_points2, i+left_points2
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coeff(k)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coeff(k)**2
         enddo
         local_data1_err(i) = sqrt(local_data1_err(i))
      enddo

C If you are close to the end of the data there may be less points to
C the right then you want. In this case, do something similar like before.

      do i=num_of_channels-right_points1+1, num_of_channels-1
         right_points2 = num_of_channels-i
         if (order_of_polynom1 .gt. 2*right_points2) then
            order_of_polynom2 = 2*right_points2
         else
            order_of_polynom2 = order_of_polynom1
         endif
         points = 2*right_points2+1
         call savgol(coeff, points, right_points2, 
     1        right_points2, 0, order_of_polynom2)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, right_points2+1
            swap_coeff(k) = coeff(right_points2+2-k)
         enddo
         do k=1, right_points2+1
            coeff(k) = swap_coeff(k)
         enddo
         do k=1, right_points2
            swap_coeff(k) = coeff(2*right_points2+2-k)
         enddo
         do k=1, right_points2
            coeff(k+right_points2+1) = swap_coeff(k)
         enddo
C Smooth the data now
         do k = i-right_points2,i+right_points2
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coeff(right_points2+k-i+1)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coeff(right_points2+k-i+1)**2
         enddo
         local_data1_err(i) = sqrt(local_data1_err(i))
      enddo

C For all other cases the full Savitzky-Golay filtering can be
C performed.

      call savgol(coeff, no_of_points1, left_points1,
     1     right_points1, 0, order_of_polynom1)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, left_points1+1
            swap_coeff(k) = coeff(left_points1+2-k)
         enddo
         do k=1, left_points1+1
            coeff(k) = swap_coeff(k)
         enddo
         do k=1, right_points1
            swap_coeff(k) = coeff(left_points1+right_points1+2-k)
         enddo
         do k=1, right_points1
            coeff(k+left_points1+1) = swap_coeff(k)
         enddo
      do i=left_points1+1,num_of_channels-right_points1
C Smooth the data now
         do k=i-left_points1, i+right_points1
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coeff(left_points1+k-i+1)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coeff(left_points1+k-i+1)**2
         enddo
      enddo

C Ok, the data are now smoothed. Search now for peaks or not...

C If the threshold was 0.0 then replace the original data with the
C fully smoothed curve and reverse the array again (very important!)

      if (threshold .eq. 0.0) then
         do i=1, num_of_channels
            local_data(i) = local_data1(i)
         enddo
         do i=1, num_of_channels
            local_swap_Q(i) = local_data_Q(num_of_channels-i+1)
            local_swap_data(i) = local_data(num_of_channels-i+1)
            local_swap_err(i) = local_data_err(num_of_channels-i+1)
         enddo
         
         do i=1, num_of_channels
            local_data_Q(i) = local_swap_Q(i)
            local_data(i) = local_swap_data(i)
            local_data_err(i) = local_swap_err(i)
         enddo

         return
      endif

C Subtract now the smoothed data from the original data.

      do i=1, num_of_channels
         local_diff(i) = local_data(i) - local_data1(i)
      enddo

C Make an intermediate plot-file to see how it looks like...

C      call uKuplotOut1(31, local_out_file,".deriv",
C     1     local_data_Q, local_diff, local_data1_err, 
C     1     num_of_channels, local_isg, 1.)

C Ok, we have the difference curve, calculate now the average
C and average fluctuation.

      local_average = 0.0
      local_fluct = 0.0
      no_of_averaged_points = 0
      do i=1, num_of_channels-1
         if ((local_data_Q(i) .gt. 2.5) .and. 
     1        (local_data_Q(i) .lt. 14.0)) then
            local_average = local_average + local_diff(i)
            no_of_averaged_points = no_of_averaged_points + 1
         endif
      enddo

      local_average = local_average/no_of_averaged_points

      do i=1, num_of_channels-1
         if ((local_data_Q(i) .gt. 2.5) .and. 
     1        (local_data_Q(i) .lt. 14.0)) then
            local_fluct = local_fluct + abs(local_diff(i) - 
     1           local_average)
         endif
      enddo

      local_fluct = local_fluct/no_of_averaged_points

C Search now for peaks and kill them instantly. A peak has usually first
C a positive slope on the rising edge and then a negative slope on the
C falling edge.

      total_killed_points = 0
      do i=2, num_of_channels-1
            local_diff1 = abs(local_diff(i) - local_average)
C Find a suspicious point
            if (local_diff1 .gt. (threshold*local_fluct)) then
               call peak_and_kill(i, local_data_Q, local_diff,
     1              local_data, local_data1, num_of_channels,
     1              local_average, local_fluct, killed_points,
     1              print_level)
               total_killed_points = total_killed_points + killed_points
            endif
      enddo

               
      
         
C Reverse the arrays again

      do i=1, num_of_channels
         local_swap_Q(i) = local_data_Q(num_of_channels-i+1)
         local_swap_data(i) = local_data(num_of_channels-i+1)
         local_swap_err(i) = local_data_err(num_of_channels-i+1)
      enddo

      do i=1, num_of_channels
         local_data_Q(i) = local_swap_Q(i)
         local_data(i) = local_swap_data(i)
         local_data_err(i) = local_swap_err(i)
      enddo

      if (print_level .ge. 1) then
         percent = float(total_killed_points)/
     1        (float(num_of_channels))*100.
         write(6, *) '%%info: ', total_killed_points, ' out of ',
     1               num_of_channels, ' points were removed (',
     1               percent, '%).'
      endif

      return
      end


C-----------------------------------------------------------------
C
C subroutine peak_and_kill
C
C-----------------------------------------------------------------
      subroutine peak_and_kill(index, data_Q, data_diff, data, data1,
     1     num_of_channels, average, fluctuation, killed, 
     1     print_level)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Replaces a peak by the smoothed data.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      real data_Q(n_chn),
     1     data1(n_chn),
     1     data(n_chn),
     1     data_diff(n_chn),
     1     sigmaleft,
     1     sigmaright,
     1     average,
     1     fluctuation

      integer index,
     1        num_of_channels,
     1        maxindex,
     1        leftindex,
     1        rightindex,
     1        lowerindex,
     1        upperindex,
     1        killed,
     1        print_level


      maxindex = index

C Find the maximum of the peak. But do not look too far.

      if (maxindex .ge. num_of_channels) goto 100

      do i = maxindex+1, num_of_channels
         if (((data_Q(i) - data_Q(index)) .ge. 0.1) .or. 
     1        (i .eq. num_of_channels)) then
            goto 10
         endif
         if (abs(data_diff(i)) .gt. abs(data_diff(maxindex))) then
            maxindex = i
         endif
      enddo

C Ok found the maximum. Determine the half-width at half maximum (HWHM)
C on both sides of the peak. But do not look too far away from the maximum. 
C The index of the point that determines the HWHM on the left side of the
C peak is in leftindex. The index of the corresponding point on the right
C side is in rightindex.
 
 10   leftindex = maxindex
      if (data_Q(leftindex) - data_Q(1) .lt. 0.1) then
         goto 100
      endif
      do i = maxindex-1, 1,-1
         if (leftindex .eq. 1) then
            goto 100
         endif
         if ((abs(data_Q(i) - data_Q(maxindex)) .ge. 0.1) .and. 
     1        (leftindex .eq. maxindex)) then
            goto 100
C     Found no point with 50% intensity nearby. So that was probably 
C     not a peak.
         endif
         if ((abs(data_diff(i)) .lt. 
     1        abs(0.5*data_diff(maxindex))) .and. 
     1        (abs(data_Q(i) - data_Q(maxindex)) .lt. 0.1)) then
            leftindex = i
            if (leftindex .eq. 1) then
               goto 100
            endif
            goto 20
         endif
      enddo


C Found the leftindex. Determine next rightindex. Do not look to far.

 20   rightindex = maxindex
      if (rightindex .ge. num_of_channels) then
         goto 100
      endif
      do i= maxindex+1, num_of_channels
         if (rightindex .eq. num_of_channels) then
            goto 100
         endif
         if ((abs(data_Q(i) - data_Q(maxindex)) .ge. 0.1) .and.
     1        (rightindex .eq. maxindex)) then
            goto 100
         endif
         if (abs(data_diff(i)) .lt. abs(0.5*data_diff(maxindex))) 
     1        then
            rightindex = i
            if (rightindex .eq. num_of_channels) then
               goto 100
            endif
            goto 30
         endif
      enddo

C OK, found a peak and its HWHM. Now kill that peak. The peak position
C is at Qpeak = Q(maxindex). The HWHM on both sides is given by Q(maxindex)-
C Q(leftindex) and Q(rightindex)-Q(maxindex). Go now to the points at 
C Qpeak - 3*sigmaleft and Qpeak + 3*sigmaright and replace the peak by
C the smoothed data.

 30   sigmaleft = data_Q(maxindex) - data_Q(leftindex)
      sigmaright = data_Q(rightindex) - data_Q(maxindex)

C Find the two data points that are at 3.*leftsigma and 3.*rightsigma
C with respect to the peak position. Find first the lower of these two 
C points.

      lowerindex = leftindex
      if (leftindex .eq. 1) then
         goto 40
      endif
      do i = leftindex-1,1,-1
         lowerindex = i
         if (lowerindex .eq. 1) then
            goto 40
         endif
         if (data_Q(lowerindex) .lt. (data_Q(maxindex) - 
     1        3.*sigmaleft)) then
            goto 40
         endif
      enddo

 40   upperindex = rightindex
      if (rightindex .eq. num_of_channels) then
         goto 50
      endif
      do i = rightindex+1, num_of_channels
         upperindex = i
         if (upperindex .eq. num_of_channels) then
            goto 50
         endif
         if (data_Q(upperindex) .gt. (data_Q(maxindex) + 
     1        3.*sigmaright)) then
            goto 50
         endif
      enddo

C Kill here officially the peak
 50   if (print_level .gt. 1) then
         write(6, *) '%%info:Killing a peak at ', data_Q(maxindex)
      endif
      do i = lowerindex+1, upperindex-1
         data(i)= data1(i)
         data_diff(i) = average
      enddo
      killed = upperindex - lowerindex - 1

 100  return
      end
      
