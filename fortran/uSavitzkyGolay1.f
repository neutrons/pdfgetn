      subroutine uSavitzkyGolay1(local_data, local_data_err, 
     1     num_of_channels, left_points, right_points, 
     1     order_of_polynom) 

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
C Purpose: Smoothes data using the Savitzky-Golay smoothing
C filter. uSavitzkyGolay1 uses the subroutine savgol. The
C subroutine savgol calls the subroutines ludcmp and lubksp.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      include 'parameters.inc'

      real     local_data(n_chn),
     1         local_data_err(n_chn),
     1         coefficients(n_chn),
     1         swap_coeff(n_chn),
     1         local_data1(n_chn),
     1         local_data1_err(n_chn)

      integer  num_of_channels,
     1         left_points,
     1         right_points,
     1         no_of_points,
     1         points,
     1         order_of_polynom,
     1         index,
     1         left_points1,
     1         right_points1,
     1         order_of_polynom1


      write(6, *) '%%info: smoothing using Savitzky-Golay filter'

C Initialize the smoothed array data1.

C      write(6, *) 'Am I here?', num_of_channels
      do i=1, num_of_channels
         local_data1(i) = 0.0
         local_data1_err(i) = 0.0
      enddo

C      write(6, *) 'And now here?'
      no_of_points = left_points+right_points+1

C Check whether the parameters for the Savitzky-Golay filter are
C valid.

      if ((left_points .lt. 0) .or. (right_points .lt. 0) .or.
     1     (order_of_polynom .lt. 0)) then
         write(6, *) '%%warn: negative value Savitzky-Golay'
         write(6, *) '        input encountered'
         stop
      endif

      if (order_of_polynom .gt. 6) then
         write(6, *) '%%warn: maximum order of polynom is 6.'
         stop
      endif

      if ((left_points+right_points) .lt. order_of_polynom) then
         write(6, *) '%%warn: the sum of number of points'
         write(6, *) '        left and right must be larger than the'
         write(6, *) '        order of the polynom'
         stop
      endif

      if (no_of_points .gt. num_of_channels) then
         write(6, *) '%%warn: filter is larger than the'
         write(6, *) '        actual number of data points.'
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

C      write(6, *) '%%Info: Treating lower boundary'

C Distinguish whether the filter is symmetrical or asymmetrical, i.e.
C whether left_points = right_points or not, respectively. If the filter
C is symmetric then treat the lower and upper boundary symmetric otherwise
C treat it asymmetric.

      do i=2, left_points
         left_points1 = i-1
         if (order_of_polynom .gt. 2*left_points1) then
            order_of_polynom1 = 2*left_points1
         else
            order_of_polynom1 = order_of_polynom
         endif
         points = 2*left_points1+1
         call savgol(coefficients, points, left_points1, 
     1        left_points1, 0, order_of_polynom1)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, left_points1+1
            swap_coeff(k) = coefficients(left_points1+2-k)
         enddo
         do k=1, left_points1+1
            coefficients(k) = swap_coeff(k)
         enddo
         do k=1, left_points1
            swap_coeff(k) = coefficients(2*left_points1+2-k)
         enddo
         do k=1, left_points1
            coefficients(k+left_points1+1) = swap_coeff(k)
         enddo
C Smooth the data now
C         write(6, *) 'Including left points... '
         do k = i-left_points1, i+left_points1
C            write(6, *), k, coefficients(i-k+1)
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coefficients(k)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coefficients(k)**2
         enddo
         local_data1_err(i) = sqrt(local_data1_err(i))
      enddo

C If you are close to the end of the data there may be less points to
C the right then you want. In this case, do something similar like before.

C      write(6, *) '%%Info: Treating upper boundary'
      do i=num_of_channels-right_points+1, num_of_channels-1
         right_points1 = num_of_channels-i
         if (order_of_polynom .gt. 2*right_points1) then
            order_of_polynom1 = 2*right_points1
         else
            order_of_polynom1 = order_of_polynom
         endif
         points = 2*right_points1+1
         call savgol(coefficients, points, right_points1, 
     1        right_points1, 0, order_of_polynom1)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, right_points1+1
            swap_coeff(k) = coefficients(right_points1+2-k)
         enddo
         do k=1, right_points1+1
            coefficients(k) = swap_coeff(k)
         enddo
         do k=1, right_points1
            swap_coeff(k) = coefficients(2*right_points1+2-k)
         enddo
         do k=1, right_points1
            coefficients(k+right_points1+1) = swap_coeff(k)
         enddo
C Smooth the data now
         do k = i-right_points1,i+right_points1
C            write(6, *), k, coefficients(i-k+1)
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coefficients(right_points1+k-i+1)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coefficients(right_points1+k-i+1)**2
         enddo
         local_data1_err(i) = sqrt(local_data1_err(i))
      enddo

C For all other cases the full Savitzky-Golay filtering can be
C performed.

C      write(6, *) '%%Info: Treating general case'
      call savgol(coefficients, no_of_points, left_points,
     1     right_points, 0, order_of_polynom)
C Give the Savitzky-Golay coefficients a more natural order from -l to +r.
         do k=1, left_points+1
            swap_coeff(k) = coefficients(left_points+2-k)
         enddo
         do k=1, left_points+1
            coefficients(k) = swap_coeff(k)
         enddo
         do k=1, right_points
            swap_coeff(k) = coefficients(left_points+right_points+2-k)
         enddo
         do k=1, right_points
            coefficients(k+left_points+1) = swap_coeff(k)
         enddo
      do i=left_points+1,num_of_channels-right_points
C Smooth the data now
         do k=i-left_points, i+right_points
C            write(6, *), k, coefficients(i-k+1)
            local_data1(i) = local_data1(i)+local_data(k)*
     1           coefficients(left_points+k-i+1)
            local_data1_err(i) = local_data1_err(i)+
     1        local_data_err(k)**2*coefficients(left_points+k-i+1)**2
         enddo
         local_data1_err(i) = sqrt(local_data1_err(i))
      enddo

C Overwrite now the data array with the smoothed arrays data1 and 
C its error.

      do i=2, num_of_channels-1
         local_data(i) = local_data1(i)
         local_data_err(i) = local_data1_err(i)
      enddo

      return
      end

C-----------------------------------------------------------------
C
C subroutine savgol
C
C-----------------------------------------------------------------
      subroutine savgol(c, np, nl, nr, ld, m)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: returns the coefficients c(1:np) used to carry out
C the smoothing. np is the number of points.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      integer ld, m, nl, np, nr, MMAX

      real    c(np)

      parameter (MMAX = 6)

      integer imj, ipj, j, k, kk, mm, indx(MMAX+1)

      real    d, fac, sum, a(MMAX+1, MMAX+1), b(MMAX+1)

C Set up the normal equations

      do ipj=0, 2*m
         sum = 0.
         if (ipj .eq. 0) sum = 1.
         do k=1, nr
            sum = sum+float(k)**ipj
         enddo
         do k=1, nl
            sum = sum+float(-k)**ipj
         enddo
         mm = min(ipj, 2*m-ipj)
         do imj=-mm, mm, 2
            a(1+(ipj+imj)/2, 1+(ipj-imj)/2) = sum
         enddo
      enddo

C Solve the normal equations using LU-decomposition.

      call ludcmp(a, m+1, MMAX+1, indx, d)
      do j=1, m+1
         b(j) = 0.
      enddo
      b(ld+1) = 1.
      call lubksp(a, m+1, MMAX+1, indx, b)
      do kk=1, np
         c(kk) = 0.
      enddo
      do k=-nl, nr
         sum = b(1)
         fac = 1.
         do mm=1, m
            fac = fac*k
            sum = sum+b(mm+1)*fac
         enddo
         kk=mod(np-k, np)+1
         c(kk)=sum
      enddo
      return
      end

C-----------------------------------------------------------------
C
C subroutine ludcmp
C
C-----------------------------------------------------------------
      subroutine ludcmp(a, n, np, indx, d)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: performs the LU decomposition of a matrix into
C a lower (L) and upper (U) triangular matrix.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      integer n, np, indx(n), NMAX

      real d, a(np, np), TINY

      parameter (NMAX=500, TINY=1.0e-20)

      integer i, imax, j, k

      real    aamax, dum, sum, vv(NMAX)

      d = 1.

      do i=1, n
         aamax = 0.
         do j=1, n
            if (abs(a(i, j)) .gt. aamax) aamax=abs(a(i, j))
         enddo
         if (aamax .eq. 0) then
            write(6, *) 'Singular matrix in ludcmp'
            stop
         endif
         vv(i) = 1./aamax
      enddo
      do j=1, n
         do i=1, j-1
            sum = a(i, j)
            do k=1, i-1
               sum = sum-a(i, k)*a(k, j)
            enddo
            a(i, j) = sum
         enddo
         aamax = 0.
         do i=j, n
            sum = a(i, j)
            do k=1, j-1
               sum=sum-a(i, k)*a(k, j)
            enddo
            a(i, j) = sum
            dum=vv(i)*abs(sum)
            if (dum .ge. aamax) then
               imax = i
               aamax = dum
            endif
         enddo
         if (j .ne. imax) then
            do k=1, n
               dum = a(imax, k)
               a(imax, k) = a(j, k)
               a(j, k) = dum
            enddo
            d = -d
            vv(imax) = vv(j)
         endif
         indx(j) = imax
         if (a(j, j) .eq. 0.) a(j, j) = TINY
         if (j .ne. n) then
            dum=1./a(j, j)
            do i=j+1, n
               a(i, j) = a(i, j)*dum
            enddo
         endif
      enddo
      return
      end

C-----------------------------------------------------------------
C
C subroutine lubksp
C
C-----------------------------------------------------------------
      subroutine lubksp(a, n, np, indx, b)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Makes forward and backward substitution to solve
C a LU-decomposed system of linear equations.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      integer n, np, indx(n)

      real    a(np, np), b(n)

      integer i, ii, j, ll

      real    sum

      ii = 0
      do i=1, n
         ll = indx(i)
         sum = b(ll)
         b(ll) = b(i)
         if (ii .ne. 0) then
            do j=ii, i-1
               sum = sum-a(i, j)*b(j)
            enddo
         elseif (sum .ne. 0) then
            ii = i
         endif
         b(i) = sum
      enddo
      do i=n, 1, -1
         sum = b(i)
         do j=i+1, n
            sum = sum-a(i, j)*b(j)
         enddo
         b(i) = sum/a(i, i)
      enddo
      return
      end
