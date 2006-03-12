      subroutine uRebinQ2(local_data_Q, local_data, local_data_err, 
     1     local_nmin, local_nmax, 
     1     local_Qstep, local_Qbig, local_Qmin, local_Qmax,
     1     lreverse)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Rebins data on an evenly spaced Q-grid. The idea is as
C follows. The interval for rebinning is given by Q-Qstep/2 <= Q
C <= Q+Qstep/2, where Q is a point on the Q-grid. All data points
C that lie within this interval are binned together. 
C If there is no data point within that interval then give it a
C value of zero. At the end, the Q-grid is extended down to Q=0, 
C otherwise blend is screwing up.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      parameter (Pi = 3.141592653589)

      real local_data(n_chn),
     1     local_data_err(n_chn),
     1     local_data_Q(n_chn),
     1     local_rebin(n_chn),
     1     local_rebin_err(n_chn),
     1     local_swap(n_chn),
     1     local_swap_err(n_chn),
     1     local_swap_Q(n_chn),
     1     local_diff(n_chn),
     1     local_Qstep,
     1     local_Q(n_chn),
     1     local_Qbig,
     1     local_Qmin,
     1     local_Qmax

      integer local_num_of_channels,
     1        local_no_of_points,
     1        local_nmin,
     1        local_nmax,
     1        isp,
     1        local_points(n_chn),
     1        ibin,
     1        minindex,
     1        maxindex

      character filename*6

      logical   lreverse

C Check, whether the data is already binned on a constant Q-grid.
C If it is then do not modify it.
c
      local_diff(1) = local_data_Q(2) - local_data_Q(1)

      do i=2, local_nmax-1
         local_diff(i) = local_data_Q(i+1) - local_data_Q(i)
         if (Abs(local_diff(i) - local_diff(1)) .gt. 0.005) then
            goto 10
         endif
      enddo

      if (Abs((Abs(local_diff(1)) - local_Qstep)).gt.0.00001) then
        write(*,500) Abs(local_diff(1))
500     format (' %%Warning: Data already binned on constant grid of ',
     1          f7.4,' A**-1')           
      endif

C Initialize swap-array:

10    continue
      if (lreverse) write(*,510) local_Qstep
510   format (' %%Info: Data will be rebinned to constant grid of  ',
     1        f7.4,' A**-1')           


C Reverse the array so that Q is ascending with increasing array index.
C Reverse the data and error array and initialize the rebinned Q array.
C It appears that the data array index passed to this routine is from nmin
C (which is identical to local_nmin) to nmax.

      if (lreverse) then
        do i=local_nmin, local_nmax
           local_swap(i) = 0.0
           local_swap_err(i) = 0.0
        enddo

        do i=local_nmin, local_nmax
           local_swap(i) = local_data(local_nmax-i+1)
           local_swap_err(i) = local_data_err(local_nmax-i+1)
           local_swap_Q(i) = local_data_Q(local_nmax-i+1)
        enddo

        do i=local_nmin, local_nmax
           local_data(i) = local_swap(i)
           local_data_err(i) = local_swap_err(i)
           local_data_Q(i) = local_swap_Q(i)
        enddo
      endif

C Get some resolution information and write it on screen

      dq_max  = 0.0
      dq_min  = 1000.0
      dq_sum  = 0.0
      dq_suml = 0.0
      dq_summ = 0.0
      dq_sumh = 0.0

      npts_h  = 0
      npts_m  = 0
      npts_l  = 0
      ipts_h  = 0
      ipts_m  = 0
      ipts_l  = 0

      do i=local_nmin+1, local_nmax
        if ((local_data_Q(i) .ge. local_Qmin)
     1       .and. (local_data_Q(i) .le. local_Qmax)) then
           dq = abs(local_data_Q(i)-local_data_Q(i-1))
  	   dq_max = max(dq_max,dq)
	   dq_min = min(dq_min,dq)
	   dq_sum = dq_sum + dq
	   if     (local_data_Q(i).gt.20.0) then
	      dq_sumh = dq_sumh + dq
	      npts_h  = npts_h + 1
	   elseif (local_data_Q(i).gt.10.0) then
	      dq_summ = dq_summ + dq
	      npts_m  = npts_m + 1
	   else
	      dq_suml = dq_suml + dq
	      npts_l  = npts_l + 1
           endif	
         endif	
      enddo

      ave_h = 0.0
      ave_m = 0.0
      ave_l = 0.0

      if (npts_h.gt.0) ave_h = dq_sumh / npts_h
      if (npts_m.gt.0) ave_m = dq_summ / npts_m
      if (npts_l.gt.0) ave_l = dq_suml / npts_l

C Create the rebinned Q-grid and initialize its values

      local_num_of_channels = nint(local_Qbig/local_Qstep)+1
      local_no_of_points = 0
      do i=1, local_num_of_channels
         isp = i
         local_Q(i) = 0.0 + float(isp-1)*local_Qstep
         local_rebin(i) = 0.0
         local_rebin_err(i) = 0.0
         local_points(i) = 0
         if (local_Q(i) .gt. local_Qbig) then
            goto 20
         endif
         local_no_of_points = local_no_of_points + 1
      enddo

C Start the rebinning here. Very efficient formula! Throw all points 
C that lie within the closed interval [Q-Qstep/2., Q+Qstep/2.] in the 
C same bin.

 20   do k=local_nmin, local_nmax
         if ((local_data_Q(k) .ge. local_Qmin)
     1        .and. (local_data_Q(k) .le. local_Qmax)) then

            ibin = nint(local_data_Q(k)/local_Qstep)+1
            local_points(ibin) = local_points(ibin)+1
            local_rebin(ibin)  = local_rebin(ibin)+local_data(k)
            local_rebin_err(ibin) = local_rebin_err(ibin) +
     1           local_data_err(k)**2
         endif
      enddo

      minindex = nint(local_Qmin/local_Qstep)+1
      maxindex = nint(local_Qmax/local_Qstep)+1

      do ibin=minindex, maxindex
         if (local_rebin_err(ibin) .gt. 0.0) then
            local_rebin_err(ibin) = sqrt(local_rebin_err(ibin))/
     1           local_points(ibin)
            local_rebin(ibin) = local_rebin(ibin)/local_points(ibin)
         endif
         if ((local_Q(ibin) .ge. local_Qmin) .and. 
     1       (local_Q(ibin) .le. local_Qmax)) then
	    if     (local_Q(ibin).gt.20.0) then
	       ipts_h  = ipts_h + 1
	    elseif (local_Q(ibin).gt.10.0) then
	       ipts_m  = ipts_m + 1
	    else
	       ipts_l  = ipts_l + 1
            endif	
         endif	
      enddo


C Assign the rebinned array to the original data array (overwrite it)

      do i=1, local_no_of_points
         local_data_Q(i) = local_Q(i)
         local_data(i) = local_rebin(i)
         local_data_err(i) = local_rebin_err(i)
      enddo

      local_nmin = 1
      local_nmax = local_no_of_points

      if (lreverse) then
        write(*,520) dq_min,dq_max,dq_sum/(local_nmax-local_nmin+1)
        write(*,530) ave_l,ave_m,ave_h
        write(*,540) npts_l,npts_m,npts_h
        write(*,550) ipts_l,ipts_m,ipts_h
      endif

520   format (' %%Info: Resolution dQ (min/max/average) : ',
     1        f7.4,'/',f7.4,'/',f7.4, ' A**-1')
530   format (' %%Info: Average    dQ (low/ >10 / >20)  : ',
     1        f7.4,'/',f7.4,'/',f7.4, ' A**-1')
540   format (' %%Info: Data points   (low/ >10 / >20)  : ',
     1        i7,'/',i7,'/',i7)
550   format (' %%Info: Grid points   (low/ >10 / >20)  : ',
     1        i7,'/',i7,'/',i7)

      return
      end
