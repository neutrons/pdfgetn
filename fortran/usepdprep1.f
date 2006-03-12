      Program USEPDPREP1
C program to prepare sepd data for PDF analysis.  Born out of sepdprep and 
C before that newinta.
C
C Now, ipns runfiles are read using the ipns library routines in c and
C relevant information is stored in ascii files for archival, device 
C independent storage.  USEPDPREP reads the data and executes the
C subroutines to carry out initial preparation of the data for absolute
C scattering cross-section calculations.
C
C Simon Billinge, 7/99
C
C $Id: usepdprep1.f,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: usepdprep1.f,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C--------------------------------------------------------
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
C--------------------------------------------------------
C
      include 'parameters.inc'
      include 'usepdprep1.inc'

C--------------------------------------------------------
C begin executing here
C--------------------------------------------------------
C
C basic input information is on standard input

      call uReadInputs()
      call uSepdInitialize()

C Read some basic info from the data file.

      call how_long1(dfl, out_file)
      call uSepdReadHeader1(data_file(1), out_file(1:dfl))

C average angle and distance for each bank.

C for test purposes:
C      vanthreshold = 0.0
C      bkgthreshold = 4.0

      call uGroup(dist, twotht, nd, jm1, jm2, group_label,
     1     sec_flight_path, source_to_sample, num_of_det_groups,
     1     detect_map_table_size, first_monitor_num, 
     1     second_monitor_num, detector_angle, print_level,
     1     more_hist_bit,num_grps_to_go,num_grps_to_miss,num_det,
     1     extracted_histogram,bank_extracted)

C Initialize output file 

      call uSepdInt_OutOpen1(iout,out_file(1:dfl), user_name, 
     !                       run_title,num_grps_to_go)

C Use the info to group the detectors correctly and calculate the
C read now the monitor bank from the data files, apply multiplicative and
C additive corrections, create error array and sum the monitor
C up. Subtract the delayed neutron fraction.
C Integrate the monitor at the end. We are not yet in the big
C loop.

      write(6, *) ' '
      write(6, *) '-----------------------------------------'
      write(6, *) 'Monitor'
      write(6, *) '-----------------------------------------'
      write(6, *) '%%info: getting monitor of the data'
      write(6, *) '%%info: data set 1'
      write(6, *) ' '


      call uReadThisBank1(data_file(1), first_monitor_num, 
     1     data, data_err, add_to_data, mult_data, num_chan,
     1     nd(first_monitor_num), dangle, 1.0, print_level)

C-If there is more than one data file read it and sum the monitor up

      do i=2, num_data_sets
         write(6, *) '%%info: data set', i
         write(6, *) '%%info: getting monitor of the',
     1        ' data'
         call uReadThisBank1(data_file(i), first_monitor_num, 
     1        data1, data1_err, add_to_data, mult_data, num_chan,
     1        nd(first_monitor_num), dangle, 1.0, print_level)
         call uSumTheseData1(data, data_err, data1, data1_err, data, 
     1        data_err, num_chan)
      enddo

C-Calculate the wavelengths for the monitor. I assume it is the
C same for all banks (makes sense)

      call uLamCal1(calib_difc, calib_difa, calib_t0, t_min, t_max,
     1     t_step, num_of_det_groups, num_chan, 
     1     twotht(first_monitor_num), bnk_angle, 
     1     dist(first_monitor_num), first_monitor_num, lambda, 
     1     print_level)

C-Subtract the delayed neutron fraction here

      call uDelayedNeutFrac1(data, data_err, num_chan, 
     1     delayed_neutron_flag, dly_ntrn_corr, lambda, print_level)

C-Integrate the monitor here

      call uIntegrateMonitor1(data, data_err, num_chan, 
     1     data_norm_factor, data_norm_factor_err, print_level)

C-Do now the same for the monitor of the instrument background runs:
C Load the data of the incident monitor, sum up data from
C different files, and integrate the data finally. 
C We are still not in the big loop.

      if(lbgd) then
         write(6, *) ' '
         write(6, *) '%%info: getting monitor of the background'
         write(6, *) '%%info: background set 1'
         call uReadThisBank1(bkg_file(1), first_monitor_num, 
     1        bkg_data, bkg_data_err, add_to_bkg, mult_bkg, 
     1        num_chan, nd(first_monitor_num), dangle, 1.0, print_level)
         do i=2, num_bkg_sets
            write(6, *) '%%info: background set', i
            call uReadThisBank1(bkg_file(i), first_monitor_num, 
     1           bkg_data1, bkg_data1_err, add_to_bkg, mult_bkg,
     1           num_chan, 1, dangle, 1.0, print_level)
            call uSumTheseData1(bkg_data, bkg_data_err, bkg_data1, 
     1           bkg_data1_err, bkg_data, bkg_data_err, num_chan)
         enddo
         call uDelayedNeutFrac1(bkg_data, bkg_data_err, num_chan, 
     1        delayed_neutron_flag, dly_ntrn_corr, lambda, print_level)
         call uIntegrateMonitor1(bkg_data, bkg_data_err, num_chan, 
     1        bkg_norm_factor, bkg_norm_factor_err, print_level)

      endif
      write(6, *) ' '

c--------------------------------
c loop over banks. Here starts the big loop!

      do i = 1, num_of_det_groups
         isg = i
c--------------------------------
c check whether this bank should be missed

         lmiss = (bank_extracted(isg).eq.0)
         do j = 1, num_grps_to_miss
           if(isg.eq.missed_grps(j)) lmiss = .true.
         enddo

         if(isg.eq.first_monitor_num) lmiss = .true.
         if(isg.eq.second_monitor_num) lmiss = .true.

C-If this bank should not be missed then use it! Proceed by first
C reading and summing the files. Apply multiplicative and additive
C corrections and create the error array for a proper error 
C propagation.

         if (lmiss .eqv. .false.) then
            write(6, *) '------------------------------------------'
            write(6, *) 'Bank', isg
            write(6, *) '------------------------------------------'
            write(6, *) '%%info: processing data from bank', isg,
     1           ' ...'

            write(6, *) '%%info: data set 1 '
            call uReadThisBank1(data_file(1), isg, data, data_err, 
     1           add_to_data, mult_data, num_chan, nd(isg), dangle,
     1           t_step(isg), print_level)
            do k=2, num_data_sets
               write(6, *) '%%info: data set ', k
               call uReadThisBank1(data_file(k), isg, data1, 
     1              data1_err, add_to_data, mult_data, num_chan,
     1              nd(isg), dangle, t_step(isg), print_level)
               call uSumTheseData1(data, data_err, data1, data1_err,
     1              data, data_err, num_chan)
            enddo

C-Calculate wavelengths for this bank

           call uLamCal1(calib_difc, calib_difa, calib_t0, t_min, 
     1           t_max, t_step, num_of_det_groups, num_chan, 
     1           twotht(isg), bnk_angle, dist(isg), isg, lambda,
     1           print_level)

C-Subtract the delayed neutron fraction

           call uDelayedNeutFrac1(data, data_err, num_chan,
     1          delayed_neutron_flag, dly_ntrn_corr, lambda, 
     1          print_level)

C-Normalize the data with the integrated monitor.

           call uNormalizeThisBank1(data, data_err, data_norm_factor,
     1          data_norm_factor_err, num_chan)

C-Do the same for the background data, if it exists

           if (lbgd) then
              write(6, *) '%%info: processing background from bank',
     1             isg,' ...'
              write(6, *) '%%info: background set 1'
              call uReadThisBank1(bkg_file(1), isg, bkg_data, 
     1             bkg_data_err, add_to_bkg, mult_bkg, num_chan, 
     1             nd(isg), dangle, t_step(isg), print_level)
              do k=2, num_bkg_sets
                 write(6, *) '%%info: background set', i
                 call uReadThisBank1(bkg_file(k), isg, bkg_data1, 
     1                bkg_data1_err, add_to_data, mult_data, num_chan,
     1                nd(isg), dangle, t_step(isg), print_level)
                 call uSumTheseData1(bkg_data, bkg_data_err, bkg_data1, 
     1                bkg_data1_err, bkg_data, bkg_data_err, num_chan)
              enddo


C-Subtract the delayed neutron fraction

              call uDelayedNeutFrac1(bkg_data, bkg_data_err, 
     1             num_chan, delayed_neutron_flag, 
     1             dly_ntrn_corr, lambda, print_level)

C-Normalize the data with the integrated monitor.

              call uNormalizeThisBank1(bkg_data, bkg_data_err, 
     1             bkg_norm_factor, bkg_norm_factor_err, num_chan)

C-endif *if (lbgd)*
           endif

C-Do now the smoothing and subtraction. Remember: Subtract first, if
C both data and background are to be smoothed anyways (e.g. if data is
C from vanadium rod). Smooth first the background and then subtract it
C from the data, if the data is from the really interesting sample, i.e.
C if it is not the empty can or the vanadium rod. There are three smoothing
C options: bkg_smth_input = 0, 1, 2. If bkg_smth_input is 0 then
C no smoothing is performed. For bkg_smth_input = 1 the data is smoothed
C using overlapping second order polynomials. This works most times fine,
C but sometimes not. For bkg_smth_input = 2 a Savitzky-Golay smoothing
C filter is applied. You specify the no. of points to use for averaging
C and the order of the polynomial. Additionally it is now possible to
C remove Braggpeaks from the vanadium rod after background subtraction and, 
C if desired, from the background separately prior to subtraction. The
C vanthreshold and bkgthreshold parameters control to what extent the
C Bragg peaks will be removed. A negative threshold means no peak removal
C at all, a positive threshold will remove certain or all Bragg peaks.
C A threshold of 0.0 will return a fully smoothed vanadium or background. 
C This is because the peak removal algorithm takes a copy of the data and 
C smoothes it and uses that smoothed curve as a model background to search 
C for peaks. For a threshold of 0.0 the smoothed copy will be returned by the
C subroutine uKillPeaks1.

C Treat here the vanadium rod. Case 1: A background will be subtracted.

           if (lvan) then
              call uSepdLambdatoQ1(Q, lambda, twotht(isg),
     1             num_chan)
C              do j=1, num_chan
C                 write(6, *) 'Here', Q(j), lambda(j)
C              enddo
              if (lbgd) then
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 call uSubtractData1(data, data_err, bkg_data, 
     1                bkg_data_err, num_chan)
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(25, out_file, ".vraw", 
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif 
                 if (bkg_smth_input .eq. 1) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(25, out_file, ".vraw", 
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uVanProc1(lambda, data, data_err, num_chan,
     1                   twotht(isg), dqfit, fitreg, dqpeak)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(27, out_file,".vraw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(27, out_file,".vkill",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht(isg))
C                    endif
                    call uSavitzkyGolay1(data, data_err, num_chan, 
     1                   no_points_left, no_points_right,
     1                   degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(27, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif   
                 endif
              endif

C Case 2: No background will be subtracted.

              if (lbgd .eqv. .false.) then
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(25, out_file, ".vraw", 
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 1) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(29, out_file,".vraw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uVanProc1(lambda, data, data_err, 
     1                   num_chan, twotht(isg), dqfit, fitreg, 
     1                   dqpeak)
                    if (print_level .ge. 1 ) then
                       call uKuplotOut1(29, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(29, out_file,".vraw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".vkill",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht(isg))
C                    endif
                    call uSavitzkyGolay1(data, data_err, num_chan, 
     1                   no_points_left, no_points_right,
     1                   degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif 
                 endif
              endif
C-endif *if (lvan)*
           endif

C Treat the empty can data here. Case 1: A background will be subtracted.

           if (lcan) then
              call uSepdLambdatoQ1(Q, lambda, twotht(isg),
     1             num_chan)
C              do j=1, num_chan
C                 write(6, *) 'And now Q = ', Q(j)
C              enddo
              if (lbgd) then
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 call uSubtractData1(data, data_err, bkg_data,
     1                bkg_data_err, num_chan)
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(25, out_file, ".craw", 
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                       call uKuplotOut1(25, out_file,".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 1) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".craw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif 
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht(isg), dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".craw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif
                    call uSavitzkyGolay1(data, data_err, 
     1                   num_chan, no_points_left, 
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file, ".csmo", Q,
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
              endif

C Case 2: No background will be subtracted.

              if (lbgd .eqv. .false.) then
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
C                       call uSepdLambdatoQ1(Q, lambda, twotht(isg),
C     1                      num_chan)
                       call uKuplotOut1(25, out_file, ".craw", 
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                       call uKuplotOut1(25, out_file,".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 1) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".craw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif 
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht(isg), dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".craw",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht(isg))
                    endif 
                    call uSavitzkyGolay1(data, data_err, 
     1                   num_chan, no_points_left, 
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q,
     1                      data, data_err, num_chan, isg, twotht(isg))
                    endif
                 endif
              endif
C endif *if (lcan)*
           endif

C Treat the sample here. Case 1: A background will be subtracted.

           if ((lcan .eqv. .false.) .and. (lvan .eqv. .false.)) then
              call uSepdLambdatoQ1(Q, lambda, twotht(isg),
     1             num_chan)
              if (lbgd) then
                 if (print_level .ge. 1) then
                    call uKuplotOut1(29, out_file,".braw",
     1                   Q, bkg_data, bkg_data_err, num_chan, 
     1                   isg, twotht(isg))
                 endif
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 if (bkg_smth_input .eq. 0) then
                    call uKuplotOut1(29, out_file,".bsmo",
     1                   Q, bkg_data, bkg_data_err, num_chan, 
     1                   isg, twotht(isg))
                    call uSubtractData1(data, data_err,
     1                   bkg_data, bkg_data_err, num_chan)
                 endif
                 if (bkg_smth_input .eq. 1) then 
                    call uSmoothThis1(lambda, bkg_data, 
     1                   bkg_data_err, num_chan, twotht(isg), 
     1                   dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".bsmo",
     1                      Q, bkg_data, bkg_data_err, num_chan, 
     1                      isg, twotht(isg))
                    endif 
                    call uSubtractData1(data, data_err, 
     1                   bkg_data, bkg_data_err, num_chan)
                 endif
                 if (bkg_smth_input .eq. 2) then
                    call uSavitzkyGolay1(bkg_data, bkg_data_err, 
     1                   num_chan, no_points_left,
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".bsmo",
     1                      Q, bkg_data, bkg_data_err, num_chan, 
     1                      isg, twotht(isg))
                    endif
                    call uSubtractData1(data, data_err,
     1                   bkg_data, bkg_data_err, num_chan)
                 endif
              endif

C Case 2: No background will be subtracted. This case treats itself.


C endif * if ((lcan .eqv. .false.) .and. (lvan .eqv. .false.))* 
           endif

C Determine here the useful data range, i.e. the data range
C that contains non-zero data.

           call uDetermineDataRange1(lambda, data, data_err,
     1          num_chan, dly_ntrn_corr(2), dly_ntrn_corr(3))

C-Write results to an old-fashioned .int-file. Hopefully, the
C call to uSepdInt_Out1 will soon be replaced by uKuplotOut1 or
C uSepdNrmOut1.

           call uSepdInt_Out1(iout, isg, twotht(isg), 
     1          dist(isg), source_to_sample, num_chan, t_step(isg), 
     1          lambda, data, data_err)
           write(6, *) ' '

C-endif *if (lmiss .eqv. false)*
        endif

      enddo

C close output file

      call uSepdInt_OutClose1 (iout)
      print *, 'successful termination of prepsepd'
      end

C--------------------------------------------------------
C subroutines
C--------------------------------------------------------

C--------------------------------------------------------
      subroutine uReadInputs()

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Reads input supplied by the user either from a file or
C interactively.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'
      include 'usepdprep1.inc'

c--general info
      read(5,1000) data_path
      read(5,1000) instr_name
      read(5,1000) out_file
      read(5,1000) data_type

c--data-file info
      read(5,*) num_data_sets
      do i = 1,num_data_sets
         read(5,1000) data_file(i)
      enddo

c--bkg-file info
      read(5,*) num_bkg_sets
      do i = 1,num_bkg_sets
         read(5,1000) bkg_file(i)
      enddo
        
c--control parameters/group info
      read(5,*)   first_monitor_num,
     1            second_monitor_num,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr

c--smoothing parameters
      read(5,*)   bkg_smth_input

      if (bkg_smth_input .eq. 1) then
         read(5,*)   dqfit,
     1               fitreg,
     1               v_brg_pk_wdth
      endif

      if (bkg_smth_input .eq. 2) then
         read(5,*)   no_points_left,
     1               no_points_right,
     1               degree_of_polynom
      endif

c--missed group info
      if(num_grps_to_miss .gt. 0) then
           read(5,'(20i4)') (missed_grps(i),i=1,num_grps_to_miss)
      endif

c--additive corrections to these groups
      if(num_grps_addtve_corr .gt. 0) then
         do i=1,num_grps_addtve_corr
           read(5,*) adt_corr_grps(i),add_to_data(i),add_to_bkg(i)
         enddo
      endif
      if(num_grps_mult_corr .gt. 0) then
         do i=1,num_grps_mult_corr
           read(5,*) mlt_corr_grps(i),mult_data(i),mult_bkg(i)
         enddo
      endif

c--delayed neutron correction parameters
      read(5,*) delayed_neutron_flag,
     1          (dly_ntrn_corr(i), i=1,5)

c--calibration constants
      read(5,*) num_calib_entries
      do i=1,num_calib_entries
         read(5,*)   bnk_angle(i),
     1		  calib_difc(i),
     1		  calib_difa(i),
     1 		  calib_t0(i)
      enddo

      if ((data_type .eq. 'v') .or. (data_type .eq. 'V')) then
         read(5,*) vanthreshold
      endif
      if (num_bkg_sets .gt. 0) then
         read(5,*) bkgthreshold
      endif

C-----
C write it all to standard output
c-----

      If (print_level .eq. 2) then
         write(6,'(/a/)') ' Variables read from standard input:'
         write(6,*) ' instr_name          : ',instr_name
         write(6,*) ' data_type           : ',data_type
         call how_long1(dfo,out_file)
         write(6,*) ' out_file            : ',out_file(1:dfo)
         write(6,*) ' num_data_sets       :',num_data_sets

         call how_long1(dpl,data_path)
         do i=1,num_data_sets
            call how_long1(dfl,data_file(i))
            write(6,'(1x,a,i2,2a)') ' data_file(',i,')       : ',
     1           data_path(1:dpl)//data_file(i)(1:dfl)
         enddo

         write(6,*) ' num_bkg_sets        :',num_bkg_sets
         do i=1,num_bkg_sets
            call how_long1(dfl,bkg_file(i))
            write(6,'(1x,a,i2,2a)') ' bkg_file (',i,')       : ',
     1           data_path(1:dpl)//bkg_file(i)(1:dfl)
         enddo

         write(6,*)  ' first_monitor_num   :',first_monitor_num
         write(6,*)  ' second_monitor_num  :',second_monitor_num
         write(6,*)  ' print_level         :',print_level
         write(6,*)  ' num_grps_to_miss    :',num_grps_to_miss
         write(6,*)  ' num_grps_addtve_corr:',num_grps_addtve_corr
         write(6,*)  ' num_grps_mult_corr  :',num_grps_mult_corr
         write(6,*)  ' bkg_smth_input      :',bkg_smth_input
         if (bkg_smth_input .eq. 1) then
            write(6,*)  ' dqfit               :',dqfit
            write(6,*)  ' fitreg              :',fitreg
            write(6,*)  ' v_brg_pk_wdth       :',v_brg_pk_wdth
         else
            write(6, *) ' no_points_left      :',no_points_left
            write(6, *) ' no_points_right     :',no_points_right
            write(6, *) ' degree_of_polynom   :',degree_of_polynom
         endif


         if(num_grps_to_miss .gt. 0) then
            do i=1,num_grps_to_miss
               write(6,'(1x,a,i2,a,i3)') ' missed_grps(',i,')     :',
     1              missed_grps(i)
            enddo
         endif
         if(num_grps_addtve_corr .gt. 0) then
            do i=1,num_grps_addtve_corr
               write(6,'(1x,a,i2,a,i3)') ' adt_corr_grps(',i,
     1              ')   :',adt_corr_grps(i)
               write(6,'(1x,a,i2,a,g12.6)') ' add_to_data(',i,
     1            ')     :',add_to_data(i)
               write(6,'(1x,a,i2,a,g12.6)') ' add_to_bkg(',i,
     1            ')      :',add_to_bkg(i)
            enddo
         endif
         if(num_grps_mult_corr .gt. 0) then
            do i=1,num_grps_mult_corr
               write(6,'(1x,a,i2,a,i3)') ' mult_corr_grps(',i,
     1              ')  :',mlt_corr_grps(i)
               write(6,'(1x,a,i2,a,g12.6)') ' mult_data(',i,
     1              ')       :',mult_data(i)
               write(6,'(1x,a,i2,a,g12.6)') ' mult_bkg(',i,
     1              ')        :',mult_bkg(i)
            enddo
         endif
         write(6,*) ' delayed_neutron_flag:',delayed_neutron_flag
         write(6,*) ' dly_ntrn_factor     :',dly_ntrn_corr(1)
         write(6,*) ' dly_ntrn_min_wvlngth:', dly_ntrn_corr(2)
         write(6,*) ' dly_ntrn_max_wvlngth:', dly_ntrn_corr(3)
         write(6,*) ' dly_ntrn_cut_min_wvlngth:', 
     1        dly_ntrn_corr(4)
         write(6,*) ' dly_ntrn_cut_max_wvlngth:', 
     1        dly_ntrn_corr(5)
         
         write(6,*) ' num_calib_entries   :',num_calib_entries
         do i = 1,num_calib_entries
            write(6,'(1x,a,i2,a,f13.6)') ' bnk_angle(',i,')       :',
     1           bnk_angle(i)
            write(6,'(1x,a,i2,a,f13.6)') ' calib_difc(',i,')      :',
     1           calib_difc(i)
            write(6,'(1x,a,i2,a,f13.6)') ' calib_difa(',i,')      :',
     1           calib_difa(i)
            write(6,'(1x,a,i2,a,f13.6)') ' calib_t0(',i,')        :',
     1           calib_t0(i)
         enddo
         write(6, *) ' Peak removal threshold(van): ', vanthreshold
         write(6, *) ' Peak removal threshold(bkg): ', bkgthreshold
      endif
      return
c
1000  format(a)
      end
c---------------------------------------------------------------------------
c
c---------------------------------------------------------------------------
      subroutine uSepdInitialize()

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: initializes some of the arrays used in usepdprep.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'
      include 'usepdprep1.inc'

      real   temp_add_to_data(n_banks),
     1       temp_add_to_bkg(n_banks),
     1       temp_mult_data(n_banks),
     1       temp_mult_bkg(n_banks)

      if (print_level .eq. 2) then
         write(6,*) ' '
         write(6,*) '-------------------------------------- '
         write(6,*) ' %%info: initializing variables '
         write(6,*) ' '
      endif


c culled from sepdprep.  I would rather enter these parameters from a 
c file in case they change at some point but leave that for later.
C
C Define some variables and machine constants
C sjb6/99 I think dangle is the solid angle of a single detector since
C later DOMEGA is defined as DOMEGA = ND*DANGLE where (I think) ND is
C the number of detectors in a bank.
      DANGLE=38.1*1.112/150.0/150.0
      PI=4.0*ATAN(1.0D0)
C monitor constants - instrument dependent
C sjb6/99 These define monitor detector efficiency such that in subroutine
C MONSPC the counts are given as SPMON(N) = 1.E-3 * A(N)/(FACMON*XLAMM(N))
C where A(N) is the uncorrected counts
C     EFFMON=0.0000556
C     AMON=1.27*5.08
C     FACMON=AMON*EFFMON
C detector constants - instrument dependent
C sjb6/99 These constants are related to sigmar,the PRODUCT OF REACTION 
C  CROSS SECTION AND the detector radius: 
C  SIGMAR=DENDET*XLAM(N)*RDET and is used in calculating the detector
C  efficiency.
C     PDET=10.
C     RDET=0.556
C     DENDET=PDET*0.602252*(273.15/293.15)/22413.6
C     DENDET=DENDET*5327./1.8

c--end of ugly insert

      if (print_level .eq. 2) then
         write(6,*) ' det_solid_angle     :',dangle
      endif

c--check smoothing parameters are greater than zero
      if (bkg_smth_input .eq. 1) then
         if(dqfit .le. 0.) then
            dqfit = 0.2
            write(6,*) '%%warn: invalid dqfit reset to 0.2 '
         endif
         if(fitreg .le. 0.) then
            fitreg = 0.05
            write(6,*) '%%warn: invalid fitreg reset to 0.05 '
         endif
         if (dqpeak .le. 0.) then
            dqpeak = 0.005
            write(6, *) '%%warn: invalid dqpeak reset to 0.005 '
         endif
      endif

      if (bkg_smth_input .eq. 2) then
         if(no_points_left .le. 0) then
            no_points_left = 1
            write(6, *) '%%warn: invalid left_points reset to 1 '
         endif
         if (no_points_right .le. 0) then
            no_points_right = 1
            write(6, *) '%%warn: invalid right_points reset to 1 '
         endif
         if (degree_of_polynom .lt. 0) then
            degree_of_polynom = 0
            write(6, *) '%%warn: invalid degree_of_polynom reset ', 
     1           'to 0 '
         endif
      endif

c--general information
      if (print_level .eq. 2) then
         write(6,*) '%%info: det_solid_angle can only'
     1        ,' be changed in the source code'
      endif

c--initialize data_type logicals
      lcan = .false.
      lvan = .false.
      lbgd = .false.

      if (data_type .eq. 'c' .or. data_type .eq. 'C') then
          lcan = .true.
          write(6,*) '%%info: data will be smoothed - can data '
      elseif (data_type .eq. 'v' .or. data_type .eq. 'V') then
          lvan = .true.
         write(6,*) '%%info: data will be smoothed and V Bragg'
     1 ,' peaks will be removed - V data'
      else
          write(6,*) '%%info: data will NOT be smoothed '
      endif
      if (num_bkg_sets .gt. 0) then
          lbgd = .true.
          write(6,*) '%%info: background will be subtracted '
      endif
      if (bkg_smth_input .gt. 0) then 
          write(6,*) '%%info: background will be smoothed '
      else
          write(6,*) '%%info: background will NOT be smoothed '
      endif
c--------------------------------------------------
c initialize arrays

C Initialize the number of detectors in a group (nd), average distance
C (dist) and average twotht arrays.
      do i=1, n_banks
         nd(i) = 0
         dist(i) = 0.0
         twotht(i) = 0.0
      enddo

C Initialize the difc, difa and tzero values but do not override user-
C supplied input.

      do i = num_calib_entries+1, n_banks
         calib_difc(i)=0.0
         calib_difa(i)=0.0
         calib_t0(i)=0.0
      enddo

C Initialize multiplicative and additive correction arrays
C but do not override user-supplied input.

      do i = 1, num_grps_addtve_corr
         temp_add_to_data(i) = add_to_data(i)
         temp_add_to_bkg(i) = add_to_bkg(i)
      enddo

      do i = 1, num_grps_mult_corr
         temp_mult_data(i) = mult_data(i)
         temp_mult_bkg(i) = mult_bkg(i)
      enddo

      do i = 1, n_banks
         do k = 1, num_grps_addtve_corr
            if (adt_corr_grps(k) .eq. i) then
               add_to_data(i) = temp_add_to_data(k)
               add_to_bkg(i) = temp_add_to_bkg(k)
               goto 10
            endif
         enddo
         add_to_data(i) = 0.
         add_to_bkg(i) = 0.
 10      continue
      enddo

      do i=1, n_banks
         do k=1, num_grps_mult_corr
            if (mlt_corr_grps(k) .eq. i) then
               mult_data(i) = temp_mult_data(k)
               mult_bkg(i) = temp_mult_bkg(k)
               goto 20
            endif
         enddo
         mult_data(i) = 1.
         mult_bkg(i) = 1.
 20      continue
      enddo

      if ((data_type .ne. 'v') .or. (data_type .ne. 'V')) then
         vanthreshold = -1.0
      endif
      if (num_bkg_sets .eq. 0) then
         bkgthreshold = -1.0
      endif

      return
      end



C--------------------------------------------------------
      subroutine uGroup(dist, twotht, nd, jm1, jm2, group_label,
     1     sec_flight_path, source_to_sample, num_of_det_groups,
     1     detect_map_table_size, first_monitor_num, 
     1     second_monitor_num, detector_angle, print_level,
     1     more_hist_bit,num_grps_to_go,num_grps_to_miss,num_det,
     1     extracted_histogram,bank_extracted)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Groups the detectors for each bank correctly together,
C finds the incident and transmission monitor. Calculates average
C twotht and average flightpath.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      include 'parameters.inc'

      real      dist(n_banks),
     1          twotht(n_banks),
     1          sec_flight_path(n_det),
     1          detector_angle(n_det),
     1          source_to_sample

      integer   nd(n_banks),
     1          group_label(n_det),
     1          more_hist_bit(n_det),
     1          jm1,
     1          jm2,
     1          num_of_det_groups,
     1          num_grps_to_miss,
     1          num_grps_to_go,
     1          num_det,
     1          detect_map_table_size,
     1          first_monitor_num,
     1          second_monitor_num,
     1          print_level,
     1		extracted_histogram,
     1	        bank_extracted(n_banks)

c assigns each detector to a group.  Information held in array nd(jsg)
c finds the detector number for monitor 1 and monitor 2

      do i=1,n_banks
        bank_extracted(i)=0
      enddo

      num_of_det_groups=0

      istart = (extracted_histogram-1)*num_det + 1
      iend   = extracted_histogram*num_det

      do i = 1,detect_map_table_size
         if(group_label(i) .ne. 0) then
            nd(group_label(i)) = nd(group_label(i)) + 1
            if(group_label(i) .eq. first_monitor_num) jm1 = i
            if(group_label(i) .eq. second_monitor_num) jm2 = i
            num_of_det_groups=max(num_of_det_groups,group_label(i))
	    if (i.ge.istart .and. i.le.iend) then
	       bank_extracted(group_label(i)) = 1
            endif
         endif
      enddo

      num_of_det_used=0

      do i=1,num_of_det_groups
        if(bank_extracted(i).eq.1) num_of_det_used=num_of_det_used+1
      enddo

c--how many subgroups propagate?

      num_grps_to_go = num_of_det_used - num_grps_to_miss

      write(6,*) '%%info: num_of_det_groups  : ',num_of_det_groups
      write(6,*) '%%info: num_grps_to_go     : ',num_grps_to_go

c------------------------------------
c calculate the average flight path and angle
c arrays initialized in Initialize subroutine

      do i = 1, detect_map_table_size
	 idet = mod(i-1,num_det) + 1
	 if (group_label(i) .ne. 0) then
            dist(group_label(i)) = dist(group_label(i)) + 
     1                   sec_flight_path(idet)
            twotht(group_label(i)) = twotht(group_label(i))
     1                 + abs(detector_angle(idet))
	 endif
      enddo

      do i = 1,num_of_det_groups
	 if (nd(i) .ne. 0) then
           dist(i) = dist(i)/nd(i) + source_to_sample
           twotht(i) = twotht(i)/nd(i)
	 endif
      enddo

      If (print_level .eq. 2) then
         write(6,*) '  '
         write(6,*) '      monitor 1 is detector ',jm1,
     1        ' at angle ', twotht(first_monitor_num)
         write(6,*) '      monitor 2 is detector ',jm2,
     1        ' at angle ', twotht(second_monitor_num)
         do i = 1,num_of_det_groups
            write(6,*) '      detector group      : ',i
            write(6,*) '      average angle       : ',twotht(i)
            write(6,*) '      average flight path : ',dist(i)
         enddo
      endif

      return
      end

c---------------------------------------------------------------------------
c
c---------------------------------------------------------------------------




