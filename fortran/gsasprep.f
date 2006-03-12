      Program GSASPREP
c-------------------------------------------------------------------------
c Program to prepare GSAS data for PDF analysis.
c-------------------------------------------------------------------------
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
c-------------------------------------------------------------------------
c
      include 'parameters.inc'
      include 'gsasprep.inc'
c
      real     mon
c
      write (6,1100)
c
c-------------------------------------------------------------------------
c Begin executing here
c-------------------------------------------------------------------------
c
c Basic input information is on standard input
c
      call uGsasReadInputs()
      call uGsasGetNumberOfBanks(run_title,
     &                           data_file(1),
     &	                         num_of_det_groups,bank_id)
      call uGsasInitialize()
      call uGsasGetWeights(data_file,num_data_sets,weight_data)
      call uGsasGetWeights(bkg_file,num_bkg_sets,weight_bkg)
      call uSepdInt_OutOpen1(iout,out_file,user_name,run_title,
     &                       num_grps_to_go)

c-------------------------------------------------------------------------
c Loop over banks. Here starts the big loop!
c-------------------------------------------------------------------------

      do i = 1, num_of_det_groups
         isg = i

c Check whether this bank should be missed

         lmiss = .false.
         do j = 1, num_grps_to_miss
            if(isg.eq.missed_grps(j)) lmiss = .true.
         enddo

c If this bank should not be missed then use it! Proceed by first
c reading and summing the files. Apply multiplicative and additive
c corrections and create the error array for a proper error 
c propagation.

         if (lmiss .eqv. .false.) then
            write(6,1000)
            write(6,*) 'Bank', isg
            write(6,1000)
            write(6,*) '%%info: processing data from bank', isg,
     1           ' ...'
            write(6, *) '%%info: data set 1 - weight ',
     1                  weight_data(1)
            ind_min=-1
            ind_max=-1
            call uGsasReadBank1(data_file(1),isg,tof,data,data_err, 
     1           add_to_data(isg), mult_data(isg), num_chan, twotht, 
     1           dist,xf,difc,difa,tzero,print_level,ind_min,ind_max,
     1           monitor,bank_id,weight_data(1))
            do k=2, num_data_sets
               write(6, *) '%%info: data set ', k, ' - weight ',
     1                     weight_data(k)
               call uGsasReadBank1(data_file(k), isg, tof, data1, 
     1              data1_err, add_to_data(isg), mult_data(isg), 
     1              num_chan,twotht,dist,xf,difc,difa,tzero,
     1              print_level,ind_min,ind_max,mon,bank_id,
     1              weight_data(k))
               call uSumTheseData1(data, data_err, data1, data1_err,
     1              data, data_err, num_chan)
               monitor = monitor + mon
            enddo
            monitor_err = 0.0
            write(*,*) '%%info: Total monitor for data :',monitor

c Convert to wavelength

            call uGsasTofToLambda(tof,lambda,num_chan,
     1                            twotht,difc,difa,tzero)

c Normalize the summed data and divide the summed data by the number
c of data files. We divide the monitor by 10000. to have numbers 
c close to unity (as done in old HIPD_PREP)

	    if (monitor.gt.10000.0) then
              monitor=monitor/10000.
              monitor_err=monitor_err/10000.
	    endif
            call uNormalizeThisBank1(data,data_err,monitor,
     1                               monitor_err,num_chan)

c Do the same for the background data, if it exists

           if (lbgd) then
              write(6, *) '%%info: processing background from bank',
     1             isg,' ...'
              write(6, *) '%%info: background set 1'
              call uGsasReadBank1(bkg_file(1), isg, tof, bkg_data, 
     1             bkg_data_err, add_to_bkg(isg), mult_bkg(isg), 
     1             num_chan, twotht,dist,xf,difc,difa,tzero,
     1	           print_level,ind_min,ind_max,monitor,bank_id,
     1             weight_bkg(1))
              do k=2, num_bkg_sets
                 write(6, *) '%%info: background set', k
                 call uGsasReadBank1(bkg_file(k),isg,tof,bkg_data1, 
     1                bkg_data1_err, add_to_data(isg), mult_data(isg), 
     1                num_chan, twotht,dist,xf,difc,difa,tzero,
     1	              print_level,ind_min,ind_max,mon,bank_id,
     1                weight_bkg(k))
                 call uSumTheseData1(bkg_data, bkg_data_err, bkg_data1, 
     1                bkg_data1_err, bkg_data, bkg_data_err, num_chan)
                 monitor = monitor + mon
              enddo
              monitor_err = 0.0
              write(*,*) '%%info: Total monitor for background :',
     1                   monitor

c Normalize the summed data and divide the summed data by the number
c of data files.

	      if (monitor.gt.10000.0) then
                monitor=monitor/10000.
                monitor_err=monitor_err/10000.
	      endif
              call uNormalizeThisBank1(bkg_data,bkg_data_err,
     1                                 monitor,monitor_err,num_chan)
ctep              do j=1, num_chan
ctep                 bkg_data(j) = bkg_data(j)/num_bkg_sets
ctep                 bkg_data_err(j) = bkg_data_err(j)/num_bkg_sets
ctep              enddo

c endif *if (lbgd)*
           endif

c Do now the smoothing and subtraction. Remember: Subtract first, if
c both data and background are to be smoothed anyways (e.g. if data is
c from vanadium rod). Smooth first the background and then subtract it
c from the data, if the data is from the really interesting sample, i.e.
c if it is not the empty can or the vanadium rod. There are three smoothing
c options: bkg_smth_input = 0, 1, 2. If bkg_smth_input is 0 then
c no smoothing is performed. For bkg_smth_input = 1 the data is smoothed
c using overlapping second order polynomials. This works most times fine,
c but sometimes not. For bkg_smth_input = 2 a Savitzky-Golay smoothing
c filter is applied. You specify the no. of points to use for averaging
c and the order of the polynomial. Additionally it is now possible to
c remove Braggpeaks from the vanadium rod after background subtraction and, 
c if desired, from the background separately prior to subtraction. The
c vanthreshold and bkgthreshold parameters control to what extent the
c Bragg peaks will be removed. A negative threshold means no peak removal
c at all, a positive threshold will remove certain or all Bragg peaks.
c A threshold of 0.0 will return a fully smoothed vanadium or background. 
c This is because the peak removal algorithm takes a copy of the data and 
c smoothes it and uses that smoothed curve as a model background to search 
c for peaks. For a threshold of 0.0 the smoothed copy will be returned by the
c subroutine uKillPeaks1.

c Treat here the vanadium rod. Case 1: A background will be subtracted.

           if (lvan) then
              call uLambdatoQ1(Q, lambda, twotht,
     1             num_chan)              
              if (lbgd) then
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 call uSubtractData1(data, data_err, bkg_data, 
     1                bkg_data_err, num_chan)
                 if (print_level .ge. 1) then
                    call uKuplotOut1(25, out_file, ".vraw", 
     1                   Q, data, data_err, num_chan, isg,
     1                   twotht)
                 endif
                 if (bkg_smth_input .eq. 0) then
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(27, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif           
                 if (bkg_smth_input .eq. 1) then
                    call uVanProc1(lambda, data, data_err, num_chan,
     1                   twotht, dqfit, fitreg, dqpeak)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file,".vsmo", Q,
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    call uSavitzkyGolay1(data, data_err, num_chan, 
     1                   no_points_left, no_points_right,
     1                   degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(27, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif
              endif

C Case 2: No background will be subtracted.

              if (lbgd .eqv. .false.) then
                 if (print_level .ge. 1) then
                    call uKuplotOut1(29, out_file,".vraw",
     1                   Q, data, data_err, num_chan, isg,
     1                   twotht)
                 endif
                 if (bkg_smth_input .eq. 0) then
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif                 
                 if (bkg_smth_input .eq. 1) then
                    call uVanProc1(lambda, data, data_err, 
     1                   num_chan, twotht, dqfit, fitreg, 
     1                   dqpeak)
                    if (print_level .ge. 1 ) then
                       call uKuplotOut1(29, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    call uSavitzkyGolay1(data, data_err, num_chan, 
     1                   no_points_left, no_points_right,
     1                   degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif
              endif
c endif *if (lvan)*
           endif

c Treat the empty can data here. Case 1: A background will be subtracted.

           if (lcan) then
              call uLambdatoQ1(Q, lambda, twotht,
     1             num_chan)
              if (lbgd) then
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 call uSubtractData1(data, data_err, bkg_data,
     1                bkg_data_err, num_chan)
                 if (print_level .ge. 1) then
                    call uKuplotOut1(29, out_file,".craw",
     1                   Q, data, data_err, num_chan, isg,
     1                   twotht)
                 endif
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".csmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 1) then
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht, dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    call uSavitzkyGolay1(data, data_err, 
     1                   num_chan, no_points_left, 
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file, ".csmo", Q,
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
              endif

C Case 2: No background will be subtracted.

              if (lbgd .eqv. .false.) then
                 if (print_level .ge. 1) then
                    call uKuplotOut1(29, out_file,".craw",
     1                   Q, data, data_err, num_chan, isg,
     1                   twotht)
                 endif
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".csmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 1) then
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht, dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
                    call uSavitzkyGolay1(data, data_err, 
     1                   num_chan, no_points_left, 
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q,
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
              endif
c endif *if (lcan)*
           endif

c Treat the sample here. Case 1: A background will be subtracted.

           if ((lcan .eqv. .false.) .and. (lvan .eqv. .false.)) then
              call uLambdatoQ1(Q, lambda, twotht,
     1             num_chan)
              if (lbgd) then
                 if (print_level .ge. 1) then
                    call uKuplotOut1(29, out_file,".braw",
     1                   Q, bkg_data, bkg_data_err, num_chan, 
     1                   isg, twotht)
                 endif
                 call uKillPeaks1(bkg_data, bkg_data_err, Q, num_chan, 
     1                print_level, bkgthreshold)
                 if (bkg_smth_input .eq. 0) then
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".bsmo",
     1                      Q, bkg_data, bkg_data_err, num_chan, 
     1                      isg, twotht)
                    endif
                    call uSubtractData1(data, data_err,
     1                   bkg_data, bkg_data_err, num_chan)
                 endif
                 if (bkg_smth_input .eq. 1) then
                    call uSmoothThis1(lambda, bkg_data, 
     1                   bkg_data_err, num_chan, twotht, 
     1                   dqfit, fitreg)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".bsmo",
     1                      Q, bkg_data, bkg_data_err, num_chan, 
     1                      isg, twotht)
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
     1                      isg, twotht)
                    endif
                    call uSubtractData1(data, data_err,
     1                   bkg_data, bkg_data_err, num_chan)
                 endif
              endif

c endif * if ((lcan .eqv. .false.) .and. (lvan .eqv. .false.))* 
           endif

c Write results to an old-fashioned .int-file.

           call uSepdInt_Out1(iout, isg, twotht, 
     1          dist, xf, num_chan, 1., lambda, data,data_err)
           write(6, *) ' '

c endif *if (lmiss .eq. false)*
         endif
      enddo

c close output file

      call uSepdInt_OutClose1 (iout)
      write(6,1000)
      write(6,*) 'successful termination of prepgsas '

1000  format (1x,78('-'))
1100  format (1x,78('-'),/,' GSASPREP Version 1.0',/,1x,78('-'))
      end

c-------------------------------------------------------------------------
c Subroutines
c-------------------------------------------------------------------------

      subroutine uGsasGetWeights(fname,nfiles,weight)
c-------------------------------------------------------------------------
c Purpose: Gets weights for normalized data sets.
c-------------------------------------------------------------------------

      include 'parameters.inc'

      character*80   fname(n_runs)
      integer        nfiles
      real           weight(n_runs)

      character*80   line
      real           cur(n_runs)
      real           mo(n_runs)
      integer        t0(n_runs)

      do i=1,nfiles
        t0(i)=0
        cur(i)=0.0
        mo(i)=0

        open(73,file=fname(i),status='old')

5       continue
        read(73,1000,end=10) line
        icol=index(line,':')
        if ((line(1:8).eq.'Monitor:') .or.
     &	    (line(1:8).eq.'MONITOR:')     ) then
          read(line(icol+1:80),*) mo(i)
        endif
        if (line(1:14).eq.'# Tzero_in_run') then
          read(line(icol+1:80),*) t0(i)
        endif
        if (line(1:20).eq.'# Integrated_current') then
          read(line(icol+1:80),*) cur(i)
        endif
        if (line(1:8).eq.'TIME_MAP') goto 10
        goto 5
10      continue
        close(73)
      enddo

      tot_t0=0.0
      tot_cur=0.0
      tot_mo=0.0
      do i=1,nfiles
        tot_t0=tot_t0+float(t0(i))
        tot_cur=tot_cur+cur(i)
        tot_mo =tot_mo+mo(i)
      enddo

      do i=1,nfiles
        if (mo(i).gt.1) then
          weight(i)=float(nfiles)*mo(i)/tot_mo
          write(6,*)'%%info: File ',i,' - weight (Monitor) ',weight(i)
        else
          if (tot_cur.gt.0) then
            weight(i)=float(nfiles)*cur(i)/tot_cur
            write(6,*)'%%info: File ',i,' - weight (uAmp) ',weight(i)
          else
            weight(i)=float(nfiles)*t0(i)/tot_t0
            write(6,*)'%%info: File ',i,' - weight (T0) ',weight(i)
          endif
        endif
      enddo

1000  format(a)
      end

c-------------------------------------------------------------------------

      subroutine uGsasReadInputs()
c-------------------------------------------------------------------------
c Purpose: Reads input supplied by the user either from a file or
c interactively.
c-------------------------------------------------------------------------

      include 'parameters.inc'
      include 'gsasprep.inc'

c general info

      read(*,'(a)') data_path
      read(*,'(a)') instr_name
      read(*,'(a)') out_file
      read(*,'(a)') iparm_file
      read(*,'(a)') data_type

c data-file info

      read(*,*) num_data_sets
      do i = 1,num_data_sets
         read(*,'(a)') data_file(i)
      enddo

c bkg-file info

      read(*,*) num_bkg_sets
      do i = 1,num_bkg_sets
         read(*,'(a)') bkg_file(i)
      enddo
        
c control parameters/group info

      read(*,*) print_level,num_grps_to_miss,
     1          num_grps_addtve_corr,num_grps_mult_corr

c smoothing parameters

      read(*,*) bkg_smth_input

      if (bkg_smth_input .eq. 1) then
         read(*,*) dqfit,fitreg,v_brg_pk_wdth
      endif

      if (bkg_smth_input .eq. 2) then
         read(*,*) no_points_left,no_points_right,degree_of_polynom
      endif

c missed group info

      if(num_grps_to_miss .gt. 0) then
           read(*,*) (missed_grps(i),i=1,num_grps_to_miss)
      endif

c additive corrections to these groups

      if(num_grps_addtve_corr .gt. 0) then
         do i=1,num_grps_addtve_corr
           read(*,*) adt_corr_grps(i),add_to_data(i),add_to_bkg(i)
         enddo
      endif
      if(num_grps_mult_corr .gt. 0) then
         do i=1,num_grps_mult_corr
           read(*,*) mlt_corr_grps(i),mult_data(i),mult_bkg(i)
         enddo
      endif

      if ((data_type .eq. 'v') .or. (data_type .eq. 'V')) then
         read(*,*) vanthreshold
      endif
      if (num_bkg_sets .gt. 0) then
         read(*,*) bkgthreshold
      endif

c write it all to standard output

      if (print_level .eq. 2) then
         call how_long1(dfo,out_file)
         call how_long1(dpl,data_path)
         call how_long1(ipa,iparm_file)
         write(6,*)'%%info: Variables read from standard input:'
         write(6,*)'%%info: instr_name            : ',instr_name
         write(6,*)'%%info: data_type             : ',data_type
         write(6,*)'%%info: out_file              : ',out_file(1:dfo)
         write(6,*)'%%info: iparm_file            : ',iparm_file(1:ipa)
         write(6,*)'%%info: num_data_sets         : ',num_data_sets

         do i=1,num_data_sets
            call how_long1(dfl,data_file(i))
            write(6,'(1x,a,i2,2a)') 
     1           '%%info: data_file(',i,')         : ',
     1           data_path(1:dpl)//data_file(i)(1:dfl)
         enddo

         write(6,*)'%%info: num_bkg_sets          :',num_bkg_sets
         do i=1,num_bkg_sets
            call how_long1(dfl,bkg_file(i))
            write(6,'(1x,a,i2,2a)') ' bkg_file (',i,')         : ',
     1           data_path(1:dpl)//bkg_file(i)(1:dfl)
         enddo

         write(6,*)'%%info: print_level           :',print_level
         write(6,*)'%%info: num_grps_to_miss      :',num_grps_to_miss
         write(6,*)'%%info: num_grps_addtve_corr  :',
     1             num_grps_addtve_corr
         write(6,*)'%%info: num_grps_mult_corr    :',num_grps_mult_corr
         write(6,*)'%%info: bkg_smth_input        :',bkg_smth_input
         if (bkg_smth_input .eq. 1) then
            write(6,*)'%%info: dqfit                 :',dqfit
            write(6,*)'%%info: fitreg                :',fitreg
            write(6,*)'%%info: v_brg_pk_wdth         :',v_brg_pk_wdth
         else
            write(6, *)'%%info: no_points_left        :',no_points_left
            write(6, *)'%%info: no_points_right       :',no_points_right
            write(6, *)'%%info: degree_of_polynom     :',
     1                 degree_of_polynom
         endif


         if(num_grps_to_miss .gt. 0) then
            do i=1,num_grps_to_miss
               write(6,'(1x,a,i2,a,i3)') 
     1              '%%info: missed_grps(',i,')       :',missed_grps(i)
            enddo
         endif
         if(num_grps_addtve_corr .gt. 0) then
            do i=1,num_grps_addtve_corr
               write(6,'(1x,a,i2,a,i3)') '%%info: adt_corr_grps(',i,
     1              ')     :',adt_corr_grps(i)
               write(6,'(1x,a,i2,a,g12.6)') '%%info: add_to_data(',i,
     1            ')       :',add_to_data(i)
               write(6,'(1x,a,i2,a,g12.6)') '%%info: add_to_bkg(',i,
     1            ')        :',add_to_bkg(i)
            enddo
         endif
         if(num_grps_mult_corr .gt. 0) then
            do i=1,num_grps_mult_corr
               write(6,'(1x,a,i2,a,i3)') '%%info: mult_corr_grps(',i,
     1              ')    :',mlt_corr_grps(i)
               write(6,'(1x,a,i2,a,g12.6)') '%%info: mult_data(',i,
     1              ')         :',mult_data(i)
               write(6,'(1x,a,i2,a,g12.6)') '%%info: mult_bkg(',i,
     1              ')          :',mult_bkg(i)
            enddo
         endif
         write(6, *) '%%info: Peak removal thresh(v): ', vanthreshold
         write(6, *) '%%info: Peak removal thresh(b): ', bkgthreshold
      endif
      return
      end
c-------------------------------------------------------------------------

      subroutine uGsasInitialize()
c-------------------------------------------------------------------------
c Purpose: initializes some of the arrays used in usepdprep.
c-------------------------------------------------------------------------

      include 'parameters.inc'
      include 'gsasprep.inc'

      real   temp_add_to_data(n_banks),
     1       temp_add_to_bkg(n_banks),
     1       temp_mult_data(n_banks),
     1       temp_mult_bkg(n_banks)

      if (print_level .eq. 2) then
         write(6,1000)
         write(6,*) '%%info: initializing variables '
      endif

c Check smoothing parameters are greater than zero

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

c How many subgroups propagate?

         num_grps_to_go = num_of_det_groups - num_grps_to_miss
         if (print_level .eq. 2) then
            write(6,*) '%%info: num_grps_to_go      :',num_grps_to_go
         endif

c Defaults

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

      do i = 1, num_grps_addtve_corr
         temp_add_to_data(i) = add_to_data(i)
         temp_add_to_bkg(i) = add_to_bkg(i)
      enddo

      do i = 1, num_grps_mult_corr
         temp_mult_data(i) = mult_data(i)
         temp_mult_bkg(i) = mult_bkg(i)
      enddo

      do i = 1, num_of_det_groups
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

      do i=1, num_of_det_groups
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

ctep  if ((data_type .ne. 'v') .or. (data_type .ne. 'V')) then
ctep     vanthreshold = -1.0
ctep  endif
ctep  if (num_bkg_sets .eq. 0) then
ctep     bkgthreshold = -1.0
ctep  endif

      user_name = 'PDF user'

      return
1000  format (1x,78('-'))
      end
