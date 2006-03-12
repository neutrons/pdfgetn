      Program ARIELPREP
C program to prepare glad data for PDF analysis.  Born out of usepdprep.
C
C Currently, the .NRM files from GLAD have to be converted to ASCII using
C OPENGENIE. Hopefully, GLADPREP will soon be able to read the raw .NRM
C files or there will be a separate program similar to SEPDBTOA to perform
C a direct ASCII conversion.  GLADPREP reads the data and executes the
C subroutines to carry out initial preparation of the data for absolute
C scattering cross-section calculations.
C
C Simon Billinge, 9/99
C
C $Id: arielprep.f,v 1.1 2006/03/12 09:59:24 tproffen Exp $
C
C $Log: arielprep.f,v $
C Revision 1.1  2006/03/12 09:59:24  tproffen
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
C Modified for ARIEL SPEC files by Thomas Proffen, Dec 1999
C
      include 'parameters.inc'
      include 'arielprep.inc'

C--------------------------------------------------------
C begin executing here
C--------------------------------------------------------
C
C basic input information is on standard input

      call uArielReadInputs()
      call uArielGetNumberOfBanks(data_file(1),num_of_det_groups)
      call uArielInitialize()

C Read some basic info from the data file.

      user_name = 'User unknown'
      run_title = 'Created with prepariel ..'
      call uSepdInt_OutOpen1(iout,out_file, user_name,
     !                       run_title,num_grps_to_go)

c--------------------------------
c loop over banks. Here starts the big loop!

      do i = 1, num_of_det_groups
         isg = i
c--------------------------------
c check whether this bank should be missed

         lmiss = .false.
         do j = 1, num_grps_to_miss
            if(isg.eq.missed_grps(j)) lmiss = .true.
         enddo

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
            ind_min=-1
            ind_max=-1
            call uArielReadBank1(data_file(1),isg,lambda,data,data_err, 
     1           add_to_data(isg), mult_data(isg), num_chan, twotht, 
     1           dist,xf,print_level,ind_min,ind_max)
            do k=2, num_data_sets
               write(6, *) '%%info: data set ', k
               call uArielReadBank1(data_file(k), isg, lambda, data1, 
     1              data1_err, add_to_data(isg), mult_data(isg), 
     1              num_chan,twotht,dist,xf, 
     1	            print_level,ind_min,ind_max)
               call uSumTheseData1(data, data_err, data1, data1_err,
     1              data, data_err, num_chan)
            enddo

C Normalize the summed data by dividing the summed data by the number
C of data files. The .nrm files are supposed to be already normalized.

            do j=1, num_chan
               data(j) = data(j)/num_data_sets
               data_err(j) = data_err(j)/num_data_sets
            enddo

C-Do the same for the background data, if it exists

           if (lbgd) then
              write(6, *) '%%info: processing background from bank',
     1             isg,' ...'
              write(6, *) '%%info: background set 1'
              call uArielReadBank1(bkg_file(1), isg, lambda, bkg_data, 
     1             bkg_data_err, add_to_bkg(isg), mult_bkg(isg), 
     1             num_chan, twotht,dist,xf,
     1	           print_level,ind_min,ind_max)
              do k=2, num_bkg_sets
                 write(6, *) '%%info: background set', k
                 call uArielReadBank1(bkg_file(k),isg,lambda,bkg_data1, 
     1                bkg_data1_err, add_to_data(isg), mult_data(isg), 
     1                num_chan, twotht,dist,xf, 
     1	              print_level,ind_min,ind_max)
                 call uSumTheseData1(bkg_data, bkg_data_err, bkg_data1, 
     1                bkg_data1_err, bkg_data, bkg_data_err, num_chan)
              enddo

C Normalize the summed background by dividing by the number of background
C files:

              do j=1, num_chan
                 bkg_data(j) = bkg_data(j)/num_bkg_sets
                 bkg_data_err(j) = bkg_data_err(j)/num_bkg_sets
              enddo

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
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(27, out_file,".vraw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(27, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif           
                 if (bkg_smth_input .eq. 1) then
C                    call uQtoLambda1(Q, lambda, twotht,
C     1                   num_chan)
                    call uVanProc1(lambda, data, data_err, num_chan,
     1                   twotht, dqfit, fitreg, dqpeak)
C                    call uLambdatoQ1(lambda, lambda, twotht,
C     1                   num_chan)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file,".vsmo", Q,
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(27, out_file,".vraw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
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
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".vraw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif 
                    call uKillPeaks1(data, data_err, Q, num_chan, 
     1                   print_level, vanthreshold)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(29, out_file,".vsmo",
     1                      Q, data, data_err, num_chan, isg,
     1                      twotht)
                    endif
                 endif                 
                 if (bkg_smth_input .eq. 1) then
C                    call uQtoLambda1(Q, lambda, twotht,
C     1                   num_chan)                    
                    call uVanProc1(lambda, data, data_err, 
     1                   num_chan, twotht, dqfit, fitreg, 
     1                   dqpeak)
C                    call uLambdatoQ1(lambda, lambda, twotht,
C     1                   num_chan)                    
                    if (print_level .ge. 1 ) then
                       call uKuplotOut1(29, out_file,".vsmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".vraw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
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
C-endif *if (lvan)*
           endif

C Treat the empty can data here. Case 1: A background will be subtracted.

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
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".craw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
C                    call uQtoLambda1(lambda, lambda, twotht,
C     1                   num_chan)
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht, dqfit, fitreg)
C                    call uLambdatoQ1(Q, lambda, twotht,
C     1                   num_chan)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".craw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
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
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".craw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif
C                    call uQtoLambda1(Q, lambda, twotht,
C     1                   num_chan)
                    call uSmoothThis1(lambda, data, data_err,
     1                   num_chan, twotht, dqfit, fitreg)
C                    call uLambdatoQ1(Q, lambda, twotht,
C     1                   num_chan)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q, 
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
                 if (bkg_smth_input .eq. 2) then
C                    if (print_level .ge. 1) then
C                       call uKuplotOut1(29, out_file,".craw",
C     1                      Q, data, data_err, num_chan, isg,
C     1                      twotht)
C                    endif 
                    call uSavitzkyGolay1(data, data_err, 
     1                   num_chan, no_points_left, 
     1                   no_points_right, degree_of_polynom)
                    if (print_level .ge. 1) then
                       call uKuplotOut1(25, out_file, ".csmo", Q,
     1                      data, data_err, num_chan, isg, twotht)
                    endif
                 endif
              endif
C endif *if (lcan)*
           endif

C Treat the sample here. Case 1: A background will be subtracted.

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
C                    call uQtoLambda1(Q, lambda, twotht,
C     1                   num_chan)
                    call uSmoothThis1(lambda, bkg_data, 
     1                   bkg_data_err, num_chan, twotht, 
     1                   dqfit, fitreg)
C                    call uLambdatoQ1(lambda, lambda, twotht,
C     1                   num_chan)
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

C endif * if ((lcan .eqv. .false.) .and. (lvan .eqv. .false.))* 
           endif

C-Write results to an old-fashioned .int-file. Hopefully, the
C call to uSepdInt_Out1 will soon be replaced by uKuplotOut1 or
C uSepdNrmOut1.

           call uSepdInt_Out1(iout, isg, twotht, 
     1          dist, xf, num_chan, 1., lambda, data,data_err)
           write(6, *) ' '

C-endif *if (lmiss .eq. false)*
         endif

      enddo

C close output file

      call uSepdInt_OutClose1 (iout)
      write(6, *) 'successful termination of prepariel '

      end

C--------------------------------------------------------
C subroutines
C--------------------------------------------------------

C--------------------------------------------------------
      subroutine uArielReadInputs()

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
      include 'arielprep.inc'

c--general info
      read(5,'(a255)') data_path
      read(5,'(a4)') instr_name
      read(5,'(a80)') out_file
      read(5,'(a1)') data_type

c--data-file info
      read(5,*) num_data_sets
      do i = 1,num_data_sets
         read(5,'(a80)') data_file(i)
      enddo

c--bkg-file info
      read(5,*) num_bkg_sets
      do i = 1,num_bkg_sets
         read(5,'(a80)') bkg_file(i)
      enddo
        
c--control parameters/group info
      read(5,*)   print_level,
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
         write(6, *) ' Peak removal threshold(van): ', vanthreshold
         write(6, *) ' Peak removal threshold(bkg): ', bkgthreshold
      endif
      return
      end
c---------------------------------------------------------------------------
c
c---------------------------------------------------------------------------
      subroutine uArielInitialize()

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: initializes some of the arrays used in usepdprep.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'
      include 'arielprep.inc'

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

c--check smoothing parameters are greater than zero
      if (bkg_smth_input .eq. 1) then
         if(dqfit .le. 0.) then
            dqfit = 0.2
            write(6,*) '%%warning: invalid dqfit reset to 0.2 '
         endif
         if(fitreg .le. 0.) then
            fitreg = 0.05
            write(6,*) '%%warning: invalid fitreg reset to 0.05 '
         endif
         if (dqpeak .le. 0.) then
            dqpeak = 0.005
            write(6, *) '%%warning: invalid dqpeak reset to 0.005 '
         endif
      endif

      if (bkg_smth_input .eq. 2) then
         if(no_points_left .le. 0) then
            no_points_left = 1
            write(6, *) '%%warning: invalid left_points reset to 1 '
         endif
         if (no_points_right .le. 0) then
            no_points_right = 1
            write(6, *) '%%warning: invalid right_points reset to 1 '
         endif
         if (degree_of_polynom .lt. 0) then
            degree_of_polynom = 0
            write(6, *) '%%warning: invalid degree_of_polynom reset ', 
     1           'to 0 '
         endif
      endif

c--how many subgroups propagate?

         num_grps_to_go = num_of_det_groups - num_grps_to_miss
         if (print_level .eq. 2) then
            write(6,*) ' num_grps_to_go      :',num_grps_to_go
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

      if ((data_type .ne. 'v') .or. (data_type .ne. 'V')) then
         vanthreshold = -1.0
      endif
      if (num_bkg_sets .eq. 0) then
         bkgthreshold = -1.0
      endif

      return
      end



C--------------------------------------------------------




