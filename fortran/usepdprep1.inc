C parameters and variable definitions for usepdprep
C
C simon billinge 6/99
C
C $Id: usepdprep1.inc,v 1.1 2006/03/12 09:59:25 tproffen Exp $
C
C $Log: usepdprep1.inc,v $
C Revision 1.1  2006/03/12 09:59:25  tproffen
C Initial revision
C
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
C--------------
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
C--------------
C declare control input variables (read from stdinp)
C--------------

c--unit numbers for files

	parameter (iout = 17)

c--integer scalars
        integer   num_data_sets,
     1            num_bkg_sets,
     1            dpl,dfl,dfo,	!len of strngs for prty output
     1            first_monitor_num,
     1            second_monitor_num,
     1		  hist_num,
     1            num_of_det_groups,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr,
     1		  delayed_neutron_flag,
     1		  num_calib_entries,
     1            bkg_smth_input,
     1            no_points_left,
     1            no_points_right,
     1            degree_of_polynom

c--integer arrays
        integer   run_numbers(n_runs),
     1            bkg_numbers(n_runs),
     1		  missed_grps(n_banks),
     1		  adt_corr_grps(n_banks),
     1		  mlt_corr_grps(n_banks)

c--floating point scalars
	real      dqfit,
     1            fitreg,
     1		  v_brg_pk_wdth,
     1            vanthreshold,
     1            bkgthreshold


c--floating point arrays
	real      add_to_data(n_banks),
     1		  add_to_bkg(n_banks),
     1		  mult_data(n_banks),
     1		  mult_bkg(n_banks),
     1		  bnk_angle(n_banks),
     1		  calib_difc(n_banks),
     1		  calib_difa(n_banks),
     1 		  calib_t0(n_banks),
     1            dly_ntrn_corr(5)

c--character strings
        character data_path*255,
     1		  data_file(n_runs)*80,
     1		  bkg_file(n_runs)*80,
     1		  out_file*(80),
     1            instr_name*4,
     1            data_type*1,
     1            dummy*80

c--logicals

c---------------------------------------------------------------
c--initialize()
c---------------------------------------------------------------

	logical    lvan,lcan,lbgd
	integer    num_grps_to_go
	integer    nd(n_banks)

	real  	   dangle,
     1             pi,
     1             fmon,
     1		   amon,
     1		   facmon,
     1		   pdet,
     1		   rdet,
     1             dendet

	real	   dist(n_banks),
     1		   twotht(n_banks)
c---------------
c- read data
c---------------

	character  ipns_runfile*80,
     1		   run_title*80,
     1             user_name*80

	integer    num_det,
     1		   num_of_histograms,
     1		   extracted_histogram,
     1		   bank_extracted(n_banks),
     1		   detect_map_table_size,
     1		   num_of_time_fields

	integer    det_address(n_det),
     1		   group_label(n_det),
     1		   more_hist_bit(n_det),
     1		   t_len(n_banks),
     1		   num_ch_bit(n_banks),
     1		   time_focus(n_banks),
     1             dummy_int_array(n_det)

	real       source_to_sample

	real       detector_angle(n_det),
     1             sec_flight_path(n_det),
     1		   t_min(n_banks),
     1		   t_max(n_banks),
     1		   t_step(n_banks)

     
c-------------------------------
c group detectors into banks
c-------------------------------
	integer    jm1,jm2


C-------------------------------
C read a single detector bank
C-------------------------------

	logical    lmiss

	integer    num_chan

	real	   data(n_chn),
     1	           data1(n_chn),
     1             bkg_data(n_chn),
     1             bkg_data1(n_chn),
     1             data_norm_factor,
     1             data_norm_factor_err,
     1             bkg_norm_factor,
     1             bkg_norm_factor_err,
     1             lambda(n_chn),
     1             Q(n_chn),
     1             data_err(n_chn),
     1             data1_err(n_chn),
     1             bkg_data_err(n_chn),
     1             bkg_data1_err(n_chn)


C---------------
C--common blocks
C---------------
        common/control_inputs/ 
     1            num_data_sets,
     1            num_bkg_sets,
     1            dpl,dfl,dfo,	
     1            first_monitor_num,
     1            second_monitor_num,
     1		  hist_num,
     1            num_of_det_groups,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr,
     1		  num_calib_entries,
     1            run_numbers,
     1            bkg_numbers,
     1		  missed_grps,
     1		  adt_corr_grps,
     1		  mlt_corr_grps,
     1            dqfit,
     1            fitreg,
     1		  v_brg_pk_wdth,
     1            no_points_left,
     1            no_points_right,
     1            degree_of_polynom,
     1            delayed_neutron_flag,
     1            dly_ntrn_corr,
     1            add_to_data,
     1		  add_to_bkg,
     1		  mult_data,
     1		  mult_bkg,
     1		  bnk_angle,
     1		  calib_difc,
     1		  calib_difa,
     1 		  calib_t0,
     1            data_path,
     1		  data_file,
     1		  bkg_file,
     1		  out_file,
     1            instr_name,
     1            data_type,
     1            dummy,
     1            bkg_smth_input,
     1            vanthreshold,
     1            bkgthreshold

	common/init/lvan,lcan,lbgd,
     1             num_grps_to_go,
     1  	   dangle,
     1             pi,
     1             fmon,
     1		   amon,
     1		   facmon,
     1		   pdet,
     1		   rdet,
     1             dendet

	common/readdata/ ipns_runfile,
     1		   run_title,
     1             user_name,
     1             num_det,
     1		   num_of_histograms,
     1		   extracted_histogram,
     1		   dummy_int_array,
     1		   detect_map_table_size,
     1		   num_of_time_fields,
     1             det_address,
     1		   group_label,
     1		   more_hist_bit,
     1		   t_len,
     1		   num_ch_bit,
     1		   time_focus,
     1             source_to_sample,
     1             detector_angle,
     1             sec_flight_path,
     1		   t_min,
     1		   t_max,
     1		   t_step

	common/cgroup/jm1,jm2,
     1             nd,
     1             twotht,
     1             dist


	common/readbank/num_chan,
     1             data,
     1             data1,
     1             bkg_data,
     1             bkg_data1,
     1             data_norm_factor,
     1             data_norm_factor_err,
     1             bkg_norm_factor,
     1             bkg_norm_factor_err,
     1             lambda,
     1             Q,
     1             lmiss,
     1             data_err,
     1             bkg_data_err

