c Parameters and variable definitions for prepgsas.

        parameter (iout = 17)

c Integer scalars

        integer   num_data_sets,
     1            num_bkg_sets,
     1            dpl,dfl,dfo,
     1            num_of_det_groups,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr,
     1            bkg_smth_input,
     1            no_points_left,
     1            no_points_right,
     1            degree_of_polynom
	integer   num_grps_to_go
	integer   num_chan

c Integer arrays

        integer   run_numbers(n_runs),
     1            bkg_numbers(n_runs),
     1		  missed_grps(n_banks),
     1		  adt_corr_grps(n_banks),
     1		  mlt_corr_grps(n_banks),
     1	          bank_id(n_banks)

c Floating point scalars

	real      dqfit,
     1            fitreg,
     1		  v_brg_pk_wdth,
     1            vanthreshold,
     1            bkgthreshold
	real	  dist,
     1            twotht,
     1            xf
        real*8    difc,difa,tzero
	real      source_to_sample

c Floating point arrays

	real      weight_data(n_runs)
	real      weight_bkg(n_runs)
	real      add_to_data(n_banks),
     1		  add_to_bkg(n_banks),
     1		  mult_data(n_banks),
     1		  mult_bkg(n_banks)
	real	  data(n_chn),
     1	          data1(n_chn),
     1            bkg_data(n_chn),
     1            bkg_data1(n_chn),
     1            lambda(n_chn),
     1            Q(n_chn),
     1            tof(n_chn),
     1            data_err(n_chn),
     1            data1_err(n_chn),
     1            bkg_data_err(n_chn),
     1            bkg_data1_err(n_chn)
        real      monitor,
     1            monitor_err


c Character strings

        character data_path*255,
     1		  data_file(n_runs)*80,
     1		  bkg_file(n_runs)*80,
     1            iparm_file*80,
     1		  out_file*(80),
     1            instr_name*4,
     1            data_type*1
	character run_title*80,
     1            user_name*80

c Logicals

	logical    lmiss,lvan,lcan,lbgd


c Common blocks
        common/control_inputs/ 
     1            num_data_sets,num_bkg_sets,dpl,dfl,dfo,
     1            num_of_det_groups,print_level,num_grps_to_miss,
     1            num_grps_addtve_corr,num_grps_mult_corr,
     1            run_numbers,bkg_numbers,missed_grps,adt_corr_grps,
     1		  mlt_corr_grps,dqfit,fitreg,v_brg_pk_wdth,
     1            no_points_left,no_points_right,degree_of_polynom,
     1            add_to_data,add_to_bkg,mult_data,mult_bkg,
     1            data_path,data_file,bkg_file,out_file,iparm_file,
     1            instr_name,data_type,bkg_smth_input,vanthreshold,
     1            bkgthreshold,bank_id

	common/init/
     1            lvan,lcan,lbgd,num_grps_to_go

	common/readdata/ 
     1             run_title,user_name,source_to_sample,twotht,
     1             dist,xf,difc,difa,tzero,num_chan,data,data1,
     1             bkg_data,bkg_data1,
     1             lambda,Q,tof,lmiss,data_err,bkg_data_err,
     1             monitor, monitor_err,weight_data,weight_bkg

