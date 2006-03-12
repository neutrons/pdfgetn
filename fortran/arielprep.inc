C parameters and variable definitions for usepdprep
C
C simon billinge 6/99
C
C $Id: arielprep.inc,v 1.1 2006/03/12 09:59:24 tproffen Exp $
C
C $Log: arielprep.inc,v $
C Revision 1.1  2006/03/12 09:59:24  tproffen
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
     1            num_of_det_groups,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr,
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
     1		  mult_bkg(n_banks)

c--character strings
        character data_path*255,
     1		  data_file(n_runs)*80,
     1		  bkg_file(n_runs)*80,
     1		  out_file*(80),
     1            instr_name*4,
     1            data_type*1

c--logicals

c---------------------------------------------------------------
c--initialize()
c---------------------------------------------------------------

	logical    lvan,lcan,lbgd
	integer    num_grps_to_go

	real	   dist,
     1             twotht,
     1             xf
c---------------
c- read data
c---------------

	character  ipns_runfile*80,
     1		   run_title*80,
     1             user_name*80

	real       source_to_sample


C-------------------------------
C read a single detector bank
C-------------------------------

	logical    lmiss

	integer    num_chan

	real	   data(n_chn),
     1	           data1(n_chn),
     1             bkg_data(n_chn),
     1             bkg_data1(n_chn),
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
     1            num_of_det_groups,
     1		  print_level,
     1		  num_grps_to_miss,
     1            num_grps_addtve_corr,
     1            num_grps_mult_corr,
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
     1            add_to_data,
     1		  add_to_bkg,
     1		  mult_data,
     1		  mult_bkg,
     1            data_path,
     1		  data_file,
     1		  bkg_file,
     1		  out_file,
     1            instr_name,
     1            data_type,
     1            bkg_smth_input,
     1            vanthreshold,
     1            bkgthreshold


	common/init/lvan,lcan,lbgd,
     1             num_grps_to_go

	common/readdata/ ipns_runfile,
     1		   run_title,
     1             user_name,
     1             source_to_sample

	common/cgroup/twotht,
     1             dist,
     1             xf


	common/readbank/num_chan,
     1             data,
     1             data1,
     1             bkg_data,
     1             bkg_data1,
     1             lambda,
     1             Q,
     1             lmiss,
     1             data_err,
     1             bkg_data_err

