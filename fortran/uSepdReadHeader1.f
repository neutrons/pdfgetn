c---------------------------------------------------------------------------
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
c---------------------------------------------------------------------------
      subroutine uSepdReadHeader1(local_data_file, local_out_file)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Reads the header of the data file, but not the data. Info from
C the header will be used to group the detectors, calculate the wavelengths
C etc.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'
      include 'usepdprep1.inc'

      character*(*) local_data_file,
     1              local_out_file


      call how_long1(dfo, local_out_file)
      open(23,file=local_data_file,status='old',err=999)
      write(6, *) ' '
      write(6, *) '-----------------------------------------'
      write(6,*) '%%info: getting header information '
      write(6,*) ' '

      read(23,'(a80)') dummy      
      read(23,'(a80)') ipns_runfile

      read(23,'(a80)') dummy
      read(23,'(a80)') run_title
      call how_long1(dfo, run_title)
      write(6,*) '%%info: run_title          : ', run_title(1:dfo)

      read(23,'(a80)') dummy
      read(23,'(a80)') user_name
      call how_long1(dfo, user_name)
      write(6,*) '%%info: user_name          : ', 
     1     user_name(1:dfo)

      read(23,'(a80)') dummy
      read(23,*) source_to_sample
      write(6,*) '%%info: source_to_sample   : ', 
     1     source_to_sample

      read(23,'(a80)') dummy
      read(23,*) num_det
      write(6,*) '%%info: num_det            : ', num_det

      read(23,'(a80)') dummy
      read(23,*) num_of_histograms
      write(6,*) '%%info: num_of_histograms  : ', 
     1     num_of_histograms

      read(23,'(a80)') dummy
      read(23,*) extracted_histogram
      write(6,*) '%%info: extracted_histogram: ', 
     1     extracted_histogram


      read(23,'(a80)') dummy
      read(23,*) (dummy_int_array(i),
     1              detector_angle(i),
     1              sec_flight_path(i),
     1              i=1,num_det)

      read(23,'(a80)') dummy
      read(23,*) detect_map_table_size

      read(23,'(a80)') dummy
      read(23,*) (dummy_int_array(i),
     1              det_address(i),
     1              group_label(i),
     1              more_hist_bit(i),
     1              i=1,detect_map_table_size)

      read(23,'(a80)') dummy
      read(23,*) num_of_time_fields

      read(23,'(a80)') dummy
      read(23,*) (dummy_int_array(i),
     1     t_min(i),
     1     t_max(i),
     1     t_step(i),
     1     t_len(i),
     1     num_ch_bit(i),
     1     time_focus(i),
     1     i=1,num_of_time_fields)
      
      close(23)
      return

 999  stop '%%%severe: error opening data file (read header)'
      end

