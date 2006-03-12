/***************************************************************************/
/* Simple BINARY to ASCII conversion tool for IPNS run files               */
/*-------------------------------------------------------------------------*/
/* (c) Thomas Proffen, 1999                                                */
/*-------------------------------------------------------------------------*/
/* To use on a big endian machine such as HP, define                       */
/* IPNS_BIG_ENDIAN.  For the VAX, ALPHA, PC, etc., comment out             */
/* the definition of IPNS_BIG_ENDIAN.                                      */
/***************************************************************************/
/*                                                                         */
/* This is part of the PDFgetN distribution written by Peter Peterson,     */
/* Matthias Gutmann, Thomas Proffen, and Simon Billinge.                   */
/*                                                                         */
/* Copyright 2000 Michigan State University Board of Trustees              */
/*                                                                         */
/* Use and distribution of this program is subject to the terms laid out   */
/* in the license in LICENSE.txt included with this distribution.  A copy  */
/* of the license  can be obtained from Michigan  State University office  */
/* of Libraries, Computing and Technology (517-353-0722).                  */
/*                                                                         */
/***************************************************************************/



/* #define IPNS_BIG_ENDIAN */
/* #define IPNS_FILEMANAGER_DEBUG */

#include "ipns.h"

main(int argc,char *argv[])

{
  FileHandleType fp ;
  FileIdType id;

  FILE *ia;
  int i,j,dot,idet,hist,ioff;
  unsigned int *data;

  /**************************** Check input        *************************/

  if (argc < 2) 
    { printf("Usage: sepdbtoa [-h #] file1.run file2.run ...\n"); 
      exit(1); }

  hist = 1;
  if (strcmp(argv[1],"-h") == 0) {
    hist = atoi(argv[2]);
    *++argv; *++argv; 
  }

  while (*++argv != NULL) {

    /************************ Open files         ***************************/

    InitFileList();

    if (OpenInputFile(&fp,*argv)!=FILE_OK)
      { printf("Error opening file ...\n"); exit(1); }

    printf ("Converting %s (histogram %d) ",*argv,hist);

    dot = strcspn(*argv,".run");
    *(*argv+dot+1)='a'; 
    *(*argv+dot+2)='s'; 
    *(*argv+dot+3)='c'; 
    printf ("to %s .. ",*argv);

    ia = fopen(*argv,"w");

    /************************ Header information ***************************/

    id = GetFileId(fp);
    fprintf(ia,"##### File name: f_name\n%s\n",
                file_list[id]->f_name); 
    fprintf(ia,"##### Run title: run_title\n%s\n",
                file_list[id]->header.run_title); 
    fprintf(ia,"##### User names: user_name\n%s\n",
                file_list[id]->header.user_name); 
    fprintf(ia,"##### Primary flight path: source_to_sample\n%f\n",
                file_list[id]->header.source_to_sample); 
    fprintf(ia,"##### Number of detectors: n_det\n%d\n",
                file_list[id]->header.n_det); 
    fprintf(ia,"##### Number of histograms: num_of_histograms\n%d\n",
                file_list[id]->header.num_of_histograms); 
    fprintf(ia,"##### Extracted histogram: \n%d\n",hist);

    /************************ Detector information *************************/

    fprintf(ia,"##### Detector: #, detector_angle, flight_path\n");
    for (i=1; i<=file_list[id]->header.n_det; i++)
      { fprintf(ia,"%4d   %21.16f   %21.16f\n",i,
                   file_list[id]->det_angle_table[i],
                   file_list[id]->flight_path_length_table[i]); }

    fprintf(ia,"##### Detector map size: detect_map_table.size\n%d\n",
                file_list[id]->header.detect_map_table.size/4);

    fprintf(ia,
        "##### Mapping: #, address, tf_type, more_hist_bit\n");
    for (i=1; i<=file_list[id]->header.detect_map_table.size/4; i++)
      {  fprintf(ia,"%4u   %16u  %16u  %16u\n",i,
                    file_list[id]->detector_map[i].address,
                    file_list[id]->detector_map[i].tf_type,
                    file_list[id]->detector_map[i].more_hist_bit); }

    fprintf(ia,"##### Number of time fields: header.num_of_time_fields\n%d\n",
                file_list[id]->header.num_of_time_fields);

    fprintf(ia,
    "##### Timing: #, t_min, t_max, t_step, t_len, num_ch, time_focus\n");
    for (i=1; i<=file_list[id]->header.num_of_time_fields; i++)
      {  fprintf(ia,"%4d  %11.4f %11.4f %11.4f  %8d   %2d  %2d\n",i,
                    file_list[id]->time_field[i].t_min,
                    file_list[id]->time_field[i].t_max,
                    file_list[id]->time_field[i].t_step,
                    file_list[id]->time_field[i].t_double_len,
                    file_list[id]->time_field[i].num_of_channels,
                    file_list[id]->time_field[i].time_focus_bit); }

    /************************ Finally getting data *************************/

    /* The desired histogram is the 'hist' group of 'n_det' detectors      */

    ioff = (hist-1)*file_list[id]->header.n_det;

    for (i=file_list[id]->header.num_of_time_fields; i>=1; i--)
      {  idet = -1 ;
         for (j=1; j<=file_list[id]->header.n_det; j++)
           { if (file_list[id]->detector_map[j+ioff].tf_type == i)
	        { idet = j; }}

         if (idet != -1 && !IsMonitorChannel(fp,idet))
           {  fprintf(ia,
              "##### Data: group, histogram, # channels \n%3d  %3d   %10d\n",
              i,hist,NumberOfTimeChannels(fp,idet,hist));

              data = malloc(sizeof(int)*NumberOfTimeChannels(fp,idet,hist));
              if (GetSpectrum32(fp,idet,hist,data)==DATA_ERROR) 
                { printf("Error reading spectrum for detector %d \n",idet); 
                  exit(1);
                }

              fprintf(ia,"##### Start data \n");
              for (j=1; j<=NumberOfTimeChannels(fp,idet,hist); j++)
                {  fprintf (ia,"%5d  ",data[j-1]);
                   if (j%10 == 0) {fprintf(ia,"\n"); }}
              if ((j-1)%10 != 0) {fprintf(ia,"\n"); }
           }
       }

    /************************ Close files        ***************************/

    fclose(ia);
    CloseFile(fp) ;

    printf ("done ..\n");
  }
}
