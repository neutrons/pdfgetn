/***************************************************************************/
/* Print some basic information from SEPD/IPNS binaru runfiles             */
/*-------------------------------------------------------------------------*/
/* (c) Thomas Proffen, 2000                                                */
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

  char tit[200];

  /**************************** Check input        *************************/

  if (argc < 2) 
    { printf("Usage: sepdinfo file1.run file2.run ...\n"); 
      exit(1); }

  while (*++argv != NULL) {

    /************************ Open files         ***************************/

    InitFileList();

    if (OpenInputFile(&fp,*argv)!=FILE_OK)
      { printf("Error opening file ...\n"); exit(1); }


    /************************ Header information ***************************/

    id = GetFileId(fp);
    strcpy(tit,file_list[id]->header.run_title);
    DropTrailingBlanks(tit);

    printf("#------------------------------------------------------------\n");
    printf("# File  : %s\n", file_list[id]->f_name); 
    printf("# Title : %s\n", tit); 
    printf("# Users : %s\n", file_list[id]->header.user_name); 
    printf("# Time  : %d pulses (%5.1f h) started %s %s\n", 
            file_list[id]->header.total_monitor_counts,
            file_list[id]->header.total_monitor_counts/108000.0,
            file_list[id]->header.start_date,
            file_list[id]->header.start_time); 

    /************************ Close files        ***************************/

    CloseFile(fp) ;
  }
}
