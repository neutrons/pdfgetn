#include <string.h>
#include <stdio.h>

/*++------------------------ IPNS_FileManager.h ----------------------------

IPNS_FileManager.h

Manages a collection of IPNS run files.


Programmer     Date      Modification
___________  ________  _______________________________________
J Hammonds             Initial Version.

D Szivulka   12/03/93  Modified program to work on alpha's.
                       ( NOTE: there is still a problem with the real data
                               values on Alpha/OSF1 due to different real
                               formats. DJM )

D Szivulka   01/02/94  Converted to resemble VFileManager.h & .c
                       routines.


D Mikkelson  06/16/94  Made local routines "static"
                       Added routines to access 2 D data sets.

D Mikkelson  08/04/94  Added routines to get min and max TOF values.

D Mikkelson  05/25/95  Added routine to get incident energy and flight path
                       for spectrometers:

                               GetSpectrometerInEnergy
                               GetFinalFlightPath

                       NOTE: GetFinalFlightPath only returns the final flight
                             path length from the data file.  If time focusing
                             was used for a SPECTROMETER, the effective 
                             path length can be retrieved using 
                             GetSpectrometerFinalFlightPath. 

                       Changed name on routine to get total flight path
                       length to:

                               GetTotalFlightPath

D Mikkelson  05/26/95  Added routine to get min/max time interval for 
                       spectrometers.  This includes corrections to the times
                       to account for "time focusing".

D Mikkelson  05/26/95  Added routine to convert from VAX format of floating
                       point values to the "native" floating point format of
                       the machine on which this is compiled.

                               ConvertVaxFloat

D Mikkelson  05/27/95  Changed routine GetMinAndMaxTOF to return a status of
                       "DataResultType" for consistency with the other "Get"
                       routines.

D Mikkelson  05/27/95  Added routines to access time field type information:

                            NumberOfTimeFieldTypes
                            GetSpectrometerTimeFieldData
                            GetTimeFieldType
J Hammonds,
D Mikkelson  05/25/95  Added routines to convert data to a BIG ENDIAN machine

                            ConvertShorttoBigEndian
                            ConvertLongtoBigEndian

                       To use this unit on a big endian machine such as HP,
                       define the constant IPNS_BIG_ENDIAN at the start of file 
                       IPNS_FileManager.c

D Mikkelson  07/25/95  Added routines to get the character strings identifying
                       the user, title, date, etc. from the run file.

                             GetUserName
                             GetRunTitle
                             GetRunNum
                             GetNextRun
                             GetStartDate
                             GetStartTime
                             GetEndDate
                             GetEndTime

D Mikkelson  07/31/95  Added routine to get time field data for DIFFRACTOMETER 
                       instruments.
 
                            GetDiffractometerTimeFieldData

D Mikkelson  08/15/95  Added routine to get the IPNS instrument name. 

                            IPNS_Diffractometer  
                          ( changed to IPNS_Instrument, 5/9/97 ) 

                       Currently this deduces the instrument name from the
                       data file name assuming that the data file name is
                       an eight letter file name with the initial three 
                       letters being:

                              GLA,
                              GPP,
                              HRC,
                              LRC,
                              QEN,
                              SAN,
                              SEP,
                              SCD,

D Mikkelson  08/15/95  Added routine to get final flight path for HRMECS and
                       LRMECS adjusted for time focusing. 

                            GetSpectrometerFinalFlightPath
*/

/* 
 * $Log: ipns.h,v $
 * Revision 1.1  2006/03/12 09:59:37  tproffen
 * Initial revision
 *
 * Revision 2.0  1997/09/13 17:46:08  hammonds
 * Change to new rev
 *
Have added support for retrieving hardware time field info with
GetHardwareTimeFieldData and suport for getting at run monitoring
information with
PresetMonitorCounts
PresetCycleLimit
CompletedCycles
ElapsedMonitorCounts
 * Revision 1.1  1997/09/13 17:22:35  hammonds
 * Initial revision
 *
 * Revision 1.11  1997/05/23 15:39:49  dennis
 * Added routines to "Pause" and "Resume" use of a file.
 *
 * Revision 1.10  1997/05/20 20:02:25  dennis
 * Added routines for GLAD run files:
 * int             NumberOfBanks          ( file_handle )
 * int             NumberOfDetectorsInBank( file_handle, bank )
 * int             LPSD_SpecID            ( file_handle, bank, detector, element )
 *
 * Revision 1.9  1996/12/09 21:12:20  dennis
 * Replaced "powder" with "diffractometer"  and
 *          "chopper" with "spectrometer"
 *
 * Revision 1.8  1996/05/31 15:09:01  dennis
 * Added GetFileName routine
 *
 * Revision 1.7  1996/05/23 14:14:09  dennis
 * Removed redundant Function heading from documentation.
 *
 * Revision 1.6  1996/05/22 22:46:09  dennis
 * Removed GetOverflows function from header, since it should be private
 * to the IPNS_FileManager unit.
 *
 * Revision 1.4  1996/05/20 16:48:33  dennis
 * added RCS logging
*/

/*

----------------------------------------------------------------------------
Contains Functions:

                InitFileList();
FileResultType  OpenInputFile      ( *file_handle_ptr,  file_name   )
FileResultType  CloseFile          ( *file_handle_ptr               )
IPNSMachineType IPNS_Instrument(  file_handle                   )

int             NumberOfDetectors ( file_handle )
int             NumberOfHistograms( file_handle )
int             NumberOfBanks          ( file_handle )
int		SizeOfDataArea ( file_handle )
int		HistStartAddress ( file_handle )
int		NextFreeBlock ( file_handle )
int		TotalNumberOfChannels ( file_handle )
int             NumberOfDetectorsInBank( file_handle, bank )
int             LPSD_SpecID            ( file_handle, bank, detector, element )

int             NumberOfTimeChannels( file_handle, det_num, hist_num)
int             NumberOfTimeFieldTypes( file_handle )
int		PresetMonitorCounts( file_handle )
int		PresetCycleLimit (file_handle )
int		CompletedCycles( file_handle )
int		ElapsedMonitorCounts( file_handle )

DataResultType  GetFileName ( file_handle, *f_name  ) 
DataResultType  GetUserName ( file_handle, *user_name  ) 
DataResultType  GetRunTitle ( file_handle, *run_title  ) 
DataResultType  GetRunNum   ( file_handle, *run_num    ) 
DataResultType  GetNextRun  ( file_handle, *next_run   ) 
DataResultType  GetStartDate( file_handle, *start_date ) 
DataResultType  GetStartTime( file_handle, *start_time ) 
DataResultType  GetEndDate  ( file_handle, *start_date ) 
DataResultType  GetEndTime  ( file_handle, *start_time ) 

DataResultType  GetTimeFieldType      ( file_handle, det_num, hist_num, 
                                        *tf_type                        )
DataResultType  GetSpectrometerTimeFieldData  ( file_handle, tf_type,
                                               *min_TOF, *max_TOF, *n_channels )
DataResultType  GetDiffractometerTimeFieldData( file_handle, tf_type,
                                               *min_TOF, *max_TOF, *n_channels )
DataResultType  GetHardwareTimeFields ( file_handle, det_num, hist_num,
					*min_TOF, *max_TOF, *step_TOF )
DataResultType  GetMinAndMaxTOF       ( file_handle, det_num, hist_num,
                                        *min_TOF, *max_TOF              )
int             TimeFocusingUsed      ( file_handle, det_num, hist_num  )

DataResultType  GetSubgroupIDList   ( file_handle, hist_num, *subgroup  )

DataResultType  GetSpectrum16         ( file_handle, det_num, hist_num,
                                       *data_ptr)
DataResultType  GetSpectrum32         ( file_handle, det_num, hist_num,
                                       *data_ptr)
DataResultType  GetDetAngle           ( file_handle, det_num, *data_ptr )
DataResultType  GetTotalFlightPath    ( file_handle, det_num, *data_ptr )
DataResultType  GetFinalFlightPath    ( file_handle, det_num, *data_ptr )
DataResultType  GetSpectrometerFinalFlightPath( file_handle, det_num, hist_num, 
                                               *data_ptr )
DataResultType  GetDetHeight        ( file_handle, det_num, *data_ptr )
DataResultType  GetDetDiscLevels    ( file_handle, det_num, *lower, *upper )
DataResultType  GetDetSubgroupSize  ( file_handle, det_num, *data_ptr )

DataResultType  GetSpectrometerInEnergy     ( file_handle, *data_ptr )

FileContentsType  GetFileDataType( file_handle )
DataResultType    Get2DDataSizes ( file_handle, *nx, *ny, *n_wavelengths )
DataResultType    Get2DDataSlice ( file_handle, slice_num, hist_num, *data_ptr)
DataResultType    GetDetStartAddress ( file_handle, det_num, hist_num, 
					*start_addr )

------------------------------------------------------------------------------

How to use IPNS_FileManager:

1) Initialize the file list by calling InitFileList.

2) Open an input file by calling OpenInputFile.
   You can open as many as MAX_NUM_FILES at one time.

3) Use the file handle obtained in 2) to call the other procedures.

4) Close the file by using the function CloseFile when you are done
   with it.


*--
***************************************************************************/

/*--------------------------- Interface section -------------------------*/

/* NOTE: to test the internal mechanisms of this module, use the following
         define statement and recompile this unit together with 

                  IPNS_FileManagerInternalsTestSubs.c and 
                  IPNS_FileManagerInternalsTest.c

         For normal use, comment out the following define statement !!!!

#define Test_IPNS_Filemanager_Internals  1
*/

/* --------------------------------------------------------------------------

  PUBLIC CONSTANTS
 
*/

#define MAX_NUM_FILES               5
#define MAX_STR_LENGTH              80

#define HRMECS_FOCUSED_PATH_LENGTH  4.0
#define LRMECS_FOCUSED_PATH_LENGTH  2.5

/* -------------------------------------------------------------------------

  PUBLIC TYPES

*/

typedef enum  {
               FILE_OK,
               FILE_CANNOT_BE_OPENED,
               FILE_OPEN,
               FILE_OPEN_FOR_INPUT,
               FILE_OPEN_FOR_OUTPUT,
               FILE_NOT_OPEN,
               FILE_PAUSED,
               FILE_TOO_MANY_OPEN_FILES,
               FILE_VALUE_PTR_INVALID,
               FILE_POSITION_INVALID
              } FileResultType;

typedef enum  {
               DATA_ERROR,
               DATA_OK
              } DataResultType;

typedef enum  {
                NO_DATA_FILE,
                ONE_DIMENSIONAL_DATA,
                TWO_DIMENSIONAL_DATA
              } FileContentsType;

typedef void *FileHandleType;

typedef char *StringType;

typedef enum {
               IPNS_GLAD,
               IPNS_GPPD,
               IPNS_HRMECS,
               IPNS_LRMECS,
               IPNS_QENS,
               IPNS_SAND,
               IPNS_SEPD,
               IPNS_SCD,
               IPNS_UNKNOWN 
             } IPNSMachineType;

/******************** Function Prototypes and Explanations ********************
*******************************************************************************
******************************************************************************/


/*++---------------------------- InitFileList --------------------------------

Effect:
        To initialize the file list, and check that the MAX_NUM_FILES is 
 less than FOPEN_MAX( the maximum number of files allowed open at one time
 by the current system). You must initialize the file list before any of the 
 other functions can be used.

Input:
        None.

Return Value:
        None.

*--
*/

void InitFileList();


/*++------------------------ OpenInputFile ------------------------------------

Effect:
          To open a file named (file_name) so that the user can read from
it. If the file can be opened the file_handle will not be NULL and the 
FileResultType will be FILE_OK. If a bad file name is given the FileResult
will be FILE_CANNOT_BE_OPENED. If too many files are already open the 
FileResult will be FILE_TOO_MANY_OPEN_FILES. If the file is already open
for writing then the FileResult will be FILE_OPEN_FOR_OUTPUT. String lengths
can only be as long as MAX_STR_LENGTH characters at which point the string
will be truncated and the resulting string will be used as the file_name.
Also if the string isn't terminated with a '\0' the name will end up to be
MAX_STR_LENGTH long and you will not get the right file.

Input:
          A file name.

Return Value:
         FileResult                 Output
         _________________________  _____________________________________
         FILE_OK                    Pointer to a good file handle.
         FILE_CANNOT_BE_OPENED      Pointer to a NULL file handle.
         FILE_TOO_MANY_OPEN_FILES   Pointer to a NULL file handle.
         FILE_OPEN_FOR_OUTPUT       Pointer to a NULL file hadnle.

*--
*/

FileResultType  OpenInputFile( FileHandleType  *file_handle_ptr,
                               StringType       file_name        );


/*++------------------------- CloseFile ---------------------------------------

Effect:
         Close the file associated with the file_handle pointed to by file
     handle_ptr. If FileResult is FILE_OK the file was closed, otherwise the 
     FileResult will be FILE_NOT_OPEN if the file handle was no longer active.

Input:
        Pointer to a file_handle.

Return Value:
        FileResult.

*--
*/

FileResultType CloseFile( FileHandleType  *file_handle_ptr );



/*++------------------------- PauseFile ---------------------------------------

Effect:
        Keep all of the initial data structures for the data file active,
        but close the file on disk temporarily.  This is useful because some
        systems will prevent the file from being updated by the data 
        acquisition software if the file is left open, even for reading only. 
        USE THIS CAREFULLY.  THE CALLING PROGRAM IS RESPONSIBLE FOR CALLING
        "ResumeFile" BEFORE ANY FURTHER USE IS MADE OF THE DATA FILE!!
Input:
        A file_handle.

Return Value:
        FileResult.
*--
*/

FileResultType PauseFile(FileHandleType file_handle);



/*++------------------------- ResumeFile ---------------------------------------

Effect:
        Reopen a file that was temporarily closed using "PauseFile".
Input:
        A file_handle.

Return Value:
        FileResult.
*--
*/

FileResultType ResumeFile(FileHandleType file_handle);



/*++------------------------- IPNS_Instrument ----------------------------

Effect:
       Try to determine the IPNS instrument corresponding to a file that was
    opened using OpenFile from this unit.

Input:
        The file_handle returned when the file was opened using OpenFile.

Return Value:
        The machine name for the specified file.  If the machine type cannot
     be determined, this returns "IPNS_UNKNOWN".

*--
*/

IPNSMachineType IPNS_Instrument( FileHandleType  file_handle );




/*++----------------------- NumberOfDetectors ---------------------------------

Effect:
        This function returns the number of detectors associated with 
 the runfile that is pointed to by file_handle. The function will return
 zero when there is a problem retreiving the number of detectors for the
 file_handle given.

Input:
       file_handle.

Return Value:
       The number of detectors.

*--
*/

int NumberOfDetectors( FileHandleType file_handle );


/*++----------------------- NumberOfHistograms ---------------------------------

Effect:
        This function returns the number of histograms associated with 
 the runfile that is pointed to by file_handle. The function will return
 zero when there is a problem retreiving the number of histograms for the
 file_handle given.

Input:
       file_handle.

Return Value:
       The number of histograms.

*--
*/

int NumberOfHistograms( FileHandleType file_handle );


/*++-------------------------- NumberOfBanks -------------------------------

Effect:
        This function returns the number of banks of LPSD's present in the
 run file. 

Input:
       file_handle.

Return Value:
       The number of banks of LPSDs in this run file.
*--
*/

int NumberOfBanks(FileHandleType file_handle );

/*++--------------------- SizeOfDataArea -------------------------------------
 
Effect:
        This function returns the size of the data area in this file.
 
Input:
        file_handle
 
Return Value:
        The size of the data area in this file.
 
*--
*/
 
int SizeOfDataArea ( FileHandleType file_handle );

/*++--------------------- HistStartAddress ----------------------------------
 
Effect:
        This function returns the location of the start of the data area in
        this file.
 
Input:
        file_handle
 
Return Value:
        The Byte offset to the start of the data area in this file.
 
*--
*/
 
int HistStartAddress ( FileHandleType file_handle );

/*++--------------------- NextFreeBlock -------------------------------------
 
Effect:
        This function returns the location of the start of the next free block
        in this file.
 
Input:
        file_handle
 
Return Value:
        The Byte offset to the next free block in this file.
 
*--
*/
 
int NextFreeBlock ( FileHandleType file_handle );

/*++--------------------- TotalNumberOfChannels -----------------------------
 
Effect:
        This function returns the total data channels
        in this file.
 
Input:
        file_handle
 
Return Value:
        The total number of data channels in this file.
 
*--
*/
 
int TotalNumberOfChannels ( FileHandleType file_handle );

/*++--------------------- NumberOfDetectorsInBank ----------------------------

Effect:
        This function returns the number of detectors in a particular bank
 of Linear Position Sensitive detectors. 

Input:
       file_handle   
       bank         

Return Value:
       The number of detectors in the specifed bank.
*--
*/

int NumberOfDetectorsInBank(FileHandleType file_handle, int bank );


/*++--------------------------- LPSD_SpecID --------------------------------

Effect:
        This function returns the spectrum "ID" of a particular element of
  a particular detector in a specified bank of LPSDs.  The spectrum ID can
  then be used to access the spectrum, angle, flight path, etc.

Input:
       file_handle.

Return Value:
       The id of the spectrum corresponding to the specified bank, detector
  and element.

*--
*/

int LPSD_SpecID( FileHandleType file_handle, 
                 int            bank,
                 int            detector,
                 int            element );


/*++-------------------------- GetCrateInfo -------------------------------

Effect:
      This function gets the crate, slot and input corresponding to a 
  particular LPSD in a particular bank.

Input:
      file_handle   
      bank         
      detector

Output:
      crate         The crate number for this detector
      slot          The slot number of this detector
      input         The input number of this detector

Return Value:
      Returns TRUE if the information was available. 
*--
*/
int GetCrateInfo(FileHandleType   file_handle, 
                 int              bank, 
                 int              detector,
                 int             *crate,
                 int             *slot,
                 int             *input   );


/*++----------------------- NumberOfTimeChannels ------------------------------

Effect:
        This function returns the number of time channels associated with 
 the runfile that is pointed to by file_handle and the particular 
 detector(det_num) and histogram(hist_num). The function will return
 zero when there is a problem retreiving the number of timechannels for the
 file_handle given.

Input:
       file_handle.

Return Value:
       The number of time channels for the particular detector and 
   histogram numbers given.

*--
*/

int NumberOfTimeChannels( FileHandleType  file_handle, 
                          int             det_num,
                          int             hist_num);



/*++--------------------- NumberOfTimeFieldTypes ----------------------------

Effect:
      This function returns the number of "time field types" in this file.
 Specifically, different spectra could be based on different time ranges and
 different time bin sizes.  Since typically there are only a few different
 time ranges and bin sizes used for a large number of detectors, only one
 copy of the time channel information is needed for each "time field type".
   The function will return zero when there is a problem retreiving the 
 information for the file_handle given.

Input:
       file_handle.

Return Value:
       The number of time field types for this file.

*--
*/

int NumberOfTimeFieldTypes( FileHandleType file_handle );


/*++----------------------------- PresetMonitorCounts ---------------------
 
Effect:
     This function returns the number of preset monitor counts.  This number
determines the duration of a cycle during acquisition.  At the end of each
cycle of this many counts, data is saved and the number of preset cycles is
checked to see if the run is completed.
 
Input:
      file_handle.
Return Value:
      The Number of preset monitor counts per cycle.
 
*--
*/
 
int PresetMonitorCounts( FileHandleType file_handle );
 
/*++----------------------------- PresetCycleLimit ---------------------
 
Effect:
     This function returns the number of preset cycles in a run.  This number,
combined with the preset monitor counts determines the duration of a run.  At
the end of each cycle, data is saved and the number of preset cycles is
checked to see if the run is completed.
 
Input:
      file_handle.
Return Value:
      The Number of preset cycles for this run.
 
*--
*/
 
int PresetCycleLimit( FileHandleType file_handle );
 
/*++----------------------------- CompletedCycles ---------------------
 
Effect:
     This function returns the number of completed cycles.  This number
is the actual number that were collected.  This may differ from the preset
if the run aborted due to error or if the run was manually stopped before
the preset time had elapsed.
 
Input:
      file_handle.
Return Value:
      The Number of cycles completed so far in this run.
 
*--
*/
 
int PresetCyclesCompleted( FileHandleType file_handle );
 
/*++----------------------------- ElapsedMonitorCounts ---------------------
 
Effect:
     This function returns the total number of elapsed monitor counts.
 
Input:
      file_handle.
Return Value:
      The Number of total elapsed monitor counts for this run.
 
*--
*/
 
int ElapsedMonitorCounts( FileHandleType file_handle );

/*++----------------------------- GetFileName -----------------------------

 Effect:

   Get the file name of an open run file.

 Input:
        file_handle

 Output:
        The file name is returned in the string file_name.
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to MAX_STRING_LENGTH+1 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.
*--
*/

DataResultType  GetFileName( FileHandleType  file_handle,
                             char           *f_name       );



/*++----------------------------- GetUserName -----------------------------

 Effect:

   Get the user name from the run file.   

 Input:
        file_handle

 Output:
        The user name is returned in the string user_name.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 21 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetUserName( FileHandleType  file_handle, 
                             char           *user_name     );



/*++----------------------------- GetRunTitle -----------------------------

 Effect:

   Get the run title from the run file.  

 Input:
        file_handle

 Output:
        The title is returned in the string run_title.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a 
              character string of up to 81 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetRunTitle( FileHandleType  file_handle, 
                             char           *run_title     );



/*++----------------------------- GetRunNum -----------------------------

 Effect:

   Get the run number as a character string from the run file.  

 Input:
        file_handle

 Output:
        The run number is returned in the string run_num.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 5 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetRunNum( FileHandleType  file_handle, 
                           int           *run_num      );


/*++----------------------------- GetNextRun -----------------------------

 Effect:

   Get next run number as a character string from the run file.  

 Input:
        file_handle

 Output:
        The next run number is returned in the string next_run.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 5 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetNextRun( FileHandleType  file_handle, 
                           int           *next_run      );


/*++----------------------------- GetStartDate -----------------------------

 Effect:

   Get start date from the run file.  

 Input:
        file_handle

 Output:
        The start date is returned in the string start_date.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 10 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetStartDate( FileHandleType  file_handle, 
                              char           *start_date   );


/*++----------------------------- GetStartTime -----------------------------

 Effect:

   Get start time from the run file.  

 Input:
        file_handle

 Output:
        The start time is returned in the string start_time.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 9 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetStartTime( FileHandleType  file_handle, 
                              char           *start_time   );


/*++----------------------------- GetEndDate -----------------------------

 Effect:

   Get end date from the run file.  

 Input:
        file_handle

 Output:
        The end date is returned in the string end_date.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 10 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetEndDate( FileHandleType  file_handle, 
                            char           *end_date   );




/*++----------------------------- GetEndTime -----------------------------

 Effect:

   Get end time from the run file.  

 Input:
        file_handle

 Output:
        The end time is returned in the string start_time.  
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to 9 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetEndTime( FileHandleType  file_handle,
                            char           *end_time   );





/*++---------------------- GetTimeFieldType ----------------------------------

Effect:
         This function gets the time field type for the specified detector,
 and histogram in the specified runfile.  The function returns DATA_ERROR,
 if an error occurs and DATA_OK if there is no error.

Input:
       file_handle, det_num, and hist_num.

Output:

   tf_type   The time field type for the given detector and histogram number.
             This will be a value in  [1, NumberOfTimeFieldTypes( file_handle)]
             if it is valid.  It will be set to 0 otherwise

Return Value:
       Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetTimeFieldType ( FileHandleType  file_handle,
                                  int             det_num,
                                  int             hist_num,
                                  int            *tf_type  );



/*++--------------------- GetSpectrometerTimeFieldData -----------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.
 This function checks whether or not "time focusing" has been done and corrects
 the min_TOF and max_TOF values accordingly.  This version is valid for the
 chopper instruments at IPNS.  The function will return status
 "DATA_ERROR" when there is a problem retrieving the information for the
 file_handle given and will return "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in
                     [ 1, NumberOfTimeFieldTypes(file_handle) ] 
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.
*--
*/

DataResultType  GetSpectrometerTimeFieldData( FileHandleType  file_handle, 
                                              int             tf_type,
                                              float          *min_TOF, 
                                              float          *max_TOF, 
                                              int            *n_channels );


/*++--------------------- GetTimeFieldData -----------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.
 This function checks whether or not "time focusing" has been done and corrects
 the min_TOF and max_TOF values accordingly.  This version is valid for the
 chopper instruments at IPNS.  The function will return status
 "DATA_ERROR" when there is a problem retrieving the information for the
 file_handle given and will return "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in
                     [ 1, NumberOfTimeFieldTypes(file_handle) ] 
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.
*--
*/

DataResultType  GetTimeFieldData( FileHandleType  file_handle, 
                                              int             tf_type,
                                              float          *min_TOF, 
                                              float          *max_TOF, 
                                              int            *n_channels );


/*++------------------- GetDiffractometerTimeFieldData -----------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.

 NOTE: Currently this function just returns the min and max time values from
       the data file and does not check to see if time focussing has been
       done.

  The function will return status "DATA_ERROR" when there is a problem 
  retrieving the information for the file_handle given and will return 
  "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in
                     [ 1, NumberOfTimeFieldTypes(file_handle) ]
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
       Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetDiffractometerTimeFieldData( FileHandleType  file_handle,
                                                int             tf_type,
                                                float          *min_TOF,
                                                float          *max_TOF,
                                                int            *n_channels );


/*++----------------------- GetHardwareTimeFields ------------------------
 
Effect:
        This function retrieves the detector time field information for
a particular detector.  It returns the Minimum time, maximumum time and time
step for this detector in units of clock ticks so that the detector hardware
can be setup.
 
  The function will return status "DATA_ERROR" when there is a problem
  retrieving the information for the file_handle given and will return
  "DATA_OK" if there is no problem.
 
Input:
       file_handle   Identifier for the file.
       detectorID    detector ID for which the Time field information is to be
                     returned
       hist_num      Histogram number for the specified data
Output:
 
      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given in clock ticks.
      step_TOF    the step size for this detector
*/
DataResultType GetHardwareTimeFieldData( FileHandleType file_handle,
                                         int            detectorID,
                                         int            hist_num,
                                         float            *min_TOF,
                                         float            *max_TOF,
                                         float            *step_TOF);

/*++----------------------- GetMinAndMaxTOF ------------------------------

Effect:
        This function returns the min and max time of flight for the specified
 runfile and the particular detector(det_num) and histogram(hist_num). The
 function will return a status of DATA_ERROR when there is a problem retrieving 
 the information for the file_handle given and will return a status of DATA_OK
 otherwise.

Input:
       file_handle.
       det_num
       hist_num

Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

Return Value:
       Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetMinAndMaxTOF(FileHandleType file_handle,
                                int            det_num,
                                int            hist_num,
                                float         *min_TOF,
                                float         *max_TOF     );


/*++----------------------- TimeFocusingUsed ------------------------------

Effect:
       Determine whether or not the data for the specified detector and
    histogram was "time focused".

Input:
       file_handle.
       det_num
       hist_num

Return Value:
       Returns TRUE if the data for the specified detector and histogram was
   time focused and returns false otherwise.
*--
*/

int TimeFocusingUsed( FileHandleType  file_handle,
                      int             det_num,
                      int             hist_num     );


/*++---------------------- GetSubgroupIDList --------------------------------

Effect:

    This function constructs the list of subgroup ID's for the specified file
 and histogram.  The list of subgroup ID's is constructed based on the
 addresses of the histograms in the run file.  The first group is subgroup
 number 1.  The list is stored starting at index 1, with subgroup[k] set to
 the subgroup ID for detector #k.  The list position subgroup[0] is not used.

    The function returns DATA_ERROR, if an error occurs and DATA_OK if there 
 is no error.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  The list consists of integers and position 0 is not used, 
       so the calling program must provide storage for

           1 + NumberOfDetectors( file_handle ) 

       to hold the list of subgroup ID's. 

Input:
       file_handle and hist_num.

Output:
       subgroup

       The list of subgroup ID's.  subgroup[k] will contain the subgroup ID
   for detector k. subgroup[0] is NOT USED.  

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSubgroupIDList( FileHandleType file_handle,
                                  int            hist_num,
                                  int           *subgroup  );


/*++---------------------- GetSpectrum16 ----------------------------------

Effect:
         This function gives the spectrum for the runfile associated
 with the file_handle and the particular histogram number(hist_num)
 and the particular detector number(det_num). The function returns
 DATA_ERROR, if an error occurs and DATA_OK if there is no error. If the
 counts exceed 65535, the values returned will be incorrect.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the spectrum to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       16-bit integers, so the calling program must provide

                2*NumberOfChannels( file_handle, det_num, hist_num )

       bytes of storage to hold the spectrum.

Input:
       file_handle, det_num, and hist_num.

Output:
        The spectrum.

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSpectrum( FileHandleType  file_handle,
                            int             det_num,
                            int             hist_num, 
                            void           *data_ptr  );


/*++---------------------- GetSpectrum32 ----------------------------------

Effect:
         This function gives the spectrum for the runfile associated
 with the file_handle and the particular histogram number(hist_num)
 and the particular detector number(det_num). The function returns
 DATA_ERROR, if an error occurs and DATA_OK if there is no error.  This
 routine will return counts in excess of 65535.  Specifically, the overflow
 information in the run file is used to correct the overflows.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the spectrum to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       32-bit integers, so the calling program must provide

                4*NumberOfChannels( file_handle, det_num, hist_num )

       bytes of storage to hold the spectrum.
 
Input:
       file_handle, det_num, and hist_num.

Output:
       The spectrum.

Return Value:
       Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSpectrum32(FileHandleType file_handle,
                             int            det_num,
                             int            hist_num,
                             void          *data_ptr);



/*++---------------------- GetDetAngle ----------------------------------------

Effect:
         This function will give the detector angle for detector(det_num)
 in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
      the angle of the detector in *data_ptr

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetAngle( FileHandleType  file_handle,
                            int             det_num,
                            float          *data_ptr);


 
/*++---------------------- GetTotalFlightPath -----------------------------

Effect:
         This function will give the flight path for detector(det_num)
 in the runfile associated with file_handle.  The flight path value returned
 is the total path length from the source to the detector. 

Input:
      file_handle and detector_number.

Output:
      the total flight path for the detector in *data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetTotalFlightPath( FileHandleType  file_handle,
                                   int             det_num,
                                   float          *data_ptr);


/*++---------------------- GetFinalFlightPath ------------------------------

Effect:
         This function will give the flight path for detector(det_num)
 in the runfile associated with file_handle.  The flight path value returned
 is the final path length from the sample to the detector.

Input:
      file_handle and detector_number.

Output:
      the final flight path is placed in *data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetFinalFlightPath( FileHandleType file_handle,
                                   int            det_num,
                                   float         *data_ptr);



/*++------------------- GetSpectrometerFinalFlightPath -----------------------

Effect:
      This function will give the "effective" flight path for detector(det_num)
 in the runfile associated with file_handle.  The "effective" flight path
 value returned is the final path length from the sample to the detector
 ADJUSTED FOR TIME FOCUSING.  The adjustment is only valid for the HRMECS and
 LRMECS machines at IPNS.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the "effective" flight path of the 
  detector det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSpectrometerFinalFlightPath( FileHandleType file_handle,
                                               int            det_num,
                                               int            hist_num,
                                               float         *data_ptr   );



/*++---------------------- GetDetHeight ----------------------------------------

Effect:
         This function will give the detector height for detector(det_num)
 in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the height of the detector
  det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetHeight(FileHandleType  file_handle,
                            int             det_num,
                            float          *data_ptr);


/*++---------------------- GetDetDiscLevels ----------------------------------
Effect:

Input:
	file handle and detector number

Output:
	lower - lower level discriminator
	upper - upper level discriminator

ReturnValues:
	Flag indicating DATA_OK or DATA_ERROR

*__
*/

DataResultType GetDetDiscLevels (FileHandleType file_handle,
				int det_num,
				short *lower,
				short *upper );

/*++---------------------- GetDetSubgroupSize --------------------------------

Effect:
         This function will give the detector group size for detector(det_num)
 for the FIRST HISTOGRAM in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the subgroup size of the detector
  det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetSubgroupSize(FileHandleType  file_handle,
                                  int             det_num,
                                  int            *data_ptr);



/*++----------------------- GetSpectrometerInEnergy --------------------------

Effect:

      This function will return the incident energy level for a spectrometer 
      as recorded in the specified input file.

Input:
      file_handle

Output:

      energy_in

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetSpectrometerInEnergy ( FileHandleType  file_handle,
                                          float          *energy_in    );
 
/*++------------------------ GetFileDataType --------------------------------

Effect:
         This function will indicate whether the file contains 1-Dimensional
 data corresponding to individual detector IDs, or 2-Dimensional data 
 corresponding to an area detector, organized by time channel.

Input:
      file_handle 

Output:
    
      Returns one of the following values:
                NO_DATA_FILE
                ONE_DIMENSIONAL_DATA
                TWO_DIMENSIONAL_DATA
*--
*/

FileContentsType   GetFileDataType( FileHandleType file_handle );



/*++------------------------ Get2DDataSizes --------------------------------

Effect:

      This function will return the dimensions of the data from an area
 detector.

Input:
      file_handle

Output:

      nx, ny, n_times

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  Get2DDataSizes ( FileHandleType  file_handle, 
                                 int            *nx, 
                                 int            *ny, 
                                 int            *n_wavelengths );





/*++------------------------- Get2DDataSlice -------------------------------

Effect:
         This function gets one 2-D slice of data from the runfile associated
 with the file_handle and the particular histogram number(hist_num).  The
 function returns DATA_ERROR, if an error occurs and DATA_OK if there is no
 error.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the data to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       16-bit integers, so the calling program must provide

                           2 * nx * ny

       bytes of storage to hold the slice of data.  The dimensions nx and ny
       can be found by calling Get2DDataSizes().


Input:
       file_handle, det_num, and hist_num.

Output:

       *data_ptr  The block of memory pointed to by data_ptr is filled with
                  one time slice from the area detector.  The slice is arranged
                  in row-major order with the first row representing the first
                  row across the bottom of the detector.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType Get2DDataSlice(FileHandleType file_handle,
                              int            slice_num,
                              int            hist_num,
                              void          *data_ptr);


/*++--------------------------- GetDetStartAddress --------------------------
Effect:
	This function gets the memory start address for a detector.  The 
	primary use for this function is to set the pointer used in hardware
	for the start of histogram memory for this detector.

Input:
       File_handle, det_num, and hist_num.

Output:
       *start_addr The memory start address for this detector.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetStartAddress ( FileHandleType file_handle,
				    int  det_num,
				    int hist_num,
				    long *start_addr );/*
Filename IPNS_VaxFloat_Endian_Conversions,h
Purpose  :  Interface file for routines to convert integer endian structures
            and to convert Vax Floats to IEEE format.
*/


void ConvertShorttoBigEndian(void *val_p);
void ConvertInt4toBigEndian(void *val_p);
void ConvertVaxFloat( float *val_p);

/* -------------------------------------------------------------------------

     IPNS_FileManager.c

*/

#include <stdlib.h>
#include <math.h>
#include <ctype.h>

#define Test_IPNS_FileManagerInternals

/*++----------------------- IPNS_FileManager.C -------------------------------

Programmer   Date      Modification
__________   ________  _________________________________________________
J Hammonds             Initial Version.

D Szivulka   12/03/93  Modified program to work on alpha's. 
                       ( NOTE: there is still a problem with the real data
                               values on Alpha/OSF1 due to different real
                               formats. DJM )

D Szivulka   01/02/94  Converted to resemble VFileManager.h & .c 
                       routines.

D Mikkelson  06/16/94  Made local routines "static"
                       Added routines to access 2 D data sets.

D Mikkelson  08/04/94  Added routines to get min and max TOF values.

D Mikkelson  05/25/95  Added routine to get incident energy and flight path
                       for spectrometers:

                               GetSpectrometerInEnergy
                               GetFinalFlightPath

                       Changed name on routine to get total flight path
                       length to:

                               GetTotalFlightPath

D Mikkelson  05/26/95  Added routine to get min/max time interval for 
                       spectrometers.  This includes corrections to the times
                       to account for "time focusing".


D Mikkelson  05/26/95  Added routine to convert from VAX format of floating
                       point values to the "native" floating point format of
                       the machine on which this is compiled.

                               ConvertVaxFloat


D Mikkelson  05/27/95  Changed routine GetMinAndMaxTOF to return a status of 
                       "DataResultType" for consistency with the other "Get" 
                       routines.


D Mikkelson  05/27/95  Added routines to access time field type information:  

                               NumberOfTimeFieldTypes
                               GetSpectrometerTimeFieldData
                               GetTimeFieldType
J Hammonds,
D Mikkelson  05/25/95  Added routines to convert data to a BIG ENDIAN machine

                            ConvertShorttoBigEndian
                            ConvertLongtoBigEndian

                       To use this unit on a big endian machine such as HP,
                       define the constant IPNS_BIG_ENDIAN at the start of file
                       IPNS_FileManager.c

D Mikkelson  07/25/95  Added routines to get the character strings identifying
                       the user, title, date, etc. from the run file.

                             GetUserName
                             GetRunTitle
                             GetRunNum
                             GetNextRun
                             GetStartDate
                             GetStartTime
                             GetEndDate
                             GetEndTime

D Mikkelson  07/31/95  Added routine to get time field data for Diffractometer 
                       instruments.

                            GetDiffractometerTimeFieldData

R Tronson    11/02/95  Corrected Get2DDataSlice to use
                       ConvertShorttoBigEndian when IPNS_BIG_ENDIAN is defined.

R Tronson    11/21/95  Added GetOverflows routine and changed GetSpectrum
                       to return Long integers, and to correct for the
                       channels recorded as overflowing.
*--
--------------------------------------------------------------------------- */
/*
 * $Log: ipns.h,v $
 * Revision 1.1  2006/03/12 09:59:37  tproffen
 * Initial revision
 *
 * Revision 2.2  1997/09/13 16:47:10  hammonds
 * Have added a routine which feeds back time field tables that can be loto
 * the hardware.  Please note that although there are indications that Time
 * focusing is being addressed here it is not fully implemented.  I still need
 * to decide the prpoer method for doing this on the VXI IOCs.
 *
 * Revision 2.1  1997/08/02 16:32:12  hammonds
 * Include  "../include/IPNS_VaxFloat_Endian_Conversions.c"
 *
 * Revision 2.0  1997/08/02 16:27:04  hammonds
 * Have removed the following functions to the file
 * IPNS_VaxFloat_Endian_Conversions.c :
 *      ConvertShortToBigEndian
 * 	ConvertInt4ToBigEndian
 * 	ConvertVaxFloat
 * These were removed to allow use in other pro
 * header file.
 *
 * Revision 1.1  1997/07/18 16:44:17  hammonds
 * Initial revision
 *
 * Revision 1.28  1997/05/23 16:22:24  dennis
 * *** empty log message ***
 *
 * Revision 1.27  1997/05/23 16:04:35  dennis
 * *** empty log message ***
 *
 * Revision 1.26  1997/05/23 15:39:14  dennis
 * Added routines to "Pause" and "Resume" use of a file.
 *
 * Revision 1.25  1997/05/22 15:42:29  dennis
 * Added GetCrateInfo to return information about data acquisition hardware
 * for a particular detector.
 *
 * Revision 1.23  1997/05/18 04:14:25  dennis
 * 1. Cleaned up interface to the routines that actually read in tables
 *    from the run files.
 * 2. Added routine to read the PSD map buf.
 *
 * Revision 1.22  1997/05/09 18:27:29  dennis
 * Made GetDetSubgroupSizeTable more efficient for GLAD run files.
 *
 * Revision 1.21  1996/12/09 21:13:00  dennis
 * Replaced "powder" with "diffractometer"  and
 *          "chopper" with "spectrometer"
 *
 * Revision 1.19  1996/05/31 20:26:54  dennis
 * Added "IsMonitorChannel" function which for now is just used to set all
 * counts in the monitor channel to 0 for HRMECS and LRMECS.
 *
 * Revision 1.18  1996/05/31 15:08:11  dennis
 * Added GetFileName routine
 *
 * Revision 1.17  1996/05/23 14:01:51  dennis
 * Made  "GetOverflows" private to this unit ( that is "static" ).
 *
 * Revision 1.16  1996/05/22 22:32:24  dennis
 * Replaced references to "long" with "Int4" to avoid portability problems.
 *
 * Revision 1.15  1996/05/22 21:56:16  dennis
 * Modified R.Tronson's code to apparently work on GPPD data.  This needs
 * further testing.
 *
 * Revision 1.13  1996/05/22 19:21:38  dennis
 * Removed redundant "FUNCTION" information from documentation
 *
 * Revision 1.12  1996/05/22 19:06:59  dennis
 * Fixed memory leak in routine to get 32 bit spectrum.  Also renamed the
 * routine to get 32 bit spectrum to GetSpectrum32 and renamed routine to
 * get 16 bit spectrum as GetSpectrum16
 *
 * Revision 1.11  1996/05/22 18:51:04  dennis
 * Integrated changes by R.Tronson.  The differences that remain are
 * apparently from using TAB characters to add spaces.
 *
 * Revision 1.10  1996/05/22 18:30:01  dennis
 * Added GetOverflows from R.Tronson
 *
 * Revision 1.9  1996/05/22 15:51:39  mikkelso
 * Last Version before merging with R.Tronson's work.
 *
 * Revision 1.7  1996/05/20  16:44:50  dennis
 * Added RCS logging.
 *
*/


/************************** Implementation Section ****************************
*******************************************************************************
******************************************************************************/


/*------------------------- Types and Definitions  --------------------------*/
/* NOTE: The actual length of a "long" int variable can vary from system to
         system.  Specifically, it is 8 bytes on DEC Alpha/OSF/1 and only
         4 bytes on DEC Alpha/VMS.  To avoid problems coming from this
         inconsistency, the following typedefs should be adjusted to make an
         Int4 a 4 byte integer on whatever system is used.
*/

/*****************************************************************************

    Type defining so:
               		Int4 is 4 bytes
                 	Int2 is 2 bytes
			Char1 is 1 byte
			Float4 is 4 bytes
*/

typedef int   Int4;

typedef short Int2;

typedef char  Char1;

typedef float Float4;

/*****************************************************************************/


typedef struct 
       {
	Int4 location;
	Int4 size;
       } TableType;

typedef struct 
       {
	Int2 nchann;
	Int2 flags;
	Int4 clmin;
	Int4 clrange;
	Int2 chw;
	Int2 vcwl;
       } TFTBinaryType;

typedef struct 
       {
	Float4 t_min;
	Float4 t_max;
	Float4 t_step;
	Int4   t_double_len;
	Int2   num_of_channels;
	Int2   time_focus_bit;
	Int2   emission_delay_bit;
	Int2   constant_delay_bit;
	Int2   d_bit;
	Int2   e_bit;
	Int2   f_bit;
	Int2   g_bit;
	Int2   h_bit;
       } TimeFieldTableType;

typedef Float4 DetAngleValueType;

typedef Float4 FlightPathLengthType;

typedef Float4 DetHeightValueType;

typedef Int2   DetSubgroupSizeType;

typedef Int2   TimeScaleType;

typedef struct
       {
	Int2 lower;
	Int2 upper;
       } DetDiscLevelType;

typedef struct 
       {
	Int4 address;
	Int4 tf_type;
	Int4 more_hist_bit;
       } DetectorMapTableType;


typedef struct 
 {
                                /*... Block 1 table defining dynamic regions */  
  TableType control_parameter;		/*CONTROL PARAMETER ARRAY 'VCONT'*/
  TableType detect_map_table;		/*DMAP TABLE*/
  TableType time_field_table;		/*TTYPE TABLE*/
  TableType time_scale_table;		/*KSC   TABLE*/
  TableType time_shift_table;		/*ED    TABLE*/
  TableType area_start_table;		/*AREA DET CHAN START TIMES*/
  TableType time_delay_table;		/*DETECTOR TIME-DELAY TABLE*/
  Int4      hist_start_address;		/*START OF HISTOGRAM DATA*/
  Int4      num_of_blocks;		/*NUMBER OF BLOCKS IN THIS FILE*/
  Int4      offset_to_free;		/*OFFSET TO 1ST FREE BYTE IN RUN FILE*/
  Int4      version_number;		/*VERSION NUMBER FOR THIS RUN FILE*/
  TableType detector_angle;		/*TABLE OF DETECTOR ANGLES*/
  TableType flight_path ;		/*TABLE OF FINAL FLIGHT PATHS*/
  TableType detector_height;  		/*TABLE OF DETECTOR ELEMENT HEIGHTS*/
  TableType detector_type;		/*TABLE OF DETECTOR TYPES*/
  TableType control_table;		/*TABLE OF CONTROL PARAMETERS*/
  TableType seq_hist_width;		/*TABLE OF SEQUENTIAL HIST TIME WIDTHS*/
                                /*... End of Block 1 dynamic region table */  

  Int2      n_det; 			/*NUMBER OF DETECTORS*/
  Char1     user_name[21];		/*USER'S NAME*/
  Char1     run_title[81];		/*TITLE IN HEADER*/
  Int4      run_num;			/*THIS FILES RUN NUMBER*/
  Int4      next_run;		/*RUN # OF NEXT RUN*/
  Char1     start_date[10];		/*SETUP DATE FOR THIS RUN*/
  Char1     start_time[9];		/*SETUP TIME FOR THIS RUN*/
  Char1     end_date[10];		/*ENDING DATE FOR THIS RUN*/
  Char1     end_time[9];		/*ENDING TIME FOR THIS RUN*/
  Char1     prot_status;		/*RUN PROTECTION STATUS*/
  Char1     var_to_monitor;		/*VARIABLE TO MONITOR 'VMON'*/
  Int4      preset_monitor_counts;	/*PRESET MONITOR COUNT FOR 1 CYCLE*/
  Int4      elapsed_monitor_counts;	/*ELAPSED MONITOR COUNT FOR CYCLE ICY*/
  Int2      num_of_cycles_preset;	/*TOTAL # OF CYCLES TO BE RUN*/
  Int2      num_of_cycles_completed;    /*NUMBER OF CYCLES COMPLETED*/
  Char1     run_after_finished[5];	/*RUN # TO BEGIN AT END OF FINAL CYCLE*/
			                /* OF THIS RUN*/
  Int4      total_monitor_counts;	/*TOTAL ELAPSED MONITOR COUNT*/
  Char1     detector_calib_file[5];	/*DETECTOR CALIBRATION FILE # 'NDCAL'*/
  Char1     det_loc_unit;		/*UNIT FOR REFERENCING DETECTOR*/
					/* LOCATION (DUNIT;*/
  Char1     pseudo_time_unit;		/*PSEUDOTIME UNIT 'PTUNIT'*/
  Float4    source_to_sample;		/*SOURCE TO SAMPLE PATH LENGTH*/
  Float4    source_to_chopper;		/*SOURCE TO CHOPPER PATH LENGTH*/
  Char1     moderator_calib_file[5];	/*MODERATOR CALIBRATION FILE # 'NMCAL'*/
  Int2      group_to_monitor;		/*GROUP TO MONITOR IF VMON=T*/
  Int2      channel_to_monitor;		/*CHANNEL TO MONITOR IF VMON=C*/
  Int2      num_of_histograms;		/*NUMBER OF HISTOGRAMS NH*/
  Int2      num_of_time_fields;		/*NUMBER OF TIME FIELD TYPES 'NTYPE'*/
  Int2      num_of_control;		/*NUMBER OF CONTROL PARAMETERS 'NCONT'*/
  Int2      control_flag;		/*CONTROL FLAG WORD 'ICONT'*/
  Int2      clock_shift;		/*CLOCK SHIFT 'NSHIFT' NEEDED TO INDEX*/
  Int4      total_channels;		/*TOTAL NUMBER OF CHANNELS 
                                          IN HISTOGRAM*/
  Int4      num_of_pulses;		/*TOTAL NUMBER OF PULSES, THIS RUN*/
  Int4      size_of_data_area;		/*SIZE OF HIGTOGAM DATA AREA 
                                          IN CHANNELS*/
  Int4      hardware_t_min;		/*HARDWARE TMIN*/
  Int4      hardware_t_max;		/*HARDWARE TMAX*/
  Int4      hard_time_delay;		/*HARDWARE TIME-DELAY*/
  Int2      num_of_x;			/*XNUM FOR SCD*/
  Int2      num_of_y;			/*YNUM FOR SCD*/
  Int2      num_wavelengths;		/*WLNUM FOR SCD*/
  Int4      max_wavelength;		/*WLMAX FOR SCD*/
  Int4      min_wavelength;		/*WLMIN FOR SCD*/
  Float4    dta;			/*DETA FOR SCD*/
  Float4    dtd;			/*DETD FOR SCD*/
  Float4    omega;			/*OMEGA FOR SCD*/
  Float4    chi;			/*CHI ANGLE FOR SCD*/
  Float4    phi;			/*PHI ANGLE FOR SCD*/
  Float4    x_left;			/*X-LEFT FOR SCD*/
  Float4    x_right;			/*X-RIGHT FOR SCD*/
  Float4    y_lower;			/*Y-LOWER FOR SCD*/
  Float4    y_upper;			/*Y-UPPER FOR SCD*/
  Float4    x_displacement;		/*X-DISPL FOR SCD*/
  Float4    y_displacement;		/*Y-DISPL FOR SCD*/
  Float4    x_length;			/*X-LENGTH FOR SCD*/
  Int2      area_chan_width;		/*AREA DET TIME CHAN WIDTH*/
  Int2      area_double_int;            /*area detector chaN DOUBLING INTERVAL*/
  Int4      address_1d_data;		/*Z-8001 ADDRESS OF 1-D DATA*/
  Int4      address_2d_data;		/*Z-8001 ADDRESS OF 2-D DATA*/
  Int4      end_of_overflow;		/*POINTER TO END OF OVERFLOW BUFFER.*/
  Int4      channels_1d;		/*TOTAL CHAN USED FOR 1D DETS*/
					/* WHEN A 2D DET IS ALSO USED*/
  Int2      num_of_overflows;  		/*TOTAL NUMBER OF OVERFLOWS*/
  Float4    clock_period;		/*FUNDAMENTAL AREA DET CLOCK PERIOD*/
  Float4    energy_in;			/*FIXED INCIDENT ENERGY (MEV;*/
  Float4    energy_out;			/*FIXED FINAL ENERGY (MEV;*/
  Int2      num_of_seq_hist;		/*NUMBER OF SEQUENTIAL HISTS*/
  Float4    proton_current;		/*PROTON BEAM CURRENT*/
  Int2      area_binning;		/*TYPE OF BINNING FOR AREA DET*/
  Int2      microprocessor;		/*TYPE OF MICRO USED FOR DAshort TA*/
					/* 256 (OR 0; = Z8001; 1, 2, 3, OR*/
					/* 4 INDICATES NUMBER OF N32016 MICROS*/
  Int2      num_of_lockouts;		/*NUMBER OF TIMES N32016 LOCKED OUT*/
					/* INTERRUPTS - NONZERO INDICATES*/
					/* DATA LOSS*/
  Int4      first_overflow;		/*FIRST 4 BYTES OF FIFO OVERFLOW TABLE*/
					/* NONZERO INDICATES DATA LOSS*/
  Int4      exp_num;			/*EXPERIMENT NUMBER FOR MULTIRUN EXPT*/
  Int4      first_run;		/*FIRST RUN NO IN MULTIRUN EXPT*/
  Int4      last_run;		/*LAST RUN NO IN MULTIRUN EXPT*/
  Int4      default_run;		/*Default RUN NO USED FOR SETUP*/
  Int2      sample_pos;			/*GLAD SAMPLE POSITION*/
					/* (1=DOWNSTREAM;2=UPSTREAM;*/
  Int2      num_of_head_blocks;		/*NUMBER OF HEADER BLOCKS*/
  Int2      overflow_sort;		/*OVERFLOW SORT FLAG*/
  Float4    standard_clock;             /*CLOCK PERIOD FOR STANDARD DETECTORS*/
  Float4    lpsd_clock;                 /*CLOCK PERIOD FOR LPSDs*/
                                /*... Block 1 table defining dynamic regions */  
  TableType message_region;
  TableType discriminator_settings;
  TableType PSD_ID_map; 
  
 } IPNSHeadType;
 
#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE 
#define FALSE 0 
#endif

#define NUM_EL (size_t) 1
#define PREFIX_SIZE 28

typedef enum { OPEN_FOR_INPUT, OPEN_FOR_OUTPUT, OPEN_BUT_PAUSED } FileModeType;

typedef int FileIdType;


typedef struct {   
  Int2   bank_no;
  Int2   n_dets;
  Int2   det_no;
  Int2   crate;
  Int2   slot;
  Int2   input;
  Int2   min_id;
} PSD_map_buf_entry;


typedef struct {
                FileModeType          file_mode;
                int                   num_users;
                char                  f_name[MAX_STR_LENGTH + 1];
                FileIdType            file_id;
                FILE                 *f;
                IPNSHeadType          header;
                TimeFieldTableType   *time_field;
                DetectorMapTableType *detector_map;
                DetAngleValueType    *det_angle_table;
                FlightPathLengthType *flight_path_length_table;
                DetHeightValueType   *det_height_table;
                DetSubgroupSizeType  *det_sub_group_size;
                int                   max_bank;
                int                  *bank_start;
                PSD_map_buf_entry    *PSD_map_buf;
		DetDiscLevelType     *det_disc_levels;
		TimeScaleType       *time_scale_table;
               } FileType;
typedef FileType *FilePointerType;
typedef FilePointerType FileListType[MAX_NUM_FILES + 1];



static FileListType file_list;


/****************************** Local Procedures ******************************
*******************************************************************************
******************************************************************************/


/*++-----------  ConvertRunToInt
Effect:
	Converts an int which stores a 4 digit run number as ascii characters to the equivalent integer number */

 static void ConvertRunToInt(int *number)
{
int tempint;
char *tempchar;

  tempint = *number;
  tempchar = (char *)&tempint;
  *number = 1000*(tempchar[0]-48)+100*(tempchar[1]-48)+10*(tempchar[2]-48)+(tempchar[3]-48);
}

/*++----------------------- DropTrailingBlanks ---------------------------

*--
*/

static void DropTrailingBlanks( char *char_str )
{
  int  i;

  i = strlen( char_str ) - 1;
  while ( (i >= 0) && ( (char_str[i] == ' ') || (char_str[i] == '\0') ) )
    {
      char_str[i] = '\0';
      i--;
    }
}

/* ---------------------------- EqualStrings ----------------------------- */
/*
   This was needed since VMS C does not have an strncasecmp function.
*/

static int EqualStrings( char *s1, char *s2, int n )
{
int    i;

for( i = 0; i < n; i++ )
  if ( toupper(s1[i]) != toupper(s2[i]) )
    return( FALSE );

return( TRUE );
}


/*++------------------------- IPNS_Instrument_From_Name ---------------------

Effect:
         This function will determine the IPNS instrument type from the file
         name.

Input:
         Character string containing the file name.

Output:
         Ennumeration value giving the IPNS instrument.
*--
*/

static IPNSMachineType IPNS_Instrument_From_Name( char * f_name )
{
#define NUM_INSTRUMENTS   8 

  char             *instruments[ 8 ]    = { "GLA", "GPP",
                                            "HRC", "LRC", 
                                            "QEN", "SAN", 
                                            "SEP", "SCD"   };

  IPNSMachineType  instrument_type[ 8 ] = {  IPNS_GLAD,   IPNS_GPPD,   
                                             IPNS_HRMECS, IPNS_LRMECS, 
                                             IPNS_QENS,   IPNS_SAND,   
                                             IPNS_SEPD,   IPNS_SCD    };
  int              i, 
                   dot_pos,
                   first;
  char             file_prefix[10];

  dot_pos = strlen( f_name ) - 1;
  while ( (dot_pos > 7) && ( f_name[dot_pos] != '.') )
    dot_pos--;

  if ( dot_pos <= 7 )
    return( IPNS_UNKNOWN );

  first = dot_pos - 8; 
  for ( i = first; i < first+3; i++ )
    file_prefix[ i-first ] = f_name[i];
  file_prefix[3] = '\0';

  for ( i = 0; i < NUM_INSTRUMENTS; i++ )
    if ( EqualStrings( file_prefix, instruments[i], 3 ) )
      return( instrument_type[i] );

  return( IPNS_UNKNOWN );
}




/*++--------------------------- GetIPNSHeader ---------------------------------

Effect: 
         This function will read the header information from the file
      pointed to by Runfile and store the header in the space pointed
      to by head_ptr.

Input:
         A file pointer.

Output:
         The header information stored in the structure.

*--
*/

static void GetIPNSHeader(FILE *run_file, IPNSHeadType *head_ptr)
{


if (run_file == NULL) 
 {
  printf("Null file pointer passed into GetIPNSHeader function.\n");
  exit(0);
 }
if (head_ptr == NULL)
 {
  printf("Null IPNSHeadType pointer passed into GetIPNSHeader function.\n");
  exit(0);
 }
rewind(run_file);
fread((void *)&(head_ptr->control_parameter),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->detect_map_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->time_field_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->time_scale_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->time_shift_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->area_start_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->time_delay_table),sizeof(TableType),1,run_file);

fread((void *)&(head_ptr->hist_start_address),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->num_of_blocks),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->offset_to_free),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->version_number),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->detector_angle),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->flight_path),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->detector_height),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->detector_type),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->control_table),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->seq_hist_width),sizeof(TableType),1,run_file);

fread((void *)&(head_ptr->n_det),sizeof(Int2),1,run_file);
fread((void *)(head_ptr->user_name),1,20,run_file);
(head_ptr->user_name)[20] = '\0';
fseek(run_file,142L,SEEK_SET);
fread((void *)(head_ptr->run_title),1,80,run_file);
(head_ptr->run_title)[80] = '\0';
fseek(run_file,222L,SEEK_SET);
fread((void *)&(head_ptr->run_num),1,4,run_file);
fread((void *)&(head_ptr->next_run),1,4,run_file);
fread((void *)(head_ptr->start_date),1,9,run_file);
(head_ptr->start_date)[9] = '\0';
fread((void *)(head_ptr->start_time),1,8,run_file);
(head_ptr->start_time)[8] = '\0';
fread((void *)(head_ptr->end_date),1,9,run_file);
(head_ptr->end_date)[9] = '\0';
fread((void *)(head_ptr->end_time),1,8,run_file);
(head_ptr->end_time)[8] = '\0';

fread((void *)&(head_ptr->prot_status),1,1,run_file);
fread((void *)&(head_ptr->var_to_monitor),1,1,run_file);

fread((void *)&(head_ptr->preset_monitor_counts),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->elapsed_monitor_counts),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->num_of_cycles_preset),2,1,run_file);
fread((void *)&(head_ptr->num_of_cycles_completed),2,1,run_file);

fread((void *)(head_ptr->run_after_finished),1,4,run_file);
(head_ptr->run_after_finished)[4] = '\0';

fread((void *)&(head_ptr->total_monitor_counts),sizeof(Int4),1,run_file);

fread((void *)(head_ptr->detector_calib_file),1,4,run_file);
(head_ptr->detector_calib_file)[4] = '\0';
fread((void *)&(head_ptr->det_loc_unit),1,1,run_file);
fread((void *)&(head_ptr->pseudo_time_unit),1,1,run_file);

#ifdef IPNS_FILEMANAGER_DEBUG
printf("=========================================================\n");
printf("Pseudo_time_unit = [%c]\n", head_ptr->pseudo_time_unit );
#endif

fread((void *)&(head_ptr->source_to_sample),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->source_to_chopper),sizeof(Float4),1,run_file);

fread((void *)(head_ptr->moderator_calib_file),1,4,run_file);
(head_ptr->moderator_calib_file)[4] = '\0';

fread((void *)&(head_ptr->group_to_monitor),2,1,run_file);
fread((void *)&(head_ptr->channel_to_monitor),2,1,run_file);
fread((void *)&(head_ptr->num_of_histograms),2,1,run_file);
fread((void *)&(head_ptr->num_of_time_fields),2,1,run_file);
fread((void *)&(head_ptr->num_of_control),2,1,run_file);
fread((void *)&(head_ptr->control_flag),2,1,run_file);
fread((void *)&(head_ptr->clock_shift),2,1,run_file);

fread((void *)&(head_ptr->total_channels),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->num_of_pulses),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->size_of_data_area),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->hardware_t_min),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->hardware_t_max),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->hard_time_delay),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->num_of_x),2,1,run_file);
fread((void *)&(head_ptr->num_of_y),2,1,run_file);
fread((void *)&(head_ptr->num_wavelengths),2,1,run_file);

fread((void *)&(head_ptr->max_wavelength),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->min_wavelength),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->dta),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->dtd),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->omega),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->chi),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->phi),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->x_left),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->x_right),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->y_lower),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->y_upper),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->x_displacement),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->y_displacement),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->x_length),sizeof(Float4),1,run_file);


fread((void *)&(head_ptr->area_chan_width),2,1,run_file);
fread((void *)&(head_ptr->area_double_int),2,1,run_file);

fread((void *)&(head_ptr->address_1d_data),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->address_2d_data),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->end_of_overflow),sizeof(Int4),1,run_file);
fread((void *)&(head_ptr->channels_1d),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->num_of_overflows),2,1,run_file);

fread((void *)&(head_ptr->clock_period),sizeof(Float4),1,run_file);

fseek(run_file,462L,SEEK_SET);
fread((void *)&(head_ptr->energy_in),sizeof(Float4),1,run_file);
fread((void *)&(head_ptr->energy_out),sizeof(Float4),1,run_file);

fread((void *)&(head_ptr->num_of_seq_hist),2,1,run_file);

fread((void *)&(head_ptr->proton_current),sizeof(Float4),1,run_file);

fread((void *)&(head_ptr->area_binning),2,1,run_file);
fread((void *)&(head_ptr->microprocessor),2,1,run_file);
fread((void *)&(head_ptr->num_of_lockouts),2,1,run_file);

fread((void *)&(head_ptr->first_overflow),sizeof(Int4),1,run_file);

fread((void *)&(head_ptr->exp_num),1,4,run_file);
fread((void *)&(head_ptr->first_run),1,4,run_file);
fread((void *)&(head_ptr->last_run),1,4,run_file);

fread((void *)&(head_ptr->sample_pos),2,1,run_file);
fread((void *)&(head_ptr->default_run),1,4,run_file);
fseek(run_file,508L,SEEK_SET);
fread((void *)&(head_ptr->num_of_head_blocks),2,1,run_file);
fread((void *)&(head_ptr->overflow_sort),2,1,run_file);

fread((void *)&(head_ptr->message_region),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->discriminator_settings),sizeof(TableType),1,run_file);
fread((void *)&(head_ptr->PSD_ID_map),sizeof(TableType),1,run_file);

/* The following conversions are needed for BIG_ENDIAN MACHINES, (eg. HP)  */
/* NOT needed for VAX, ALPHA, PC ...                                       */

#ifdef IPNS_BIG_ENDIAN

  	ConvertInt4toBigEndian( &(head_ptr->control_parameter.location) );
	ConvertInt4toBigEndian( &(head_ptr->control_parameter.size) );
	ConvertInt4toBigEndian( &(head_ptr->detect_map_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->detect_map_table.size) );
	ConvertInt4toBigEndian( &(head_ptr->time_field_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->time_field_table.size) );
	ConvertInt4toBigEndian( &(head_ptr->time_scale_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->time_scale_table.size) );
  	ConvertInt4toBigEndian( &(head_ptr->time_shift_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->time_shift_table.size) );
	ConvertInt4toBigEndian( &(head_ptr->area_start_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->area_start_table.size) );
	ConvertInt4toBigEndian( &(head_ptr->time_delay_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->time_delay_table.size) );

	ConvertInt4toBigEndian( &(head_ptr->hist_start_address) );
	ConvertInt4toBigEndian( &(head_ptr->num_of_blocks) );
	ConvertInt4toBigEndian( &(head_ptr->offset_to_free) );
	ConvertInt4toBigEndian( &(head_ptr->version_number) );
	
  	ConvertInt4toBigEndian( &(head_ptr->detector_angle.location) );
	ConvertInt4toBigEndian( &(head_ptr->detector_angle.size) );
	ConvertInt4toBigEndian( &(head_ptr->flight_path.location) );
	ConvertInt4toBigEndian( &(head_ptr->flight_path.size) );
	ConvertInt4toBigEndian( &(head_ptr->detector_height.location) );
	ConvertInt4toBigEndian( &(head_ptr->detector_height.size) );
	ConvertInt4toBigEndian( &(head_ptr->detector_type.location) );
	ConvertInt4toBigEndian( &(head_ptr->detector_type.size) );
  	ConvertInt4toBigEndian( &(head_ptr->control_table.location) );
	ConvertInt4toBigEndian( &(head_ptr->control_table.size) );
	ConvertInt4toBigEndian( &(head_ptr->seq_hist_width.location) );
	ConvertInt4toBigEndian( &(head_ptr->seq_hist_width.size) );
															 
	ConvertShorttoBigEndian( &(head_ptr->n_det) );

	ConvertInt4toBigEndian( &(head_ptr->preset_monitor_counts) );
	ConvertInt4toBigEndian( &(head_ptr->elapsed_monitor_counts) );

	ConvertShorttoBigEndian( &(head_ptr->num_of_cycles_preset) ) ;
	ConvertShorttoBigEndian( &(head_ptr->num_of_cycles_completed) );

	ConvertInt4toBigEndian( &(head_ptr->total_monitor_counts) );

	ConvertShorttoBigEndian( &(head_ptr->group_to_monitor) ) ;
	ConvertShorttoBigEndian( &(head_ptr->channel_to_monitor) ) ;
	ConvertShorttoBigEndian( &(head_ptr->num_of_histograms) );
	ConvertShorttoBigEndian( &(head_ptr->num_of_time_fields) );
	ConvertShorttoBigEndian( &(head_ptr->num_of_control) ) ;
	ConvertShorttoBigEndian( &(head_ptr->control_flag) );
	ConvertShorttoBigEndian( &(head_ptr->clock_shift) )	;

 	ConvertInt4toBigEndian( &(head_ptr->total_channels) );
	ConvertInt4toBigEndian( &(head_ptr->num_of_pulses) );
	ConvertInt4toBigEndian( &(head_ptr->size_of_data_area) );
	ConvertInt4toBigEndian( &(head_ptr->hardware_t_min) );
	ConvertInt4toBigEndian( &(head_ptr->hardware_t_max) );
	ConvertInt4toBigEndian( &(head_ptr->hard_time_delay) );

	ConvertShorttoBigEndian( &(head_ptr->num_of_x) );
	ConvertShorttoBigEndian( &(head_ptr->num_of_y) );
	ConvertShorttoBigEndian( &(head_ptr->num_wavelengths) );

	ConvertInt4toBigEndian( &(head_ptr->max_wavelength) );
	ConvertInt4toBigEndian( &(head_ptr->min_wavelength) );

	ConvertShorttoBigEndian( &(head_ptr->area_chan_width) );
	ConvertShorttoBigEndian( &(head_ptr->area_double_int) );
														   
	ConvertInt4toBigEndian( &(head_ptr->address_1d_data) );
	ConvertInt4toBigEndian( &(head_ptr->address_2d_data) );
	ConvertInt4toBigEndian( &(head_ptr->end_of_overflow) );
	ConvertInt4toBigEndian( &(head_ptr->channels_1d) );

 	ConvertShorttoBigEndian( &(head_ptr->num_of_overflows) );

 	ConvertShorttoBigEndian( &(head_ptr->num_of_seq_hist) );

  	ConvertShorttoBigEndian( &(head_ptr->area_binning) );
 	ConvertShorttoBigEndian( &(head_ptr->microprocessor) );
 	ConvertShorttoBigEndian( &(head_ptr->num_of_lockouts) );

	ConvertInt4toBigEndian( &(head_ptr->first_overflow) );

 	ConvertShorttoBigEndian( &(head_ptr->sample_pos) );

 	ConvertShorttoBigEndian( &(head_ptr->num_of_head_blocks) );
 	ConvertShorttoBigEndian( &(head_ptr->overflow_sort) );

	ConvertInt4toBigEndian( &(head_ptr->discriminator_settings.location) );
	ConvertInt4toBigEndian( &(head_ptr->discriminator_settings.size) );

#endif

/* The following conversions are necessary if the version number is less */
/* than 3.  Run Numbers were switch from ascii charaters to long ints */
/* with version 3 to accomodate numbers above 9999 */
if (head_ptr->version_number < 3)
{
	ConvertRunToInt(&(head_ptr->run_num) );
	ConvertRunToInt(&(head_ptr->next_run) );
	ConvertRunToInt(&(head_ptr->exp_num) );
	ConvertRunToInt(&(head_ptr->first_run) );
	ConvertRunToInt(&(head_ptr->last_run) );
	ConvertRunToInt(&(head_ptr->default_run) );
	}
else
{
#ifdef IPNS_BIG_ENDIAN
	ConvertInt4toBigEndian(&(head_ptr->run_num) );
	ConvertInt4toBigEndian(&(head_ptr->next_run) );
#endif
	}




/* ... Floating point values must be convereted for ALL non-VMS machines. */

#ifndef VMS
  ConvertVaxFloat( &(head_ptr->source_to_sample) );
  ConvertVaxFloat( &(head_ptr->source_to_chopper) );
  ConvertVaxFloat( &(head_ptr->dta) );
  ConvertVaxFloat( &(head_ptr->dtd) );
  ConvertVaxFloat( &(head_ptr->omega) );
  ConvertVaxFloat( &(head_ptr->chi) );
  ConvertVaxFloat( &(head_ptr->phi) );
  ConvertVaxFloat( &(head_ptr->x_left) );
  ConvertVaxFloat( &(head_ptr->x_right) );
  ConvertVaxFloat( &(head_ptr->y_lower) );
  ConvertVaxFloat( &(head_ptr->y_upper) );
  ConvertVaxFloat( &(head_ptr->x_displacement) );
  ConvertVaxFloat( &(head_ptr->y_displacement) );
  ConvertVaxFloat( &(head_ptr->x_length) );

  ConvertVaxFloat( &(head_ptr->clock_period) );

  ConvertVaxFloat( &(head_ptr->energy_in) );
  ConvertVaxFloat( &(head_ptr->energy_out) );
  ConvertVaxFloat( &(head_ptr->proton_current) );
#endif

if(head_ptr->version_number > 3) {
    fseek(run_file, 632L, SEEK_SET);
    fread((void *)&(head_ptr->standard_clock),4,1,run_file);
    fread((void *)&(head_ptr->lpsd_clock),4,1,run_file);
    ConvertVaxFloat( &(head_ptr->standard_clock) );
    ConvertVaxFloat( &(head_ptr->lpsd_clock) );
    }
else
    {
    head_ptr->standard_clock = .125;
    head_ptr->lpsd_clock = .5;
    }
#ifdef IPNS_FILEMANAGER_DEBUG
printf("standard_clock      == %f\n", head_ptr->standard_clock);
printf("Hardware time delay == %d\n", head_ptr->hard_time_delay );
printf("Hardware t_min      == %d\n", head_ptr->hardware_t_min );
printf("Hardware t_max      == %d\n", head_ptr->hardware_t_max );
#endif

return;
}
       


/*-------------------------- MallocTimeFieldTable -----------------------------

Effect:
        This function will allocate space for a time field table.
     num_of_tf is the number of time fields in the table, and 
     num_of_tf can be obtained in the field (num_of_time_fields)
     from the IPNSHeadType given by the function GetIPNSHeader.

Input: 
       num_of_tf.

Output:
       Space for the time field table.

*--
*/

static TimeFieldTableType *MallocTimeFieldTable(Int2 num_of_tf)

{
 TimeFieldTableType *tft;

 tft = (TimeFieldTableType *)calloc( (size_t)(num_of_tf + 1),
                     sizeof(TimeFieldTableType) );
 if ( tft == NULL )
  {
   printf("Memory allocation error in MallocTimeFieldTable.\n");
   exit(0);
  }

 return(tft);
}



/*------------------------------- FreeTable -----------------------------------

Effect:
          This function will free the space allocated by the Malloc___Table
     functions in this header, so that the memory can be used for other 
     purposes.

Input:
         table.

Ouput:
         none.

*--
*/


static void FreeTable(void *table)
{
 if (table != NULL)
  {
   free(table);
  }
}



/*---------------------------- GetTimeFieldTable ------------------------------

Effect:
        This function reads the time field table stored in the run_file.
     The table's first element is not used so that the first element
     can start at position time_field_table[1]. It then converts the 
     table into a more useble form and places the more useable table 
     in the space pointed to by time_field. table.location is the location of 
     the time field table in the run_file. table.size is the size of the 
     time field data in the run file, and num_of_tf is the number of 
     time fields.  

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.
     num_of_tf      Number of time field types

Output:
        Time field stored in the space pointed to by time_field.

*--
*/

static void GetTimeFieldTable(FILE               *run_file,
                              TimeFieldTableType *time_field,
                              TableType           table,
                              Int2                num_of_tf)
{
TFTBinaryType *time_field_binary;
Int4           n;
Int4	       version_number;
Int2          *temp_ptr;
int		k;
float		clk_freq, clk_per;

#ifdef IPNS_FILEMANAGER_DEBUG
printf("In GetTimeFieldTable, size = %d, num_of_tof = %d\n", table.size, 
                                                             num_of_tf );
#endif
fseek(run_file, 68L, SEEK_SET);
fread((void *)&version_number,4,1,run_file);
if(version_number > 3) {
    fseek(run_file, 632L, SEEK_SET);
    fread((void *)&clk_per,4,1,run_file);
    ConvertVaxFloat( &clk_per );
    }
else
    {
    clk_per = .125;
    }
clk_freq = 1.000000/clk_per;
time_field_binary = (TFTBinaryType *)malloc((size_t)table.size);
if ( time_field_binary == NULL )
 {
  printf("Memory Allocation Error in function GetTimeFieldTable.\n");
  exit(0);
 }

/*  Read the time field table in binary form   */

fseek(run_file, (long)table.location, SEEK_SET);
fread((void *)time_field_binary,(size_t)1, (size_t)table.size,run_file);

#ifdef IPNS_BIG_ENDIAN

temp_ptr = time_field_binary;
                                        /* for each time field type */
for( k = 1; k <= num_of_tf; k++ )
  {
    for ( n = 0; n < 2; n++ )         /* convert 2 shorts */
      {
        ConvertShorttoBigEndian( temp_ptr );
        temp_ptr = (unsigned short *)temp_ptr + 1;
       }
    for ( n = 0; n < 2; n++ )         /* convert 2 int32s */
      {
        ConvertInt4toBigEndian( temp_ptr );
        temp_ptr = (Int4 *)temp_ptr + 1;
       }
    for ( n = 0; n < 2; n++ )         /* convert 2 shorts */
      {
        ConvertShorttoBigEndian( temp_ptr );
        temp_ptr = (unsigned short *)temp_ptr + 1;
      }
  }
#endif

/*  Convert binary time fields into a useable form  */

#ifdef IPNS_FILEMANAGER_DEBUG
printf("Time field table------------------------------------\n");
#endif

for (n = 1;n <= num_of_tf; n++){

	(time_field[n]).num_of_channels = (time_field_binary[n-1]).nchann;
	time_field[n].t_min = time_field_binary[n-1].clmin/clk_freq;
	time_field[n].t_max = time_field_binary[n-1].clrange/clk_freq;
	time_field[n].t_step = (time_field_binary[n-1].chw)/clk_freq;
	time_field[n].t_double_len = time_field_binary[n-1].vcwl;
	time_field[n].time_focus_bit = (time_field_binary[n-1].flags >>15) & 1;
	time_field[n].emission_delay_bit = (time_field_binary[n-1].flags >>14) 
                                            & 1;
	time_field[n].constant_delay_bit = (time_field_binary[n-1].flags >>13) 
                                            & 1;
	time_field[n].d_bit = (time_field_binary[n-1].flags >> 12) & 1;
	time_field[n].e_bit = (time_field_binary[n-1].flags >> 11) & 1;
	time_field[n].f_bit = (time_field_binary[n-1].flags >> 10) & 1;
	time_field[n].g_bit = (time_field_binary[n-1].flags >> 9) & 1;
	time_field[n].h_bit = (time_field_binary[n-1].flags >> 8) & 1;
     
#ifdef IPNS_FILEMANAGER_DEBUG
    printf("t_min, tmax, t_step, num_channels, t_focus = %f, %f, %f, %d, %d\n",
            time_field[n].t_min, time_field[n].t_max, time_field[n].t_step,
            time_field[n].num_of_channels, time_field[n].time_focus_bit );
#endif
	}

free((void *)time_field_binary); 

}




/*------------------------ MallocDetectorMapTable -----------------------------

Effect:
        This function will allocate space for a detector map table.
     num_of_det is the number of detectors used, and num_of_histograms
     is the number of histograms. Both can be obtained in the fields
     ((num_of_histograms) and (n_det) respectively) from the IPNSHeadType 
     given by the function GetIPNSHeader.

Input: 
       num_of_det and num_of_histograms.

Output:
       Space for the detector map table.

*--
*/

static DetectorMapTableType *MallocDetectorMapTable(Int2 num_of_det,
                                                    Int2 num_of_histograms)
{
 DetectorMapTableType *map;

 map = (DetectorMapTableType *)calloc( (size_t)((num_of_det * num_of_histograms
                                       ) + 1), sizeof(DetectorMapTableType) );
 if ( map == NULL )
  {
   printf("Memory allocation error in MallocDetectorMapTable.\n");
   exit(0);
  }

 return(map);
}






/*-------------------------- GetDetectorMapTable ------------------------------

Effect:
        This function reads the detector map from run_file. It then converts
     the detector map to a more useable form and places the information
     in the space pointed to by detector_map. The table doesn't use the first
     element so that the first valid peice of information is located at
     detector_map[1], this allows the use of the detector number to access
     that specific detector's data structure not (det_num - 1). 

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.
     num_of_det     Number of detectors

Output:
       The detector map table pointed to by detector_map.
*--
*/ 

static void GetDetectorMapTable(FILE                 *run_file,
                                IPNSMachineType       inst_type,
                                DetectorMapTableType *detector_map,
                                TableType             table,
                                Int2                  num_of_det,
                                Int2                  num_of_hist)
{
Int4 *l_temp;
Int4 n;
Int2 num_of_entries;

#ifdef  IPNS_FILEMANAGER_DEBUG
  printf("Start of GetDetectorMapTable\n");
#endif

l_temp = (Int4 *)malloc((size_t)table.size);
if ( l_temp == NULL )
 {
  printf("Memory Allocation Error in function GetDetectorMapTable.\n");
  exit(0);
 }

/*  Read the Detector Mapping Table in binary form   */

fseek(run_file, (long)table.location, SEEK_SET);
if((fread((void *)l_temp,(size_t)1,(size_t)table.size,run_file)) != table.size)
 {
  printf("Error reading from the file.\n");
  exit(0);
 }

/*  Convert binary Detector Map into a useable form  */

num_of_entries = num_of_det * num_of_hist;

#ifdef IPNS_BIG_ENDIAN
for ( n = 0; n < num_of_entries; n++ )
  ConvertInt4toBigEndian( &(l_temp[n]) );
#endif

if ( inst_type != IPNS_GLAD )                   /* Non-GLAD version */
  {
#ifdef  IPNS_FILEMANAGER_DEBUG
  printf("REGULAR VERSION OF GET DETECTOR MAP TABLE \n");
#endif
  for (n = 1;n <= num_of_entries; n++){
    detector_map[n].address = ((l_temp[n-1]) & 037777777) ;
    detector_map[n].tf_type = ((l_temp[n-1]) >> 24  ) & 0377    ;
    detector_map[n].more_hist_bit = ((l_temp[n-1]) >> 23) & 01;
    }  
  }
else                                            /* GLAD version: */
  {
#ifdef  IPNS_FILEMANAGER_DEBUG
  printf("GLAD VERSION OF GET DETECTOR MAP TABLE \n");
#endif
  for (n = 1;n <= num_of_entries; n++){
    detector_map[n].address = ((l_temp[n-1]) & 0177777);
    detector_map[n].address = (detector_map[n].address) << 8;
    detector_map[n].tf_type = ((l_temp[n-1]) >> 24  ) & 0377    ;
    detector_map[n].more_hist_bit = ((l_temp[n-1]) >> 23) & 01;
    }  
  }
free((void *)l_temp); 

#ifdef  IPNS_FILEMANAGER_DEBUG
  printf("End of GetDetectorMapTable\n");
#endif
}



/*------------------------ MallocDetectorAngleTable ---------------------------

Effect:
        This function will allocate space for a detector angle table.
     size is the size of the whole table, and size can be obtained in 
     the field (detector_angle) from the IPNSHeadType given by the 
     function GetIPNSHeader.

Input: 
       size.

Output:
       Space for the detector angle table.

*--
*/

static DetAngleValueType *MallocDetectorAngleTable(Int4 size)

{
 DetAngleValueType *angle_tab;

 angle_tab = (DetAngleValueType *)malloc( (size_t)(size) + 
               sizeof(DetAngleValueType) );
 if ( angle_tab == NULL )
  {
   printf("Memory allocation error in MallocDetectorAngleTable.\n");
   exit(0);
  }

 return(angle_tab);
}






/* --------------------------- GetDetectorAngleTable ---------------------------

Effect:  
          This function will retrieve the detector angle table from
       run_file and store the array of 4-byte floats at the location
       pointed to by det_angle_table. The first piece of valid 
       information is stored at location det_ang_table[1], this allows 
       access to information by each detector's number not (det_num - 1). 

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.
Output: 
        Detector angle table stored in the space pointed to by det_angle_table.
*--
*/

static void GetDetectorAngleTable(FILE              *run_file,
                                  DetAngleValueType *det_angle_table,
                                  TableType          table )
{
 DetAngleValueType *det_ang_tab;
 int                det,
                    n_dets;
 
 det_ang_tab = &(det_angle_table[1]);
 fseek(run_file, (long)table.location, SEEK_SET);
 fread((void *)det_ang_tab, (size_t)1, (size_t)table.size, run_file);

/* The following conversions are needed when not on VAX/VMS */

#ifndef VMS

  n_dets = table.size / 4;
  for ( det = 1; det <= n_dets; det++ )
    ConvertVaxFloat( &(det_angle_table[ det ]) );

#endif
}





/*--------------------- MallocFlightPathLengthTable ---------------------------

Effect:
        This function will allocate space for a flight path length table.
     size is the size of the whole table, and size can be obtained in 
     the field (flight_path) from the IPNSHeadType given by the 
     function GetIPNSHeader.

Input: 
       size.

Output:
       Space for the flight path length table.

*--
*/

static FlightPathLengthType *MallocFlightPathLengthTable(Int4 size)

{
 FlightPathLengthType *flight_tab;

 flight_tab = (FlightPathLengthType *)malloc( (size_t)(size) + 
                sizeof(FlightPathLengthType) );
 if ( flight_tab == NULL )
  {
   printf("Memory allocation error in MallocFlightPathLengthTable.\n");
   exit(0);
  }

 return(flight_tab);
}


/*--------------------------- GetFlightPathLength -----------------------------

Effect:  
          This function will retrieve the flight path length table from
       run_file and store the array of 4-byte floats at the location
       pointed to by flight_path_length_tab. The first piece of valid 
       information is located at flight_path_length_tab[1] to allow access 
       by detector number not (det_num -1) 

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.

Output: 
         Flight path length table stored in the space pointed to by 
      flight_path_length_tab.
*--
*/

static void GetFlightPathLength(FILE                 *run_file,
                                FlightPathLengthType *flight_path_table,
                                TableType             table )
{
 FlightPathLengthType *flght_path_tab;
 int                   det,
                       n_dets;
 
 flght_path_tab = &(flight_path_table[1]);
 fseek(run_file, (long)table.location, SEEK_SET);
 fread((void *)flght_path_tab, (size_t)1, (size_t)table.size, run_file);

/* The following conversions are needed when not on VAX/VMS */

#ifndef VMS
  n_dets = table.size / 4;
  for ( det = 1; det <= n_dets; det++ )
    ConvertVaxFloat( &(flight_path_table[ det ]) );

#endif
}


/*------------------------ MallocDetHeightValueTable ---------------------------

Effect:
        This function will allocate space for a detector height table.
     size is the size of the whole table, and size can be obtained in 
     the field (detector_height) from the IPNSHeadType given by the 
     function GetIPNSHeader.

Input: 
       size.

Output:
       Space for the detector height table.

*--
*/

static DetHeightValueType *MallocDetHeightValueTable(Int4 size)

{
 DetHeightValueType *height_tab;

 height_tab = (DetHeightValueType *)malloc( (size_t)(size) + 
               sizeof(DetHeightValueType) );
 if ( height_tab == NULL )
  {
   printf("Memory allocation error in MallocDetectorHeightTable.\n");
   exit(0);
  }

 return(height_tab);
}


/*------------------------- GetDetectorHeightTable ----------------------------

Effect:  
          This function will retrieve the detector height table from
       run_file and store the array of 4-byte floats at the location
       pointed to by detector_height_table. The first location in the 
       table is not used so that the table can be accessed using the 
       detector number not ( det_num -1 ). 

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.

Output: 
         Detector Height table stored in the space pointed to by 
       det_height_table.

*--
*/

static void GetDetectorHeightTable(FILE               *run_file, 
                                   DetHeightValueType *det_height_table,
                                   TableType           table )
{
 DetHeightValueType *det_height_tab;
 int                 det,
                     n_dets;
 
 det_height_tab = &(det_height_table[1]);
 fseek(run_file, (long)table.location, SEEK_SET);
 fread((void *)det_height_tab, (size_t)1, (size_t)table.size, run_file);

/* The following conversions are needed when not on VAX/VMS */

#ifndef VMS

  n_dets = table.size / 4;
  for ( det = 1; det <= n_dets; det++ )
    ConvertVaxFloat( &(det_height_table[ det ]) );

#endif
}

/*------------------------ MallocDetectorDiscLevels --------------------
Effect:
	This function will allocate space for a table of Discriminator
	levels for pulse height rejection.  size is the size of the whole
	table and can be obtained in the discriminator_settings.size field of
	header obtained by GetIPNSHeader.

Input:
	size

Output:
	space for the discriminator level table.
*__
*/
static DetDiscLevelType *MallocDetectorDiscLevels ( Int4 size )
{
 DetDiscLevelType *disc_tab;

 disc_tab = (DetDiscLevelType *)malloc( (size_t)(size +
			sizeof(DetDiscLevelType)) );

 if (disc_tab == NULL )
   {
    printf("Memoory allocation error in MallocDetectorDiscLevels.\n");
    exit(0);
   }
 return(disc_tab);
}

/*------------------------ GetDetectorDiscLevels -----------------------
Effect:

Input:
	run_file
	table

Output:
	Discriminator Level Tables

*__
*/
static void GetDetectorDiscLevels( FILE *run_file,
				   DetDiscLevelType *disc_level_table,
				   TableType   table)
{
 DetDiscLevelType *disc_tab;
#ifdef IPNS_BIG_ENDIAN
 int det, n_dets;
#endif

 disc_tab = &disc_level_table[1];
 fseek(run_file, (long)table.location, SEEK_SET );
 fread( (void *)disc_tab, (size_t)1, (size_t)table.size, run_file);

#ifdef IPNS_BIG_ENDIAN
 n_dets = table.size / (sizeof(DetDiscLevelType)*8);
 for (det = 0; det <= n_dets; det++)
   {
    ConvertShorttoBigEndian( &(disc_tab[ det ].lower) );
    ConvertShorttoBigEndian( &(disc_tab[ det ].upper) );
   }
#endif
}
/*------------------------ MallocPSD_map_buf ---------------------------

Effect:
        This function will allocate space for PSD_map_buf table.
     The input parameter size is the size of the whole table.

Input: 
       size.

Output:
       Space for the detector PSD_map_buf table.

*--
*/

static PSD_map_buf_entry *MallocPSD_map_buf(Int4 size)

{
 PSD_map_buf_entry  *map_buf;

 map_buf = (PSD_map_buf_entry *)malloc( (size_t)(size / 14 * 
                                         sizeof(PSD_map_buf_entry)) );
 if ( map_buf == NULL )
  {
   printf("Memory allocation error in MallocPSD_map_buf.\n");
   exit(0);
  }

 return(map_buf);
}

/*------------------------- GetPSD_map_buf ----------------------------

Effect:  
          This function will retrieve the PSD_map_buf table from the
       run_file and store it at the location pointed to by PSD_map_buf.

Input:
     run_file       The open run file
     table          Structure containing location and size of the table in
                    the runfile.

Output: 
         Detector Height table stored in the space pointed to by 
       det_height_table.

*--
*/

static void GetPSD_map_buf(FILE               *run_file, 
                           int                *max_bank,
                           int                *bank_start_ptr[],
                           PSD_map_buf_entry   PSD_map_buf[],
                           TableType           table )
{
 int    i;
 Int2 * buffer;

 int    bank_no, 
        n_dets_in_bank, 
        j,
        k;
 int    n_PSD_buf_entries,
        bank;

 buffer = ( Int2 * )malloc( (size_t)table.size );
 fseek(run_file, (long)table.location, SEEK_SET);
 fread((void *)buffer, (size_t)1, (size_t)table.size, run_file);

 i = 0;                             /* copy map buf into list of structs */
 j = 0;
 while ( i < table.size/2 )
 {
   bank_no              = buffer[i];
   n_dets_in_bank       = buffer[i+1]; 

   PSD_map_buf[j].bank_no = buffer[i];
   PSD_map_buf[j].n_dets  = buffer[i + 1];
   PSD_map_buf[j].det_no  = buffer[i + 2];
   PSD_map_buf[j].crate   = buffer[i + 3];
   PSD_map_buf[j].slot    = buffer[i + 4];
   PSD_map_buf[j].input   = buffer[i + 5];
   PSD_map_buf[j].min_id  = buffer[i + 6];
   i = i + 7;
   j++;
   for ( k = 0; k < n_dets_in_bank; k++ )
     {
       PSD_map_buf[j].bank_no = bank_no;
       PSD_map_buf[j].n_dets  = n_dets_in_bank;
       PSD_map_buf[j].det_no  = buffer[i];
       PSD_map_buf[j].crate   = buffer[i + 1];
       PSD_map_buf[j].slot    = buffer[i + 2];
       PSD_map_buf[j].input   = buffer[i + 3];
       PSD_map_buf[j].min_id  = buffer[i + 4];
       i = i + 7;
       j++;
     }
  }
  free( buffer );

  n_PSD_buf_entries = table.size / 14;
  *max_bank       = PSD_map_buf[ n_PSD_buf_entries - 1 ].bank_no;
 
#ifdef IPNS_FILEMANAGER_DEBUG
  printf("After increments, j = %d\n", j );
  printf("n_PSD_buf_entries   = %d\n", n_PSD_buf_entries);
  for ( j = 0; j < n_PSD_buf_entries; j++ )
    {
      printf("%6d",   PSD_map_buf[j].bank_no ); 
      printf("%6d",   PSD_map_buf[j].n_dets ); 
      printf("%6d",   PSD_map_buf[j].det_no ); 
      printf("%6d",   PSD_map_buf[j].crate ); 
      printf("%6d",   PSD_map_buf[j].slot ); 
      printf("%6d",   PSD_map_buf[j].input ); 
      printf("%6d\n", PSD_map_buf[j].min_id ); 
    }
#endif

  *bank_start_ptr = (int *) malloc( sizeof(int) * (*max_bank) );
  i = 0;
  for ( bank = 0; bank <= *max_bank; bank++ )
  {
    if ( PSD_map_buf[i].bank_no == bank )
    {
      (*bank_start_ptr)[bank] = i;
      i += PSD_map_buf[i].n_dets + 1;
    }
    else
      printf("Missing start of bank # %d\n", bank );
  }

#ifdef IPNS_FILEMANAGER_DEBUG
  for ( bank = 0; bank <= *max_bank; bank++ )
    printf("Bank %d starts at %d\n", bank, (*bank_start_ptr)[bank] );
#endif

 printf("\n");

}


/*---------------------- MallocDetSubgroupSizeTable ---------------------------

Effect:
        This function will allocate space for a detector subgroup size table.
     num_detectors is the number of detectors, and num_detectors can be 
     obtained in the field (n_det) from the IPNSHeadType given by the 
     function GetIPNSHeader.

Input: 
       num_detectors.

Output:
       Space for the detector subgroup size table.

*--
*/

static DetSubgroupSizeType *MallocDetSubgroupSizeTable(Int2 num_detectors)

{
 DetSubgroupSizeType *sub_group_tab;

 sub_group_tab = (DetSubgroupSizeType *)malloc( (size_t)(num_detectors) * 
               sizeof(DetSubgroupSizeType) +  sizeof(DetSubgroupSizeType));
 if ( sub_group_tab == NULL )
  {
   printf("Memory allocation error in MallocDetectorSubgroupSizeTable.\n");
   exit(0);
  }

 return(sub_group_tab);
}



/*------------------------- GetDetSubgroupSizeTable ----------------------------

Effect:  
          This function will calculate the detector subgroup size table from
       the run_file header and store the array of 2-byte ints at the location
       pointed to by ptr ->det_sub_group_size. The first location in the 
       table is not used so that the table can be accessed using the 
       detector number not ( det_num -1 ). THIS FUNCTION ASSUMES THAT
       THE DETECTORS ARE GROUPED THE SAME AS THE FIRST HISTOGRAM
       FOR ALL HISTOGRAMS

Input:
       run_file, size, location.

Output: 
       Detector subgroup size table stored in ptr->det_sub_group_size. 
*--
*/

static void GetDetSubgroupSizeTable(FilePointerType ptr)
{
#define  ID_NOT_PROCESSED     -1
#define  ID_IN_CURRENT_GROUP  -2

 int id, num_sg, new_id, n_marked;
 Int4 tf_type, address, tft;

#ifdef  IPNS_FILEMANAGER_DEBUG
 printf("Start of GetDetSubgroupSizeTable\n");
#endif

 for ( id = 0; id <= ptr->header.n_det; id++ )
   ptr -> det_sub_group_size[id] = ID_NOT_PROCESSED;

 for ( id = 1; id <= ptr -> header.n_det; id++)
 {
    if ( ptr -> det_sub_group_size[id] == ID_NOT_PROCESSED ) 
    {
      num_sg = 0;
      tf_type = ptr -> detector_map[id].tf_type;
      if (  (tf_type != 0) && (ptr -> time_field[tf_type].t_min != 0) &&
            (ptr -> time_field[tf_type].t_max != 0) &&
            (ptr -> time_field[tf_type].t_step != 0) )
       {
        address = ptr -> detector_map[id].address;
        for ( new_id = id; new_id <= ptr -> header.n_det; new_id++)
         if ( ptr -> detector_map[new_id].address == address )
          {
           tft = ptr -> detector_map[new_id].tf_type;
           if ( (tft != 0) && (ptr->time_field[tft].t_min != 0)
                && (ptr -> time_field[tft].t_max != 0)
                && (ptr -> time_field[tft].t_step != 0) )
             {
               ++ num_sg;
               ptr -> det_sub_group_size[new_id] = ID_IN_CURRENT_GROUP;
             }
          }
       }
      if ( num_sg == 0 )
        ptr -> det_sub_group_size[id] = 0;
      else
                                     /* assign size to all dets in subgroup */
      {
        new_id = id;
        n_marked = 0;
        while ( n_marked < num_sg )
        {
          if (ptr -> det_sub_group_size[new_id] == ID_IN_CURRENT_GROUP )
            {
              ptr -> det_sub_group_size[new_id] = num_sg;
              n_marked++;
            }
           new_id++;
         }  
      }
    }
 }         

#ifdef  IPNS_FILEMANAGER_DEBUG
 printf("End of GetDetSubgroupSizeTable\n");
#endif

}



/*++---------------------------- MallocTimeScaleTable -------------------------

Effect:
	This function will allocate space for the time scale table. (Used for 
   time focusing detectors.)

Input:
	size

Output:
	Pointer to allocated space

*--
*/
static TimeScaleType *MallocTimeScaleTable(Int4 size)
{
  TimeScaleType *time_scale_tab;

  time_scale_tab = (TimeScaleType *)malloc( (size_t)(size) +
		 sizeof( TimeScaleType));

  if(time_scale_tab == NULL)
     {
      printf("Memory allocation error in MallocTimeScaleTable. \n");
      exit(0);
     }
   return(time_scale_tab);
 }

/*++-------------------------GetTimeScaleTable --------------------------------

Effect:
	This function will retrieve the time scale table from the run file and store the array of 2 byte ints at the location pointed to by time_scale_tab.  The first piece of valid information is located at time_scale_tab[1] to allow
access by detector number (not detector number -1 ).

Input:
	runfile -  the open input file

	table - structure containing the location and size of the table in the
		runfile

Output:
	Time scale table stored in hte space pointed to by time_scale_table.

*--
*/

static void GetTimeScaleTable (FILE *runfile,
				TimeScaleType *time_scale_table,
				TableType table)
{
 TimeScaleType *time_scale_tab;
 int ii;

 time_scale_tab = &(time_scale_table[1]);
 fseek(runfile, (long)table.location, SEEK_SET);
 fread((void *)time_scale_tab, (size_t) 1, (size_t)table.size, runfile);

/* the following conversions are necessary on big endian machines only */
#ifdef IPNS_BIG_ENDIAN
 for (ii = 1; ii<= table.size/sizeof(TimeScaleType); ii++)
	ConvertShorttoBigEndian(&(time_scale_tab[ii]));
	printf("%d \n",time_scale_table[ii]);
#endif
}

/*++---------------------------- GetFileId -------------------------------------

Effect:
        This function will pass back the number of the file according 
   to its position in the array of files. If a zero is passed back then
   the file_handle is not an active file_handle at this time.

Input:
        A file_handle.

Output:
        The function passes back the file's id number, otherwise 
    a zero if that file_handle is no longer active.

*--
*/

static FileIdType GetFileId(FilePointerType file_handle)

{
 FileIdType temp_id, i = 0;
 int        found = 0;

 
 temp_id = 0;
 if (file_handle != NULL)
  {
   while ((++i <= MAX_NUM_FILES) && (found == 0))
         found = (file_list[i] == file_handle);
   if ( found != 0 )
      if ((file_handle -> file_id > 0) && (file_handle -> file_id <= 
                                               MAX_NUM_FILES))
         if ( file_list[file_handle->file_id] == file_handle )
              temp_id = file_handle -> file_id;
  } /* IF */
 return (temp_id);
} /* GetFileId */


/*++------------------------- GetOverflows -------------------------------

Effect:

        This function reads overflows from the overflow table, returning the
        overflows with addresses between first_addr and end_addr.

        NOTE:   The calling program is responsible for allocating memory
                for the resulting data. (try taking the number of overflows
                from the header ).

Input:

  file_handle, first_addr, end_addr

Output:

  data_ptr
        The block of memory pointed to by data_ptr, contains
        the addresses of the overflows between first_addr, and end_addr.
        Stored as 4 byte integers in a 1D array.

*--
*/

static int GetOverflows( FileHandleType  file_handle,
                         int             first_addr,
                         int             end_addr,
                         Int4           *data_ptr )
{

 FileIdType     id;
 int            i,
                count;
 short          num_overflows;
 Int4           offset,
                start_addr,
                oned_addr;
 Int4           *overflows;

 id = GetFileId( file_handle );

 start_addr = file_list[id]->header.hist_start_address;
 offset = 2 * file_list[id]->header.total_channels;
 num_overflows = file_list[id]->header.num_of_overflows;
 oned_addr = file_list[id]->header.address_1d_data;

 overflows = (Int4 *)malloc( 4 * num_overflows );

 fseek( file_list[id]->f,  start_addr + offset, SEEK_SET );
 fread( overflows, 4, num_overflows, file_list[id]->f );

 count = 1;
 for ( i = 0; i < num_overflows; i++ )
 {
#ifdef IPNS_BIG_ENDIAN
   ConvertInt4toBigEndian( &overflows[i] );
#endif

  if( overflows[i] > file_list[id]->header.address_2d_data
                && file_list[id]->header.num_wavelengths > 0 )
     {
        overflows[i] =
                ( overflows[i] - file_list[id]->header.address_2d_data );
     } else
     if ( overflows[i] > oned_addr )
     {
        overflows[i] = ( overflows[i] - oned_addr );
        if ( file_list[id]->header.num_wavelengths > 0 )
                overflows[i] = overflows[i] + offset;
     }
     if( ( overflows[i] >= first_addr ) && ( overflows[i] <= end_addr ) )
     {
       data_ptr[ count - 1 ]  = overflows[i];
       count++;
/*
       printf( "data_ptr        %d\n", data_ptr[ count -2] );
*/
     }
 }

 free( overflows );
 return count - 1;
}


/* --------------------------- IsMonitorChannel ---------------------------- */

static int IsMonitorChannel( FileHandleType file_handle, int det_num ) 
{
   switch ( IPNS_Instrument( file_handle ) )
   {
     case IPNS_HRMECS : 
     case IPNS_LRMECS : if ( det_num == 1 || det_num == 2 )
                          return( TRUE ); 
     default : return( FALSE );
   } 
}



/************************ Externally Visible Procedures ***********************
*******************************************************************************
******************************************************************************/

/*++---------------------------- InitFileList --------------------------------

Effect:
        To initialize the file list, and check that the MAX_NUM_FILES is 
 less than FOPEN_MAX( the maximum number of files allowed open at one time
 by the current system). You must initialize the file list before any of the 
 other functions can be used.

Input:
        None.

Return Value:
        None.
*--
*/

void InitFileList()

{
 FileIdType i;

 if ( MAX_NUM_FILES > FOPEN_MAX)
   {
    printf("Error in InitFileList!\n");
    printf("VFileManager uses %2d files and ",MAX_NUM_FILES);
    printf(" your system only allows %2d.\n",FOPEN_MAX);
    printf("Program is aborted.\n\n\n\n\n");
    exit(0);
   }
 else
   for ( i = 0; i <= MAX_NUM_FILES; ++i)
    {
     file_list[i] = NULL;
    }
} /* InitFileList */


/******************************************************************************
 
     The following include statement can be used to test the results 
obtained from using this program module, i.e. the information from the 
file is displayed on the screen to allow the user to see it. When this 
module is not being tested just comment out the include statement, and
the function call (in the function OpenInputFile) to the function 
TestIpnsFileManager();

******************************************************************************/

#ifdef  Test_IPNS_Filemanager_Internals
#include "IPNS_FileManagerInternalsTestSubs.c"
#endif

/*****************************************************************************/


/*++------------------------ OpenInputFile ------------------------------------

Effect:
          To open a file named (file_name) so that the user can read from
it. If the file can be opened the file_handle will not be NULL and the 
FileResultType will be FILE_OK. If a bad file name is given the FileResult
will be FILE_CANNOT_BE_OPENED. If too many files are already open the 
FileResult will be FILE_TOO_MANY_OPEN_FILES. If the file is already open
for writing then the FileResult will be FILE_OPEN_FOR_OUTPUT. String lengths
can only be as long as MAX_STR_LENGTH characters at which point the string
will be truncated and the resulting string will be used as the file_name.
Also if the string isn't terminated with a '\0' the name will end up to be
MAX_STR_LENGTH long and you will not get the right file.

Input:
          A file name.

Output:
         FileResult                 Output
         _________________________  _____________________________________
         FILE_OK                    Pointer to a good file handle.
         FILE_CANNOT_BE_OPENED      Pointer to a NULL file handle.
         FILE_TOO_MANY_OPEN_FILES   Pointer to a NULL file handle.
         FILE_OPEN_FOR_OUTPUT       Pointer to a NULL file hadnle.
*--
*/

FileResultType  OpenInputFile(FileHandleType *file_handle_ptr,
                              StringType      file_name)
{
 FileResultType   status;
 FileIdType       i;
 int              done;
 FileType         file;
 IPNSMachineType  inst_type;

 inst_type = IPNS_Instrument_From_Name( file_name );

#ifdef  IPNS_FILEMANAGER_DEBUG
 printf("In OpenInputFile, f_name = %s\n", file_name );   
#endif

 done = 0;
 status = FILE_OK;
 *file_handle_ptr = NULL;
 if ( file_name == NULL )
    status = FILE_CANNOT_BE_OPENED;
 else
   if ( strcmp (file_name,"") == 0)
     status = FILE_CANNOT_BE_OPENED;
   else
     /****** See if file is already open *****/
     for( i = 1; ( i <= MAX_NUM_FILES ) && ( done == 0); ++i)
     if ( file_list[i] != NULL )
        if ( strncmp(file_list[i] -> f_name, file_name, MAX_STR_LENGTH) == 0 )
             if ( file_list[i] -> file_mode == OPEN_FOR_INPUT )
                {
                  *file_handle_ptr = file_list[i];
                  file_list[i] -> num_users += 1;
                  done = 1;
                }
             else
                {
                 status = FILE_OPEN_FOR_OUTPUT;
                 done = 1;
                }
 if ( ((*file_handle_ptr) == NULL) && (status == FILE_OK) )
 /************** if file isn't open **************/
   {
    i = 1;
    done = 0;
    while (( i <= MAX_NUM_FILES) && ( done == 0 ))
       if (file_list[i] == NULL)
       /*** found a space so use it ***/
         {
          done = 1;
          strncpy(file.f_name, file_name, MAX_STR_LENGTH);
          file.f_name[MAX_STR_LENGTH] = '\0';
          file.f = fopen(file.f_name, "rb");
          if (file.f == NULL)
              status = FILE_CANNOT_BE_OPENED;
          else
            {
             file.file_mode = OPEN_FOR_INPUT;
             file.num_users = 1;
             file.file_id = i;
             GetIPNSHeader(file.f, &(file.header));

#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Header: n_histograms = %d, n_ids = %d\n",
                     file.header.num_of_histograms, file.header.n_det );
#endif

             if ( file.header.time_field_table.size <= 0 )
                  file.time_field = NULL;
             else
              {
               file.time_field = MallocTimeFieldTable(file.header.
                                                    num_of_time_fields);
               GetTimeFieldTable(file.f,file.time_field, 
                                 file.header.time_field_table, 
                                 file.header.num_of_time_fields );
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Time Field Table, size = %d, loc = %d\n",
                     file.header.time_field_table.size, 
                     file.header.time_field_table.location );
#endif
       
             if ( file.header.detect_map_table.size <= 0 )
                  file.detector_map = NULL;
             else
              {
               file.detector_map = MallocDetectorMapTable(file.header.n_det,
                                                        file.header.
                                                        num_of_histograms);
               GetDetectorMapTable(file.f, 
                                   inst_type,
                                   file.detector_map,
                                   file.header.detect_map_table, 
                                   file.header.n_det,
                                   file.header.num_of_histograms);
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Detector Map Table, size = %d, loc = %d\n",
                    file.header.detect_map_table.size,
                    file.header.detect_map_table.location  );
#endif

             if ( file.header.detector_angle.size <= 0 )
                   file.det_angle_table = NULL;
             else
              {
               file.det_angle_table = MallocDetectorAngleTable(
                                     file.header.detector_angle.size);
               GetDetectorAngleTable(file.f, 
                                     file.det_angle_table, 
                                     file.header.detector_angle );
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Detector Angle Table, size =%d, loc = %d\n",
                     file.header.detector_angle.size, 
                     file.header.detector_angle.location );
#endif

             if ( file.header.flight_path.size <= 0 )
                 file.flight_path_length_table = NULL;
             else
              {
               file.flight_path_length_table = MallocFlightPathLengthTable(
                                              file.header.flight_path.size);
               GetFlightPathLength(file.f, 
                                   file.flight_path_length_table,
                                   file.header.flight_path );
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Flight Path Length Table, size =%d, loc = %d\n",
                     file.header.flight_path.size, 
                     file.header.flight_path.location );
#endif

             if ( file.header.detector_height.size <= 0 )
                   file.det_height_table = NULL;
             else
              {
               file.det_height_table = MallocDetHeightValueTable(
                                      file.header.detector_height.size);
               GetDetectorHeightTable(file.f, 
                                      file.det_height_table,
                                      file.header.detector_height );
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Detector Height Table, size =%d, loc = %d\n",
                     file.header.detector_height.size,
                     file.header.detector_height.location );
#endif

	     if ( file.header.discriminator_settings.size <= 0 )
		    file.det_disc_levels = NULL;
	     else
	      {
		file.det_disc_levels = MallocDetectorDiscLevels(
				      file.header.discriminator_settings.size);
		GetDetectorDiscLevels(file.f,
					file.det_disc_levels,
					file.header.discriminator_settings );
		}
#ifdef IPNS_FILEMANAGER_DEBUG
	      printf("Got Detector Discriminator Levels, size =%d, loc = %d\n",
			file.header.discriminator_settings.size,
			file.header.discriminator_settings.location );
#endif
             
	     if( file.header.time_scale_table.size <= 0 )
		     file.time_scale_table = NULL;
	     else
	      {
		file.time_scale_table = MallocTimeScaleTable(
					file.header.time_scale_table.size);
		GetTimeScaleTable(file.f,
					file.time_scale_table,
					file.header.time_scale_table);
		}
#ifdef IPNS_FILEMANAGER_DEBUG
		printf("Got Time Scale Table, size =%d, loc =%d\n",
			file.header.time_scale_table.size,
			file.header.time_scale_table.location );
#endif
             if ( file.header.PSD_ID_map.size <= 0 )
              {
                file.PSD_map_buf = NULL;
                file.bank_start  = NULL;
              }
             else
              {
               file.PSD_map_buf = MallocPSD_map_buf( 
                                      file.header.PSD_ID_map.size);

               GetPSD_map_buf(file.f,
                             &file.max_bank,
                             &file.bank_start,
                              file.PSD_map_buf,
                              file.header.PSD_ID_map );
              }
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got Detector Height Table, size =%d, loc = %d\n",
                     file.header.detector_height.size,
                     file.header.detector_height.location );
#endif


             if ( (file.time_field == NULL) || 
                  (file.detector_map == NULL) )
                 file.det_sub_group_size = NULL;
             else
              {
               file.det_sub_group_size = MallocDetSubgroupSizeTable(
                        file.header.n_det);
               GetDetSubgroupSizeTable(&file);
              }  
#ifdef IPNS_FILEMANAGER_DEBUG
             printf("Got DetSubgroupSizeTable\n" );
#endif
             file_list[i] = (FilePointerType)malloc(sizeof(FileType));
             *file_handle_ptr = file_list[i];
             *file_list[i] = file;
            } /* else */
         }  /* if */  
       else
         i += 1;
    if ( i == MAX_NUM_FILES + 1 )
        status = FILE_TOO_MANY_OPEN_FILES;
   } /* IF */


 /***************************************************************************

    The following function call is used only when this module is being 
 tested. The function prints the information being read from the file
 to the screen so the user may see it. When testing is not being done
 the function should be commented out along with the include statement
 above this function. When testing it will be necessary to use the above
 include statement.

****************************************************************************/
/*
#ifdef  Test_IPNS_Filemanager_Internals
 printf("At end of OpenInputFile, status = %d\n", status );
 if ( status == FILE_OK )
   TestIPNSFileManager(*file_handle_ptr);
#endif
*/
/***************************************************************************/


 return(status);
} /* OpenInputFile */


/*++------------------------- CloseFile ---------------------------------------

Effect:
         Close the file associated with the file_handle pointed to by file
     handle_ptr. If FileResult is FILE_OK the file was closed, otherwise the 
     FileResult will be FILE_NOT_OPEN if the file handle was no longer active.

Input:
        Pointer to a file_handle.

Return Value:
        FileResult.
*--
*/

FileResultType CloseFile(FileHandleType *file_handle_ptr)

{
 FileIdType     id;
 FileResultType status;


 status = FILE_NOT_OPEN;
 if ( file_handle_ptr != NULL )
  {
   id = GetFileId(*file_handle_ptr);
   if ( id != 0 )
    {
     file_list[id] -> num_users -= 1;
     if ( file_list[id] -> num_users == 0 )
      {
#ifdef IPNS_FILEMANAGER_DEBUG
       printf("CLOSING FILE & FREEING SPACE................\n");
#endif
       if (file_list[id] -> file_mode != OPEN_BUT_PAUSED)
         fclose( file_list[id] -> f );

       FreeTable( (void *)file_list[id] -> time_field );
       FreeTable( (void *)file_list[id] -> detector_map );
       FreeTable( (void *)file_list[id] -> det_angle_table );
       FreeTable( (void *)file_list[id] -> flight_path_length_table );
       FreeTable( (void *)file_list[id] -> det_height_table );
       FreeTable( (void *)file_list[id] -> det_sub_group_size );
       FreeTable( (void *)file_list[id] -> det_disc_levels );
       FreeTable( (void *)file_list[id] -> time_scale_table );
       if ( file_list[id] -> PSD_map_buf != NULL )
       {
         FreeTable( (void *)file_list[id] -> PSD_map_buf ); 
         free(      (void *)file_list[id] -> bank_start );
       }
       free( file_list[id] );
       file_list[id] = NULL;
      }
     *file_handle_ptr = NULL;
     status = FILE_OK;
    }
  }
 return( status );
} /* CloseFile */




/*++------------------------- PauseFile ---------------------------------------

Effect:
        Keep all of the initial data structures for the data file active,
        but close the file on disk temporarily.  This is useful because some
        systems will prevent the file from being updated by the data 
        acquisition software if the file is left open, even for reading only. 
        USE THIS CAREFULLY.  THE CALLING PROGRAM IS RESPONSIBLE FOR CALLING
        "ResumeFile" BEFORE ANY FURTHER USE IS MADE OF THE DATA FILE!!
Input:
        A file_handle.

Return Value:
        FileResult.
*--
*/

FileResultType PauseFile(FileHandleType file_handle)
{
 FileIdType     id;
 FileResultType status;

 status = FILE_NOT_OPEN;
 id = GetFileId(file_handle);
 if ( id != 0 )
  {
    fclose( file_list[id] -> f );
    file_list[id] -> f = NULL;
    file_list[id] -> file_mode = OPEN_BUT_PAUSED;
    status = FILE_PAUSED;
  }

 return( status );
} /* PauseFile */


/*++------------------------- ResumeFile ---------------------------------------

Effect:
        Reopen a file that was temporarily closed using "PauseFile".
Input:
        A file_handle.

Return Value:
        FileResult.
*--
*/

FileResultType ResumeFile(FileHandleType file_handle)

{
 FileIdType     id;
 FileResultType status;

 status = FILE_NOT_OPEN;
 id = GetFileId(file_handle);
 if ( id != 0 )
  {
    file_list[id]->f = fopen(file_list[id]->f_name, "rb");
    file_list[id] -> file_mode = OPEN_FOR_INPUT;
    status = FILE_OPEN_FOR_INPUT;
  }

 return( status );
} /* ResumeFile */


/*++------------------------- IPNS_Instrument ----------------------------

Effect:
       Try to determine the IPNS instrument corresponding to a file that was
    opened using OpenFile from this unit.

Input:
        The file_handle returned when the file was opened using OpenFile.

Return Value:
        The machine name for the specified file.  If the machine type cannot
     be determined, this returns "IPNS_UNKNOWN".
*--
*/

IPNSMachineType IPNS_Instrument( FileHandleType  file_handle )
{
  FileIdType       id;

  id = GetFileId(file_handle);
  if ( id == 0 )
    return( IPNS_UNKNOWN );

  return( IPNS_Instrument_From_Name( file_list[id]->f_name ) );
}


/*++----------------------- NumberOfDetectors ---------------------------------

Effect:
        This function returns the number of detectors associated with 
 the runfile that is pointed to by file_handle. The function will return
 zero when there is a problem retreiving the number of detectors for the
 file_handle given.

Input:
       file_handle.

Return Value:
       The number of detectors.
*--
*/

int NumberOfDetectors(FileHandleType file_handle)
{
 FileIdType id;
 int        n_det = 0;

 id = GetFileId(file_handle);
 if ( id != 0 )
   n_det = file_list[id]->header.n_det;
 return ( n_det );
}


/*++----------------------- NumberOfHistograms ---------------------------------

Effect:
        This function returns the number of histograms associated with 
 the runfile that is pointed to by file_handle. The function will return
 zero when there is a problem retreiving the number of histograms for the
 file_handle given.

Input:
       file_handle.

Return Value:
       The number of histograms.
*--
*/

int NumberOfHistograms(FileHandleType file_handle)
{
 FileIdType id;
 int        n_hist = 0;

 id = GetFileId(file_handle);
 if ( id != 0 )
   n_hist = file_list[id]->header.num_of_histograms;
 return ( n_hist );
}


/*++-------------------------- NumberOfBanks -------------------------------

Effect:
        This function returns the number of banks of LPSD's present in the
 run file. 

Input:
       file_handle.

Return Value:
       The number of banks of LPSDs in this run file.
*--
*/

int NumberOfBanks(FileHandleType file_handle )
{
 FileIdType id;
 int        n_banks = -1;

 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->PSD_map_buf != NULL )
     n_banks = file_list[id]->max_bank + 1;

 return ( n_banks );
}

/*++--------------------- SizeOfDataArea -------------------------------------

Effect:
	This function returns the size of the data area in this file.

Input:
	file_handle

Return Value:
	The size of the data area in this file.

*--
*/

int SizeOfDataArea ( FileHandleType file_handle )
{
 FileIdType id;
 int size = 0;

 id = GetFileId ( file_handle );
 if ( id != 0 )
    size = file_list[id]->header.size_of_data_area;
 return ( size );
}

/*++--------------------- HistStartAddress ----------------------------------

Effect:
	This function returns the location of the start of the data area in
	this file.

Input:
	file_handle

Return Value:
	The Byte offset to the start of the data area in this file.

*--
*/

int HistStartAddress ( FileHandleType file_handle )
{
 FileIdType id;
 int start_addr = 0;

 id = GetFileId ( file_handle );
 if ( id != 0 )
    start_addr = file_list[id]->header.hist_start_address;
 return ( start_addr );
}

/*++--------------------- NextFreeBlock -------------------------------------

Effect:
	This function returns the location of the start of the next free block
	in this file.

Input:
	file_handle

Return Value:
	The Byte offset to the next free block in this file.

*--
*/

int NextFreeBlock ( FileHandleType file_handle )
{
 FileIdType id;
 int next_free = 0;

 id = GetFileId ( file_handle );
 if ( id != 0 )
    next_free = file_list[id]->header.offset_to_free;
 return ( next_free );
}

/*++--------------------- TotalNumberOfChannels -----------------------------

Effect:
	This function returns the total data channels
	in this file.

Input:
	file_handle

Return Value:
	The total number of data channels in this file.

*--
*/

int TotalNumberOfChannels ( FileHandleType file_handle )
{
 FileIdType id;
 int total_channels = 0;

 id = GetFileId ( file_handle );
 if ( id != 0 )
    total_channels = file_list[id]->header.total_channels;
 return ( total_channels );
}

/*++--------------------- NumberOfDetectorsInBank ----------------------------

Effect:
        This function returns the number of detectors in a particular bank
 of Linear Position Sensitive detectors. 

Input:
       file_handle   
       bank         

Return Value:
       The number of detectors in the specifed bank.
*--
*/

int NumberOfDetectorsInBank(FileHandleType file_handle, int bank )
{
 FileIdType id;
 int        n_det = 0;
 int        index;

 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->PSD_map_buf != NULL )
   {
     index = file_list[id]->bank_start[bank]; 
     n_det = file_list[id]->PSD_map_buf[index].n_dets;
   }

 return ( n_det );
}


/*++--------------------------- LPSD_SpecID --------------------------------

Effect:
        This function returns the spectrum "ID" of a particular element of
  a particular detector in a specified bank of LPSDs.  The spectrum ID can
  then be used to access the spectrum, angle, flight path, etc.

Input:
       file_handle.

Return Value:
       The id of the spectrum corresponding to the specified bank, detector
  and element.

*--
*/

int LPSD_SpecID( FileHandleType file_handle, 
                 int            bank,
                 int            detector,
                 int            element     )
{
  FileIdType id;
  int        spec_id = 0;
  int        index;
  int        n_dets;

  id = GetFileId(file_handle);
  if ( id != 0 )
    if ( file_list[id]->PSD_map_buf != NULL )
    {
      index  = file_list[id]->bank_start[bank]; 
      n_dets = file_list[id]->PSD_map_buf[index].n_dets;
      if ( n_dets > 0 && detector > 0 && detector <= n_dets )
        {
          index += detector;         
          spec_id = file_list[id]->PSD_map_buf[index].min_id; 
          if ( spec_id != 0 )
            spec_id = spec_id + element - 1; 
        }
    }

  return ( spec_id );
}


/*++-------------------------- GetCrateInfo -------------------------------

Effect:
      This function gets the crate, slot and input corresponding to a 
  particular LPSD in a particular bank.

Input:
      file_handle   
      bank         
      detector

Output:
      crate         The crate number for this detector
      slot          The slot number of this detector
      input         The input number of this detector

Return Value:
      Returns TRUE if the information was available. 
*--
*/

int GetCrateInfo(FileHandleType   file_handle, 
                 int              bank, 
                 int              detector,
                 int             *crate,
                 int             *slot,
                 int             *input   )
{
  FileIdType  id;
  int         n_dets = 0;
  int         index;
  int         status;

  status = FALSE;
  id = GetFileId(file_handle);
  if ( id != 0 )
    if ( file_list[id]->PSD_map_buf != NULL )
    {
      index  = file_list[id]->bank_start[bank]; 
      n_dets = file_list[id]->PSD_map_buf[index].n_dets;
      if ( n_dets > 0 && detector > 0 && detector <= n_dets )
      {
        index += detector;         
        *crate = file_list[id]->PSD_map_buf[index].crate; 
        *slot  = file_list[id]->PSD_map_buf[index].slot; 
        *input = file_list[id]->PSD_map_buf[index].input; 
        status = TRUE;
      }
    }

  return ( status );
}


/*++----------------------- NumberOfTimeChannels ------------------------------
 
Effect:
        This function returns the number of time channels associated with 
 the runfile that is pointed to by file_handle and the particular 
 detector(det_num) and histogram(hist_num). The function will return
 zero when there is a problem retreiving the number of timechannelss for the
 file_handle given.

Input:
       file_handle.
       det_num.
       hist_num.

Return Value:
       The number of time channels for the particular detector and 
   histogram numbers given.

*--
*/

int NumberOfTimeChannels(FileHandleType file_handle, 
                         int            det_num,
                         int            hist_num)
{
 FileIdType id;
 int        num_time_channels,
            index;
 Int4       tf_type;

 num_time_channels = 0;
 id = GetFileId(file_handle);
 if ( id != 0 )
  if ( (file_list[id] -> time_field != NULL) &&
       (file_list[id] -> detector_map != NULL) )
    if ( (det_num >= 1)                           && 
         (det_num <= file_list[id]->header.n_det) &&
         (hist_num >= 1)                          && 
         (hist_num <= file_list[id]->header.num_of_histograms))
     {
      index = ((hist_num -1) * file_list[id]->header.n_det) + det_num;
      tf_type = file_list[id]->detector_map[index].tf_type;
      if ( (tf_type != 0) && (file_list[id]->time_field[tf_type].t_min != 0)
           && (file_list[id]->time_field[tf_type].t_max != 0)
           && (file_list[id]->time_field[tf_type].t_step != 0) )
        num_time_channels = (int)((file_list[id]->time_field[tf_type].t_max -
                        file_list[id]->time_field[tf_type].t_min) /
                        file_list[id]->time_field[tf_type].t_step);

/* #### removed -1  DJM, 8/4/94
                        (file_list[id]->time_field[tf_type].t_step) - 1);
*/
     }
 return(num_time_channels);   
}


/*++--------------------- NumberOfTimeFieldTypes ----------------------------

Effect:
      This function returns the number of "time field types" in this file.
 Specifically, different spectra could be based on different time ranges and
 different time bin sizes.  Since typically there are only a few different
 time ranges and bin sizes used for a large number of detectors, only one
 copy of the time channel information is needed for each "time field type".
   The function will return zero when there is a problem retreiving the 
 information for the file_handle given.

Input:
       file_handle.

Return Value:
       The number of time field types for this file.

*--
*/

int NumberOfTimeFieldTypes( FileHandleType  file_handle )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   return( file_list[id]->header.num_of_time_fields ); 
 return( 0 );  
}


/*++----------------------------- PresetMonitorCounts ---------------------

Effect:
     This function returns the number of preset monitor counts.  This number
determines the duration of a cycle during acquisition.  At the end of each 
cycle of this many counts, data is saved and the number of preset cycles is
checked to see if the run is completed.

Input:
      file_handle.
Return Value:
      The Number of preset monitor counts per cycle.

*--
*/

int PresetMonitorCounts( FileHandleType file_handle )
{
 FileIdType	id;
 int monitorCounts;

 monitorCounts = -1;
 id = GetFileId( file_handle );
 if ( id != 0 )
      monitorCounts = file_list[id]->header.preset_monitor_counts;
 return ( monitorCounts );
}

/*++----------------------------- PresetCycleLimit ---------------------

Effect:
     This function returns the number of preset cycles in a run.  This number,
combined with the preset monitor counts determines the duration of a run.  At 
the end of each cycle, data is saved and the number of preset cycles is 
checked to see if the run is completed.

Input:
      file_handle.
Return Value:
      The Number of preset cycles for this run.

*--
*/

int PresetCycleLimit( FileHandleType file_handle )
{
 FileIdType	id;
 int cycleLimit;

 cycleLimit = -1;
 id = GetFileId( file_handle );
 if ( id != 0 )
      cycleLimit = file_list[id]->header.num_of_cycles_preset;
 return ( cycleLimit );
}

/*++----------------------------- CompletedCycles ---------------------

Effect:
     This function returns the number of completed cycles.  This number
is the actual number that were collected.  This may differ from the preset
if the run aborted due to error or if the run was manually stopped before 
the preset time had elapsed.

Input:
      file_handle.
Return Value:
      The Number of cycles completed so far in this run.

*--
*/

int PresetCyclesCompleted( FileHandleType file_handle )
{
 FileIdType	id;
 int cycles;

 cycles = -1;
 id = GetFileId( file_handle );
 if ( id != 0 )
      cycles = file_list[id]->header.num_of_cycles_completed;
 return ( cycles );
}

/*++----------------------------- ElapsedMonitorCounts ---------------------

Effect:
     This function returns the total number of elapsed monitor counts.  

Input:
      file_handle.
Return Value:
      The Number of total elapsed monitor counts for this run.

*--
*/

int ElapsedMonitorCounts( FileHandleType file_handle )
{
 FileIdType	id;
 int monitorCounts;

 monitorCounts = -1;
 id = GetFileId( file_handle );
 if ( id != 0 )
      monitorCounts = file_list[id]->header.total_monitor_counts;
 return ( monitorCounts );
}

/*++----------------------------- GetFileName -----------------------------

 Effect:

   Get the file name of an open run file.

 Input:
        file_handle

 Output:
        The file name is returned in the string file_name.
        NOTE: The calling program MUST PROVIDE STORAGE space for a
              character string of up to MAX_STRING_LENGTH+1 CHARACTERS.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.
*--
*/

DataResultType  GetFileName( FileHandleType  file_handle,
                             char           *f_name       )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( f_name, file_list[id]->f_name );
     DropTrailingBlanks( f_name );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetUserName -----------------------------

 Effect:

   Get the user name from the run file.

 Input:
        file_handle

 Output:
        The user name is returned in the string user_name.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetUserName( FileHandleType  file_handle,
                             char           *user_name     )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( user_name, file_list[id]->header.user_name ); 
     DropTrailingBlanks( user_name );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetRunTitle -----------------------------

 Effect:

   Get the run title from the run file.

 Input:
        file_handle

 Output:
        The title is returned in the string run_title.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetRunTitle( FileHandleType  file_handle,
                             char            *run_title     )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( run_title, file_list[id]->header.run_title );
     DropTrailingBlanks( run_title );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetRunNum -----------------------------

 Effect:

   Get the run number as a character string from the run file.

 Input:
        file_handle

 Output:
        The run number is returned in the string run_num.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetRunNum( FileHandleType  file_handle,
                           Int4           *run_num      )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     *run_num = file_list[id]->header.run_num;
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetNextRun -----------------------------

 Effect:

   Get next run number as a character string from the run file.

 Input:
        file_handle

 Output:
        The next run number is returned in the string next_run.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetNextRun( FileHandleType  file_handle,
                            Int4            *next_run      )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     *next_run = file_list[id]->header.next_run;
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetStartDate -----------------------------

 Effect:

   Get start date from the run file.

 Input:
        file_handle

 Output:
        The start date is returned in the string start_date.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetStartDate( FileHandleType  file_handle,
                              char           *start_date   )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( start_date, file_list[id]->header.start_date );
     DropTrailingBlanks( start_date );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetStartTime -----------------------------

 Effect:

   Get start time from the run file.

 Input:
        file_handle

 Output:
        The start time is returned in the string start_time.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetStartTime( FileHandleType  file_handle,
                              char           *start_time   )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( start_time, file_list[id]->header.start_time );
     DropTrailingBlanks( start_time );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetEndDate -----------------------------

 Effect:

   Get end date from the run file.

 Input:
        file_handle

 Output:
        The end date is returned in the string end_date.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetEndDate( FileHandleType  file_handle,
                            char           *end_date   )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( end_date, file_list[id]->header.end_date );
     DropTrailingBlanks( end_date );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++----------------------------- GetEndTime -----------------------------

 Effect:

   Get end time from the run file.

 Input:
        file_handle

 Output:
        The end time is returned in the string start_time.
        NOTE: The calling program MUST PROVIDE STORAGE space for the
              character string.

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetEndTime( FileHandleType  file_handle,
                            char           *end_time   )
{
 FileIdType     id;

 id = GetFileId(file_handle);
 if ( id != 0 )
   {
     strcpy( end_time, file_list[id]->header.end_time );
     DropTrailingBlanks( end_time );
     return( DATA_OK );
   }
 return( DATA_ERROR );
}


/*++---------------------- GetTimeFieldType ----------------------------------

Effect:
         This function gets the time field type for the specified detector,
 and histogram in the specified runfile.  The function returns DATA_ERROR,
 if an error occurs and DATA_OK if there is no error.

Input:
       file_handle, det_num, and hist_num.

Output:

   tf_type   The time field type for the given detector and histogram number.
             This will be a value in  [1, NumberOfTimeFieldTypes( file_handle)]
             if it is valid.  It will be set to 0 otherwise

 Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetTimeFieldType ( FileHandleType  file_handle,
                                  int             det_num,
                                  int             hist_num,
                                  int            *tf_type  )
{
 FileIdType     id;
 int            index;

 id = GetFileId(file_handle);
 if ( id != 0 )
  if ( (file_list[id] -> time_field != NULL) &&
       (file_list[id] -> detector_map != NULL) )
    if ( (det_num >=1) && (det_num <= file_list[id]->header.n_det) &&
         (hist_num>=1) && (hist_num<=file_list[id]->header.num_of_histograms))
     {
      index = ((hist_num -1) * file_list[id]->header.n_det) + det_num;
      *tf_type = file_list[id]->detector_map[index].tf_type;
      return( DATA_OK );
     }
 *tf_type = 0;
 return( DATA_ERROR );
}


/*++----------------------- GetSpectrometerTimeFieldData ---------------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.
 This function checks whether or not "time focusing" has been done and corrects
 the min_TOF and max_TOF values accordingly.  This version is valid for the
 chopper instruments at IPNS.  The function will return status
 "DATA_ERROR" when there is a problem retrieving the information for the
 file_handle given and will return "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in 
                     [ 1, NumberOfTimeFieldTypes(file_handle) ] 
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetSpectrometerTimeFieldData( FileHandleType  file_handle,
                                              int             tf_type,
                                              float          *min_TOF,
                                              float          *max_TOF,
                                              int            *n_channels )
{
 FileIdType     id;
 float          us_to_sample,
                us_correction;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
  if (file_list[id] -> time_field != NULL)
     {

#ifdef IPNS_FILEMANAGER_DEBUG
      printf("In GetSpectrometerTimeFieldData, tf_type = %d, t_step = %f, e_in = %f\n",
                tf_type, 
                file_list[id]->time_field[tf_type].t_step,
                file_list[id]->header.energy_in  );
#endif

      if ( (tf_type > 0)                                      && 
           (tf_type <= NumberOfTimeFieldTypes(file_handle))   &&
           (file_list[id]->time_field[tf_type].t_step > 0 )   &&
           (file_list[id]->header.energy_in  > 0)         )
        {
          *min_TOF    = file_list[id]->time_field[tf_type].t_min;
          *max_TOF    = file_list[id]->time_field[tf_type].t_max;
          *n_channels = (int)((file_list[id]->time_field[tf_type].t_max -
                               file_list[id]->time_field[tf_type].t_min) /
                               file_list[id]->time_field[tf_type].t_step);

          /* now adjust for time focusing, following   */
          /* John Hammonds, GET_IPNS_DATA_SOURCE.FOR   */


          us_to_sample = file_list[id]->header.source_to_sample
                         / sqrt( file_list[id]->header.energy_in/5.22707e6 );

           if ( file_list[id]->header.pseudo_time_unit != 'I' )
            {
                us_correction = 0;
                /* no time focusing done */
                *min_TOF = *min_TOF + us_correction;
                *max_TOF = *max_TOF + us_correction;
                }
          else
            {
              if ( file_list[id]->time_field[tf_type].time_focus_bit == 0 )
                {                             /* this time field NOT focused */
                   us_correction = -file_list[id]->header.hard_time_delay * 
					file_list[id]->header.standard_clock
                                + us_to_sample;

                   *min_TOF = *min_TOF + us_correction ;
                   *max_TOF = *max_TOF + us_correction ;
                   }
              else
                {                             /* this time field WAS FOCUSED */
                  us_correction = 0;
                  /*us_correction = us_to_sample;*/
                  *min_TOF = *min_TOF + us_correction;
                  *max_TOF = *max_TOF + us_correction;
                  }
             }


#ifdef IPNS_FILEMANAGER_DEBUG
           printf("min_TOF = %f, max_TOF = %f, n_channels = %d\n",
                  *min_TOF, *max_TOF, *n_channels );
#endif
           return( DATA_OK );
        }
     }
 *min_TOF    = 0;
 *max_TOF    = 0;
 *n_channels = 0;
 return( status );
}



/*++----------------------- GetTimeFieldData ---------------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.
 This function checks whether or not "time focusing" has been done and corrects
 the min_TOF and max_TOF values accordingly.  This version is valid for the
 chopper instruments at IPNS.  The function will return status
 "DATA_ERROR" when there is a problem retrieving the information for the
 file_handle given and will return "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in 
                     [ 1, NumberOfTimeFieldTypes(file_handle) ] 
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetTimeFieldData( FileHandleType  file_handle,
                                              int             tf_type,
                                              float          *min_TOF,
                                              float          *max_TOF,
                                              int            *n_channels )
{
DataResultType status;
status = GetSpectrometerTimeFieldData( file_handle, tf_type, min_TOF, 
							max_TOF, n_channels);
return (status);

}
/*++--------------------- GetDiffractometerTimeFieldData ---------------------

Effect:
        This function returns the min and max time of flight and number of
 time channels for the specified "time field type" in the specified file.

 NOTE: Currently this function just returns the min and max time values from
       the data file and does not check to see if time focussing has been
       done.

  The function will return status "DATA_ERROR" when there is a problem
  retrieving the information for the file_handle given and will return
  "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.

       tf_type       The time field type.  This must be an integer in
                     [ 1, NumberOfTimeFieldTypes(file_handle) ]
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given.

      n_channels  The number of time channels for this time field type

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetDiffractometerTimeFieldData( FileHandleType  file_handle,
                                                int             tf_type,
                                                float          *min_TOF,
                                                float          *max_TOF,
                                                int            *n_channels )
{
 FileIdType     id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
  if (file_list[id] -> time_field != NULL)
     {
      if ( (tf_type > 0)                                    && 
           (tf_type <= NumberOfTimeFieldTypes(file_handle)) &&
           (file_list[id]->time_field[tf_type].t_step > 0 )    )
        {
          *min_TOF    = file_list[id]->time_field[tf_type].t_min;
          *max_TOF    = file_list[id]->time_field[tf_type].t_max;
          *n_channels = (int)((file_list[id]->time_field[tf_type].t_max -
                               file_list[id]->time_field[tf_type].t_min) /
                               file_list[id]->time_field[tf_type].t_step);
          return( DATA_OK );
        }
     }
 *min_TOF    = 0;
 *max_TOF    = 0;
 *n_channels = 0;
 return( status );
}



/*++----------------------- GetHardwareTimeFields ------------------------

Effect:
	This function retrieves the detector time field information for
a particular detector.  It returns the Minimum time, maximumum time and time 
step for this detector in units of clock ticks so that the detector hardware 
can be setup.  

  The function will return status "DATA_ERROR" when there is a problem
  retrieving the information for the file_handle given and will return
  "DATA_OK" if there is no problem.

Input:
       file_handle   Identifier for the file.
       detectorID    detector ID for which the Time field information is to be
                     returned
       hist_num      Histogram number for the specified data
Output:

      min_TOF,
      max_TOF     The min and max time of flight for the particular detector
                  and histogram numbers given in clock ticks.
      step_TOF    the step size for this detector
*/
DataResultType GetHardwareTimeFieldData( FileHandleType file_handle,
					 int		detectorID,
					 int		hist_num,
					 float		*min_TOF,
					 float		*max_TOF,
					 float		*step_TOF)

{
FileIdType id;
DataResultType status;
int tf_type;
float clock_period;
float us_correction;
int index;

status = DATA_ERROR;
id = GetFileId(file_handle);
clock_period = file_list[id]->header.standard_clock;
if(id != 0 )
  if ( (file_list[id] -> time_field != NULL)   &&
       (file_list[id] -> detector_map != NULL) )
     {
      if ( (detectorID >= 1) && 
           (detectorID <= file_list[id] -> header.n_det) &&
           (hist_num >= 1) && 
           (hist_num <= file_list[id]->header.num_of_histograms))
      {
       index = ((hist_num - 1) * file_list[id]->header.n_det) + detectorID;
       tf_type = file_list[id] -> detector_map[index].tf_type;
       if ( (tf_type > 0) &&
            (tf_type <= NumberOfTimeFieldTypes(file_handle)) &&
            (file_list[id] -> time_field[tf_type].t_step > 0 ))
          {
           switch ( file_list[id]->header.pseudo_time_unit ){
           case 'I':    /* Spectrometer time focusing  */
            {
              if( file_list[id]->time_field[tf_type].time_focus_bit == 0)
		{
                   us_correction = 0; 
		   *min_TOF = (int) ( (file_list[id]->time_field[tf_type].t_min - 
                               us_correction)/clock_period);
		   *max_TOF = (int) ((file_list[id]->time_field[tf_type].t_max- 
                              us_correction) /clock_period) ;
                   *step_TOF  = (int)(file_list[id]->time_field[tf_type].t_step 
                               / clock_period);
		}  /*  End if time_focus_bit == 0  */
              else
                {
                   us_correction = -file_list[id]->header.hard_time_delay
				* clock_period;

		   *min_TOF = (float) ( (file_list[id]->time_field[tf_type].t_min
		/(1 + file_list[id]->time_scale_table[detectorID]/pow(2,15))
                                  - us_correction)/clock_period);
		   *max_TOF = (float) ( (file_list[id]->time_field[tf_type].t_max
		/(1 + file_list[id]->time_scale_table[detectorID]/pow(2,15))
                                  - us_correction) /clock_period);
                   *step_TOF  = (float)(file_list[id]->time_field[tf_type].t_step 
		/(1 + file_list[id]->time_scale_table[detectorID]/pow(2,15))
                               / clock_period );

                }  /* end else for time_focus_bit == 0 */
                break;
            } /* End Spectrometer focusing case 'I' */      
	   case 'D':    /* Diffractometer time focusing */
            {
               *min_TOF  = file_list[id]->time_field[tf_type].t_min / 
                                clock_period;
               *max_TOF  = file_list[id]->time_field[tf_type].t_max / 
                                clock_period;
               *step_TOF  = file_list[id]->time_field[tf_type].t_step / 
                                clock_period;
               break;
             }     /* End Diffractometer focusing  */
           default:     /* Default case - NO focusing */
            {
                   us_correction = -file_list[id]->header.hard_time_delay
				* clock_period;
               *min_TOF  = (file_list[id]->time_field[tf_type].t_min 
				- us_correction)/ clock_period;
               *max_TOF  = (file_list[id]->time_field[tf_type].t_max 
				- us_correction)/ clock_period;
               *step_TOF  = file_list[id]->time_field[tf_type].t_step / 
                                clock_period;
               break;
             }     /*  End No focus Case  */
            }    /* End switch on pseudo_time_unit */
            return(DATA_OK);
           }    /* End if check on time field type bounds  */
       }        /* End if check on detectorID and hist_num bounds  */
     }
     *min_TOF = 0;
     *max_TOF = 0;
     *step_TOF = 0;
     return(status);
}
/*++----------------------- GetMinAndMaxTOF ------------------------------
 
Effect:
        This function returns the min and max time of flight for the specified
 runfile and the particular detector(det_num) and histogram(hist_num). The 
 function will return a status of DATA_ERROR when there is a problem retrieving
 the information for the file_handle given and will return a status of DATA_OK
 otherwise.

Input:
       file_handle.
       det_num
       hist_num

Output:
 
      min_TOF,
      max_TOF     The min and max time of flight for the particular detector 
                  and histogram numbers given.

Return Value:
       Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetMinAndMaxTOF(FileHandleType file_handle, 
                                int            det_num,
                                int            hist_num,
                                float         *min_TOF,
                                float         *max_TOF     )
{
 FileIdType     id;
 int            index;
 Int4           tf_type;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
  if ( (file_list[id] -> time_field != NULL) &&
       (file_list[id] -> detector_map != NULL) )
    if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) &&
         (hist_num >= 1) && (hist_num <= file_list[id]->header.
          num_of_histograms) )
     {
      index = ((hist_num -1) * file_list[id]->header.n_det) + det_num;
      tf_type = file_list[id]->detector_map[index].tf_type;
      if ( tf_type != 0)
        { 
          *min_TOF = file_list[id]->time_field[tf_type].t_min;
          *max_TOF = file_list[id]->time_field[tf_type].t_max;
          status = DATA_OK;
          return( status );
        }
     }
 return( status );   
}


/*++----------------------- TimeFocusingUsed ------------------------------

Effect:
       Determine whether or not the data for the specified detector and
    histogram was "time focused".

Input:
       file_handle.
       det_num
       hist_num

Return Value:
       Returns TRUE if the data for the specified detector and histogram was
   time focused and returns false otherwise.
*--
*/

int TimeFocusingUsed( FileHandleType  file_handle,
                      int             det_num,
                      int             hist_num     )
{
 FileIdType     id;
 int            tf_type;

 if ( GetTimeFieldType( file_handle, det_num, hist_num, &tf_type ) != DATA_OK )
   return( FALSE );

 if ( (tf_type <= 0)  ||
      (tf_type > NumberOfTimeFieldTypes(file_handle)) )
   return( FALSE );

 id = GetFileId(file_handle);
 if ( id != 0 )
  if (file_list[id] -> time_field != NULL)
   if ( file_list[id]->header.pseudo_time_unit != 'I' )
     return( FALSE );                              /* no time focusing done */
   else
     {
       if ( file_list[id]->time_field[tf_type].time_focus_bit == 0 )
         return( FALSE );                    /* this time field NOT focused */
       else
         return( TRUE );                     /* this time field WAS FOCUSED */
     }
  return( FALSE );
}




/*++---------------------- GetSubgroupIDList --------------------------------

Effect:

    This function constructs the list of subgroup ID's for the specified file
 and histogram.  The list of subgroup ID's is constructed based on the 
 addresses of the histograms in the run file.  The first group is subgroup 
 number 1.  The list is stored starting at index 1, with subgroup[k] set to
 the subgroup ID for detector #k.  The list position subgroup[0] is not used.

    The function returns DATA_ERROR, if an error occurs and DATA_OK if there 
 is no error.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  The list consists of integers and position 0 is not used, 
       so the calling program must provide storage for 

           1 + NumberOfDetectors( file_handle )

       to hold the list of subgroup ID's. 

Input:
       file_handle and hist_num.

Output:
       subgroup

       The list of subgroup ID's.  subgroup[k] will contain the subgroup ID
   for detector k. subgroup[0] is NOT USED.  

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSubgroupIDList( FileHandleType file_handle,
                                  int            hist_num,
                                  int           *subgroup  )
{
 FileIdType      id;
 int             num_of_detectors,
                 det_id,
                 k;
 int             group_num,
                 index;
 Int4           *address;
 Int4           *tf_type;

 DataResultType  status;

 status = DATA_ERROR;
 num_of_detectors = NumberOfDetectors( file_handle );
 if ( num_of_detectors != 0 )
  {
   id = GetFileId(file_handle);
   if ( id != 0 )
    {
     for ( det_id = 1; det_id <= num_of_detectors; det_id++ )
       subgroup[det_id] = -1;

     address = (Int4 *)malloc( (num_of_detectors + 1) * sizeof(Int4) ); 
     tf_type = (Int4 *)malloc( (num_of_detectors + 1) * sizeof(Int4) ); 
     for ( det_id = 1; det_id <= num_of_detectors; det_id++ )
       {
         index = ((hist_num -1) * (file_list[id]->header.n_det)) + det_id;
         address[det_id] = file_list[id]->detector_map[index ].address;
         tf_type[det_id] = file_list[id]->detector_map[index ].tf_type;
       }

     det_id    = 1;
     group_num = 0;
     while( det_id <= num_of_detectors )
       {
         if ( (subgroup[det_id] == -1) &&
                    (file_list[id]->time_field[tf_type[det_id]].t_step > 0) &&
                    (tf_type[det_id] !=0) )
           {
             group_num++;
             subgroup[det_id] = group_num;
             for ( k = det_id+1; k <= num_of_detectors; k++ )
               if ( (address[k] == address[det_id]) && 
                    (file_list[id]->time_field[tf_type[k]].t_step > 0) &&
                    (tf_type[k] !=0) )
                 subgroup[k] = group_num;
           }
         det_id++;  
       }
     free( address );
     free( tf_type );
     status = DATA_OK;
    }
  }
 return(status);
}


/*++---------------------- GetSpectrum16 ----------------------------------

Effect:
         This function gives the spectrum for the runfile associated
 with the file_handle and the particular histogram number(hist_num)
 and the particular detector number(det_num). The function returns
 DATA_ERROR, if an error occurs and DATA_OK if there is no error. If the
 counts exceed 65535, the values returned will be incorrect.  

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the spectrum to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       16-bit integers, so the calling program must provide

                2*NumberOfChannels( file_handle, det_num, hist_num )

       bytes of storage to hold the spectrum.
 
Input:
       file_handle, det_num, and hist_num.

Output:
        The spectrum.

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.
*--
*/

DataResultType GetSpectrum16(FileHandleType file_handle,
                             int            det_num,
                             int            hist_num,
                             void          *data_ptr)
{
 FileIdType     id;
 int            num_of_time_channels,
                index;
 int            i, ch;
 DataResultType status;

 status = DATA_ERROR;
 num_of_time_channels = NumberOfTimeChannels(file_handle,det_num,hist_num);
 if ( num_of_time_channels != 0 )
  {
   id = GetFileId(file_handle);
   if ( id != 0 )
    {
     index = ((hist_num -1) * (file_list[id]->header.n_det)) + det_num;
     fseek( file_list[id]->f, file_list[id]->detector_map[index].address +
            file_list[id]->header.hist_start_address + 4, SEEK_SET );
     fread(data_ptr,2,(size_t)num_of_time_channels,file_list[id] -> f);

#ifdef IPNS_BIG_ENDIAN
     for ( ch = 0; ch < num_of_time_channels; ch++ )
       ConvertShorttoBigEndian( & (((unsigned short*)data_ptr)[ch]) );
#endif
     
     /* KLUGE .... zero out monitor channels ... */
     if ( IsMonitorChannel( file_handle, det_num ) ) 
       for ( i = 0; i < num_of_time_channels; i++ )
         ((Int2 *)data_ptr)[i] = 0;

     status = DATA_OK;
    }
  }
 return(status);
}



/*++---------------------- GetSpectrum32 ----------------------------------

Effect:
         This function gives the spectrum for the runfile associated
 with the file_handle and the particular histogram number(hist_num)
 and the particular detector number(det_num). The function returns
 DATA_ERROR, if an error occurs and DATA_OK if there is no error.  This
 routine will return counts in excess of 65535.  Specifically, the overflow
 information in the run file is used to correct the overflows.   

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the spectrum to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       32-bit integers, so the calling program must provide

                4*NumberOfChannels( file_handle, det_num, hist_num )

       bytes of storage to hold the spectrum.
  
Input:
       file_handle, det_num, and hist_num.

Output:
        The spectrum.

Return Value:
        Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSpectrum32(FileHandleType file_handle,
                             int            det_num,
                             int            hist_num, 
                             void          *data_ptr)
{
 FileIdType     id;
 int            num_of_time_channels,
		data_size,
                index,
                overflow_index,
                num_overflows,
		version_number;
 DataResultType status;
 int            ch, 
                i;
 Int4           *data_long, 
                *overf_ptr;

 unsigned short *temp_ptr;
 long 		*templ_ptr;

 status = DATA_ERROR;
 num_of_time_channels = NumberOfTimeChannels(file_handle,det_num,hist_num);
 if ( num_of_time_channels != 0 )
  {
   id = GetFileId(file_handle);
   if ( id != 0 )
    {
     version_number = file_list[id]->header.version_number;
     if ( version_number < 4 )
        {
        data_size = 2;
        temp_ptr = (unsigned short * )malloc( data_size* num_of_time_channels );
        index = ((hist_num -1) * (file_list[id]->header.n_det)) + det_num;
        fseek( file_list[id]->f, file_list[id]->detector_map[index].address +
            file_list[id]->header.hist_start_address + 4, SEEK_SET );
        fread(temp_ptr, data_size ,(size_t)num_of_time_channels, 
			file_list[id] -> f);
         }
     else
	{
	data_size = 4;
        templ_ptr = (long *)malloc( data_size* num_of_time_channels );
        index = ((hist_num -1) * (file_list[id]->header.n_det)) + det_num;
        fseek( file_list[id]->f, file_list[id]->detector_map[index].address +
            file_list[id]->header.hist_start_address + 4, SEEK_SET );
        fread(templ_ptr, data_size ,(size_t)num_of_time_channels, 
			file_list[id] -> f);
        } 

#ifdef IPNS_BIG_ENDIAN
     for ( ch = 0; ch < num_of_time_channels; ch++ )
       if (version_number < 4 )
	  {
          ConvertShorttoBigEndian( & ((( short *)temp_ptr)[ch]) );
	  }
       else
	  {
	  ConvertInt4toBigEndian ( & ((( long *)templ_ptr)[ch]) );
	  }
#endif
      if ( version_number < 4)
	{
         data_long = (Int4 *)data_ptr;
         for ( ch = 0; ch < num_of_time_channels; ch++ )
         {
           data_long[ch] = ( Int4 )(temp_ptr[ch]);
         }

          overf_ptr =
            (Int4 *)malloc( 4*file_list[id]->header.num_of_overflows  + 4);
          num_overflows = GetOverflows( file_handle,
                        file_list[id]->detector_map[index].address,
                        file_list[id]->detector_map[index].address +
                                        2 * num_of_time_channels,
                        overf_ptr );
          for( i = 0; i < num_overflows; i ++ )
          {
            overflow_index
             = (overf_ptr[i] - file_list[id]->detector_map[index].address)/2;

/* Why do we need the - 2, I would guess this might be two - 1 errors
   made in the conversion from FORTRAN to C or from 2 places where the
   address should start at x + 4 instead of x -- RWT 17/11/95 */

            data_long[ overflow_index - 2 ] =
                        data_long[ overflow_index - 2] + 65536;
          }

          free( overf_ptr );
          free( temp_ptr );
	}
     else
	{
	 data_long = (Int4 *)data_ptr;
	 for ( ch = 0; ch < num_of_time_channels; ch++ )
           {
           data_long[ch] = ( Int4 )(templ_ptr[ch]);
           }
         free(templ_ptr);
	}
/*  This KLUGE removed by J. Hammonds since monitor data is needed elsewhere
    this check should be done at application level
      KLUGE .... zero out monitor channels ... 
     if ( IsMonitorChannel( file_handle, det_num ) ) 
       for ( i = 0; i < num_of_time_channels; i++ )
         data_long[i] = 0;
*/
     status = DATA_OK;
    }
  }
 return(status);

}



/*++---------------------- GetDetAngle ----------------------------------------

Effect:
         This function will give the detector angle for detector(det_num)
 in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
      the angle of the detector in *data_ptr

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetAngle( FileHandleType file_handle,
                            int            det_num,
                            float         *data_ptr  )
{
 FileIdType     id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->det_angle_table != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        *data_ptr = (float)(file_list[id]->det_angle_table[det_num]);
        status = DATA_OK;
       }
 return(status);
}


 
/*++---------------------- GetTotalFlightPath ------------------------------

Effect:
         This function will give the flight path for detector(det_num)
 in the runfile associated with file_handle.  The flight path value returned
 is the total path length from the source to the detector.

Input:
      file_handle and detector_number.

Output:
      the total flight path for the detector in *data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetTotalFlightPath( FileHandleType file_handle,
                                   int            det_num,
                                   float         *data_ptr)
{
 FileIdType     id;
 DataResultType status;
 float          li;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->flight_path_length_table != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        li = file_list[id]->header.source_to_sample;
        *data_ptr = li + 
                   (float)(file_list[id]->flight_path_length_table[det_num]);
        status = DATA_OK;
       }
 return(status);
}



/*++---------------------- GetFinalFlightPath ------------------------------

Effect:
         This function will give the flight path for detector(det_num)
 in the runfile associated with file_handle.  The flight path value returned
 is the final path length from the sample to the detector.

Input:
      file_handle and detector_number.

Output:
      the final flight path is placed in *data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetFinalFlightPath( FileHandleType file_handle,
                                   int            det_num,
                                   float         *data_ptr)
{
 FileIdType     id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->flight_path_length_table != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        *data_ptr = (float)(file_list[id]->flight_path_length_table[det_num]);
        status = DATA_OK;
       }
 return(status);
}



/*++------------------- GetSpectrometerFinalFlightPath -----------------------

Effect:
      This function will give the "effective" flight path for detector(det_num)
 in the runfile associated with file_handle.  The "effective" flight path 
 value returned is the final path length from the sample to the detector 
 ADJUSTED FOR TIME FOCUSING.  The adjustment is only valid for the HRMECS and 
 LRMECS machines at IPNS.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the "effective" flight path of the 
  detector det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetSpectrometerFinalFlightPath( FileHandleType file_handle,
                                               int            det_num,
                                               int            hist_num,
                                               float         *data_ptr   )
{
 FileIdType     id;
 DataResultType status;
 float          length;
 
 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->flight_path_length_table != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        length = (float)(file_list[id]->flight_path_length_table[det_num]);
        if ( TimeFocusingUsed( file_handle, det_num, hist_num ) )
          {
            if ( IPNS_Instrument( file_handle ) == IPNS_HRMECS ) 
              length = HRMECS_FOCUSED_PATH_LENGTH;
            else if ( IPNS_Instrument( file_handle ) == IPNS_LRMECS )
              length = LRMECS_FOCUSED_PATH_LENGTH;
          }
        *data_ptr = length;
         return( DATA_OK );
       }
 return(status);
}


/*++---------------------- GetDetHeight ----------------------------------------

Effect:
         This function will give the detector height for detector(det_num)
 in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the height of the detector
  det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetHeight( FileHandleType file_handle,
                             int            det_num,
                             float         *data_ptr )
{
 FileIdType     id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->det_height_table != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        *data_ptr = (float)(file_list[id]->det_height_table[det_num]);
        status = DATA_OK;
       }
 return(status);
}



/*++---------------------- GetDetDiscLevels   --------------------------------
Effect:

Input:
	file_handle and detectorNumber
Output:
	lower - lower level discriminator
	upper - upper level discriminator

Return Values:
	Flag indicating DATA_OK or DATA_ERROR
*_
*/
DataResultType GetDetDiscLevels( FileHandleType file_handle,
				int det_num,
				short *lower,
				short *upper)
{
 FileIdType id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId( file_handle );
 if (id != 0 )
    if ( (det_num >= 0) && (det_num <= file_list[id]->header.n_det) )
	{
	 *lower = (short)(file_list[id]->det_disc_levels[det_num].lower);
	 *upper = (short)(file_list[id]->det_disc_levels[det_num].upper);
	/*printf("GetDetDiscLevels: lower, upper: %d, %d\n", *lower, *upper);*/
	}
 return(status);
}



/*++---------------------- GetDetSubgroupSize --------------------------------

Effect:
         This function will give the detector group size for detector(det_num)
 for the FIRST HISTOGRAM in the runfile associated with file_handle.

Input:
      file_handle and detector_number.

Output:
        If the DataResult is FILE_OK then the subgroup size of the detector
  det_num is placed in the space provided by data_ptr.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType GetDetSubgroupSize( FileHandleType file_handle,
                                   int            det_num,
                                   int           *data_ptr )
{
 FileIdType     id;
 DataResultType status;

 status = DATA_ERROR;
 id = GetFileId(file_handle);
 if ( id != 0 )
   if ( file_list[id]->det_sub_group_size != NULL )
     if ( (det_num >= 1) && (det_num <= file_list[id]->header.n_det) )
       {
        *data_ptr = (int)(file_list[id]->det_sub_group_size[det_num]);
        status = DATA_OK;
       }

 return(status);
}



/*++----------------------- GetSpectrometerInEnergy --------------------------

Effect:
     
      This function will return the incident energy level for a
      spectrometer as recorded in the specified input file.

Input:
      file_handle

Output:
     
      energy_in 

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  GetSpectrometerInEnergy ( FileHandleType  file_handle,
                                          float          *energy_in    )
{
 FileIdType id;

 if ( GetFileDataType( file_handle ) != ONE_DIMENSIONAL_DATA )
   return( DATA_ERROR );

 id = GetFileId(file_handle);
 *energy_in  = file_list[id]->header.energy_in;

 return( DATA_OK );
}




/*++------------------------ GetFileDataType --------------------------------

Effect:
     
      This function will indicate whether the file contains 1-Dimensional
 data corresponding to individual detector IDs, or 2-Dimensional data
 corresponding to an area detector, organized by time channel.

Input:
      file_handle

Output:

      Returns one of the following values:

                NO_DATA_FILE
                ONE_DIMENSIONAL_DATA
                TWO_DIMENSIONAL_DATA
*--
*/

FileContentsType GetFileDataType( FileHandleType file_handle )

{

  FileIdType id;

  id = GetFileId(file_handle);
  if ( id != 0 )
    {
      if ( file_list[id]->header.num_wavelengths != 0 )
        return(TWO_DIMENSIONAL_DATA);
      else
        return(ONE_DIMENSIONAL_DATA);
    }
  else
    return( NO_DATA_FILE );

}
 

/*++-------------------------- Get2DDataSizes --------------------------------

Effect:
     
      This function will return the dimensions of the data from an area 
 detector. 

Input:
      file_handle

Output:
     
      nx, ny, n_times

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType  Get2DDataSizes ( FileHandleType  file_handle,
                                 int            *nx,
                                 int            *ny,
                                 int            *n_wavelengths )
{
 FileIdType id;

 if ( GetFileDataType( file_handle ) != TWO_DIMENSIONAL_DATA )
   return( DATA_ERROR );

 id = GetFileId(file_handle);
 *nx            = file_list[id]->header.num_of_x;
 *ny            = file_list[id]->header.num_of_y;
 *n_wavelengths = file_list[id]->header.num_wavelengths;

 return( DATA_OK );
}



/*++------------------------- Get2DDataSlice -------------------------------

Effect:
         This function gets one 2-D slice of data from the runfile associated
 with the file_handle and the particular histogram number(hist_num).  The 
 function returns DATA_ERROR, if an error occurs and DATA_OK if there is no 
 error.

 NOTE:
       The calling program is responsible for providing the storage space for
       the data.  This routine only copies the data to the address passed
       in in the parameter data_ptr.  Also, the spectrum consists of unsigned
       16-bit integers, so the calling program must provide

                           2 * nx * ny

       bytes of storage to hold the slice of data.  The dimensions nx and ny 
       can be found by calling Get2DDataSizes().
  

Input:
       file_handle, det_num, and hist_num.

Output:

       *data_ptr  The block of memory pointed to by data_ptr is filled with 
                  one time slice from the area detector.  The slice is arranged
                  in row-major order with the first row representing the first
                  row across the bottom of the detector.

Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.

*--
*/

DataResultType Get2DDataSlice(FileHandleType file_handle,
                              int            slice_num,
                              int            hist_num, 
                              void          *data_ptr)
{
 FileIdType     id;
 DataResultType status;
 int            start_addr,
                slice_size,
                offset;
 int            i, nx, ny, n_wavelengths;

 status = Get2DDataSizes ( file_handle, &nx, &ny, &n_wavelengths );
 if ( status != DATA_OK )
   return( status );

 if ( (slice_num < 1) || (slice_num > n_wavelengths) )
   return( DATA_ERROR );

 id = GetFileId(file_handle);
 start_addr = file_list[id]->header.hist_start_address + 4;

                             /* NOTE: the time slices are stored starting on */
                             /*       256 byte boundaries.  Apparently the   */
                             /*       size was "rounded up" to the next      */
                             /*       larger multiple of 256, even if it was */
                             /*       originally a multiple of 256!! (DJM)   */

 slice_size = nx * ny * 2; 
 slice_size = ( slice_size / 256 + 1 ) * 256;

 offset     = ( n_wavelengths * (hist_num - 1) + (slice_num - 1)) * slice_size;

 fseek( file_list[id]->f, offset + start_addr, SEEK_SET);
 fread( data_ptr, 2, (size_t)(nx * ny), file_list[id]->f );

#ifdef IPNS_BIG_ENDIAN
    for ( i = 0; i < nx*ny; i++ )
    ConvertShorttoBigEndian( & (((unsigned short*)data_ptr)[i]) );
#endif

 return(DATA_OK);

}
/*++--------------------------- GetDetStartAddress --------------------------
Effect:
        This function gets the memory start address for a detector.  The
        primary use for this function is to set the pointer used in hardware
        for the start of histogram memory for this detector.
 
Input:
       File_handle, det_num, and hist_num.
 
Output:
       *start_addr The memory start address for this detector.
 
Return Value:
      Flag indicating DATA_OK, or DATA_ERROR.
 
*--
*/
 
DataResultType GetDetStartAddress ( FileHandleType file_handle,
                                    int  det_num,
                                    int hist_num,
                                    long *start_addr )

{
 FileIdType id;
 DataResultType status;
 int index;

 status = DATA_ERROR;
 id = GetFileId( file_handle );
 if (id != 0 )
    if ( (det_num >= 0) && (det_num <= file_list[id]->header.n_det) )
	{
	index = ((hist_num - 1) * (file_list[id]->header.n_det)) + det_num;
	*start_addr = file_list[id]->detector_map[index].address;
	}
 return(status);
}

#include <math.h>
/*
Filename    : IPNS_VaxFloat_BigEndian_Conversions.c
Purpose     : Ipmplementation Code for routines that Convert between integers 
              between Little Endian and Big Endian and Convert Vax Floats to 
	      the IEEE format. 
*/
/*---------------------------------------------------------
 * $Log: ipns.h,v $
 * Revision 1.1  2006/03/12 09:59:37  tproffen
 * Initial revision
 *
 * Revision 1.3  1997/08/08 22:03:09  hammonds
 * add RCS messaging
 *
-----------------------------------------------------------*/
/*++ ---------------------- ConvertShortToBigEndian ---------------------------

  Effect: This function will convert a 2 BYTE "little endian" quantity to 
          a 2 BYTE "big endian" quantity.  This is needed when moving a
          binary file from a little endian to a big endian architecture.
          If the input is a sequence of two bytes:

                           AB

           this function changes it to

                           BA
  IN/OUT PARAMTER

      val_p  -- This must be a pointer to the the 2 byte little endian  
                quantity to be converted.
*--
*/
/*
#define IPNS_BIG_ENDIAN
*/
void ConvertShorttoBigEndian(void *val_p)
{
unsigned short  val;
unsigned char   temp;
void           *val_ptr;


val = *(unsigned short *)val_p;

val_ptr = &val;

temp = *(unsigned char *)val_ptr;
*((unsigned char*)val_ptr) = *(((unsigned char*)val_ptr)+1);
*(((unsigned char*)val_ptr)+1) = temp;

*(unsigned short *)val_p = val;
}

/*++ ---------------------- ConvertInt4ToBigEndian ---------------------------

   Effect: This function will convert a 4 BYTE "little endian" quantity to
           a 4 BYTE "big endian" quantity.  This is needed when moving a
           binary file from a little endian to a big endian architecture.
           If the input is a sequence of four bytes:

                           ABCD

           this function changes it to

                           DCBA
  IN/OUT PARAMTER

      val_p  -- This must be a pointer to the the 4 byte little endian 
                quantity to be converted.
*--
*/

void ConvertInt4toBigEndian(void *val_p)
{
unsigned int    val;
unsigned short  temp;
void              *val_ptr;


val = *(unsigned int *)val_p;

val_ptr = &val;

temp = *(unsigned short *)val_ptr;
*((unsigned short*)val_ptr) = *(((unsigned short*)val_ptr)+1);
*(((unsigned short*)val_ptr)+1) = temp;

ConvertShorttoBigEndian( (unsigned short *)val_ptr );
ConvertShorttoBigEndian( (unsigned short *)val_ptr + 1);
*(unsigned int *)val_p = val;
}

/*++ -------------------------- ConvertVaxFloat ------------------------------

   Effect: This function will convert a 4 BYTE VAX floating point value
           ( F_Floating format ) to the native floating point format of the
           machine on which it was compiled.

           This has only been tested on the following platforms:
                   DEC MIPS,    running ULTRIX
                   DEC ALPHA,   running OSF/1 
                   HP 9000/735, running HP-UX
                   Sun Sparc,   Running Solaris
                   Motorola 680x0,  Running VxWorks (compiled using gnu 
                                      cross compiler on the sun)
                   Intel X86,   running Linux and Windows NT

           In this initial version, any value with a zero as the exponent
           is returned as zero.  That means that reserved operands such as
           INFINITIY and NAN if present in the F_Floating format will not
           be recognized, but will return 0.0

  IN/OUT PARAMTER

      val_p  -- This must be a pointer to the VAX format floating point value
                when the function is called.  The value will be altered to
                the native floating point format, assuming the byte ordering
                of the native machine is the same as the DEC MIPS machine.
*--
*/
void ConvertVaxFloat( float *val_p )
{
  unsigned int  val;
  int           hi_mant,
                low_mant,
                sign,
                exp;
  float f_val;

  val = *((unsigned int *)val_p);

#ifdef IPNS_BIG_ENDIAN
  ConvertInt4toBigEndian( &val);
#endif

  hi_mant  = (val & 127) + 128;          /* add 128 to put in the implied 1 */
  val      = val >> 7;

  exp      = ((int)(val & 255)) - 128;   /* exponent is "excess 128"        */
  val      = val >> 8;

  sign     = val & 1;
  low_mant = val >> 1;

  if ( exp == -128 )
    f_val = 0;                          /* This could also be a "reserved"  */
                                        /* operand of some sort?            */
  else
    f_val = (hi_mant / 256.0 + low_mant / 16777216.0) * pow(2.0, (double)exp );

  if ( sign == 1 )
    f_val = -f_val;

  *val_p = f_val;
}
