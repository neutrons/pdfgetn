      subroutine uLamCal1(local_difc, local_difa, local_tzero,
     1           local_t_min, local_t_max, local_t_step, 
     1           local_num_of_det_groups, local_num_of_channels,
     1           local_twotht, local_bnk_angle, local_dist,
     1           local_i, local_lambda, local_print_level)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: Calculates the wavelengths for a bank using the diffracto-
C meter constants, if they are given. If the diffractometer constants
C are not given an alternative formula using the flight path is
C applied.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      parameter    (Pi=3.14159265359)

      real         local_difc(n_banks),
     1             local_difa(n_banks),
     1             local_tzero(n_banks),
     1             local_t_min(n_banks),
     1             local_t_max(n_banks),
     1             local_t_step(n_banks),
     1             local_twotht,
     1             local_time,
     1             local_dist,
     1             DIFC,
     1             DIFA,
     1             TZERO,
     1             local_bnk_angle(n_banks),
     1             R1,
     1             local_difference,
     1             F1,
     1             local_lambda(n_chn)


C--R1 is used in the wavelength calculation as an intermediate result.
     
      integer      local_num_of_det_groups,
     1             local_num_of_channels,
     1             local_i,
     1             local_print_level

      write(6, *) '%%info: calculating wavelengths for this', 
     1     ' bank '

C--I assume the first 8 entries in the time field correspond to the
C  time channel info for banks 1 to 8. At least, this is consistent
C  with the number of channels.

C--First, I have to make sure that the diffractometer constants and
C  channel info are assigned to the correct bank. This is done as
C  follows: To assign the diffractometer constants to a bank I compare the
C  angle of the bank with the bank angle given by the user. If the 
C  difference between these two values is less than 5 degrees then the 
C  DIFC, DIFA and TZERO belong to this bank. To assign the channel info 
C  to the bank I compare the group no. of the bank array with the 
C  index of the array that holds the channel info 
C  local_t_min(n_banks), local_t_max(n_banks), 
C  local_tzero(n_banks). The index of the arrays mentioned before 
C  corresponds to the group no.


C--Assign now the diffractometer constants to the correct bank by
C  comparing the bank angle local_twotht with the user supplied angle
C  local_bnk_angle.

      do k=1, local_num_of_det_groups
         local_difference=abs(abs(local_twotht)-
     1                    abs(local_bnk_angle(k)))
         if ((local_difference .le. 5.0) .and. 
     1        (local_twotht .gt. 0.0)) then
            DIFC=local_difc(k)
            DIFA=local_difa(k)
            TZERO=local_tzero(k)
            if (local_print_level .eq. 2 ) then
               write(6, *) '      The following diffractometer', 
     1              ' constants' 
               write(6, *) '      have been assigned to the ',
     1              local_twotht,' degree bank: '
               write(6, *) '      DIFC      : ', DIFC
               write(6, *) '      DIFA      : ', DIFA
               write(6, *) '      TZERO     : ', TZERO
               write(6, *) '      Group no  : ', local_i
            endif
            goto 10
         else
            DIFC=0.0
            DIFA=0.0
            TZERO=0.0
         endif
      enddo
               
 10   continue


C--Compute now the wavelengths for this bank. Remember, the array element
C  local_i holds the number of the bank. Its channel info has the index
C  local_i in the local_t_min, local_t_max, and local_t_step arrays.
                     
      local_num_of_channels=(local_t_max(local_i)-
     1     local_t_min(local_i))/
     1     local_t_step(local_i)

C--Loop that calculates the wavelength array

      if (DIFC .gt. 1.0) then
         do N=1, local_num_of_channels
            local_time=local_t_min(local_i)+(N-0.5)*
     1           local_t_step(local_i)
            R1=4.0*DIFA*(TZERO-local_time)/DIFC/DIFC

C--Big formula now:
            if (R1 .gt. 0.001)
     1           F1=(sqrt(DIFC*DIFC- 4.0*DIFA*(TZERO-
     1           local_time))-DIFC)/DIFA

C--Another big formula:

            if (R1 .le. 0.001)
     1           F1=2.0*(local_time-TZERO)/DIFC-2.0*DIFA*
     1           (TZERO-local_time)**2/DIFC/DIFC/DIFC

C--Finally the wavelength:

            local_lambda(N)=F1*abs(sin(local_twotht*
     1           Pi/360.0))
         enddo

      else

C--If DIFC<1.0 then calculate the wavelength using the flightpath
C  of this bank
         write(6, *) '%%warn: this is an uncalibrated bank'
         write(6, *) '      --> Lambda is computed using the average',
     1        ' path length'
         do N=1, local_num_of_channels
            local_time=local_t_min(local_i)+
     1           (N-0.5)*local_t_step(local_i)
            local_lambda(N)=3956.05*local_time*
     1              1.0E-06/(local_dist)
          enddo
      endif
      if (local_print_level .eq. 2 ) then
         write(6, *) '      Bank ',local_i
         write(6, *) '      Lamba(min) : ', local_lambda(1)
         write(6, *) '      Lambda(max): ', local_lambda
     1        (local_num_of_channels)
      endif
      return
      end



