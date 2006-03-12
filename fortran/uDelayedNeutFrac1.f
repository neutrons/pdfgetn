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
      subroutine uDelayedNeutFrac1(local_data, local_data_err, 
     1     local_num_of_channels, local_delayed_neutron_flag, 
     1     local_dly_ntrn_corr, local_lambda, local_print_level)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: This routine subtracts the delayed neutron fraction from
C the data. The user may have specified a wavelength (lambda) range,
C where this correction should be performed. The program first determines
C the array position local_xmin and local_xmax corresponding to
C the desired lambda_min(=dly_ntrn_min_wvlngth) and lambda_max
C (=dly_ntrn_max_wvlngth). Then it will subtract the delayed neutron
C fraction from the data.
C
C Created by S. J. L. Billinge (Michigan State University) and 
C M. Gutmann (ETH Zurich & Michigan State University) on 8/23/1999.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C The local_ntrn_corr array holds the following 5 elements:
C
C local_ntrn_corr(1) : Delayed neutron fraction
C local_ntrn_corr(2) : Minimum wavelength for correction to apply
C local_ntrn_corr(3) : Maximum     "       "      "       "   "
C local_ntrn_corr(4) : Minimum cut wavelength
C local_ntrn_corr(5) : Maximum cut wavelength
C
C Note: local_ntrn_corr(4 & 5) are only used for the sawtooth correction.
C The sawtooth correction subtracts a constant delayed neutron fraction
C between the minimum and maximum cut wavelentgh. The correction decays
C linearly to zero between minimum and minimum cut wavelength and maximum
C cut and maximum wavelength, respectively.

      include 'parameters.inc'


      integer   local_num_of_channels,
     1          local_delayed_neutron_flag,
     1          local_xmin,
     1          local_xmax,
     1          local_print_level
     
      real   local_data(n_chn),
     1       local_data_err(n_chn),
     1       local_lambda(n_chn),
     1       local_sum,
     1       local_sum_err,
     1       local_dly_ntrn_corr(5)


      write(6,*) '%%info: subtracting delayed neutron fraction '

C-Initialyze

      local_sum=0
      local_sum_err=0
      local_xmin=1
      local_xmax=local_num_of_channels

C-Determine range of integration

      if ((local_delayed_neutron_flag .eq. 1) .or.
     1     (local_delayed_neutron_flag .eq. 2)) then
         do i=1, local_num_of_channels
            if (local_lambda(i) .le. local_dly_ntrn_corr(2)) then
               local_xmin=i
            endif
            if (local_lambda(i) .le. local_dly_ntrn_corr(3)) then
               local_xmax=i
            endif
         enddo
      endif

C-Integrate now the data over the data range from local_xmin 
C to local_xmax

      do i=local_xmin, local_xmax
         local_sum=local_sum+local_data(i)
         local_sum_err=sqrt(local_sum_err**2+local_data_err(i)**2)
      enddo

      if (local_print_level .eq. 2) then
         write(6, *) '      Total intensity before delayed neutron', 
     1        ' correction:' 
         write(6, *) '      ',local_sum, '(', local_sum_err,')'
      endif

C-If the user wants delayed neutron fraction correction then we do it
C First here is the code for the constant delayed neutron fraction

      if (local_delayed_neutron_flag .eq. 1) then
         do i=1, local_num_of_channels

            local_data_err(i)=sqrt(local_data_err(i)**2+
     1           (local_dly_ntrn_corr(1)*local_sum_err/(local_xmax-
     1           local_xmin+1.))**2)

            local_data(i)=local_data(i)-local_dly_ntrn_corr(1)*
     1           local_sum/(local_xmax-local_xmin+1.)
            local_data(i)=max(local_data(i), 0.0)
         enddo

C-And here is the code for the sawtooth correction

      else
         if (local_delayed_neutron_flag .eq. 2) then
            do i=1, local_num_of_channels

C-Big formulas!
               if (local_lambda(i) .le. 
     1              local_dly_ntrn_corr(4)) then

                  local_data_err(i)=sqrt(local_data_err(i)**2+
     1                 (local_dly_ntrn_corr(1)*local_sum_err/
     1                 (local_xmax-local_xmin+1.))**2)

                  local_data(i)=local_data(i)-local_dly_ntrn_corr(1)*
     1                 local_sum/(local_xmax-local_xmin+1.)
                  local_data(i)=max(local_data(i), 0.0)

               else

C-Another set of big formulas!

                  if (local_lambda(i) .lt. 
     1                 local_dly_ntrn_corr(5)) then

                     local_data_err(i)=sqrt(local_data_err(i)**2+
     1                    (local_dly_ntrn_corr(1)*local_sum_err/
     1                    (local_xmax-local_xmin+1.)*
     1                    (local_dly_ntrn_corr(5)-local_lambda(i))/
     1                    (local_dly_ntrn_corr(5)-
     1                    local_dly_ntrn_corr(4)))**2)

                     local_data(i)=local_data(i)-local_dly_ntrn_corr(1)*
     1                    local_sum/(local_xmax-local_xmin+1.)*
     1                    (local_dly_ntrn_corr(5)-
     1                    local_lambda(i))/
     1                    (local_dly_ntrn_corr(5)-
     1                    local_dly_ntrn_corr(4))
                     local_data(i)=max(local_data(i), 0.0)
                  endif
               endif
C-End of another big formula
C-enddo *if (local_delayed_neutron_flag .eq. 2)*

            enddo   

C-Maybe there is no delayed neutron fraction correction at all.

         else
            write(6, *) '%%info: no delayed neutron fraction',
     1           ' subtracted'
         endif
      endif

      local_sum=0
      do i=local_xmin, local_xmax
         local_sum=local_sum+local_data(i)
      enddo

      if (local_print_level .eq. 2) then
         write(6, *) '      After delayed neutron correction:',
     1        '      ',local_sum
      endif

      return

      end
