      subroutine uSepdLambdatoQ1(local_Q, local_lambda, 
     1     local_twotht, local_num_of_channels)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C Purpose: converts Q to lambda. This step is necessary since the
C other programs, like SOQD etc., require the data versus lambda.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      include 'parameters.inc'

      parameter  (Pi=3.14159265359)

      real       local_Q(n_chn),
     1           local_lambda(n_chn),
     1           local_twotht

      integer    local_num_of_channels

      write(6, *) '%%info: converting to Q'

      do i=1, local_num_of_channels
         local_Q(i) = 4.0*Pi*Sin(local_twotht/360.*Pi)/local_lambda(i)
C         write(6, *) 'I am here and Q = ', local_Q(i)
      enddo

      return
      end

