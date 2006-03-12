c---------------------------------------------------------------------------
c This is part of the PDFgetN distribution written by Peter Peterson,
c Matthias Gutmann, Thomas Proffen, and Simon Billinge.
c 
c Copyright 2000 Michigan State University Board of Trustees
c 
c Use and distribution of this program is subject to the terms laid out
c in the license in LICENSE.txt included with this distribution.  A copy
c of the license  can be obtained from Michigan  State University office
c of Libraries, Computing and Technology (517-353-0722).  
c---------------------------------------------------------------------------

      subroutine kuplotOpenCorrFiles (novan,lcan)
c---------------------------------------------------------------------------
c Purpose: Opening files to write correction data from SOQD
c---------------------------------------------------------------------------
      logical novan,lcan
      common /unit/ isabs,icabs,ivabs,ivplc,isplc,ivmsc,ismsc
c
      isabs=31
      icabs=32
      ivabs=33
      isplc=34
      ivplc=35
      ismsc=36
      ivmsc=37
c
      open(unit=isabs,file="soqd_sabs.dat",status="unknown",err=999)
      open(unit=isplc,file="soqd_splc.dat",status="unknown",err=999)
      open(unit=ismsc,file="soqd_smsc.dat",status="unknown",err=999)
c
      if (.not.novan) then
        open(unit=ivabs,file="soqd_vabs.dat",status="unknown",err=999)
        open(unit=ivplc,file="soqd_vplc.dat",status="unknown",err=999)
        open(unit=ivmsc,file="soqd_vmsc.dat",status="unknown",err=999)
      endif
c
      if (lcan) then
        open(unit=icabs,file="soqd_cabs.dat",status="unknown",err=999)
      endif
      return
c
999   stop "%%%severe: Error opening soqd correction log files"
c
      end

c---------------------------------------------------------------------------

      subroutine kuplotCloseCorrFiles (novan,lcan)
c---------------------------------------------------------------------------
c Purpose: Closing files to write correction data from SOQD
c---------------------------------------------------------------------------
      logical novan,lcan
      common /unit/ isabs,icabs,ivabs,ivplc,isplc,ivmsc,ismsc
c
      close(unit=isabs)
      close(unit=isplc)
      close(unit=ismsc)
c
      if (.not.novan) then
        close(unit=ivabs)
        close(unit=ivplc)
        close(unit=ivmsc)
      endif
c
      if (lcan) then
        close(unit=icabs)
      endif
c
      return
      end
c---------------------------------------------------------------------------

      subroutine kuplotCorrWrite (iunit,title,label,ibank,nchan,
     1                            qlam,xlam,dat)
c---------------------------------------------------------------------------
c Purpose: Writing correction data from SOQD
c---------------------------------------------------------------------------
      include 'parameters.inc'

      character*(*) title,label
      real          xlam(n_chn),dat(n_chn)
c
      write (iunit,1000) ibank,title
      write (iunit,1100) label
c
      do j=1,nchan,4
        write (iunit,1200) qlam/xlam(j),dat(j)
      enddo
c
1000  format ('#S ',i2,' - ',a)
1100  format ('#L Q ',a)
1200  format (e11.4,1x,e11.4)
c
      return
      end
