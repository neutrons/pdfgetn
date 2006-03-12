c****************************************************************************
c	PDFCONV - PDF conversion program
c
c	This program converts binary PDFs (Simon's format) to ASCII
c	files that can be used by PDFFIT.
c
c	Version: 1.3
c	Author : Thomas Proffen
c
c	Changes: Added processing of commandline arguments (11/99)
c	         Added optional output rho(r) and N(r)     (01/00)
c	         Added integral rG(r)                      (11/00)
c
c****************************************************************************
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
c****************************************************************************
	program pdfconv
c
	parameter	(pi     = 3.141592654)
	parameter	(maxdat = 15000)
c
	character*80    fname,aname,title,hist,cdummy
	real		xa(maxdat),pdf(maxdat),dpdf(maxdat),y2(maxdat)
	real*4          rfit
c
	narg = iargc()
	if (narg.ge.2) then
	  call getarg (1,fname)
	  call getarg (2,aname)
	else
	  write (*,1000)
	  read(*,5000) fname
	  write (*,1100)
	  read(*,5000) aname
	endif
c
	if (narg.eq.3) then
	  call getarg(3,cdummy)
	  read (cdummy,*) rmax
	else
	  rmax = 1.5
	endif
c
	call pdfread(fname,xa,pdf,dpdf,y2,dr,rho0,j,n,title,hist)
	call pdfint(pdf,dpdf,n,j,maxdat,dr,rgint,drgint,fint,dfint,rmax,rfit)
c
	open (1,file=aname,status='unknown')
	if (rho0.gt.0.0) then
	  write(1,1500)
	  write(1,2010) rgint,drgint,fint,dfint,rmax,rfit,rho0
	  write(1,2020)
	else
	  write(1,1510)
	  write(1,2015) rgint,drgint,fint,dfint,rmax,rfit
	  write(1,2030)
	endif
c
	rn = 0.0
	do i=j,n
	  r    = float(i)*dr
	  sr   = 0.0
	  g    = 4.0*pi*r* pdf(i)
	  sg   = 4.0*pi*r*dpdf(i)
	  rho  = pdf(i)+rho0
	  drho = dpdf(i)
	  rn   = rn + dr*(r*g + 4.0*pi*r*r*rho0)
c
	  if (rho0.gt.0.0) then
	    write(1,3000) r,g,sr,sg,rho,drho,rn
	  else
	    write(1,3010) r,g,sr,sg
	  endif
	enddo
	close(1)
c
1000	format ('Give name of input binary PDF file  : ',$)
1100	format ('Give name of output ascii PDF file  : ',$)
1500	format ('#O0 rg_int sig_rg_int low_int sig_low_int rmax rhofit',
     &          ' rho0',/,'#S 1 - PDF from PDFgetN')
1510	format ('#O0 rg_int sig_rg_int low_int sig_low_int rmax rhofit',
     &	        /,'#S 1 - PDF from PDFgetN')
2010	format ('#P0 ',4(f10.5,1x),2x,f5.2,2x,f6.4,2x,f12.8)
2015	format ('#P0 ',4(f10.5,1x),2x,f5.2,2x,f6.4)
2020	format ('#L r G(r) dr dG(r) rho(r) drho(r) N(r)')
2030	format ('#L r G(r) dr dG(r)')
3000	format (f7.3,1x,f10.3,1x,f5.1,1x,f10.4,f10.3,1x,f10.4,1x,f15.4)
3010	format (f7.3,1x,f10.3,1x,f5.1,1x,f10.4)
5000	format (a)
	end
C****************************************************************************
	subroutine pdfint(pdf,dpdf,n,j,maxdat,dr,
     &	                  rgint,drgint,fint,dfint,rmax,rfit)
c+
c	Calculate integral rG(r) ..
c-
	parameter	(pi = 3.141592654)
c
	character*40	cdummy
	real*4		pdf(maxdat),dpdf(maxdat)
c
	real*4 r0,r
	real*4 rg0,rg
	real*4 drg0,drg
	real*4 difG,difG0
	real*4 Gtot,Rtot,lint
c
	fint   = 0.0
	lint   = 0.0
	dfint  = 0.0
	rgint  = 0.0
	drgint = 0.0
c
c       Calculate rhofit (rfit)
	Gtot=0.0
	Rtot=0.0
	do i=j,n-1
	   r    = float(i)*dr
	   rg   = 4.0*pi*r*r*pdf(i)
	   
	   if(r.le.rmax)then
c	      Gtot = Gtot + (r**2)*rg
c	      Rtot = Rtot + r**3
	      Gtot = Gtot + ((float(i)*dr)**3)*4.0*pi*pdf(i)
	      Rtot = Rtot + (float(i)*dr)**3
	   endif
	enddo
	rfit=-Gtot/(4*pi*Rtot)

c       Calculate the quality factors
	do i=j+1,n
	  r0    = float(i-1)*dr
	  r     = float(i)*dr
	  rg0   = 4.0*pi*r0*r0*pdf(i-1)
	  rg    = 4.0*pi*r*r*pdf(i)
	  drg0  = 4.0*pi*r0*r0*dpdf(i-1)
	  drg   = 4.0*pi*r*r*dpdf(i)
	  g     = 4.0*pi*r*pdf(i)
	  g0    = 4.0*pi*r0*pdf(i-1)
c
	  rgint  = rgint + abs(r-r0)*(rg+rg0)/2.0
	  drgint = drgint + abs(r-r0)**2*(drg**2+drg0**2)/4.0
c
	  if (r.le.rmax+dr) then
c	     diffG   = abs(r*g  + 4.0*pi*(r**2)*rfit)
c	     diffG0  = abs(r*g0 + 4.0*pi*(r0**2)*rfit)
	     diffG   = abs(r*g  + 4.0*pi*(r**2)*rfit)**2
	     diffG0  = abs(r*g0 + 4.0*pi*(r0**2)*rfit)**2
c	     diffG   = (r**2)*diffG
c	     diffG0  = (r0**2)*diffG0

	     glnorm  = (4.0*pi*(r**2)*rfit)**2
	     glnorm0 = (4.0*pi*(r0**2)*rfit)**2

	     fint  = fint + 0.5*dr*(diffG+diffG0)
	     lint  = lint + 0.5*dr*(glnorm+glnorm0)
	     dfint = dfint + dr**2*(r**4*drg**2+r0**4*drg0**2)/4.0
	  endif
	enddo
c
	fint = abs(fint/lint)
	drgint = sqrt(drgint)
	dfint  = sqrt(dfint)
c
	end
C****************************************************************************
	SUBROUTINE PDFREAD(FILROOT,XA,PDF,DPDF,Y2,DELTAR,RHOINP,J
     1         ,NRDF,TITLE,HISTRY)
C****************************************************************************
	CHARACTER*(80) 	FILROOT	!(i)name of file to be read (no extension)
	REAL*4          XA(*)   !(o)array for first column of data	
	REAL*4 		PDF(*)  !(o)array for pdf
	REAL*4          DPDF(*) !(o)array for pdf error (DRDF)
	REAL*4          Y2(*)   !(o)array for fourth column of data
	REAL*4		DELTAR	!(o)spacing of r-points 	
	REAL*4		RHOINP	!(o)number density (no longer used)
	INTEGER*4	J 	!(o)number of the first point in the PDF
	INTEGER*4	NRDF	!(o)number of points in PDF.
	CHARACTER*(*)	HISTRY	!(o)header information
	CHARACTER*(*)	TITLE	!(o)header information

        OPEN(UNIT=2,FILE=FILROOT,
     1	STATUS='OLD',FORM='UNFORMATTED',ACCESS='SEQUENTIAL')

	READ(2) TITLE
	READ(2) HISTRY
	READ(2) J,NRDF,DELTAR,RHOINP
        READ(2) (XA(I),PDF(I),DPDF(I),Y2(I),I=J,Nrdf)
	CLOSE(2)

	END 

