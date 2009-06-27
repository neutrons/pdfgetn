#######################################################################
# Simple makefile to compile and install the bin2asc utilities ..
#######################################################################

F77  =  gfortran
FFLAGS  = -O0 -fno-f2c -ffixed-line-length-78 -std=legacy -w
LDFLAGS =

INSTALL_DIR  = /usr/local/lib/PDFgetN-1.6.4/binary
INSTALL_MOD = 755

all: pdf2asc int2asc soq2asc soq2gsa

pdf2asc: pdf2asc.o
	$(F77) -o pdf2asc pdf2asc.o $(LDFLAGS)
int2asc: int2asc.o
	$(F77) -o int2asc int2asc.o $(LDFLAGS)
soq2asc: soq2asc.o
	$(F77) -o soq2asc soq2asc.o  $(LDFLAGS)
soq2gsa: soq2gsa.o lenstr.o soqread.o invnorm.o maketmap.o readiparm.o
	$(F77) -o soq2gsa soq2gsa.o lenstr.o soqread.o invnorm.o maketmap.o readiparm.o $(LDFLAGS)

.f.o:
	$(F77) -c $(FFLAGS) $<

install: pdf2asc int2asc soq2asc soq2gsa
	cp * $(INSTALL_DIR)

clean:
	rm -f *~ *.o

