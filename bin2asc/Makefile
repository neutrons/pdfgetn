#######################################################################
# Simple makefile to compile and install the bin2asc utilities ..
#######################################################################

F77         =  gfortran
FFLAGS      = -O0 -fno-f2c -ffixed-line-length-78 -std=legacy -w
LDFLAGS     =
INSTALL_DIR = ../binary
INSTALL_MOD = 755

#######################################################################

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
	cp pdf2asc  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/pdf2asc
	cp int2asc  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/int2asc
	cp soq2asc  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/soq2asc
	cp soq2gsa  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/soq2gsa

clean:
	rm -f *~ *.o

