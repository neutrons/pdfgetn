#####################################################################
# Makefile for IPNS binary to ascii conversion program
#####################################################################

CC       = gcc
CCFLAGS  = -g

F77          = gfortran
FFLAGS       = -O0 -fno-f2c -ffixed-line-length-78 -std=legacy -w
LDFLAGS      = -lm
INSTALL_DIR  = ../../binary
INSTALL_MOD  = 755

OBJC_BTOA = sepdbtoa.o
OBJC_INFO = sepdinfo.o
OBJF = asc2plot.o 

.f.o:
	$(F77) -c $(FFLAGS) $<

all: sepdbtoa sepdinfo asc2plot

sepdbtoa: $(OBJC_BTOA)
	$(CC) -o sepdbtoa $(OBJC_BTOA) $(LDFLAGS)

sepdinfo: $(OBJC_INFO)
	$(CC) -o sepdinfo $(OBJC_INFO) $(LDFLAGS)

asc2plot: $(OBJF)
	$(F77) -o asc2plot $(OBJF) $(LDFLAGS)

install: sepdbtoa sepdinfo asc2plot
	cp sepdbtoa $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/sepdbtoa
	cp sepdinfo $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/sepdinfo
	cp asc2plot $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/asc2plot

clean: 
	rm -f sepdbtoa sepdinfo asc2plot *.o

