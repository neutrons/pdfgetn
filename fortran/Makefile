##############################################################################
# Makefile for data processing codes ..
##############################################################################

F77  =  g77
FFLAGS  = -O0 -fno-f2c
LDFLAGS = 

INSTALL_DIR  = /usr/local/lib/PDFgetN-1.6.1/binary
INSTALL_MOD = 755

##############################################################################
# no changes should be needed below this line
##############################################################################

USEPDPREP1_INC = parameters.inc usepdprep1.inc
USEPDPREP1_OBJ = usepdprep1.o how_long1.o uSepdReadHeader1.o uReadThisBank1.o \
		 uSumTheseData1.o uIntegrateMonitor1.o uDelayedNeutFrac1.o \
                 uLamCal1.o uNormalizeThisBank1.o uSubtractData1.o \
                 uKuplotOut1.o uVanProc1.o uSmoothThis1.o uSepdInt_Out1.o \
                 uSavitzkyGolay1.o uDetermineDataRange1.o uKillPeaks1.o \
                 uSepdLambdatoQ1.o

GLADPREP1_INC = parameters.inc gladprep1.inc
GLADPREP1_OBJ = gladprep1.o how_long1.o uGladReadBank1.o uSumTheseData1.o \
                uSubtractData1.o uLambdaQ1.o uKuplotOut1.o uVanProc1.o \
                uSavitzkyGolay1.o uSmoothThis1.o uSepdInt_Out1.o uKillPeaks1.o

ARIELPREP_INC = parameters.inc arielprep.inc
ARIELPREP_OBJ = arielprep.o how_long1.o uArielReadBank1.o uSumTheseData1.o \
                uSubtractData1.o uKuplotOut1.o uVanProc1.o \
                uSavitzkyGolay1.o uSmoothThis1.o uSepdInt_Out1.o \
                uLambdaQ1.o uKillPeaks1.o

GSASPREP_INC  = parameters.inc gsasprep.inc
GSASPREP_OBJ  = gsasprep.o how_long1.o uGsasRead.o uSumTheseData1.o \
                uSubtractData1.o uKuplotOut1.o uVanProc1.o \
                uSavitzkyGolay1.o uSmoothThis1.o uSepdInt_Out1.o \
                uKillPeaks1.o uLambdaQ1.o uNormalizeThisBank1.o

SEPDPREP_INC = parameters.inc
SEPDPREP_OBJ = sepdprep.o group.o cmima.o lamcal.o fndlam.o prepdat.o monspc.o \
           detspc.o effcyl.o vanproc.o canproc.o add.o datin.o smooth.o \
	   poly2fit.o 

CORPS_INC = const.inc
CORPS_OBJ = corps.o

SOQD_INC = parameters.inc
SOQD_OBJ = soqd.o how_long1.o reduct.o corpl.o misc.o uKuplotCorrOut.o \
	   poly2fit.o smooth.o

BLEND_INC = const.inc
BLEND_OBJ = ublend4.o uRebinQ2.o checkroot.o soqread.o how_long1.o

DAMP_INC = const.inc
DAMP_OBJ = damp.o

FT_INC =
FT_OBJ = ft.o

all: prepnorm prepariel prepsepd prepgsas corps soqd blend damp ft

# Fortran

.f.o:
	$(F77) -c $(FFLAGS) $<

# programs

prepsepd:$(USEPDPREP1_OBJ)
	$(F77) -o prepsepd $(USEPDPREP1_OBJ) $(LDFLAGS)

prepnorm:$(GLADPREP1_OBJ)
	$(F77) -o prepnorm $(GLADPREP1_OBJ) $(LDFLAGS)

prepariel:$(ARIELPREP_OBJ)
	$(F77) -o prepariel $(ARIELPREP_OBJ) $(LDFLAGS)

prepgsas:$(GSASPREP_OBJ)
	$(F77) -o prepgsas $(GSASPREP_OBJ) $(LDFLAGS)

corps:$(CORPS_OBJ)
	$(F77) -o corps $(CORPS_OBJ) $(LDFLAGS)

soqd:$(SOQD_OBJ) poly2fit.o smooth.o
	$(F77) -o soqd $(SOQD_OBJ) $(LDFLAGS)

blend:$(BLEND_OBJ) 
	$(F77) -o blend $(BLEND_OBJ) $(LDFLAGS)

damp:$(DAMP_OBJ)
	$(F77) -o damp $(DAMP_OBJ) $(LDFLAGS)

ft: $(FT_OBJ)
	$(F77) -o ft $(FT_OBJ) $(LDFLAGS)

# installation

install: prepsepd prepnorm prepariel prepgsas corps soqd blend damp ft 
	cp prepsepd  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/prepsepd
	cp prepnorm  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/prepnorm
	cp prepariel  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/prepariel
	cp prepgsas  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/prepgsas
	cp corps  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/corps
	cp soqd $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/soqd
	cp blend  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/blend
	cp damp  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/damp
	cp ft  $(INSTALL_DIR)
	chmod $(INSTALL_MOD) $(INSTALL_DIR)/ft

local: prepsepd prepnorm prepariel prepgsas corps soqd blend damp ft 
	cp prepsepd  ../binary
	chmod $(INSTALL_MOD) ../binary/prepsepd
	cp prepnorm  ../binary
	chmod $(INSTALL_MOD) ../binary/prepnorm
	cp prepariel  ../binary
	chmod $(INSTALL_MOD) ../binary/prepariel
	cp prepgsas  ../binary
	chmod $(INSTALL_MOD) ../binary/prepgsas
	cp corps  ../binary
	chmod $(INSTALL_MOD) ../binary/corps
	cp soqd ../binary
	chmod $(INSTALL_MOD) ../binary/soqd
	cp blend  ../binary
	chmod $(INSTALL_MOD) ../binary/blend
	cp damp  ../binary
	chmod $(INSTALL_MOD) ../binary/damp
	cp ft  ../binary
	chmod $(INSTALL_MOD) ../binary/ft

# cleanup

clean:
	rm -f *.o prepnorm prepsepd prepariel prepgsas corps soqd \
	blend damp ft

