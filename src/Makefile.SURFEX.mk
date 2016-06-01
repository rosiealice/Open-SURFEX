##########################################################
#                                                        #
#           Initialisation of some variables             #
#                                                        #
##########################################################
ifdef OBJDIR_PATH
OBJDIR_ROOT=${OBJDIR_PATH}/dir_obj
else
OBJDIR_ROOT=${PWD}/dir_obj
endif
LIB_OBJS_ROOT=lib
#
ARCH_XYZ=${ARCH}${MNH_INT}-${VERSION_XYZ}
##########################################################
#                                                        #
#            Source DIRECTORY                            #
#                                                        #
##########################################################

##########################################################
#           Source MYSRC                                 #
##########################################################
ifdef VER_USER
DIR_USER += ${VER_USER}
#RJ: unsafe
#RJ DIR_USER += ${VER_USER}_OFFLIN
#RJ DIR_USER += ${VER_USER}_ASSIM
endif
##########################################################
#           Source OFFLIN                                #
##########################################################
# PRE_BUG TEST !!!
#DIR_SURFEX += ARCH_SRC/bug_surfex
# PRE_BUG TEST !!!
#
DIR_OFFLIN += OFFLIN
FPPFLAGS_OFFLIN= -DSFX_BIN -DSFX_TXT -DSFX_OL -DSFX_NC
#FPPFLAGS_OFFLIN += -DTXT
#
ifdef DIR_OFFLIN
DIR_MASTER += $(DIR_OFFLIN)
FPPFLAGS   += $(FPPFLAGS_OFFLIN)
#VER_SURFEX=SURFEX-7-2-0
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_MYSRC)

#OBJS_NOCB +=  spll_mode_cover_301_573.o 

$(OBJS0): OPT = $(OPT0) 

endif

##########################################################
#           Source SURFEX                                #
##########################################################
# PRE_BUG TEST !!!
#DIR_SURFEX += ARCH_SRC/bug_surfex
# PRE_BUG TEST !!!
#
FUNDEFS += -USFX_ASC -USFX_OL -USFX_NC -USFX_BIN -USFX_TXT
FUNDEFS += -USFX_ARO -USFX_MNH -USFX_OFF -UAIX64 -URS6K
#RJ: next ones are only for phasing and must be removed from code in final version
FUNDEFS += -URJ_OFIX -URJ_PFIX
#
DIR_SURFEX += SURFEX
FPPFLAGS_SURFEX= -DSFX_ASC
INC_SURFEX = -I$(B)include
#
ifdef DIR_SURFEX
DIR_MASTER += $(DIR_SURFEX)
FPPFLAGS   += $(FPPFLAGS_SURFEX)
INC         += $(INC_SURFEX)
VER_SURFEX=SURFEX-7-2-0
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_MYSRC)

#OBJS_NOCB +=  spll_mode_cover_301_573.o 

$(OBJS0): OPT = $(OPT0) 

endif

##########################################################
#           Source ASSIM                                 #
##########################################################
# PRE_BUG TEST !!!
#DIR_SURFEX += ARCH_SRC/bug_surfex
# PRE_BUG TEST !!!
#
DIR_ASSIM += ASSIM
FPPFLAGS_ASSIM= -DUSE_SODA
#
ifdef DIR_ASSIM
DIR_MASTER += $(DIR_ASSIM)
FPPFLAGS   += $(FPPFLAGS_ASSIM)
#VER_SURFEX=SURFEX-7-2-0
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_MYSRC)

#OBJS_NOCB +=  spll_mode_cover_301_573.o 

$(OBJS0): OPT = $(OPT0)

endif

##########################################################
#           Source TRIP                                  #
##########################################################
# PRE_BUG TEST !!!
#DIR_SURFEX += ARCH_SRC/bug_surfex
# PRE_BUG TEST !!!
#
FUNDEFS += -UCPLOASIS
#
#DIR_TRIP += LIB/TRIPv2
#FPPFLAGS_TRIP=
#
ifdef DIR_TRIP
DIR_MASTER += $(DIR_TRIP)
FPPFLAGS   += $(FPPFLAGS_TRIP)
#VER_SURFEX=SURFEX-7-2-0
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_MYSRC)

#OBJS_NOCB +=  spll_mode_cover_301_573.o 

$(OBJS0): OPT = $(OPT0) 

endif
##########################################################
#           Source TOPD                                 #
##########################################################
#
FUNDEFS += -UTOPD
#
DIR_TOPD += LIB/TOPD
FPPFLAGS_TOPD= -DTOPD
#
ifdef DIR_TOPD
DIR_MASTER += $(DIR_TOPD)
FPPFLAGS   += $(FPPFLAGS_TOPD)

$(OBJS0): OPT = $(OPT0) 

endif
##########################################################
#           Source GELATO                                #
##########################################################
#
FUNDEFS += -Uin_surfex -Uin_nemo -Uin_arpege -Ukey_mpp_mpi
#
DIR_GELATO += LIB/GELATO
FPPFLAGS_GELATO= -Din_surfex
#
ifdef DIR_GELATO
DIR_MASTER += $(DIR_GELATO)
FPPFLAGS   += $(FPPFLAGS_GELATO)
VER_GELATO=GELATO-6-0-34
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_GELATO)

$(OBJS0): OPT = $(OPT0) 

endif
##########################################################
#           Source LFI                                   #
##########################################################
#RJ: unneeded?
ifdef LFI_COMPRESS
DIR_LFIC      += LIB/LFI_COMPRESS/src
PATH_LFIC     += LIB/LFI_COMPRESS/srcc
CPPFLAGS_LFIC = -DSWAPIO -DLINUX -DBIG_endian -Df2cFortran
INC_LFIC      = -I$(B)LIB/LFI_COMPRESS/include

ifdef DIR_LFIC
#
# Management/parametrisation of size of INTEGER ofr file > 16 GO & RECL for LFI
#
DIR_MASTER          += $(DIR_LFIC)
CPPFLAGS            += $(CPPFLAGS_LFIC)
OBJS_LISTE_MASTER   += bitbuff.o ieee_is_nan.o nearestpow2.o
INC                 += $(INC_LFIC)
VPATH               += $(PATH_LFIC)
#VER_NEWLFI=
#ARCH_XYZ    := $(ARCH_XYZ)-$(VER_NEWLFI)
endif
endif
##########################################################
#           Librairie DR_HOOK                            #
##########################################################

FUNDEFS += -USFX_MPL -ULINUX

ifeq "$(VER_DRHOOK)" "BYPASS"
DIR_HOOK = LIB/DRHOOK/BYPASS
endif

#RJ: minimal version of DRHOOK, mainly for profiling only
ifeq "$(VER_DRHOOK)" "MINI"
DIR_HOOK = LIB/DRHOOK/MINI
INC_HOOK = -I$(B)LIB/DRHOOK/MINI
FPPFLAGS_HOOK += -DLINUX
CPPFLAGS_HOOK += -DLINUX

OBJS_LISTE_MASTER += drhook.o crc.o addrdiff.o cargs.o endian.o env.o linuxtrbk.o
OBJS_LISTE_MASTER += gethwm.o getstatm.o getrss.o getstk.o getpag.o getstackusage.o getcurheap.o

#RJ: optional MPL layer support in DRHOOK for testing compatibility only!
ifneq "$(VER_MPI)" "NOMPI"
ifeq "$(USE_MPL)" "YES"
FPPFLAGS_HOOK += -DSFX_MPL
CPPFLAGS_HOOK += -DSFX_MPL
DIR_HOOK += LIB/DRHOOK/MPL
endif
endif
endif

ifeq "$(VER_DRHOOK)" "SIMPLE"
DIR_HOOK = LIB/DRHOOK_SIMPLE
INC_HOOK = -I$(B)LIB/DRHOOK_SIMPLE
FPPFLAGS_HOOK += -DLINUX
CPPFLAGS_HOOK += -DLINUX

OBJS_LISTE_MASTER += addrdiff.o cargs.o crc.o drhook.o endian.o env.o getcurheap.o 
OBJS_LISTE_MASTER += gethwm.o getpag.o getrss.o  getstackusage.o getstatm.o getstk.o 
OBJS_LISTE_MASTER += linuxtrbk.o mpe_locking.o
endif
#
ifdef DIR_HOOK
DIR_MASTER += $(DIR_HOOK)
FPPFLAGS   += $(FPPFLAGS_HOOK)
CPPFLAGS   += $(CPPFLAGS_HOOK)
INC        += $(INC_HOOK)
VPATH      += $(DIR_HOOK)
endif
#
##########################################################
#           Source XRD                                   #
##########################################################

#RJ: XRD40 for cross validation
VER_XRD = NONE
#VER_XRD ?= XRD39
#VER_XRD ?= XRD40

ifneq "$(VER_XRD)" "NONE"
FUNDEFS += -UFA_GRIBEX -UUSE_SAMIO -UNECSX -UVPP
DIR_XRD += LIB/$(VER_XRD)/FA
DIR_XRD += LIB/$(VER_XRD)/LFI
DIR_XRD += LIB/$(VER_XRD)/grib_mf
DIR_XRD += LIB/$(VER_XRD)/module
#DIR_XRD += LIB/$(VER_XRD)/support
DIR_XRD += LIB/$(VER_XRD)/utilities
INC_XRD = -I$(B)LIB/$(VER_XRD)/include -I$(B)LIB/$(VER_XRD)/FA -I$(B)LIB/$(VER_XRD)/LFI
#RJ: for time being avoid interfaces usage from xrd/fa cpp prototyping, not needed with backported patches
#RJ FPPFLAGS_XRD = -DHIGHRES -DLINUX -DLITTLE_ENDIAN -DLITTLE -DOFF
FPPFLAGS_XRD = -DHIGHRES -DLINUX -DLITTLE_ENDIAN -DLITTLE
CPPFLAGS_XRD = -DLINUX -DLITTLE_ENDIAN -DLITTLE
endif
#
ifdef DIR_XRD
DIR_MASTER += $(DIR_XRD)
FPPFLAGS   += $(FPPFLAGS_XRD)
CPPFLAGS   += $(CPPFLAGS_XRD)
INC        += $(INC_XRD)
endif
##########################################################
#           Source FM                                    #
##########################################################
ifneq "$(VER_XRD)" "NONE"
DIR_FM += LIB/FM
#FPPFLAGS_FM =
#INC_FM=
#
ifdef DIR_FM
DIR_MASTER += $(DIR_FM)
#FPPFLAGS   += $(FPPFLAGS_FM)
INC        += $(INC_FM)
endif
endif
##########################################################
#           Librairie OASIS3-MCT                         #
##########################################################
#RJ: must be before netcdf LIBS to correctly link
#
ifeq "$(VER_OASIS)" "mct"
ifneq "$(VER_MPI)" "NOMPI"
#
DIR_OASIS?=${SRC_SURFEX}/src/LIB/oasis${VERSION_OASIS}
OASIS_PATH?=${DIR_OASIS}-${ARCH}
LIB_OASIS?=-L${OASIS_PATH}/lib -lpsmile.MPI1 -lmct -lmpeu -lscrip
INC_OASIS?=-I${OASIS_PATH}/build/lib/psmile.MPI1
OASIS_KEY?=${OASIS_PATH}/build/lib/psmile.MPI1/mod_oasis.mod
#
FPPFLAGS_OASIS?= -DCPLOASIS
VPATH      += ${OASIS_PATH}/build/lib/psmile.MPI1
endif
endif
#
ifeq "$(VER_OASIS)" "mct_EXT"
ifneq "$(VER_MPI)" "NOMPI"
FPPFLAGS_OASIS?= -DCPLOASIS
VPATH      += ${OASIS_PATH}/build/lib/psmile.MPI1
endif
endif
#
ifneq "x$(VER_OASIS)" "x"
FPPFLAGS   += $(FPPFLAGS_OASIS)
LIBS       += $(LIB_OASIS)
INC        += $(INC_OASIS)
endif
##########################################################
#           Source MPIVIDE                               #
##########################################################
#
FUNDEFS += -UNOMPI -USFX_MPI
#
#RJ: moved from all configs here
ifneq "$(VER_MPI)" "NOMPI"
FPPFLAGS += -DSFX_MPI
endif
#
ifndef VER_MPI
VER_MPI=MPIVIDE
endif
#VER_MPI=MPIVIDE,LAMMPI,LAMMPI-IB,MPICH-IB
#
#   MPIVIDE
#
ifeq "$(VER_MPI)" "MPIVIDE"
DIR_MPI               += LIB/MPIvide
INC_MPI                = -I$(B)$(DIR_MPI)
DIR_MASTER            += $(DIR_MPI)
OBJS_LISTE_MASTER     += mpivide.o
INC                   += $(INC_MPI)
mpivide.o  : CPPFLAGS  = -DFUJI -I$(DIR_MPI)/include
VPATH                 += $(DIR_MPI)
endif
#
#   LAMMPI
#
ifeq "$(VER_MPI)" "LAMMPI"
# Standard Lam mpi
#INC_MPI     = -I$(B)/opt/lam/include
#LIB_MPI     = -L/opt/lam/lib   -lmpi -llammpi++ -llammpio -llamf77mpi -lmpi -llam -lpthread -ldl
# default 64 bits SUSE 9 version
INC_MPI     = -I$(B)/usr/include
LIB_MPI     = -lmpi -llammpi++ -llammpio -llamf77mpi -lmpi -llam -lpthread -ldl -lutil 
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif
#
#   LAMMPI-IB
#
ifeq "$(VER_MPI)" "LAMMPI-IB"
INC_MPI     = -I/home/sila/LAM-7.1.1/include
LIB_MPI     = -L/usr/local/ibgd/driver/infinihost/lib64 -L/home/sila/LAM-7.1.1/lib \
-llammpio -llamf77mpi -lmpi -llam -lutil -lmosal -lmpga -lmtl_common -lvapi -ldl  -lpthread
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif
#
#   MPICH-IB
#
ifeq "$(VER_MPI)" "MPICH-IB"
INC_MPI     = -I/usr/local/ibgd/mpi/osu/f95/mvapich-0.9.5/include
LIB_MPI     = -L/usr/local/ibgd/driver/infinihost/lib64 \
                 -L/usr/local/ibgd/mpi/osu/f95/mvapich-0.9.5/lib \
                 -lmpich -lmtl_common -lvapi -lmosal -lmpga -lpthread
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif
#
#   MPICH-2 CNRM
#
ifeq "$(VER_MPI)" "MPICH2"
INC_MPI     = -I/usr/include
LIB_MPI     = -lmpichf90 -lmpich 
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif

#
#   OPENMPI 1.1 CNRM
#
ifeq "$(VER_MPI)" "OMPICNRM"
MPI_ROOT=/opt/openmpi
INC_MPI = -I${MPI_ROOT}/include  -I${MPI_ROOT}/include/openmpi/ompi -I${MPI_ROOT}/lib64
LIB_MPI     = -L${MPI_ROOT}/lib64 -lmpi -lopen-rte -lopen-pal -lutil -lnsl -ldl -Wl,--export-dynamic -lm -lutil -lnsl -ldl
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif

#
#   OPENMPI 1.1 BPROC + OPENIB + IFORT
#
ifeq "$(VER_MPI)" "OMPIIFORT"
MPI_ROOT=/home/sila/DEV/OPEN-MPI-11-IFORT-BPROC-OPENIB
INC_MPI     = -I${MPI_ROOT}/include -I${MPI_ROOT}/include/openmpi/ompi -I${MPI_ROOT}/lib
LIB_MPI     = -L${MPI_ROOT}/lib -lmpi -lorte -lopal -lutil -lnsl -ldl -Wl,--export-dynamic -lm -lutil -lnsl -ldl
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif

#
#   OPENMPI 1.1.4 IFORT BPROC
#
ifeq "$(VER_MPI)" "OMPI114IFORT"
MPI_ROOT=/home/sila/DEV/OPEN-MPI-114-IFORT-BPROC-OPENIB
INC_MPI     = -I${MPI_ROOT}/include -I${MPI_ROOT}/include/openmpi/ompi -I${MPI_ROOT}/lib
LIB_MPI     = -L${MPI_ROOT}/lib -lmpi -lorte -lopal -lutil -lnsl -ldl -Wl,--export-dynamic -lm -lutil -lnsl -ldl
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif

#
#   OPENMPI 1.2.2 G95 BPROC
#
ifeq "$(VER_MPI)" "OMPI122G95"
MPI_ROOT=/home/sila/DEV/OPEN-MPI-122-G95-BPROC-OPENIB
INC_MPI     = -I${MPI_ROOT}/include -I${MPI_ROOT}/include/openmpi/ompi -I${MPI_ROOT}/lib
LIB_MPI     = -L${MPI_ROOT}/lib -lmpi_f90 -lmpi_f77 -lmpi -lopen-rte -lopen-pal -Wl,--export-dynamic -lm -lutil -lnsl -ldl
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif
#
#   OPENMPI12X
#
ifeq "$(VER_MPI)" "OMPI12X"
INC_MPI     = -I${MPI_ROOT}/include -I${MPI_ROOT}/include/openmpi/ompi -I${MPI_ROOT}/lib
LIB_MPI     = -L${MPI_ROOT}/lib -lmpi_f90 -lmpi_f77 -lmpi -lopen-rte -lopen-pal -Wl,--export-dynamic -lm -lutil -lnsl -ldl
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif
#
#   MPI for SGI-ICE 
#
ifeq "$(VER_MPI)" "MPIICE"
INC_MPI     = 
LIB_MPI     = -lmpi
INC            += $(INC_MPI)
LIBS           += $(LIB_MPI)
endif


ARCH_XYZ    := $(ARCH_XYZ)-$(VER_MPI)-$(VER_OMP)
##########################################################
#           Librairie GRIBEX                             #
##########################################################
#ifneq "$(ARCH)" "BG"
# Gribex bypass on BG for the moment
#DIR_GRIBEX     +=  LIB/GRIBEX
#endif
#
#ifdef DIR_GRIBEX
#LIB_GRIBEX     =  $(DIR_GRIBEX)_$(ARCH)/libgribexR64.a
#LIBS          +=    $(LIB_GRIBEX)
#R64_GRIBEX=R64
#endif
##########################################################
#           Librairie GRIBAPI                            #
##########################################################
#
ifneq "$(VER_GRIBAPI)" "NONE"
#
ifeq "$(VER_GRIBAPI)" "GRIBAPI_AUTO"
DIR_GRIBAPI?=${SRC_SURFEX}/src/LIB/grib_api-${VERSION_GRIBAPI}
#RJ: avoid non standard libs! Can create non conforming outputs
GRIBAPI_PATH?=${DIR_GRIBAPI}-${ARCH}
GRIBAPI_INC?=${GRIBAPI_PATH}/include/grib_api.mod
#
INC_GRIBAPI   ?= -I${GRIBAPI_PATH}/include
LIB_GRIBAPI   ?= -L${GRIBAPI_PATH}/lib -L${GRIBAPI_PATH}/lib64 -lgrib_api_f90 -lgrib_api
endif

ifeq "$(VER_GRIBAPI)" "SOPRANO"
GRIBAPI_PATH=/usr/local/sopra/grib_api
INC_GRIBAPI   ?= -I${GRIBAPI_PATH}/include
LIB_GRIBAPI   ?= -L${GRIBAPI_PATH}/lib -L${GRIBAPI_PATH}/lib64 -lgrib_api_f90 -lgrib_api
endif

ifneq "x$(VER_GRIBAPI)" "x"
INC           += $(INC_GRIBAPI)
LIBS          += $(LIB_GRIBAPI)
endif
#
endif
#
##########################################################
#           Librairie NETCDF                             #
##########################################################
#
# NetCDF  : AUTO install of netcdf-3.6.X or 4.x.x on PC linux to avoid problem with compiler
#
ifneq "$(VER_CDF)" "NONE"
#
ifeq "$(VER_CDF)" "CDFAUTO"
DIR_CDF?=${SRC_SURFEX}/src/LIB/netcdf-${VERSION_CDF}
#RJ: avoid non standard libs! Can create non conforming outputs
CDF_PATH?=${DIR_CDF}-${ARCH}
CDF_INC?=${CDF_PATH}/include/netcdf.mod
#
INC_NETCDF     ?= -I${CDF_PATH}/include
ifeq "$(VERSION_CDF)" "3.6.3"
LIB_NETCDF     ?= -L${CDF_PATH}/lib -L${CDF_PATH}/lib64 -lnetcdf
else
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdff -lnetcdf
endif
endif
#
# NetCDF in SGI ICE
#
ifeq "$(VER_CDF)" "CDFICE"
CDF_PATH?=/opt/software/SGI/netcdf/4.0
INC_NETCDF     ?= -I${CDF_PATH}/include
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdff  -lnetcdf -i_dynamic 
endif
#
# NetCDF in NEC SX
#
ifeq "$(VER_CDF)" "CDFSX"
CDF_PATH?=/SXlocal/pub/netcdf/3.6.1
INC_NETCDF     ?= -I${CDF_PATH}/include
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdf_c++ -lnetcdf
endif
#
ifeq "$(VER_CDF)" "CDFMFSX"
CDF_PATH?=/usr/local/SX/lib/NETCDF_size_t32
INC_NETCDF     ?= -I${CDF_PATH}/include
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdf
endif
#
# NetCDF in AIX S
#
ifeq "$(VER_CDF)" "CDFAIX"
CDF_PATH?=/usr/local/pub/NetCDF/3.6.2
INC_NETCDF     ?= -I${CDF_PATH}/include
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdf_c++ -lnetcdf
endif

#
# Linux with gfortran SUSE10.3
#
ifeq "$(VER_CDF)" "CDFGFOR"
INC_NETCDF     ?=  -I/usr/include
LIB_NETCDF     ?=  -lnetcdf -lnetcdff /usr/lib64/libgfortran.so.2
endif

#
# Linux with netcdf CTI 3.6.3
#
ifeq "$(VER_CDF)" "CDFCTI"
CDF_PATH?=/usr
INC_NETCDF     = -I${CDF_PATH}/include
LIB_NETCDF     = -L${CDF_PATH}/lib64 -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lsz -lz
endif

#
# Linux with gfortran SUSE11.1
#
ifeq "$(VER_CDF)" "CDF3GFOR"
CDF_PATH       ?=/opt/netcdf3
INC_NETCDF     ?= -I${CDF_PATH}/include
LIB_NETCDF     ?= -L${CDF_PATH}/lib64  -lnetcdf_c++ -lnetcdf
endif

#
# Linux with SOPRANO
#
ifeq "$(VER_CDF)" "CDFSOPRANO"
CDF_PATH?=/usr
INC_NETCDF     = -I${CDF_PATH}/include
LIB_NETCDF     = -L${CDF_PATH}/lib64 -lnetcdff -lnetcdf
endif

#
# Linux on Beaufix
#
ifeq "$(VER_CDF)" "CDFBOFX"
CDF_PATH       ?= /opt/softs/libraries/ICC13.1.4.183/netcdf-4.3.0
INC_NETCDF     ?= -I${CDF_PATH}/include 
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdff -lnetcdf -Wl,-rpath,$(CDF_PATH)/lib
endif

#
# Linux on prolix
#
ifeq "$(VER_CDF)" "CDFPROLX"
CDF_PATH       ?= /opt/softs/libraries/ICC16.1.150/netcdf-4.4.0
INC_NETCDF     ?= -I${CDF_PATH}/include 
LIB_NETCDF     ?= -L${CDF_PATH}/lib -lnetcdff -lnetcdf -Wl,-rpath,$(CDF_PATH)/lib
endif


ifneq "x$(VER_GRIBAPI)" "x"
INC            += $(INC_NETCDF)
LIBS           += $(LIB_NETCDF)
endif
#
endif
#
##########################################################
#           Number of NESTED MODEL                       #
##########################################################
NSOURCE=8
##########################################################
#                                                        #
# PROG_LIST : Main program liste to compile              #
#                                                        #
##########################################################
#
TRIP_LIST += TRIP_PREP TRIP_MASTER TRIP_CHANGE_DATE
#
ifeq "$(ARCH)" "BG"
PROG_LIST += OFFLINE 
else
PROG_LIST += PGD PREP OFFLINE SODA SXPOST
#PROG_LIST +=  $(TRIP_LIST)
#PGD PREP OFFLINE SODA SXPOST
endif
#
#RJ: only include during 'make user' to avoid 'bad' programs
#
#RJ: should PGD,PREP,OFFLINE,SODA be in VER_USER==smth case?
#PROG_LIST_USER+=PGD PREP OFFLINE SODA
#
PROG_LIST_USER = $(PROG_LIST)
#
ifeq "$(VER_USER)" "FORC"
PROG_LIST_USER += PRE_INPUT_EXPERIMENT
endif
#
##########################################################
#                                                        #
# LIB_OBJS : Librarie of all *.o                         #
#                                                        #
##########################################################
#
ARCH_XYZ        := $(ARCH_XYZ)-$(OPTLEVEL)
OBJDIR_ROOT     := $(OBJDIR_ROOT)-$(ARCH_XYZ)
LIB_OBJS_ROOT   := $(LIB_OBJS_ROOT)-$(ARCH_XYZ)
#
##########################################################
#                                                        #
# IGNORE_OBJS : some *.o to ignore                       #
#       ---> unused unsupported old routines             #
#                                                        #
##########################################################
#
IGNORE_OBJS += 
IGNORE_DEP_MASTER += 
IGNORE_DEP_MASTER +=
#
#
##########################################################
#                                                        #
#  VPATH_EXCLUDE : Some sources directory to exclude     #
#                                                        #
##########################################################
#
VPATH_EXCLUDE= %/CVS
#
