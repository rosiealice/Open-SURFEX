####
MAKEFLAGS += -r
####
##########################################################
#                                                        #
#  MACRO : dependance_flist                              #
#          generate the dependance list for ALL fortran  #
#          "*.f*" files                                  #
#          searching in PATH=$(1)                        #
##########################################################
#
dependance_flist = $(notdir $(shell find $(1) -follow -type f \
               -iname '*.f' -o -iname '*.f90' -o -name '*.fx90' \
               | grep -v ',v' | sed -e 's/\.f.*/.D/g' -e 's/\.F.*/.D/g' ))
obj_flist        = $(notdir $(shell find $(1) -follow -type f \
               -iname 'spll_*.f*' | sed -e 's/\..*/.o/g' ))
#
##########################################################
#                                                        #
# COMPILER & ARCHITECTURE CONFIGURATION                  #
#                                                        #
##########################################################
#
include Rules.$(ARCH)$(F).mk
#
#RJ guess real FC if using wrappers
REALFC ?= $(F90)
#
#RJ try to get fortran module output flag if not provided
#RJ if fails, use workaround for *.mod file movement
ifeq "x$(MODOUT_FLAG)" "x"
ifneq (,$(findstring gfortran,$(REALFC)))
MODOUT_FLAG=-J
endif
#RJ be more careful, since all mpi implemetations switched to common mpifort wrapper
ifneq (,$(findstring _ifort,_$(REALFC)))
MODOUT_FLAG=-module
endif
ifneq (,$(findstring ftn,$(REALFC)))
MODOUT_FLAG=-e m -J
endif
ifneq (,$(findstring pgf90,$(REALFC)))
MODOUT_FLAG=-module
endif
#RJ from xlf documentation -I path is also needed search paths
ifneq (,$(findstring xlf,$(REALFC)))
MODOUT_FLAG=-qmoddir
##MODOUT?=$(MODOUT_FLAG) $(OBJDIR)/MOD -I $(OBJDIR)/MOD
endif
#sun
#MODOUT_FLAG=-moddir=
endif
#
#RJ check if needed workaround to move *.mod files and force serial compilation
ifeq "x$(MODOUT_FLAG)" "x"
MODOUT=-I $(OBJDIR)/MOD
.NOTPARALLEL :
else
#RJ be more generous to all compilers
#RJ however it might be useful to strip part of fullpath in OBJDIR and OBJDIR_MASTER here...
MODOUT?=$(MODOUT_FLAG) $(OBJDIR)/MOD -I$(OBJDIR)/MOD
endif
#
#RJ add 'make user' support
ifdef DO_COMP_USER
MODOUT+= -I$(OBJDIR_MASTER)/MOD
endif

#
##########################################################
##########################################################
##########################################################
#                                                        #
#    DONT MODIFY LINES AFTER THIS  !!!!!!!!!!!!!         #
#                                                        #
##########################################################
##########################################################
##########################################################

##########################################################
#                                                        #
#    AUTOMATIQUE  DEPENDANCE GENERATION !!!!!!!!!!!!!    #
#                                                        #
#   "*.D" files contain dependance deduceed              #
#   form all "*.f*" FORTRAN sources files                #
#                                                        #
# "use" key word                                         #
#     --> dependance on "*.mod" module files             #
# "module" key word key word                             #
#     --> creation of"*.mod"  module files               #
#                                                        #
#   The "*.D" list of files is contructed from every     #
#   directories of fortran sources :                     #
#       ---> user, master , bugfix , etc...              #
#   with or without RCS version control                  #
#                                                        #
#   The "*.D" list is reconstructed automaticaly         #
#   by the commande "spll" , each time a FORTRAN source  #
#   are modified or added .                              #
#                                                        #
#   All modification are allowed !!!!!                   #
#   adding new subroutines                               #
#       or                                               #
#   adding new modules                                   #
#                                                        #
#   REM : if during modification, you deleting some      #
#         FORTRAN subroutines you must also deleted the  #
#         "spll_*.o" files created by this one by hand   #
#         to avoid potential link problems               #
#                                                        #
##########################################################

##########################################################
#                                                        #
# DEP_MASTER : Automatique -->                           #
#    Dependance file of MASTER source                    #
#                                                        #
##########################################################
#
OBJDIR_MASTER=$(OBJDIR_ROOT)/MASTER
DEP_MASTER= $(call dependance_flist,$(DIR_MASTER))
#
#  init VPATH depending on compilation phase
#
ifdef DO_DEP_MASTER
VPATH += $(shell find $(DIR_MASTER) -follow -type d )
VPATH += $(OBJDIR_MASTER)
endif
#
ifdef DO_COMP_MASTER
VPATH += $(OBJDIR_MASTER) $(OBJDIR_MASTER)/MOD
endif
#
##########################################################
#                                                        #
# DEP_USER : Automatique -->                             #
#    Dependance file of MY source                        #
#                                                        #
##########################################################
#
ifdef VER_USER
OBJDIR_USER=$(OBJDIR_ROOT)/$(VER_USER)
#
#  init VPATH depending on compilation phase
#
ifdef DO_DEP_USER
DEP_USER= $(call dependance_flist,$(DIR_USER))
VPATH += $(shell find $(DIR_USER) -follow -type d )
VPATH += $(OBJDIR_USER)
endif
#
ifdef DO_COMP_USER
VPATH += $(OBJDIR_USER) $(OBJDIR_USER)/MOD $(OBJDIR_MASTER) $(OBJDIR_MASTER)/MOD
#RJ fix, include it above
##INC += -I $(OBJDIR_MASTER)/MOD
endif
#
endif
##########################################################
#                                                        #
# VPATH ADJUSTEMENT :                                    #
#                                                        #
##########################################################
#
#
#VPATH := $(filter-out $(VPATH_EXCLUDE),$(VPATH))
#
##########################################################
#                                                        #
# OBJS : all *.o to compile                              #
#                                                        #
##########################################################
#
# MASTER
#
OBJS_LISTE_MASTER :=  $(OBJS_LISTE_MASTER) $(notdir $(shell find $(OBJDIR_MASTER) -follow -type f -iname "spll_*.f*" | sed -e 's/\..*/.o/g' ))
#OBJS_LISTE_MASTER :=  $(OBJS_LISTE_MASTER) $(obj_flist $(OBJDIR_MASTER))
#
OBJS_LISTE_MASTER := $(sort $(filter-out $(IGNORE_OBJS),$(OBJS_LISTE_MASTER)))
#
LIB_MASTER=$(OBJDIR_MASTER)/$(LIB_OBJS_ROOT)-MASTER.a
#
# USER
#
#ifdef DO_COMP_USER
OBJS_LISTE_USER :=  $(OBJS_LISTE_USER) $(notdir $(shell find $(OBJDIR_USER) -follow -type f -iname "spll_*.f*" | sed -e 's/\..*/.o/g' ))
#OBJS_LISTE_USER :=  $(OBJS_LISTE_USER) $(obj_flist $(OBJDIR_USER))
#
OBJS_LISTE_USER := $(sort $(filter-out $(IGNORE_OBJS), $(OBJS_LISTE_USER) $(OBJS_LISTE_MASTER) ))
#
LIB_USER=$(OBJDIR_USER)/$(LIB_OBJS_ROOT)-$(VER_USER).a
#endif
##########################################################
#                                                        #
# DEP_ALL : All dependance *.D to generate               #
#                                                        #
##########################################################
#
DEP_ALL_MASTER = $(sort $(filter-out $(IGNORE_DEP_MASTER),$(DEP_MASTER)) )
#
DEP_ALL_USER   = $(sort $(filter-out $(IGNORE_DEP_USER)  ,$(DEP_USER)) )
#

.PHONY : clean all objdir objdirmaster depall objs lib prog print

.DELETE_ON_ERROR:

.SUFFIXES:
.SUFFIXES:  .o .F90 .f90 .f .fx90 .mod .D .a

.INTERMEDIATE:   $(LIB_MASTER)


##########################################################
#                                                        #
# MASTER RULES                                           #
#                                                        #
##########################################################
#RJ: compile internal packages as REDISTS, before compilation of the SURFEX code
ifeq "$(VER_GRIBAPI)" "GRIBAPI_AUTO"
REDISTS += gribapi
endif

ifeq "$(VER_CDF)" "CDFAUTO"
REDISTS += cdf
endif

#RJ: re-add oasis back
ifeq "$(VER_OASIS)" "mct"
ifneq "$(VER_MPI)" "NOMPI"
EXTCOUPLERS += oasis
endif
endif

# main projectes
PROJECTS += ecoclimap progmaster

#RJ: add new main target, must come first!
all : $(REDISTS) $(EXTCOUPLERS) $(PROJECTS)

objdirmaster :  $(OBJDIR_MASTER)/.dummy $(REDISTS) $(EXTCOUPLERS)

$(OBJDIR_MASTER)/.dummy :
	-mkdir -p $(OBJDIR_MASTER)/.dummy
	-mkdir -p $(OBJDIR_MASTER)/MOD

depmaster : objdirmaster
	@$(MAKE) DO_DEP_MASTER=YES OBJDIR=$(OBJDIR_MASTER) filedepallmaster

filedepallmaster :  $(DEP_ALL_MASTER)
	find  $(OBJDIR_MASTER) -follow -name "*.D" >  $(OBJDIR_MASTER)/filemaster
	rm -f  $(OBJDIR_MASTER)/filedepallmaster
	cat $(OBJDIR_MASTER)/filemaster  | xargs cat  >>  $(OBJDIR_MASTER)/filedepallmaster

libmaster :  depmaster
	@$(MAKE)  -I $(OBJDIR_MASTER)  DO_COMP_MASTER=YES OBJDIR=$(OBJDIR_MASTER) objmaster


bibmaster :  libmaster
	@$(MAKE)  -I $(OBJDIR_MASTER)  DO_COMP_MASTER=YES OBJDIR=$(OBJDIR_MASTER) $(LIB_MASTER)
	mv $(LIB_MASTER)  $(subst lib-,bib-,$(LIB_MASTER))


objmaster :  $(OBJS_LISTE_MASTER)

$(LIB_MASTER) :  $(OBJS_LISTE_MASTER)
	cd $(OBJDIR_MASTER) ; find . -follow -name '*.o'| xargs $(AR) r $(LIB_MASTER)

progmaster : libmaster
	@$(MAKE) -I $(OBJDIR_MASTER)  DO_COMP_MASTER=YES  \
	OBJDIR=$(OBJDIR_MASTER) \
	LIB_MNH=$(LIB_MASTER) \
	PROG_DIR=$(OBJDIR_MASTER) \
	$(PROG_LIST)
	@$(MAKE) -I $(OBJDIR_MASTER)  DO_COMP_MASTER=YES  \
	OBJDIR=$(OBJDIR_MASTER) \
	LIB_MNH=$(LIB_MASTER) \
	PROG_DIR=$(OBJDIR_MASTER) \
	cleanlibmaster

installmaster : libmaster
	@$(MAKE) -I $(OBJDIR_MASTER)  DO_COMP_MASTER=YES  OBJDIR=$(OBJDIR_MASTER) install

cleanlibmaster :
	-rm -f  $(LIB_MASTER)

cleanmaster : $(addprefix clean_,$(PROJECTS))
	-test -d $(OBJDIR_MASTER) && rm -fr $(OBJDIR_MASTER)

cleanobjmaster :
	-test -d $(OBJDIR_MASTER) && find $(OBJDIR_MASTER) -follow -name "*.o" -o -name "*.mod" | xargs rm

clean_progmaster :
	-test -d $(OBJDIR_MASTER) && cd $(OBJDIR_MASTER) && rm -f $(PROG_LIST)

##########################################################
#                                                        #
#  USER RULES                                            #
#                                                        #
##########################################################

user : proguser

objdiruser :  $(OBJDIR_USER)/.dummy $(REDISTS) $(EXTCOUPLERS)

$(OBJDIR_USER)/.dummy :
	-mkdir -p $(OBJDIR_USER)/.dummy
	-mkdir -p $(OBJDIR_USER)/MOD

#RJ: guessing that depmaster is missing here for filemaster dependency
depuser : objdiruser depmaster
	@$(MAKE) DO_DEP_USER=YES OBJDIR=$(OBJDIR_USER) filedepalluser

filedepalluser : $(DEP_ALL_USER)
	find  $(OBJDIR_USER) -follow -name "*.D" >  $(OBJDIR_USER)/fileuser
	cat $(OBJDIR_USER)/fileuser | sed -e "s!$(OBJDIR_USER)!$(OBJDIR_MASTER)!g"  >  $(OBJDIR_USER)/filenotmaster
	grep -F -v -f   $(OBJDIR_USER)/filenotmaster  $(OBJDIR_MASTER)/filemaster >  $(OBJDIR_USER)/filemaster
	rm -f  $(OBJDIR_USER)/filedepalluser
	cat $(OBJDIR_USER)/fileuser $(OBJDIR_USER)/filemaster | xargs cat >>  $(OBJDIR_USER)/filedepalluser


libuser :   depuser
	@$(MAKE) -I $(OBJDIR_USER) DO_COMP_USER=YES OBJDIR=$(OBJDIR_USER) objuser

objuser :  $(OBJS_LISTE_USER)

$(LIB_USER) :  $(OBJS_LISTE_USER)
	rm -f $(LIB_USER)
	cd $(OBJDIR_MASTER) ; find . -name "*.o" | xargs $(AR) r $(LIB_USER)
	cd $(OBJDIR_USER)   ; find . -name "*.o" | xargs $(AR) r $(LIB_USER)

proguser : libuser
	@$(MAKE) -I $(OBJDIR_USER)  DO_COMP_USER=YES \
	OBJDIR=$(OBJDIR_USER) \
	LIB_MNH=$(LIB_USER) \
	PROG_DIR="$(OBJDIR_USER) $(OBJDIR_MASTER)" \
	PROG_LIST="$(or $(PROG_LIST_USER), PROG_LIST_USER_not_set)" \
	$(PROG_LIST_USER) # cleanlibuser

cleanlibuser :
	rm -f  $(LIB_USER)

installuser :
	@$(MAKE)  -I $(OBJDIR_USER) DO_COMP_USER=YES  OBJDIR=$(OBJDIR_USER) \
	PROG_LIST="$(PROG_LIST_USER)" install

cleanproguser :
	test -d $(OBJDIR_USER) && cd $(OBJDIR_USER) && rm -f $(PROG_LIST_USER)

##########################################################
#                                                        #
# EXTRA LIB : GRIBEX                                     #
#                                                        #
##########################################################
#RJ: seems not needed
ifdef GRIBEX
ifneq "$(findstring 64,$(shell uname -m))" ""
A64=A64
endif
#$(LIB_GRIBEX) : libmaster
$(LIB_GRIBEX) :
ifneq "$(ARCH)" "SX8"
	- [ ! -d $(DIR_GRIBEX)_$(ARCH) ] && cp -Rp  $(DIR_GRIBEX) $(DIR_GRIBEX)_$(ARCH)
	cd $(DIR_GRIBEX)_$(ARCH) && $(MAKE) -j 1 ARCH=$(TARGET_GRIBEX) R64=$(R64_GRIBEX) CNAME=$(CNAME_GRIBEX) A64=$(A64)
else
ifneq "$(findstring brodie,$(shell uname -n))" ""
#brodie
	- [ ! -d $(DIR_GRIBEX)_$(ARCH) ] && mkdir -p ${workdir}/$(DIR_GRIBEX)_$(ARCH) \
	&& ln -s ${workdir}/$(DIR_GRIBEX)_$(ARCH)  $(DIR_GRIBEX)_$(ARCH)
	cp /home/rech/mnh/rmnh007/aeronec/mesonh/binaries/libemosR64.a $(DIR_GRIBEX)_$(ARCH)/libgribexR64.a
else
#tori & yuki
	- [ ! -d $(DIR_GRIBEX)_$(ARCH) ] && mkdir -p $(DIR_GRIBEX)_$(ARCH)
	cp /usr/local/SX/lib/libgribex.a $(DIR_GRIBEX)_$(ARCH)/libgribexR64.a
endif
endif

gribex_clean :
	- [ ! -d $(DIR_GRIBEX)_$(ARCH) ] || rm -fr $(DIR_GRIBEX)_$(ARCH)
endif
##########################################################
#                                                        #
# EXTRA LIB : GRIBAPI                                    #
#                                                        #
##########################################################
#RJ: fix issue with mixed compilers by clearing some env flags: LDFLAGS,CFLAGS,CPPFLAGS
ifeq "$(VER_GRIBAPI)" "GRIBAPI_AUTO"
gribapi : $(GRIBAPI_INC)
$(GRIBAPI_INC) :
	cd $(DIR_GRIBAPI) && LDFLAGS= CFLAGS= CPPFLAGS=  \
	 ./configure --disable-jpeg --prefix=${GRIBAPI_PATH} FC="$(REALFC)" && \
	 $(MAKE) -j 1 clean && \
	 $(MAKE) -j 1 && \
	 $(MAKE) -j 1 install && \
	 $(MAKE) -j 1 clean

clean_gribapi :
	- [ ! -d ${GRIBAPI_PATH} ] || rm -fr ${GRIBAPI_PATH}
endif
##########################################################
#                                                        #
# EXTRA LIB : NETCDF                                     #
#                                                        #
##########################################################
#RJ: fix issue with mixed compilers by clearing some env flags: LDFLAGS,CFLAGS,CPPFLAGS
ifeq "$(VER_CDF)" "CDFAUTO"
#
ifeq "$(VERSION_CDF)" "3.6.3"
NETCDF_OPTS ?= FC="$(REALFC)" CPPFLAGS="-DpgiFortran" --disable-shared --disable-cxx
else
#RJ: assume to build netcdf4 library with only netcdf3-classic file support
NETCDF_OPTS ?= FC="$(REALFC)" --disable-netcdf-4 --disable-hdf4 --disable-fsync --disable-dap --enable-cdmremote=no --disable-examples --disable-cxx --disable-v2 --disable-shared --disable-netcdf-4 --disable-hdf4 --disable-fsync --disable-dap --enable-cdmremote=no --disable-examples --disable-cxx --disable-v2 --disable-shared --disable-compiler-recover --with-pic
endif
#
cdf : $(CDF_INC)
$(CDF_INC) :
	cd $(DIR_CDF) && LDFLAGS= CFLAGS= CPPFLAGS=  \
	 ./configure --prefix=${CDF_PATH} $(NETCDF_OPTS) && \
	 $(MAKE) -j 1 clean && \
	 $(MAKE) -j 1 && \
	 $(MAKE) -j 1 install && \
	 $(MAKE) -j 1 clean

clean_cdf :
	- [ ! -d ${CDF_PATH} ] || rm -fr  ${CDF_PATH}
endif

##########################################################
#                                                        #
# EXTRA LIB : OASIS3-MCT                                 #
#                                                        #
##########################################################
ifeq "$(VER_OASIS)" "mct"
ifneq "$(VER_MPI)" "NOMPI"
#
oasis : $(OASIS_KEY)
$(OASIS_KEY) : $(CDF_INC)
	([ ! -d ${OASIS_PATH} ] && mkdir ${OASIS_PATH}) || true
	@echo '=> Compiling oasis'
	@echo '===> check util/make_dir/COMP.log STDOUT messages'
	@echo '===> check util/make_dir/COMP.err for STDERR logs'
	cd $(DIR_OASIS)/util/make_dir && \
	  COUPLE="$(DIR_OASIS)" ARCHDIR="${OASIS_PATH}" CPPFLAGS= LDFLAGS= \
	  F90="$(F90)" F="$(F90)" F90FLAGS_1="$(OPT) $(INC_NETCDF) $(LIB_NETCDF)" \
	    $(MAKE) -j 1 -f TopMakefileOasis3
	echo '=> Compiling oasis DONE'

clean_oasis :
	cd ${DIR_OASIS}/util/make_dir && \
	  COUPLE="$(DIR_OASIS)" ARCHDIR="${OASIS_PATH}" \
	    $(MAKE) -j 1 realclean -f TopMakefileOasis3
	- [ ! -d ${OASIS_PATH} ] || rm -fr  ${OASIS_PATH}
endif
endif
##########################################################
#                                                        #
# EXTRA LIB : ECOCLIMAP                                  #
#                                                        #
##########################################################
ecoclimap:${SRC_SURFEX}/MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin

${SRC_SURFEX}/MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin :
ifeq "$(ARCH)" "SX8"
	cd ${SRC_SURFEX}/MY_RUN/ECOCLIMAP && $(F90) $(F90FLAGS) convert_ecoclimap_param.f90 && qsub job_ecoclimap_sx8
else
ifeq "$(ARCH)" "AIX64"
	cd ${SRC_SURFEX}/MY_RUN/ECOCLIMAP && $(F90) $(F90FLAGS) convert_ecoclimap_param.f90 && llsubmit job_ecoclimap_aix64
else
	cd ${SRC_SURFEX}/MY_RUN/ECOCLIMAP && $(F90) $(F90FLAGS) convert_ecoclimap_param.f90 && ./a.out
endif
endif

clean_ecoclimap :
	cd ${SRC_SURFEX}/MY_RUN/ECOCLIMAP && rm -f ./*.bin ./a.out

##########################################################
#                                                        #
# PROGRAM RULES                                          #
#                                                        #
##########################################################
ifdef PROG_LIST

prog :   lib $(LIB_GRIBEX)
	@$(MAKE) -I $(OBJDIR) DEP=YES  $(PROG_LIST)

ifeq "$(DO_COMP_USER)" "YES"
install : $(patsubst %,%-${ARCH_XYZ}-${VER_USER},$(PROG_LIST))
else
install : $(patsubst %,%-${ARCH_XYZ},$(PROG_LIST))
endif

$(PROG_LIST) : OBJ_PROG=$(shell find $(PROG_DIR) -follow -type f -iname "spll_*.f*" \
              | xargs grep -l -E -i "^[[:space:]]*program *$@" |  sed -e 's/\.f.*/.o/gI' | head -1 )

#RJ: added hack when linking user if no master was linked
$(PROG_LIST) :  $(LIB_MNH)  $(LIB_GRIBEX)
ifeq "$(DO_COMP_USER)" "YES"
	$(F90) $(LDFLAGS) -o $(OBJDIR)/$@ \
	  $(shell ([ -e $(OBJ_PROG) ] && echo $(OBJ_PROG)) || echo $(OBJ_PROG) | sed -e 's@/MASTER/spll_@/$(VER_USER)/spll_@') \
	  $(LIB_MNH) $(LIBS)
else
	$(F90) $(LDFLAGS) -o $(OBJDIR)/$@ $(OBJ_PROG) $(LIB_MNH) $(LIBS)
endif

ifeq "$(DO_COMP_USER)" "YES"
%-${ARCH_XYZ}-${VER_USER} :%
	ln -sf $(OBJDIR)/$(*F) $(SRC_SURFEX)/exe/$(*F)-${ARCH_XYZ}-${VER_USER}
#RJ #	ln -sf $(OBJDIR)/$(*F) $(SRC_SURFEX)/exe/$(*F)-${ARCH_XYZ}-${VER_USER}
#RJ 	ln -sf $(OBJDIR)/$(*F) $(SRC_SURFEX)/exe/$(*F)${XYZ}
#	-rm -f $(SRC_SURFEX)/exe/$(*F)_${ARCH_XYZ}~
endif

%-${ARCH_XYZ} :%
	ln -sf $(OBJDIR)/$(*F) $(SRC_SURFEX)/exe/$(*F)-${ARCH_XYZ}
#	-rm -f $(SRC_SURFEX)/exe/$(*F)_${ARCH_XYZ}~

endif
##########################################################
#                                                        #
# EXAMPLES RULES                                         #
#                                                        #
##########################################################

PWD_KTEST=$(SRC_SURFEX)/MY_RUN/KTEST
EXAMPLES=001_2Drelief 002_3Drelief 003_KW78 004_Reunion 007_16janvier
EXAMPLES_NCL=004_Reunion 007_16janvier


examples : $(EXAMPLES)

ncl1_examples :  $(patsubst %,%_ncl1,$(EXAMPLES_NCL))

ncl2_examples :  $(patsubst %,%_ncl2,$(EXAMPLES_NCL))

examplesclean :  $(patsubst %,%_clean,$(EXAMPLES))

$(EXAMPLES) :
	cd $(PWD_KTEST)/$@ ; run_$@

%_ncl1 :
	cd $(PWD_KTEST)/$(*F) ; run_$(*F) all_ncl_extractdia

%_ncl2 :
	cd $(PWD_KTEST)/$(*F) ; run_$(*F) all_ncl_lfi2cdf

%_clean :
	[ ! -d "$(PWD_KTEST)/$(*F)" ] || (cd $(PWD_KTEST)/$(*F) ; $(MAKE) clean)

EXAMPLES_CHIMIE=009_ICARTT 011_KW78CHEM

chimie : $(EXAMPLES_CHIMIE)

$(EXAMPLES_CHIMIE) :
	cd $(PWD_KTEST)/$@ ; run_$@
#
# rules to remove spll* source on MASTER directory
#
purgelist : $(patsubst %,purge_%,$(notdir $(PL)))
purge_%.f90 :
	cd $(OBJDIR_MASTER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepallmaster filemaster
purge_%.fx90 :
	cd $(OBJDIR_MASTER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepallmaster filemaster
purge_%.f :
	cd $(OBJDIR_MASTER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepallmaster filemaster

#
# rules to remove spll* source on USER directory
#
purgelistu : $(patsubst %,purgeu_%,$(notdir $(PL)))
purgeu_%.f90 :
	cd $(OBJDIR_USER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepalluse fileuser
purgeu_%.fx90 :
	cd $(OBJDIR_USER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepalluse fileuser
purgeu_%.f :
	cd $(OBJDIR_USER) ; grep -l "depfile:$(*F).D" *.f* | xargs rm -f ; rm -f $(*F).D ; rm -f filedepalluse fileuser

stat_f90 :
	cd $(OBJDIR_MASTER) ; find . -name "*.f90" | wc -l
stat_o :
	cd $(OBJDIR_MASTER) ; find . -name "*.o" | wc -l



cleanuser :
	test -d $(OBJDIR_USER) && rm -fr  $(OBJDIR_USER)

clean :  examplesclean cleanmaster $(addprefix clean_,$(REDISTS)) $(addprefix clean_,$(EXTCOUPLERS))
	rm -fr  $(OBJDIR_ROOT)
cleanprog :
	cd $(OBJDIR_MASTER) ; rm -f $(PROG_LIST)

print :
	@echo $(VAR)=$($(VAR))

ARFLAGS=r
#(%.o): %.o
#	$(AR) $(ARFLAGS)   $(OBJDIR)/$(@F) $(*F).o
#	$(RM) $(*F).o

#
#  GENERATION OF DEPENDANCE FILES : *.D
#
SPLR_FLAGS = --split --modi --deps
#RJ: implement part No.7 of Hirlam technical contribution, breaks on recursive dependencies
ifeq "$(USE_SPLR_WRAP)" "YES"
SPLR_FLAGS += --wrap
endif

ifeq "$(AVOID_CPP)" "YES"
%.D:%.F90
	splr.pl $(FUNDEFS) $(FPPFLAGS) --fpp $(SPLR_FLAGS) --odir=$(OBJDIR) $<
else
%.D:%.F90
	$(CPP) $(INC) $(FPPFLAGS) $< >  $(OBJDIR)/$(*F).f90
ifeq "$(USE_SPLR)" "YES"
	splr.pl $(SPLR_FLAGS) --odir=$(OBJDIR) $(OBJDIR)/$(*F).f90
else
	spll $(NSOURCE) $(OBJDIR)/$(*F).f90
endif
	-@rm -f $(OBJDIR)/$(*F).f90
endif

#RJ: no cpp for lower case ones, actually even dangerous a bit
%.D:%.f90
ifeq "$(USE_SPLR)" "YES"
	splr.pl $(SPLR_FLAGS) --odir=$(OBJDIR) $<
else
	cp $< $(OBJDIR)/$(*F).f90
	spll $(NSOURCE) $(OBJDIR)/$(*F).f90
	-@rm -f $(OBJDIR)/$(*F).f90
endif

#RJ: strange
#%.D:%.fx90
#	$(CPP)  $(INC) $(FPPFLAGS) $< > $(OBJDIR)/$(*F).fx90
#	spll $(NSOURCE) $(OBJDIR)/$(*F).fx90
#	-@rm -f $(OBJDIR)/$(*F).fx90

%.D:%.F
	$(CPP) $(INC) $(FPPFLAGS) $< >  $(OBJDIR)/$(*F).f
	spll $(NSOURCE) $(OBJDIR)/$(*F).f
	-@rm -f $(OBJDIR)/$(*F).f

%.D:%.f
	cp $< $(OBJDIR)/$(*F).f
	spll $(NSOURCE) $(OBJDIR)/$(*F).f
	-@rm -f $(OBJDIR)/$(*F).f

#
# GENERATION OF OBJET FILES : *.o
#

#RJ: add asm_lst capabilities for debugging
#RJ: rules for F90 sources
%.o:%.F90
ifeq "$(DO_ASM)" "YES"
	$(F90) $(MODOUT) $(INC) $(F90FLAGS) $(FPPFLAGS) -c -S $< -g0 -o $(OBJDIR)/$(*F).s
endif
	$(F90) $(MODOUT) $(INC) $(F90FLAGS) $(FPPFLAGS) -c $< -o $(OBJDIR)/$(*F).o
ifeq "x$(MODOUT_FLAG)" "x"
	-@mv ./*.mod  $(OBJDIR)/MOD/. || echo mods OK
endif

%.o:%.f90
ifeq "$(DO_ASM)" "YES"
	$(F90) $(MODOUT) $(INC) -c -S $(F90FLAGS) $< -g0 -o $(OBJDIR)/$(*F).s
endif
	$(F90) $(MODOUT) $(INC) -c $(F90FLAGS) $< -o $(OBJDIR)/$(*F).o
ifeq "x$(MODOUT_FLAG)" "x"
	-@mv ./*.mod  $(OBJDIR)/MOD/. || echo mods OK
endif

%.o:%.f
ifeq "$(DO_ASM)" "YES"
	$(F77) $(MODOUT) $(INC) -c -S $(F77FLAGS) $< -g0 -o $(OBJDIR)/$(*F).s
endif
	$(F77) $(MODOUT) $(INC) -c $(F77FLAGS)  $< -o $(OBJDIR)/$(*F).o
ifeq "x$(MODOUT_FLAG)" "x"
	-@mv ./*.mod  $(OBJDIR)/MOD/. || echo mods OK
endif

#RJ: unneeded
#%.o:%.fx90
#	@cp $< $(OBJDIR)/$(*F).f
#	$(FX90) $(MODOUT) $(INC) -c $(FX90FLAGS) $(OBJDIR)/$(*F).f
#ifeq "x$(MODOUT_FLAG)" "x"
#	-@mv ./*.mod  $(OBJDIR)/MOD/. || echo mods OK
#endif

%.o : %.c
	$(CC) $(INC) $(CFLAGS) $(CPPFLAGS) -c $< -o $(OBJDIR)/$(*F).o -o $(OBJDIR)/$(*F).o

ifeq "$(DO_COMP_MASTER)" "YES"
include  filedepallmaster
endif

ifeq "$(DO_COMP_USER)" "YES"
include  filedepalluser
endif
