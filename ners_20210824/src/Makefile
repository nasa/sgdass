# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking ners                            *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.optest_o	.noopt	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95)        -I$(PETOOLS_INC) -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT)    -I$(PETOOLS_INC) -c -o $*.opt_o $*.f

.f.optest_o:
	$(MK5_F95_OPTEST)    -I$(PETOOLS_INC) -c -o $*.optest_o $*.f

.f.noopt:
	$(MK5_F95_NOOPT)  -I$(PETOOLS_INC) -c -o $*.noopt $*.f


OBJS =                  	\
	ners.o				\
	ners_apply_heo.opt_o		\
	ners_azelha_comp.opt_o		\
	ners_comp_spl.opt_o		\
	ners_e3zt_dickman1993.opt_o 	\
	ners_e3zt_re2014.opt_o 	\
	ners_erm_angs.opt_o    		\
	ners_erm_mats.opt_o    		\
	ners_fetch.o 			\
	ners_get_eop.o 			\
	ners_get_evec.o 		\
	ners_get_heo.opt_o 		\
	ners_get_series.o 		\
	ners_get_spl.opt_o 		\
	ners_get_utcmtai.o 		\
	ners_heo_mhb2000.opt_o      	\
	ners_heo_ren2000.opt_o      	\
	ners_init.o 			\
	ners_inq.o 			\
        ners_leapsec.o 			\
	ners_load.o 			\
	ners_mod.o 			\
	ners_nut_geodesic.o 		\
	ners_quit.o 			\
	ners_read_earth_ephe.o		\
	ners_rotmat.opt_o		\
	ners_version.o 			\
	ners_wrapper_underscore.o  		

OBJ_NERS_EOP =  \
	ners_eop.o

OBJ_NERS_EOPSER =  \
	ners_eopser.o

OBJ_UTCMTAI =  \
	utcmtai.o

OBJ_UT1MTAI =  \
	ut1mtai.o

OBJ_NERSAZEL =  \
	ners_azelha.o

NERS_MOD   =  $(NERS_ROOT)/src/ners_mod.mod
NERS_LIB   =  $(NERS_ROOT)/lib/libners.a
NERS_SOLIB =  libners.so
NERS_DYLIB =  libners.1.dylib

LIBS =				\
	$(NERS_LIB) 		\
	$(PETOOLS_LIB) 		\
	$(SPC_LIB) 		\
	$(SOLVE_LIB_BLAS) 	\
	$(NERS_LAPACK) 		\
	$(LDFLAGS) 

all:	ners_lib ners_bin 

ners_lib:		 $(OBJS)
	ar r $(NERS_LIB) $(OBJS) 
        ifeq ($(NERS_OS),Linux)
		$(MK5_C_LINK) -shared -Wl,-soname,$(NERS_SOLIB)  \
                              -Wl,--version-script=libners_exp_linux.symbols \
                              -o $(NERS_ROOT)/lib/$(NERS_SOLIB).1.0  $(OBJS) \
                               $(PETOOLS_LIB) $(NERS_LAPACK)
		if ( -f  $(NERS_ROOT)/lib/$(NERS_SOLIB)   ) rm $(NERS_ROOT)/lib/$(NERS_SOLIB)
		cd $(NERS_ROOT)/lib; ln -sf $(NERS_SOLIB).1.0   $(NERS_SOLIB)
        endif
        ifeq ($(NERS_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(NERS_ROOT)/lib/$(NERS_DYLIB) \
                            -o $(NERS_ROOT)/lib/$(NERS_DYLIB) \
                            $(OBJS) $(LIBS)
		cd $(NERS_ROOT)/lib; if ( -f libners.dylib ) rm libners.dylib 
		cd $(NERS_ROOT)/lib; ln -s $(NERS_DYLIB)        libners.dylib 
#		strip -u -r -s libners_exp_darwin.symbols -c $(NERS_ROOT)/lib/$(NERS_DYLIB)
#                            -Wl,-exported_symbols_list libners_exp_darwin.symbols $(OBJS) $(LIBS)
        endif
	@echo '--- Library $(NERS_SOLIB) updated at  $(NERS_OS) ---'

ners_bin:	ners_eop ners_eopser utcmtai ut1mtai ners_azelha

ners_eop:   ners_lib $(OBJ_NERS_EOP)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_eop $(OBJ_NERS_EOP) $(LIBS);

ners_eopser:   ners_lib $(OBJ_NERS_EOPSER)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_eopser $(OBJ_NERS_EOPSER) $(LIBS);

utcmtai:    ners_lib $(OBJ_UTCMTAI)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/utcmtai $(OBJ_UTCMTAI) $(LIBS);

ut1mtai:    ners_lib $(OBJ_UT1MTAI)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ut1mtai $(OBJ_UT1MTAI) $(LIBS);

ners_azelha: ners_lib $(OBJ_NERSAZEL)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_azelha $(OBJ_NERSAZEL) $(LIBS);
    

$(OBJS): 		$(NERS_ROOT)/include/ners.i
$(OBJ_NERS_EOP): 	$(NERS_ROOT)/include/ners.i
$(OBJ_NERS_EOPSER): 	$(NERS_ROOT)/include/ners.i
$(OBJ_UTCMTAI): 	$(NERS_ROOT)/include/ners.i
$(OBJ_UT1MTAI): 	$(NERS_ROOT)/include/ners.i
$(OBJ_NERSAZEL): 	$(NERS_ROOT)/include/ners.i

clean:
	rm -f $(OBJS) $(NERS_LIB) $(OBJ_NERS_EOP) $(OBJ_NERS_EOPSER) $(OBJ_UTCMTAI) $(OBJ_UT1MTAI) $(OBJ_NERSAZEL) $(NERS_MOD) \
              $(NERS_ROOT)/bin/ners_eop \
              $(NERS_ROOT)/bin/ners_eopser \
              $(NERS_ROOT)/bin/utcmtai \
              $(NERS_ROOT)/bin/ut1mtai \
              $(NERS_ROOT)/bin/ners_azelha

        ifeq ($(NERS_OS),Linux)
		rm	$(NERS_ROOT)/lib/$(NERS_SOLIB)     \
                        $(NERS_ROOT)/lib/$(NERS_SOLIB).1.0; \
                        set status = 0
        endif
        ifeq ($(NERS_OS),Darwin)
		rm	$(NERS_ROOT)/lib/libners.1.dylib \
                        $(NERS_ROOT)/lib/libners.dylib ; set status = 0
        endif
