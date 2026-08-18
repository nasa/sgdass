# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   sima  executable             *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               	\
        err_dstr_par.o          \
        get_mod_res_rms.o       \
        gnss_iono_rms.o         \
        load_iono_solve.o       \
        comp_iono_err_regr.o 	\
	wri_add_iono_wei.o


OBJ1 	=               				\
	    $(SOLVE_ROOT)/utils/iono/dtec_import.o

OBJ2 	=               				\
	    $(SOLVE_ROOT)/utils/iono/est_iono_mod.o

OBJ3 	=               				\
	    $(SOLVE_ROOT)/utils/iono/gen_iono_err_regr.o

OBJ4 	=               				\
	    $(SOLVE_ROOT)/utils/iono/gtec_iono_err.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
        $(SOLVE_ROOT)/libs/newlib/newlib.a      \
        $(SOLVE_ROOT)/libs/fclib/fclib.a        \
	$(SOLVE_LIB_GVH) 			\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
	$(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	dtec_import est_iono_mod gen_iono_err_regr gtec_iono_err

dtec_import:	$(OBJ1) 
		$(SUPPORT)/set_revision_date.csh;
		$(MK5_LINK) -o $(EXE_DIR)/dtec_import $(OBJ1) $(LIBS)

est_iono_mod:	$(OBJ2) $(OBJS)
		$(SUPPORT)/set_revision_date.csh;
		$(MK5_LINK) -o $(EXE_DIR)/est_iono_mod $(OBJ2) $(OBJS) $(LIBS)

gen_iono_err_regr: $(OBJ3) $(OBJS)
		$(SUPPORT)/set_revision_date.csh;
		$(MK5_LINK) -o $(EXE_DIR)/gen_iono_err_regr $(OBJ3) $(OBJS) $(LIBS)

gtec_iono_err:  $(OBJ4) $(OBJS)
		$(SUPPORT)/set_revision_date.csh;
		$(MK5_LINK) -o $(EXE_DIR)/gtec_iono_err  $(OBJ4) $(OBJS) $(LIBS)

est_iono_mod.o:     	    iono_from_solve.i
gen_iono_err_regr.o: 	    iono_from_solve.i

clean:  
	rm -f $(OBJS) 
	rm -f $(OBJ1) 
	rm -f $(OBJ2) 
	rm -f $(OBJ3) 
	rm -f $(OBJ4) 
	rm -f $(EXE_DIR)/dtec_import
	rm -f $(EXE_DIR)/est_iono_mod
	rm -f $(EXE_DIR)/gen_iono_err_regr
	rm -f $(EXE_DIR)/gtec_iono_err
