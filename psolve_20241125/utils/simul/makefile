# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   simul executable             *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_NOOPT) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT)   -c -o $*.opt_o $*.f  


EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=				\
		simul_main.o

OBJS 	=				\
                cov_mat_nhe.o		\
		gvh_to_simul.o		\
		simul_conf_parse.o	\
		simul_cov_intrpl.o	\
		simul_ezd_nil.o		\
		simul_ezd_cov.o		\
		simul_do.o		\
		simul_rh.o		\
		simul_to_gvf.o		\
		simul_parse_covzen.o 	\
		vex_to_simul.o 	

LIBS =				  	\
        -L/opt64/lib -lvex_parser 	\
	$(SOLVE_LIB_GVH)		\
	$(SOLVE_LIB_VTD)		\
	$(SOLVE_FITSLIB_LIB) 		\
	$(SOLVE_CFITSIO_LIB) 		\
	$(SOLVE_LIB_PETOOLS) 		\
        $(SOLVE_LIB_BLAS)		\
        $(SOLVE_EXTRA_LIB)

all:	bin

$(OBJS):	$(PETOOLS_PREFIX)/include/vex.i	\
                $(SOLVE_ROOT)/include/simul.i

$(OBJM):	$(PETOOLS_PREFIX)/include/vex.i	\
                $(SOLVE_ROOT)/include/simul.i

bin:	$(OBJS) $(OBJM) 
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/simul $(OBJM) $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/simul
