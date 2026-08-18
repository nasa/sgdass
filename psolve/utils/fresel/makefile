# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking heo_cov executable             *
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
	$(MK5_F95_OPT) $(SOLVE_FFTW_INC) -c -o $*.opt_o $*.f  


EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS =                  		\
        adh_fresel_heo.opt_o 		\
        get_heo.opt_o  			\
	nut_fresel_heo.o 		\
	fresel_har.o 			\
	fresel_drf.o 			\
	fresel_tid.o 			\
	tid_fresel_heo.o 		\
	wri_fresel_heo.o 

OBJ1 =					\
	fresel_heo.o  

OBJ2 =					\
	gen_adhoc_heo.o  

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
	-L$(FOURPACK_PREFIX)/lib -lfourpack     \
	$(SOLVE_LIB_PETOOLS) 			\
	$(SOLVE_FFTW_LIB)                       \
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ1) $(OBJ2)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/fresel_heo    $(OBJ1) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(EXE_DIR)/gen_adhoc_heo $(OBJ2) $(OBJS) $(LIBS)

fresel.i: fresel_heo.o

clean:  
	rm $(OBJS) ; set status = 0
	rm $(OBJ1) ; set status = 0
	rm $(OBJ2) ; set status = 0
	rm -f $(EXE_DIR)/fresel_heo    ; set status = 0
	rm -f $(EXE_DIR)/gen_adhoc_heo ; set status = 0
	
