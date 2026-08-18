# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  vda_sup  executable           *
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
		vda_sup.o

OBJS 	=				\

LIBS =						\
        -L/opt64/lib                    	\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_GVH)			\
	$(SOLVE_LIB_VTD)			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) 
	$(MK5_LINK) -o $(EXE_DIR)/vda_sup $(OBJM) $(LIBS)

clean:  
	rm -f $(OBJM) $(EXE_DIR)/vda_sup
