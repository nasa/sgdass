# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking solve_reset executable         *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c.	.o	.opt_o	.a	.d
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

OBJS 	=               \
	creaq.o 	\
	copy_corfil.o 	\
	copy_fdef.o
	
OBJM    = psolve_reset.o 

LIBS   = 					\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJM)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/psolve_reset $(OBJM) $(OBJS) $(LIBS)

solve_reset.o:	psolve_reset_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/psolve_reset 
