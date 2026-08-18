# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking corel executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	
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

OBJS 	=              			\
		fndarc.o 		\
		gethed.o 		\
		getpar.o 		\
		getrec.o 		\
		gettri.o 		\
		initi.o 		\
		prepas.o 		\
		proces.o 		\
		puthed.o 		\
		putpar.o 		\
		putrec.o 		\
		puttri.o 		\
		redrec.o 		\
		redsig.o 		\
		redtri.o 

OBJM 	=       corel.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) 
#	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/COREL $(OBJM) $(OBJS) $(LIBS)

corel.o:	corel_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/COREL
