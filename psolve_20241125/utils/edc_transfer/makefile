# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  edc_transfer executable       *
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
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=               		\
	edc_transfer.o 

OBJP 	=               				\
	$(SOLVE_ROOT)/progs/solve/proc/edc_init.o 	\
	$(SOLVE_ROOT)/progs/solve/proc/edc_quit.o 	\
	$(SOLVE_ROOT)/progs/solve/proc/edc_read.o 	\
	$(SOLVE_ROOT)/progs/solve/proc/edc_update.o 	\
	$(SOLVE_ROOT)/progs/solve/proc/edc_write.o 	\

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) $(OBJP)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/edc_transfer $(OBJM) $(OBJS) $(OBJP) $(LIBS)

edc.i: 	edc_transfer.o

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/edc_transfer
