# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking arcpe executable               *
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
		abmove_b1d.o 		\
		arcfile_name.o 		\
		arcpe_b1b3d.o 		\
		arcpe_b1d.o 		\
		arcpe_main.o 		\
		b1d_forward.o 		\
		elimin.o 		\
		fix_eop.o 		\
		global.o 		\
		lists.o 		\
		lists_arc.opt_o 	\
		outmt.o 		\
		reorder.o 		\
		xdder_fast.o            \
		$(SOLVE_ROOT)/progs/solve/proc/reparam.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o	\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_cns.o 

OBJM 	=              	\
		arcpe.o 

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

bin:	$(OBJS) $(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/ARCPE $(OBJM) $(OBJS) $(LIBS)

arcpe.o:	arcpe_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/ARCPE
