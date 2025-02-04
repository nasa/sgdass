# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking back  executable               *
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
		amatx.o 		\
		arc_arc.o 		\
		arc_i.o 		\
		arc_j.o 		\
		b1d_cov.o 		\
		back_b1b3d.o 		\
		back_b1d.o 		\
		back_do.o 		\
		back_main.o 		\
		covmm.o 		\
		cvrnc.o 		\
		in2ch.o 		\
		indx_parm.o 		\
		local.o 		\
		manip.o 		\
		mxcmb.o 		\
		out_cgm.o 		\
		out_lst.o 		\
		out_mtx.o 		\
		out_one.o 		\
		outfl_back.o 		\
		parminfoe.o 		\
		pmcmb.o 		\
		prnt_vect.o 		\
		reorder_x.o 		\
		setst.o 		\
		where_arc.o 		\
		write_glcorr.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/reparam.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_cns.o  		\
		$(SOLVE_ROOT)/progs/solve/norml/write_diacorr.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/solution_ident.o

OBJM =		back_head.o

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

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/BACK $(OBJM) $(OBJS) $(LIBS)

back.o:	back_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/BACK
