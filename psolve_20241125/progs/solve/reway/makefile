# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking reway executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c.	.o	.opt_o	.a	.d
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

OBJS 	=              			\
		adjust_weights.o 	\
		calc_bl_cov.o 		\
		display_wts.o 		\
		enter_chi_tol.o 	\
		enter_max_iter.o 	\
		enter_noise_ceiling.o 	\
		enter_noise_floor.o 	\
		enter_wts.o 		\
		get_bl_wts.o 		\
		get_resid.o 		\
		invert_tri.o 		\
		iterate.o 		\
		menu.o 			\
		new_weights.o 		\
		put_bl_wts.o 		\
		rate_flags.o 		\
		reorder_bl_wts.o 	\
		restore_flags.o 	\
		reway_help.o 		\
		rewaypar_save.o 	\
		rewaypar_set.o 		\
		update_resid_sigma.o 	\
		way2go.o 

OBJM 	=              			\
		reway.o 		\

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
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/REWAY $(OBJM) $(OBJS) $(LIBS)

reway.o:	reway_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/REWAY
