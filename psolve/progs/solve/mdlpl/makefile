# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking mdlpl executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	
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

OBJS 	=               		\
		first.o 		\
		getsta_pts.o 		\
		hard_reset.o 		\
		ind_par.o 		\
		jd2tag.o 		\
		jmg_hfeop.o 		\
		kchek2.o 		\
		make_psfplot.o 		\
		mdlpl_diagi.o 		\
		mdlpl_ext.o 		\
		mdlpl_hlp.o 		\
		mdlpl_plus.o 		\
		menu.o 			\
		psf_plot.o 		\
		quadp.o 		\
		reset_screen.o 		\
		screen_pause.o 		\
		secnd.o 		\
		third.o 		\
		tol.o 			\
		unix_file_sort.o 

OBJM 	=       mdlpl.o

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
#	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/MDLPL $(OBJM) $(OBJS) $(LIBS)

mdlpl.o:	mdlpl_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/MDLPL
