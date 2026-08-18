# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking spl_pos executable             *
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
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=               		\
		spl_pos.o 

OBJS 	=               		\
		displ_bspl.o 		\
                lintrend_bspl.o 	\
		parse_spe_listing.o 	\
		plot_spl.o 		\
		spl_to_uen.o 		\
		wri_bsppos.o 

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
	$(MK5_LINK) -o $(EXE_DIR)/spl_pos $(OBJM) $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/spl_pos
