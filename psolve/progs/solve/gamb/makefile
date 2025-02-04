# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking gamb executable                *
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

OBJS 	=               		\
		check_res.opt_o 	\
		gamb_get.opt_o 		\
		gamb_lirec.opt_o 	\
		gamb_menu.opt_o 	\
		gamb_put.opt_o 		\
		iono_amb.opt_o 		\
		iono_refine.opt_o 	\
		kbast.opt_o 		\
		namfile_update.opt_o 	\
		prepes.opt_o 		\
		prepes_mb.opt_o 	\
		prepes_sb.opt_o 	

OBJM	=               		\
	        gamb.opt_o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VTD)			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/GAMB $(OBJM) $(OBJS) $(LIBS)

gamb.o:	gamb_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/GAMB
