# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking getpar executable              *
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
		getpar.o 

OBJS 	=               		\
		getpar_main.o		\
		getpar_parse.o		\
		getpar_to_eob.o		\
		npv_to_bsp.o 		\
		npv_to_hps.o 		\
		parse_erm.o		\
		read_eob.o		\
		read_eops.o		\
		read_ls_tab.o		\
		read_sou.o		\
		read_sta.o		\
		read_vel.o		\
		write_eob.o		\
		write_eops.o	

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_LIB_GVH) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/getpar $(OBJM) $(OBJS) $(LIBS)

getpar.o:	getpar_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/getpar
