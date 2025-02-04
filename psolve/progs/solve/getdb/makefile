# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking getdb executable               *
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

OBJS 	=              					\
	comp_theo.o 					\
	constraint_default.o  				\
	getdb_do.o 					\
	getdb_select.o 					\
	getdb_fill_namfil.o 				\
	getdb_fill_oborg.o 				\
	getdb_fill_parfil.o 				\
	getdb_fill_socom.o 				\
	getdb_meteo_correct.o 				\
	getdb_param.o 					\
	ionfr.o  					\
	set_obstyp.o 					\
	srset.o  					\
	$(SOLVE_ROOT)/progs/solve/setfl/ofst_sta.o 

OBJM 	= getdb.o 

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_CURSES)			\
        $(SOLVE_LIB_GVH)			\
        $(SOLVE_LIB_VTD)			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/GETDB $(OBJM) $(OBJS) $(LIBS)

getdb.o:	getdb_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/GETDB $(EXE_DIR)/gvf_to_super 
