# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  get_apriori_eop  executable   *
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
		get_apriori_eop.o       \

OBJS 	=               		\
		conf_gae.o 		\
		diff_erp.o 		\
		get_ftp_eop.o 		\
		rd_erp.o 		\
		rd_finals.o 		\
		rd_iers_c04.o 		\
		wri_erp.o 		\
		wri_upm.o 		



LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
	$(SOLVE_LIB_CURSES)			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/get_apriori_eop  $(OBJM) $(OBJS) $(OBJP) $(LIBS)

get_apriori_eop.o: 	get_apriori_eop_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(OBJP) $(EXE_DIR)/get_apriori_eop  
