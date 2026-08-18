# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking cres executable                *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C)       -c -o $*.o     $*.c  

.f.o:
	$(MK5_F95)     -c -o $*.o     $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  


EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               				\
		adql.o 					\
		compute_mechi.opt_o 			\
		cremu.o 				\
		cres_do.o 				\
		eo_plot_entry.o 			\
		first.o 				\
		get_added_error.o 			\
		listing_options.o 			\
		quadp.o 				\
		post_fit.opt_o 				\
		read_sparse.o 				\
		stats.o 				\
		secnd.o 				\
		third.o 				

OBJE 	=               				\
	 	$(SOLVE_ROOT)/progs/solve/proc/apply_tcn.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_init.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_quit.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_read.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_set.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_update.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_write.o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/get_iono_avr_cov.o \
		$(SOLVE_ROOT)/progs/solve/proc/load_trp.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/parse_tcn.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/post_vtd.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/pre_vtd.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/read_trp.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_get.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_init.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_read.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_put.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_quit.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_write.o  	\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o  	\
		$(SOLVE_ROOT)/progs/solve/norml/solution_ident.o  \
		$(SOLVE_ROOT)/progs/solve/getdb/set_obstyp.o 

OBJM 	=       cres_head.o

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

bin:	$(OBJM) $(OBJS) $(OBJE)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/CRES  $(OBJM) $(OBJS) $(OBJE) $(LIBS)

cres_head.o:	cres_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/CRES
