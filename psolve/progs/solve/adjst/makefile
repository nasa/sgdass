# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking adjst executable               *
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
		a1jst.o 		\
		a1jst_init.o 		\
		a2jst.o 		\
		a2jst_segeop.o 		\
		a2jst_noeop.o 		\
		a2jst_plneop.o 		\
		a3jst.o 		\
		add_save.o 		\
		adjst_do.o 		\
		apriori_nut_applied.o 	\
		cndms.o 		\
		comp.o 			\
		compmin.o 		\
		covar2corel.o 		\
		create_covariance_matrix.o 		\
		eop_share.o 		\
		eopell.o 		\
		eheo_res.o 		\
		erm_res.o 		\
		errcmp.o 		\
		errcmp_pos.o 		\
		hpe_res.o 		\
		ind_rotcor.o 		\
		jmg_hfeop.o 		\
		map_eop_results.o 	\
		mdfil.o 		\
		mult_vmv.o 		\
		namnut.o 		\
		posmap1.o 		\
		posmap2.o 		\
		put9.o 			\
		return_adjustment.o 	\
		return_covariance.o 	\
		return_heo.o 		\
		return_hfeop.o 		\
		return_sigma.o 		\
		solve_apr_spool.o 	\
		spe_res.o 		\
		uen_vel.o 		\
		vtd_apr_spool.o 	\
		vtd_get_apr_prp.o 	\
		write_sinex.o 

OBJE =  							\
		$(SOLVE_ROOT)/progs/solve/proc/cov_reop.o		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o   	\
		$(SOLVE_ROOT)/progs/solve/cres/listing_options.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/norml/solution_ident.o	\
		$(SOLVE_ROOT)/progs/solve/basfe/bwork.o 		\
		$(SOLVE_ROOT)/progs/solve/basfe/cvmml.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/gtlhv.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/lvect.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/reada_basfe.o 		\
		$(SOLVE_ROOT)/progs/solve/basfe/spart.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/xrsort.o		

OBJM =		adjst_head.o 		\


LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_VTD)			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJE) $(OBJM)
	$(SUPPORT)/set_revision_date.csh
	$(MK5_LINK) -o $(EXE_DIR)/ADJST $(OBJM) $(OBJS) $(OBJE) $(LIBS)

adjst_head.o:	adjst_version.i

clean:  
	rm -f $(OBJM) $(OBJE) $(OBJS) $(EXE_DIR)/ADJST
