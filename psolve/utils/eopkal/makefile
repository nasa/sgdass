# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  eopkal  executable            *
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

OBJM 	=               			\
		eopkal_head.o 

OBJS 	=               			\
		add_new_estimate.o 		\
		backward_filter.o 		\
		clean_ldum.o 			\
		de_ut1s.o 			\
		extrapolate.o 			\
		forward_filter.o 		\
		get_leapsec_jmg.o 		\
		get_run_param.o 		\
		init_filter.o 			\
		init_pm.o 			\
		init_ut.o 			\
		interpolate_concrete.o 		\
		jmg_alg.o 			\
		kalman_smoother.o 		\
		make_lod_cnt.o 			\
		make_noise_covar.o 		\
		make_noise_covar_pm.o 		\
		make_noise_covar_ut.o 		\
		make_phi.o 			\
		make_phi_pm.o 			\
		make_phi_ut.o 			\
		make_proj.o 			\
		make_seasonal.o 		\
		make_seasonal_noise_covar.o 	\
		make_xref.o 			\
		output_ldum.o 			\
		propagate.o 			\
		read_con.o 			\
		read_eopb.o 			\
		read_eopjmg.o 			\
		read_iers.o 			\
		summarize_run_parm_info.o 	\
		write_out_mod_line.o 		\
		write_out_using_concrete.o 	


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
	$(MK5_LINK) -o $(EXE_DIR)/eopkal  $(OBJM) $(OBJS) $(LIBS)

eopkal_head.o:	eopkal_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/eopkal
