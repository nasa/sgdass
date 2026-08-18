# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking norml executable               *
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

OBJS 	=               			\
		add_cns.o			\
		add_constraint.o 		\
		add_dir_constraint.o 		\
		add_ind_b3d.o 			\
		add_indexed_constraint.o 	\
		add_pwc.o 			\
		add_velc.o 			\
		apply_cnstr.opt_o 		\
		create_usrg.o 			\
		cxepar2.o 			\
		do_dc.o 			\
		do_dir.o 			\
		do_nutat.o 			\
		do_pwc.o 			\
		do_ra.o 			\
		do_src.o 			\
		do_stat.o 			\
		do_statt.o 			\
		do_stavelc.o 			\
		do_suprs.o 			\
		do_supr_file.o 			\
		do_user.o 			\
		do_vel.o 			\
		do_velo.o 			\
		do_veloh.o 			\
		do_velt.o 			\
		eheo_cls_cnst.o 		\
		eheo_erm_cnst.o 		\
		erm_cnst.o 			\
		find_dc.o 			\
		find_dir.o 			\
		find_nut.o 			\
		find_ra.o 			\
		find_stat.o 			\
		find_statt.o 			\
		find_suprs.o 			\
		find_vel.o 			\
		find_velo.o 			\
		find_velt.o 			\
		get_model_covar.o 		\
		hpe_nnr_pos.o 			\
		hpe_nn_cnst.o 			\
		io_cnstr.o			\
		ios_cnstr.o			\
		mapcnst_b3d.o 			\
		nnr_pos.o 			\
		nnr_prp.o 			\
		nnr_src.o 			\
		nnr_vel.o 			\
		nnt_pos.o 			\
		nnt_vel.o 			\
		norml_b3d.o 			\
		norml_main.o 			\
		print_normal.o 			\
		solution_ident.o		\
		souadm_cns.o 			\
		splat.o 			\
		spesta_cnst.o 			\
		spe_cnst.o 			\
		stectx.o 			\
		update_spe.o 			\
		write_diacorr.o 		\
		xepar2.o 	

OBJM 	=               			\
		norml_head.o

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
	$(MK5_LINK) -o $(EXE_DIR)/NORML $(OBJM) $(OBJS) $(LIBS)

norml_head.o:	norml_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/NORML 
