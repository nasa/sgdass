# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking proc executable                *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o 	.Opt_o 	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)        -c -o $*.o     $*.f  

.f.opt_o:
	$(MK5_F95_OPT)    -c -o $*.opt_o $*.f  

.f.Opt_o:
	$(MK5_F95_OPTEST) -c -o $*.Opt_o $*.f  


EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               	\
	add_atm.o 		\
	add_clk.o 		\
	add_eop.o 		\
	apply_tcn.o 		\
	apply_trp.o 		\
	center_mass.o 		\
	cnstr.o 		\
        comp_iono_mod.o		\
        comp_iono_err_regr.o	\
	cov_nut.o 		\
	cov_reop.o 		\
	cxepar_opt20.opt_o   	\
	dbg_param.o 		\
	do_atm.o 		\
	do_atm_9612.o 		\
	do_atm_o.o 		\
	do_blc.o 		\
	do_clk.o 		\
	do_clk_9612.o 		\
	do_clk_o.o 		\
	do_eop.o 		\
	do_eop_a1.o 		\
	do_gradr.o 		\
	do_nut.o 		\
	do_reop.o 		\
	equobs.o 		\
	edc_init.o 		\
        edc_quit.o 		\
	edc_read.o 		\
	edc_set.o 		\
	edc_update.o 		\
	edc_write.o 		\
	fast_dbg.o 		\
	get_iono_avr_cov.o 	\
	get_iono_mod_res_rms.o  \
	gim_wri.o		\
	gnss_iono_rms_sta.o 	\
	iono_bias_adj.o		\
        iono_dtec_db_update.o 	\
        iono_err_dstr_par.o 	\
	iono_read_info.o	\
	iono_regr_mod.o		\
	iono_wri.o		\
	load_trp.o 		\
	loop.o 			\
	manor_b1b3d.opt_o 	\
	manor_b3d.opt_o 	\
	map_param.o 		\
	nrmeq.o 		\
	nrmst.o 		\
	parse_tcn.o 		\
	post_vtd.o 		\
	pre_vtd.o 		\
	prelp.o 		\
	proc_do.o 		\
	pstlp.o 		\
	read_trp.o 		\
	reparam.o 		\
	seg_cnstr.o 		\
	sestat_dbs.o 		\
	sestat_init.o 		\
	sestat_obs.o 		\
	sngchk.o 		\
	tpd_get.o 		\
	tpd_init.o 		\
	tpd_read.o 		\
	tpd_put.o 		\
	tpd_write.o 		\
	tpd_quit.o 		\
	test_staobs.o 		\
	write_sparse.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/add_dir_constraint.o  	\
	$(SOLVE_ROOT)/progs/solve/norml/add_cns.o  		\
	$(SOLVE_ROOT)/progs/solve/norml/apply_cnstr.opt_o	\
	$(SOLVE_ROOT)/progs/solve/norml/do_src.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/do_user.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/ios_cnstr.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/nnr_src.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/nnr_pos.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/nnt_pos.o 		\
	$(SOLVE_ROOT)/progs/solve/norml/souadm_cns.o 		\
	$(SOLVE_ROOT)/progs/solve/getdb/set_obstyp.o 


OBJM 	=               \
	proc_head.o 

OBJ2 	=               	\
	tb3d.o 	

OBJ3 	=               	\
	tb1b3d.o 

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_GVH)			\
        $(SOLVE_LIB_VTD)			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) $(OBJ2) $(OBJ3)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/PROC   $(OBJM) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(EXE_DIR)/tb3d   $(OBJ2) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(EXE_DIR)/tb1b3d $(OBJ3) $(OBJS) $(LIBS)

proc_head.o:	proc_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(OBJ2) $(OBJ3) \
              $(EXE_DIR)/PROC $(EXE_DIR)/tb3d $(EXE_DIR)/tb1b3d
