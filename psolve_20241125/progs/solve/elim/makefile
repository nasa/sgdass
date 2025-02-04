# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking elim executable                *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o 	.Opt_o 	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.Opt_o:
	$(MK5_F95_OPTEST) -c -o $*.Opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               		\
		ambcha_permit.opt_o 	\
		chi_update.Opt_o 	\
		db_scan.opt_o 		\
		elim_do.opt_o 		\
		elim_lists.opt_o 	\
		elim_menu.opt_o 	\
		elim_param.opt_o 	\
		elim_save.opt_o 	\
		elim_sngchk.opt_o 	\
		equmem.opt_o 		\
		io_obser.opt_o 		\
		io_wgt.opt_o 		\
		main_outobs.opt_o 	\
		manor.Opt_o 		\
		mares_b3d.Opt_o 	\
		masol.opt_o 		\
		mile_do.opt_o 		\
		proc_m.opt_o 		\
		refresh_wei.opt_o 	\
		resid_st.opt_o 		\
		sup_update.opt_o 	\
		upwei.opt_o 		\
		upwei_menu.opt_o 

OBJM 	=               		\
		elim.opt_o 

OBJE 	=               						\
	 	$(SOLVE_ROOT)/progs/solve/proc/apply_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/apply_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o   	\
		$(SOLVE_ROOT)/progs/solve/proc/comp_iono_mod.o		\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b1b3d.o		\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b3d.o     		\
		$(SOLVE_ROOT)/progs/solve/proc/map_param.o		\
		$(SOLVE_ROOT)/progs/solve/proc/cnstr.o			\
		$(SOLVE_ROOT)/progs/solve/proc/center_mass.o		\
	 	$(SOLVE_ROOT)/progs/solve/proc/seg_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/proc/add_atm.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/add_clk.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/add_eop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cov_nut.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cov_reop.o 		\
                $(SOLVE_ROOT)/progs/solve/proc/dbg_param.o		\
		$(SOLVE_ROOT)/progs/solve/proc/do_atm_9612.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_atm_o.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_blc.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_clk_9612.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_clk_o.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop_a1.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_gradr.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_nut.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_reop.o 		\
                $(SOLVE_ROOT)/progs/solve/proc/fast_dbg.o		\
	 	$(SOLVE_ROOT)/progs/solve/proc/get_iono_avr_cov.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/gim_wri.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_bias_adj.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_read_info.o		\
		$(SOLVE_ROOT)/progs/solve/proc/load_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/read_trp.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_init.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_obs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_dbs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sngchk.o			\
	 	$(SOLVE_ROOT)/progs/solve/proc/parse_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/pre_vtd.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/post_vtd.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/reparam.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_get.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_init.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_read.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_put.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_quit.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_write.o              \
		$(SOLVE_ROOT)/progs/solve/norml/add_cns.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_dir_constraint.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/apply_cnstr.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/norml/do_src.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_user.o      		\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/ios_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/norml/norml_b3d.o		\
                $(SOLVE_ROOT)/progs/solve/norml/nnr_pos.o		\
                $(SOLVE_ROOT)/progs/solve/norml/nnr_src.o		\
                $(SOLVE_ROOT)/progs/solve/norml/nnr_prp.o		\
                $(SOLVE_ROOT)/progs/solve/norml/nnt_pos.o		\
                $(SOLVE_ROOT)/progs/solve/norml/souadm_cns.o            \
		$(SOLVE_ROOT)/progs/solve/setfl/change_supmet.o 	\
		$(SOLVE_ROOT)/progs/solve/setfl/set_sngchk.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/set_obstyp.o 

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

bin:	$(OBJS) $(OBJM) $(OBJE)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/ELIM $(OBJM) $(OBJS) $(OBJE) $(LIBS)

elim.o:	elim_version.i

clean:  
	rm -f $(OBJS) $(OBJE) $(OBJM) $(EXE_DIR)/ELIM
