# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking pamb executable                *
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
	$(MK5_F95_OPTEST) -fstack-arrays -c -o $*.Opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               		\
		amb_cls.opt_o 		\
		amb_filter.opt_o 	\
		amb_update.opt_o 	\
		aob_update.opt_o 	\
		arf_filter.opt_o 	\
		clopa.opt_o 		\
		find_subnet.opt_o 	\
		get_arf.opt_o 		\
		make_scalis.opt_o 	\
		move_cals.opt_o 	\
		msc_check.opt_o 	\
		oscro.opt_o 		\
		pamb_elim.opt_o 	\
		pamb_freeze.opt_o 	\
		pamb_freeze_menu.opt_o 	\
		pamb_init.opt_o 	\
		pamb_init_menu.opt_o 	\
		pamb_mares.opt_o 	\
		pamb_menu.opt_o 	\
		pamb_plot_menu.opt_o 	\
		pamb_save.opt_o 	\
		pamb_showres.opt_o 	\
		pamb_sol.opt_o 		\
		paru_compile.opt_o 	\
		paru_do.opt_o 		\
		paru_elim.opt_o 	\
		paru_exec.opt_o 	\
		paru_gramb.opt_o 	\
		paru_main.opt_o 	\
		paru_menu.opt_o 	\
		pas_check.opt_o 	\
		scadam.opt_o 		\
		scadam_menu.opt_o 	\
		scadam_plot.opt_o 	\
		scatie.opt_o 		\
		selbas.opt_o 		\
		selsou.opt_o 		\
		selsta.opt_o 		\
		seltri.opt_o 		\
		show_arf.opt_o 		\
		sta_ambupd.opt_o 

OBJM = 					\
		pamb.o 			

OBJP = 					\
		paru_head.o 		

OBJE = 									\
     		$(SOLVE_ROOT)/progs/solve/elim/ambcha_permit.opt_o	\
		$(SOLVE_ROOT)/progs/solve/elim/chi_update.Opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/db_scan.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_do.opt_o  		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_lists.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/elim/elim_menu.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/elim/elim_save.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/elim/elim_sngchk.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/elim/equmem.opt_o		\
		$(SOLVE_ROOT)/progs/solve/elim/io_wgt.opt_o  		\
		$(SOLVE_ROOT)/progs/solve/elim/main_outobs.opt_o	\
		$(SOLVE_ROOT)/progs/solve/elim/manor.Opt_o  		\
		$(SOLVE_ROOT)/progs/solve/elim/mares_b3d.Opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/masol.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/mile_do.opt_o  		\
		$(SOLVE_ROOT)/progs/solve/elim/io_obser.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/proc_m.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/resid_st.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/refresh_wei.opt_o	\
		$(SOLVE_ROOT)/progs/solve/elim/sup_update.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/elim/upwei.opt_o  		\
		$(SOLVE_ROOT)/progs/solve/elim/upwei_menu.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/apply_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/apply_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/comp_iono_mod.o		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o   	\
		$(SOLVE_ROOT)/progs/solve/proc/map_param.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b1b3d.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b3d.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cnstr.o 			\
		$(SOLVE_ROOT)/progs/solve/proc/center_mass.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/seg_cnstr.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/add_atm.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/add_clk.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/add_eop.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/cov_nut.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/cov_reop.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/dbg_param.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_atm_9612.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_atm_o.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_blc.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_clk_9612.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_clk_o.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop_a1.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_gradr.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_nut.o  		\
		$(SOLVE_ROOT)/progs/solve/proc/do_reop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/fast_dbg.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/get_iono_avr_cov.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/gim_wri.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_bias_adj.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_read_info.o		\
		$(SOLVE_ROOT)/progs/solve/proc/load_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/parse_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/pre_vtd.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/post_vtd.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/read_trp.o		\
		$(SOLVE_ROOT)/progs/solve/proc/reparam.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_init.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_obs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_dbs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sngchk.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_get.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_init.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_read.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_put.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_quit.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_write.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_cns.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_dir_constraint.o	\
		$(SOLVE_ROOT)/progs/solve/norml/apply_cnstr.opt_o  	\
		$(SOLVE_ROOT)/progs/solve/norml/do_src.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_user.o      		\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/ios_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/norml/norml_b3d.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_pos.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_src.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_prp.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnt_pos.o		\
		$(SOLVE_ROOT)/progs/solve/norml/souadm_cns.o 		\
		$(SOLVE_ROOT)/progs/solve/setfl/change_datatype.o	\
		$(SOLVE_ROOT)/progs/solve/setfl/change_supmet.o		\
		$(SOLVE_ROOT)/progs/solve/setfl/set_sngchk.o		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_do.o 		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_menu.o 		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_ptoc.o  		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_solsetup.o 	\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_write.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/set_obstyp.o 

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

bin:	$(OBJS) $(OBJM) $(OBJP) $(OBJE)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/PAMB $(OBJM) $(OBJS) $(OBJE) $(LIBS)
	$(MK5_LINK) -o $(EXE_DIR)/paru $(OBJP) $(OBJS) $(OBJE) $(LIBS)

pamb.o:	pamb_version.i

clean:  
	rm -f $(OBJM) $(OBJP) $(OBJS) $(OBJE) $(EXE_DIR)/PAMB $(EXE_DIR)/paru
