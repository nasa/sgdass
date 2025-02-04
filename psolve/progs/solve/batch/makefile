# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking batch executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o 	.Opt_o 	.a	.noopt_o	
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C)       -c -o $*.o     $*.c  

.f.o:
	$(MK5_F95)     $(MK5_C_OPENMP) -c -o $*.o     $*.f  

.f.opt_o:
	$(MK5_F95_OPT) $(MK5_C_OPENMP) -c -o $*.opt_o $*.f  

.f.noopt_o:
	$(MK5_F95_NOOPT) $(MK5_C_OPENMP) -g -c -o $*.noopt_o $*.f


.f.Opt_o:
	$(MK5_F95_OPTEST) $(MK5_C_OPENMP) -fstack-arrays -c -o $*.Opt_o $*.f  



EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=              			\
		addstr.o 		\
		apnd_2_spool.o 		\
		apply_weights.o 	\
		arcset.o 		\
		atmexc.o 		\
		autat.o 		\
		autcl_mult_ref.o 	\
		batch_cleanup.o 	\
		batch.o 	        \
		calsfnd.o 		\
		cfeof.o 		\
		cfopen.o 		\
		cfpos.o 		\
		cfread.o 		\
		cfreadall.o 		\
		cfspos.o 		\
		cfunrd.o 		\
		chkcmp.o 		\
		chsup.o 		\
		chwght.o 		\
		ch_db_gvf.o 		\
		correlations_parser.o 	\
		ctrlfl.o 		\
		ctrls.o 		\
		dbg_beg.o 		\
		dbg_end.o 		\
		ddata.o 		\
		delev.o 		\
		desta.o 		\
		discont_init.o 		\
		do_atmexc.o 		\
		do_gradexc.o 		\
		docali.o 		\
		docont_mcal.o 		\
		doion.o 		\
		domapp.o 		\
		doobcont_mcal.o 	\
		dositecals.o 		\
		douse.o 		\
		eof_cvrf.o 		\
		eof_spllk.o 		\
		eof_spool.o 		\
		etimer.o 		\
		expand_cgm_eerm.o 	\
		find_weights.o 		\
		fndcls.o 		\
		fndlst.o 		\
		garc.o 			\
		gaxis.o 		\
		gbase.o 		\
		gcalib.o 		\
		gcalnm.o 		\
		gcarry.o 		\
		gconst.o 		\
		gcont.o 		\
		gdata.o 		\
		gdcosp.o 		\
		germ.o 			\
		gesm.o 			\
		get_superfile_name.o 	\
		get_weights.o 		\
		getavl.o 		\
		getgrp.o 		\
		getitm.o 		\
		getuse.o 		\
		gexp.o 			\
		gflags.o 		\
		glbset.o 		\
		glo_back.opt_o 		\
		glo_elim.opt_o 		\
		glo_forw.opt_o 		\
		glo_global.o 		\
		glo_init.o 		\
		glo_mem_fault.o 	\
		glo_norm.o 		\
		glo_readcgm.o 		\
		glo_reorder.o 		\
		glo_restore.o 		\
		glo_save.o 		\
		glo_sort.o 		\
		glsta.o 		\
		gmap.o 			\
		gmapf.o 		\
		gmcal.o 		\
		gnut.o 			\
		gnuvcov.o 		\
		gorisp.o 		\
		goutpt.o 		\
		gpart.o 		\
		gposvar.o 		\
		gpropsp.o 		\
		gpwc.o 			\
		gradexc.o 		\
		graosp.o 		\
		gsetup.o 		\
		gsrcsp.o 		\
		gstasp.o 		\
		gstatie.o 		\
		gstavelcnst.o 		\
		gsuprs.o 		\
		gtcalst.o 		\
		gtcvr.o 		\
		gtelev.o 		\
		gtlst.o 		\
		gtlst_st_ca.o 		\
		gtwvm.o 		\
		gut1.o 			\
		gvelsp.o 		\
		gveltie.o 		\
		gvlosp.o 		\
		gynexl.o 		\
		indp.o 			\
		lcsta.o 		\
		lists_update.o 		\
		newfrm.o 		\
		newsec.o 		\
		ofsts_mult_ref.o 	\
		parse_arcstr.o 		\
		parse_eldep_file.o 	\
		parse_erm_cnt.o 	\
		parse_bsp.o 		\
		parse_eheo.o 		\
		parse_hpe.o 		\
		parse_souadm.o 		\
		parse_spe.o 		\
		prces.o 		\
		prgres.o 		\
		put_weights.o 		\
		read_weights.o 		\
		read_bsp.o 		\
		real8_decode.o 		\
		rfsrc.o 		\
		rfsta.o 		\
		rstors.o 		\
		rtimer.o 		\
		sarst.o 		\
		save_weights.o 		\
		saves.o 		\
		set_calib.o 		\
		set_mask.o 		\
		set_merg.o 		\
		setcl_mult_ref.o 	\
		setfrm.o 		\
		setnam.o 		\
		setsec.o 		\
		sflags.o 		\
		shfeop.o 		\
		sinex_parser.o 		\
		sngchk_parser.o 	\
		sort_parser.o 		\
		sort_weights.o 		\
		split_and_casefold.o 	\
		split_string.o 		\
		stimer.o 		\
		train_parser.o 		\
		trp_init.o 		\
		user_restore.o 		\
		ut1pm.o

OBJM =		        		\
		batch_main.o 	

OPTIN_OBJECTS =		        					\
		$(SOLVE_ROOT)/progs/solve/optin/gamb_default.o		\
		$(SOLVE_ROOT)/progs/solve/optin/reway_default.o		\
	     	$(SOLVE_ROOT)/progs/solve/optin/solve_default.o

PROC_OBJECTS =		        					\
		$(SOLVE_ROOT)/progs/solve/proc/add_atm.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/add_clk.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/add_eop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/apply_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/apply_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/center_mass.o		\
		$(SOLVE_ROOT)/progs/solve/proc/cnstr.o         		\
		$(SOLVE_ROOT)/progs/solve/proc/comp_iono_mod.o		\
		$(SOLVE_ROOT)/progs/solve/proc/comp_iono_err_regr.o	\
		$(SOLVE_ROOT)/progs/solve/proc/cov_nut.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cov_reop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/cxepar_opt20.opt_o 	\
	 	$(SOLVE_ROOT)/progs/solve/proc/do_atm_9612.o		\
		$(SOLVE_ROOT)/progs/solve/proc/do_atm_o.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_blc.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_clk_o.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/do_clk_9612.o		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_eop_a1.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_gradr.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_nut.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/do_reop.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/dbg_param.o		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_init.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_quit.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_read.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_set.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_update.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/edc_write.o 		\
	 	$(SOLVE_ROOT)/progs/solve/proc/equobs.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/fast_dbg.o      		\
		$(SOLVE_ROOT)/progs/solve/proc/get_iono_avr_cov.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/get_iono_mod_res_rms.o   \
		$(SOLVE_ROOT)/progs/solve/proc/gnss_iono_rms_sta.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/gim_wri.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_bias_adj.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_dtec_db_update.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/iono_err_dstr_par.o 	\
		$(SOLVE_ROOT)/progs/solve/proc/iono_regr_mod.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_read_info.o		\
		$(SOLVE_ROOT)/progs/solve/proc/iono_wri.o		\
		$(SOLVE_ROOT)/progs/solve/proc/load_trp.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/loop.o 	        	\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b1b3d.o		\
		$(SOLVE_ROOT)/progs/solve/proc/manor_b3d.o     		\
		$(SOLVE_ROOT)/progs/solve/proc/map_param.o		\
		$(SOLVE_ROOT)/progs/solve/proc/nrmeq.o         		\
		$(SOLVE_ROOT)/progs/solve/proc/nrmst.o         		\
		$(SOLVE_ROOT)/progs/solve/proc/post_vtd.o 		\
	        $(SOLVE_ROOT)/progs/solve/proc/pre_vtd.o         	\
		$(SOLVE_ROOT)/progs/solve/proc/prelp.o         		\
		$(SOLVE_ROOT)/progs/solve/proc/proc_do.o		\
		$(SOLVE_ROOT)/progs/solve/proc/pstlp.o        	 	\
		$(SOLVE_ROOT)/progs/solve/proc/read_trp.o		\
		$(SOLVE_ROOT)/progs/solve/proc/reparam.o		\
		$(SOLVE_ROOT)/progs/solve/proc/seg_cnstr.o	     	\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_init.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_obs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sestat_dbs.o		\
		$(SOLVE_ROOT)/progs/solve/proc/sngchk.o			\
		$(SOLVE_ROOT)/progs/solve/proc/parse_tcn.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_get.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_init.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_read.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_put.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_quit.o 		\
		$(SOLVE_ROOT)/progs/solve/proc/tpd_write.o 

NORML_OBJECTS =		        					\
		$(SOLVE_ROOT)/progs/solve/norml/add_cns.o		\
		$(SOLVE_ROOT)/progs/solve/norml/add_constraint.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/add_dir_constraint.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/add_pwc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/add_velc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/apply_cnstr.opt_o	\
		$(SOLVE_ROOT)/progs/solve/norml/create_usrg.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/cxepar2.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_dc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_dir.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_pwc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_ra.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_src.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_stat.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_statt.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_stavelc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_suprs.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_supr_file.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_user.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_vel.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_velo.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_veloh.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/do_velt.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/erm_cnst.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/eheo_cls_cnst.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/eheo_erm_cnst.o 	\
		$(SOLVE_ROOT)/progs/solve/norml/find_dc.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_dir.o		\
		$(SOLVE_ROOT)/progs/solve/norml/find_nut.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_ra.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_stat.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_statt.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_suprs.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_vel.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_velo.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/find_velt.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/hpe_nn_cnst.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/io_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/norml/ios_cnstr.o		\
		$(SOLVE_ROOT)/progs/solve/norml/mapcnst_b3d.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_pos.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_prp.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_src.o		\
		$(SOLVE_ROOT)/progs/solve/norml/nnr_vel.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/nnt_pos.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/nnt_vel.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/norml_b3d.o		\
		$(SOLVE_ROOT)/progs/solve/norml/norml_main.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/print_normal.o         	\
		$(SOLVE_ROOT)/progs/solve/norml/solution_ident.o	\
		$(SOLVE_ROOT)/progs/solve/norml/souadm_cns.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/splat.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/spesta_cnst.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/spe_cnst.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/stectx.o		\
		$(SOLVE_ROOT)/progs/solve/norml/update_spe.o 		\
		$(SOLVE_ROOT)/progs/solve/norml/write_diacorr.o

ARCPE_OBJECTS =		        					\
		$(SOLVE_ROOT)/progs/solve/arcpe/arcfile_name.o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/arcpe_b1b3d.o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/elimin.o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/global.o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/fix_eop.o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/lists_arc.opt_o		\
		$(SOLVE_ROOT)/progs/solve/arcpe/outmt.o

BACK_OBJECTS =		        					\
		$(SOLVE_ROOT)/progs/solve/back/amatx.o 			\
		$(SOLVE_ROOT)/progs/solve/back/arc_arc.o 		\
		$(SOLVE_ROOT)/progs/solve/back/arc_i.o 			\
		$(SOLVE_ROOT)/progs/solve/back/arc_j.o 			\
		$(SOLVE_ROOT)/progs/solve/back/back_b1d.o		\
		$(SOLVE_ROOT)/progs/solve/back/back_b1b3d.o    		\
		$(SOLVE_ROOT)/progs/solve/back/back_do.o		\
		$(SOLVE_ROOT)/progs/solve/back/back_main.o		\
		$(SOLVE_ROOT)/progs/solve/back/b1d_cov.o		\
		$(SOLVE_ROOT)/progs/solve/back/covmm.o 			\
		$(SOLVE_ROOT)/progs/solve/back/cvrnc.o 			\
		$(SOLVE_ROOT)/progs/solve/back/in2ch.o 			\
		$(SOLVE_ROOT)/progs/solve/back/indx_parm.o 		\
		$(SOLVE_ROOT)/progs/solve/back/local.o 			\
		$(SOLVE_ROOT)/progs/solve/back/manip.o 			\
		$(SOLVE_ROOT)/progs/solve/back/mxcmb.o 			\
		$(SOLVE_ROOT)/progs/solve/back/out_cgm.o 		\
		$(SOLVE_ROOT)/progs/solve/back/out_lst.o 		\
		$(SOLVE_ROOT)/progs/solve/back/out_mtx.o 		\
		$(SOLVE_ROOT)/progs/solve/back/out_one.o 		\
		$(SOLVE_ROOT)/progs/solve/back/outfl_back.o 		\
		$(SOLVE_ROOT)/progs/solve/back/parminfoe.o		\
		$(SOLVE_ROOT)/progs/solve/back/pmcmb.o 			\
		$(SOLVE_ROOT)/progs/solve/back/prnt_vect.o 		\
		$(SOLVE_ROOT)/progs/solve/back/reorder_x.o 		\
		$(SOLVE_ROOT)/progs/solve/back/setst.o 			\
		$(SOLVE_ROOT)/progs/solve/back/where_arc.o		\
		$(SOLVE_ROOT)/progs/solve/back/write_glcorr.o

ADJST_OBJECTS = 							\
		$(SOLVE_ROOT)/progs/solve/adjst/add_save.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/adjst_do.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a1jst.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a1jst_init.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a2jst.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a2jst_noeop.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a2jst_plneop.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a2jst_segeop.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/a3jst.o 		\
	 	$(SOLVE_ROOT)/progs/solve/adjst/apriori_nut_applied.o	\
		$(SOLVE_ROOT)/progs/solve/adjst/cndms.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/comp.o 			\
		$(SOLVE_ROOT)/progs/solve/adjst/compmin.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/covar2corel.o		\
		$(SOLVE_ROOT)/progs/solve/adjst/create_covariance_matrix.o \
		$(SOLVE_ROOT)/progs/solve/adjst/eopell.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/eop_share.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/errcmp.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/errcmp_pos.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/eheo_res.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/erm_res.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/hpe_res.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/ind_rotcor.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/jmg_hfeop.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/map_eop_results.o	\
		$(SOLVE_ROOT)/progs/solve/adjst/mdfil.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/mult_vmv.o		\
		$(SOLVE_ROOT)/progs/solve/adjst/namnut.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/posmap1.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/posmap2.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/put9.o			\
		$(SOLVE_ROOT)/progs/solve/adjst/return_adjustment.o 	\
		$(SOLVE_ROOT)/progs/solve/adjst/return_covariance.o	\
		$(SOLVE_ROOT)/progs/solve/adjst/return_heo.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/return_hfeop.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/return_sigma.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/solve_apr_spool.o 	\
		$(SOLVE_ROOT)/progs/solve/adjst/spe_res.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/uen_vel.o 		\
		$(SOLVE_ROOT)/progs/solve/adjst/vtd_apr_spool.o 	\
		$(SOLVE_ROOT)/progs/solve/adjst/vtd_get_apr_prp.o 	\
		$(SOLVE_ROOT)/progs/solve/adjst/write_sinex.o

CRES_OBJECTS 	=							\
		$(SOLVE_ROOT)/progs/solve/cres/adql.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/compute_mechi.opt_o	\
		$(SOLVE_ROOT)/progs/solve/cres/cremu.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/cres_do.o 		\
		$(SOLVE_ROOT)/progs/solve/cres/first.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/get_added_error.o 	\
		$(SOLVE_ROOT)/progs/solve/cres/listing_options.o	\
		$(SOLVE_ROOT)/progs/solve/cres/post_fit.o		\
		$(SOLVE_ROOT)/progs/solve/cres/quadp.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/secnd.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/stats.o 			\
		$(SOLVE_ROOT)/progs/solve/cres/third.o

GETDB_OBJECTS 	=							\
		$(SOLVE_ROOT)/progs/solve/getdb/comp_theo.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/constraint_default.o  	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_do.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_fill_parfil.o 	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_fill_namfil.o 	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_fill_oborg.o 	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_fill_socom.o 	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_meteo_correct.o 	\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_select.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/getdb_param.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/ionfr.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/set_obstyp.o 		\
		$(SOLVE_ROOT)/progs/solve/getdb/srset.o 

HAUSR_OBJECTS = 							\
		$(SOLVE_ROOT)/progs/solve/hausr/bline.o 		\
		$(SOLVE_ROOT)/progs/solve/hausr/cumuloop.o 		\
		$(SOLVE_ROOT)/progs/solve/hausr/glbl_or_lcl.o 		\
		$(SOLVE_ROOT)/progs/solve/hausr/outfl_hausr.o 		\
		$(SOLVE_ROOT)/progs/solve/hausr/outwithitall.o 		\
		$(SOLVE_ROOT)/progs/solve/hausr/sourc.o

COVP_OBJECTS =           						\
	 	$(SOLVE_ROOT)/progs/solve/covp/cosrt.o 			\
	 	$(SOLVE_ROOT)/progs/solve/covp/covp_main.o

BASFE_OBJECTS 	= 							\
		$(SOLVE_ROOT)/progs/solve/basfe/bwork.o 		\
		$(SOLVE_ROOT)/progs/solve/basfe/cvmml.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/gtlhv.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/lvect.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/reada_basfe.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/spart.o 	  	\
		$(SOLVE_ROOT)/progs/solve/basfe/xrsort.o

BCLOK_OBJECTS 	=							\
                $(SOLVE_ROOT)/progs/solve/bclok/bclock_max.o


SETFL_OBJECTS 	= 							\
		$(SOLVE_ROOT)/progs/solve/setfl/ofst_sta.o  		\
		$(SOLVE_ROOT)/progs/solve/setfl/change_supmet.o 	\
		$(SOLVE_ROOT)/progs/solve/setfl/set_sngchk.o 

PAMB_OBJECTS 	= 							\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_do.opt_o		\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_compile.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/amb_cls.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/amb_filter.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/amb_update.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/aob_update.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/arf_filter.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/clopa.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/find_subnet.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/get_arf.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/make_scalis.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/move_cals.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/msc_check.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/oscro.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_elim.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_freeze.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_freeze_menu.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_init.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_init_menu.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_mares.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_menu.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_plot_menu.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_save.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_showres.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/pamb_sol.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_elim.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_exec.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_gramb.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_main.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/paru_menu.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/pas_check.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/scadam.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/scadam_menu.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/scadam_plot.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/pamb/scatie.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/selbas.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/selsou.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/selsta.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/seltri.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/show_arf.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/pamb/sta_ambupd.opt_o 

ELIM_OBJECTS 	= 							\
		$(SOLVE_ROOT)/progs/solve/elim/ambcha_permit.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/chi_update.Opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/db_scan.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_do.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_lists.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/elim_menu.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_param.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/elim_save.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/elim_sngchk.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/equmem.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/io_obser.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/io_wgt.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/main_outobs.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/manor.Opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/mares_b3d.Opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/masol.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/mile_do.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/proc_m.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/refresh_wei.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/resid_st.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/sup_update.opt_o 	\
		$(SOLVE_ROOT)/progs/solve/elim/upwei.opt_o 		\
		$(SOLVE_ROOT)/progs/solve/elim/upwei_menu.opt_o 

UPTDB_OBJECTS 	= 							\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_do.o 		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_menu.o 		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_ptoc.o  		\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_solsetup.o 	\
		$(SOLVE_ROOT)/progs/solve/uptdb/uptdb_write.o 

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

bin:	$(OBJM) $(OBJS)  		  \
                         $(ADJST_OBJECTS) \
                         $(ARCPE_OBJECTS) \
                         $(BACK_OBJECTS)  \
                         $(BASFE_OBJECTS) \
                         $(BCLOK_OBJECTS) \
                         $(COVP_OBJECTS)  \
                         $(CRES_OBJECTS)  \
                         $(HAUSR_OBJECTS) \
                         $(OPTIN_OBJECTS) \
                         $(PROC_OBJECTS)  \
                         $(GETDB_OBJECTS) \
                         $(NORML_OBJECTS) \
                         $(PAMB_OBJECTS)  \
                         $(ELIM_OBJECTS)  \
			 $(UPTDB_OBJECTS) 
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/BATCH $(OBJM) $(OBJS) 		\
						$(PROC_OBJECTS) 	\
						$(NORML_OBJECTS) 	\
						$(ARCPE_OBJECTS) 	\
						$(BACK_OBJECTS) 	\
						$(OPTIN_OBJECTS) 	\
						$(ADJST_OBJECTS)  	\
						$(CRES_OBJECTS)  	\
						$(GETDB_OBJECTS) 	\
						$(HAUSR_OBJECTS)  	\
						$(COVP_OBJECTS)  	\
						$(BASFE_OBJECTS)  	\
						$(BCLOK_OBJECTS)	\
						$(PAMB_OBJECTS) 	\
						$(ELIM_OBJECTS) 	\
						$(SETFL_OBJECTS) 	\
						$(UPTDB_OBJECTS) 	\
                                                $(LIBS)

batch.o:	batch_version.i

clean:
	rm -f $(OBJM) $(OBJS)   \
	      $(PROC_OBJECTS) 	\
	      $(NORML_OBJECTS) 	\
	      $(ARCPE_OBJECTS) 	\
	      $(BACK_OBJECTS) 	\
	      $(OPTIN_OBJECTS) 	\
	      $(ADJST_OBJECTS)  \
	      $(CRES_OBJECTS)  	\
	      $(GETDB_OBJECTS) 	\
	      $(HAUSR_OBJECTS)  \
	      $(COVP_OBJECTS)  	\
	      $(BASFE_OBJECTS)  \
	      $(BCLOK_OBJECTS)	\
	      $(PAMB_OBJECTS) 	\
	      $(ELIM_OBJECTS) 	\
	      $(SETFL_OBJECTS) 	\
	      $(UPTDB_OBJECTS) 	\
              $(EXE_DIR)/BATCH
