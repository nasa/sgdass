# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  pima  executable              *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:     	.f     	.c	.o	.opt_o	.noopt_o	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95)       -fopenmp -I $(PIMA_ROOT)/include -I $(PETOOLS_PREFIX)/include -I $(VTD_INC) -I $(GVH_INC) -I $(SPC_INC) -I $(NERS_INC) -I $(FOURPACK_INC) -I $(FFTW_INC) -I $(CFITSIO_INC) -I $(FOURPACK_INC) -c -o  $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT)   -fopenmp -I $(PIMA_ROOT)/include -I $(PETOOLS_PREFIX)/include -I $(VTD_INC) -I $(GVH_INC) -I $(SPC_INC) -I $(NERS_INC) -I $(FOURPACK_INC) -I $(FFTW_INC) -I $(CFITSIO_INC) -I $(FOURPACK_INC) -c -o $*.opt_o $*.f

.f.noopt_o:
	$(MK5_F95_NOOPT) -fopenmp -I $(PIMA_ROOT)/include -I $(PETOOLS_PREFIX)/include -I $(VTD_INC) -I $(GVH_INC) -I $(SPC_INC) -I $(NERS_INC) -I $(FOURPACK_INC) -I $(FFTW_INC) -I $(CFITSIO_INC) -I $(FOURPACK_INC) -g -c -o $*.noopt_o $*.f


PIMA_LIB = $(PIMA_ROOT)/lib/libpima.a

ifeq ($(PETOOLS_OS),Linux)
        PIMA_SLIB 	= $(PIMA_ROOT)/lib/libpima.so.1.0
endif
ifeq ($(PETOOLS_OS),Darwin)
        PIMA_SLIB 	= $(PIMA_ROOT)/lib/libpima.1.dylib
endif

#.NOTPARALLEL:

SRC  =  			  \
	ambig_resolve.f 	  \
	bpass_amp_nrml.f 	  \
	bpass_gr_del.f 		  \
	bpass_mod_linear.f 	  \
	bpass_mod_poly.f 	  \
	bpass_mod_spline.f 	  \
	bpass_phas_rate.f 	  \
	bpass_sta_stat.f 	  \
	cmpl_mul_mat.f 		  \
	find_ampl_max.f 	  \
	fft_2d.f 		  \
	find_image_noise.f 	  \
	fits_lib.f 		  \
        fits_tim_avr.f 		  \
	fits_to_map.f 	 	  \
	fits_to_radplot.f 	  \
	fits_to_uvplot.f 	  \
	fringe_adj.f 		  \
	get_band.f 		  \
	get_apr_sefd_bas.f 	  \
	get_fits_vis.f 		  \
	get_fits_map.f 		  \
	get_grpambsp.f 		  \
	gen_maptxt.f 		  \
	gen_mapplot.f 		  \
	gen_radplot.f 		  \
	get_uv_avr.f 	 	  \
	get_uvlen_amp.f 	  \
	gen_uvplot.f 		  \
	generate_cfd_table.f      \
	io_jet_ang.f 		  \
	lsqw_cns_save.f 	  \
	monica_to_antab.f 	  \
	nbas.f 			  \
	parse_bps_gen_cnt.f       \
	parse_pcal_mask_gen_cnt.f \
	parse_log.f 		  \
	pbp_wri.f 		  \
	phs_amp_rate.f 		  \
	pima.f 			  \
	pima_2d_map_plot.f 	  \
	pima_2fft.f 		  \
	pima_accor_mean.f 	  \
	pima_accnrm_kogan.f 	  \
	pima_accnrm_taper.f 	  \
	pima_acta.f 		  \
	pima_amb_res.f 	  	   \
	pima_amp_to_sigphs.f 	  \
	pima_ampl_out.f 	  \
	pima_apply_accel.f        \
	pima_apply_gr_del_phs.f   \
	pima_apr_delay.f 	  \
	pima_autc_ampl_nrml.f     \
	pima_beam_att.f 	  \
	pima_bpass.f 		  \
        pima_bpass_accum.f        \
	pima_bpass_detrend.f      \
	pima_bpass_init.f         \
	pima_bpass_fine.f         \
        pima_bpass_mod.f          \
	pima_bpass_renrml.f 	  \
	pima_check_resume.f 	  \
	pima_bpass_sta_list.f 	  \
	pima_clpc.f 		  \
	pima_clpc_arr.f	          \
	pima_compar.f 		  \
	pima_conf.f 		  \
	pima_create_srt.f 	  \
	pima_decor_smearing.f 	  \
	pima_dipc.f 	 	  \
	pima_drf_max_arr.f 	  \
	pima_exit.f 		  \
	pima_find_sub.f           \
	pima_fine_search.f	  \
	pima_apso_fix.f 	  \
	pima_fri_io.o 		  \
	pima_free_obs.f 	  \
	pima_frg_combine.f 	  \
	pima_frg_merge.f 	  \
	pima_fringe.f 		  \
	pima_fringe_res.f         \
	pima_frip.f 		  \
	pima_frip_amclo.f 	  \
	pima_frip_apr_adj.f 	  \
	pima_frip_avr.f 	  \
	pima_frip_check_miscls.f  \
	pima_frip_grid.f 	  \
	pima_frip_io.f 	 	  \
	pima_frip_load.f 	  \
	pima_frip_sc.f 	 	  \
	pima_fr1d_drf_plot.f 	  \
	pima_fr1d_frq_plot.f 	  \
	pima_fr1d_tim_plot.f 	  \
	pima_fr2d_plot.f 	  \
	pima_gaco_comp.f 	  \
	pima_gaco_comp.f 	  \
	pima_gepm.f 	  	  \
	pima_get_ac.f 	  	  \
	pima_get_addclo.f 	  \
	pima_get_antab.f 	  \
	pima_get_evn_gain.f 	  \
	pima_get_gain.f 	  \
	pima_get_key.f 		  \
	pima_get_obs.f 		  \
	pima_get_pcal.f 	  \
	pima_get_pol_conf.f 	  \
	pima_get_pol_mode.f 	  \
	pima_get_resid.f 	  \
	pima_get_tsys.f 	  \
	pima_get_uv.f 		  \
	pima_get_vera_tsys.f 	  \
	pima_get_vlba_gain.f 	  \
	pima_get_vlba_log.f 	  \
	pima_gvh_define.f 	  \
	pima_gvh_hist.f 	  \
	pima_gvh_init.f 	  \
	pima_gvh_obs.f 		  \
	pima_gvh_sess.f 	  \
	pima_gvh_wri.f 		  \
	pima_indx.f 		  \
	pima_init.f 		  \
	pima_insert_cab.f 	  \
	pima_load_kjcc_mod.f   	  \
	pima_load_sfxc_mod.f   	  \
	pima_load_soucat.f 	  \
	pima_load_stacat.f 	  \
	pima_load_vera_mod.f   	  \
	pima_load_wvr.f 	  \
	pima_mean_freq.f          \
	pima_mkdb.f 		  \
	pima_multi_pc_plot.f 	  \
	pima_onof_proc.f 	  \
	pima_onof_sess.f 	  \
	pima_opag.f               \
	pima_opal.f               \
	pima_parse_vlba_tsys.f    \
	pima_pbp_init.f 	  \
	pima_pbp_accum.f 	  \
	pima_pbp_fine.f 	  \
	pima_pc_mod.f             \
	pima_pcst.f               \
	pima_pcal_clean.f         \
	pima_pcal_scan_avr.f      \
	pima_pcal_suppress.f 	  \
	pima_plot_acrl.f 	  \
	pima_plot_bpass.f 	  \
	pima_plot_pcal.f 	  \
	pima_plot_tsys.f 	  \
	pima_prga.f 		  \
	pima_rconv.f 		  \
	pima_read_bpass.f 	  \
	pima_read_frt_offsets.f   \
	pima_read_gaco.f          \
	pima_read_obs.f 	  \
	pima_read_pbp.f 	  \
	pima_read_srt.f 	  \
	pima_read_time_flag.f 	  \
	pima_sb_search.f 	  \
	pima_scan_split.f 	  \
	pima_selsta.f 		  \
	pima_splt.f 	 	  \
	pima_splt_fitsta.f 	  \
        pima_splt_consol.f 	  \
	pima_splt_write.f 	  \
	pima_staging.f 		  \
	pima_theo.f 		  \
	pima_tim_merge.f 	  \
	pima_tsmo.f 		  \
	pima_tst1.f 		  \
        pima_update_baduv_list.f  \
        pima_upgr.f 		  \
	pima_use_pcal.f 	  \
	pima_use_obs.f 		  \
	pima_uvo_update.f 	  \
	pima_uv_drf.f 	          \
	pima_uv_drf3.f 	          \
	pima_uv_exclude.f 	  \
	pima_uv_shift.f 	  \
	pima_version_check.f      \
	pima_wea_intrp.f 	  \
	pima_write_bpass.f        \
	pima_write_gaco.f         \
	pima_write_obs.f 	  \
	pima_write_time_flag.f 	  \
	pima_wvr_to_obs.f 	  \
	pima_wvr_use.f 	          \
	read_bandpass_mask.f 	  \
	read_pcal_mask.f 	  \
        sou_free.f  		  \
	trec_const_to_antab.f 	  \
	uva_merge.f 	 	  \
	tri.f 			  \
	tsys_model.f 		  \
	wri_antab.f 		  \
	wri_bandpass_mask.f 	  \
	wri_pcal_mask.f 	  \
        wri_pcal_rms.f            \
	wri_pcal_rpt.f

OBJS =                  		\
	ambig_resolve.o 		\
	bpass_amp_nrml.opt_o 		\
	bpass_gr_del.o 		 	\
	bpass_mod_linear.o 	  	\
	bpass_mod_poly.opt_o 		\
	bpass_mod_spline.opt_o 		\
	bpass_phas_rate.opt_o 		\
	bpass_sta_stat.opt_o 		\
	cmpl_mul_mat.opt_o 		\
	fft_2d.opt_o 			\
	find_ampl_max.opt_o 	 	\
	find_image_noise.o 	 	\
	fits_lib.o 			\
        fits_tim_avr.o 			\
	fringe_adj.opt_o 		\
	gen_maptxt.o 		        \
	gen_mapplot.opt_o 		\
	gen_radplot.o 			\
	gen_uvplot.o 			\
	generate_cfd_table.opt_o 	\
	get_apr_sefd_bas.opt_o 		\
	get_band.o 		 	\
	get_fits_map.o 		 	\
	get_fits_vis.o 			\
	get_grpambsp.opt_o 		\
	get_uv_avr.o 	 	 	\
	get_uvlen_amp.o 		\
	io_jet_ang.o 			\
	monica_to_antab.o 		\
	lsqw_cns_save.opt_o 	 	\
	nbas.o 			 	\
	parse_bps_gen_cnt.o 		\
	parse_log_pre911.o 		\
	parse_pcal_mask_gen_cnt.o 	\
	pbp_wri.o 		 	\
	phs_amp_rate.opt_o 		\
	pima_2d_map_plot.o 	 	\
	pima_2fft.opt_o 		\
	pima_accnrm_kogan.opt_o 	\
	pima_accnrm_taper.opt_o 	\
	pima_accor_mean.o 		\
	pima_acta.opt_o 		\
	pima_amb_res.opt_o 	  	\
	pima_amp_to_sigphs.opt_o 	\
	pima_ampl_out.o 		\
	pima_apply_accel.opt_o          \
	pima_apply_gr_del_phs.o   	\
	pima_apr_delay.opt_o 	 	\
	pima_autc_ampl_nrml.o 		\
	pima_beam_att.o 	 	\
	pima_bpass.opt_o 		\
        pima_bpass_accum.opt_o          \
	pima_bpass_detrend.o      	\
	pima_bpass_init.opt_o           \
	pima_bpass_fine.o               \
        pima_bpass_mod.o                \
	pima_bpass_renrml.o 		\
	pima_bpass_sta_list.opt_o 	\
	pima_check_resume.o 		\
	pima_clpc.o 		        \
	pima_clpc_arr.o 		\
	pima_compar.opt_o 		\
	pima_conf.noopt_o 		\
	pima_create_srt.o 	 	\
	pima_decor_smearing.o 	  	\
	pima_dipc.opt_o 	 	\
	pima_drf_max_arr.opt_o 	 	\
	pima_exit.o 			\
	pima_find_sub.o          	\
	pima_fine_search.opt_o 		\
	pima_apso_fix.opt_o 	 	\
	pima_fr1d_drf_plot.opt_o 	\
	pima_fr1d_frq_plot.opt_o 	\
	pima_fr1d_tim_plot.opt_o 	\
	pima_fr2d_plot.o 		\
	pima_free_obs.o  		\
	pima_frg_combine.o 	 	\
	pima_frg_merge.o 	 	\
	pima_fri_io.o 			\
	pima_fringe.opt_o 		\
	pima_fringe_res.o 		\
	pima_frip.o 		 	\
	pima_frip_amclo.o 	 	\
	pima_frip_apr_adj.opt_o 	\
	pima_frip_avr.opt_o	 	\
	pima_frip_check_miscls.opt_o	\
	pima_frip_grid.opt_o	 	\
	pima_frip_io.o 	 	 	\
	pima_frip_load.o 	 	\
	pima_frip_sc.o 	 	 	\
	pima_gaco_comp.o 	    	\
	pima_gaco_init.o 	    	\
	pima_gepm.o 	  	  	\
	pima_get_ac.o 			\
	pima_get_addclo.o 		\
	pima_get_antab.o 		\
	pima_get_evn_gain.o 	 	\
	pima_get_gain.o 		\
	pima_get_key.o 			\
	pima_get_obs.o 			\
	pima_get_pcal.noopt_o 		\
	pima_get_pol_conf.o 	  	\
	pima_get_pol_mode.o 	  	\
	pima_get_resid.o 		\
	pima_get_tsys.o 		\
	pima_get_uv.o 			\
	pima_get_vera_tsys.o 		\
	pima_get_vlba_gain.o 		\
	pima_get_vlba_log.o 		\
	pima_gvh_define.o 		\
	pima_gvh_hist.o 		\
	pima_gvh_init.o 		\
	pima_gvh_obs.o 			\
	pima_gvh_sess.o 		\
	pima_gvh_wri.o 			\
	pima_indx.opt_o 		\
	pima_init.o 			\
	pima_insert_cab.o 		\
	pima_load_kjcc_mod.o   	  	\
	pima_load_sfxc_mod.o   	  	\
	pima_load_soucat.o 		\
	pima_load_stacat.o 		\
	pima_load_vera_mod.opt_o   	\
	pima_load_wvr.o 	  	\
	pima_mean_freq.o          	\
	pima_mkdb.o 			\
	pima_multi_pc_plot.o 	 	\
	pima_onof_proc.opt_o 	        \
	pima_onof_sess.o 		\
	pima_opag.o			\
	pima_opal.o			\
	pima_parse_log.o 		\
	pima_parse_vlba_tsys.o 		\
	pima_pbp_init.opt_o 	  	\
	pima_pbp_accum.opt_o 	  	\
	pima_pbp_fine.opt_o 	  	\
	pima_pcal_control.o 	 	\
	pima_pcal_clean.o               \
	pima_pcal_scan_avr.o      	\
	pima_pc_mod.o                   \
	pima_pcst.o                     \
	pima_plot_acrl.o 		\
	pima_plot_bpass.o 		\
	pima_plot_pcal.noopt_o 		\
	pima_plot_tsys.o 		\
	pima_prga.o 			\
	pima_rconv.o 			\
	pima_read_bpass.o 		\
	pima_read_frt_offsets.o 	\
	pima_read_gaco.o                \
	pima_read_obs.o 		\
	pima_read_pbp.o 	 	\
	pima_read_srt.o 	 	\
	pima_read_time_flag.o 		\
	pima_sb_search.o 		\
	pima_scan_split.o 		\
	pima_selsta.o 	 		\
	pima_splt.opt_o 	 	\
	pima_splt_fitsta.o 		\
	pima_splt_consol.o 	 	\
	pima_splt_write.o 	 	\
	pima_staging.o 	 		\
	pima_theo.o 			\
	pima_tim_merge.o 	  	\
	pima_tsmo.o 			\
	pima_tst1.o 		 	\
	pima_tsys_control.o 	 	\
        pima_update_baduv_list.o	\
        pima_upgr.o		 	\
	pima_use_obs.o 			\
	pima_use_pcal.o 		\
	pima_uv_drf.opt_o 	        \
	pima_uv_drf3.opt_o 	        \
	pima_uv_exclude.o 		\
	pima_uv_shift.opt_o 		\
	pima_uvo_update.opt_o 	 	\
	pima_version_check.o     	\
	pima_wea_intrp.o 		\
	pima_write_gaco.o               \
	pima_write_bpass.o              \
	pima_write_obs.o 		\
	pima_write_time_flag.o 		\
	pima_wvr_to_obs.o 	  	\
	pima_wvr_use.o 	          	\
	read_bandpass_mask.o 		\
	read_pcal_mask.o 	  	\
        sou_free.o  			\
	tri.o 			 	\
	trec_const_to_antab.o 	  	\
	tsys_model.o 			\
	uva_merge.o 			\
	wri_antab.o 			\
	wri_bandpass_mask.o 	 	\
	wri_pcal_mask.o 		\
	wri_pcal_rms.o 		        \
	wri_pcal_rpt.o

OBJ1 =                  		\
	pima.o

BANDPASS_MASK_GEN =            		\
	bandpass_mask_gen.o

GET_FREQ_TABLE_FROM_FITS =            \
	get_freq_table_from_fits.o

GET_SOURCE_TABLE_FROM_FITS =            \
	get_source_table_from_fits.o

GET_STATION_TABLE_FROM_FITS =            \
	get_station_table_from_fits.o

LOG_TO_ANTAB =                  	\
	log_to_antab.o

PIMA_CONTROL_UPDATE = 			\
	pima_control_update.o

PIMA_INQ =				\
	pima_inq.o

FFT_TEST = 			\
	fft_speed_test.o

FITSD = 			\
	fitsd.o

FITSH = 			\
	fitsh.o

UVA_MERGE = 			\
	uva_merge_main.o

FITS_TIM_AVR = 			\
	fits_tim_avr_main.o

FITS_TO_MAP = 			\
	fits_to_map.o

FITS_TO_RADPLOT = 		\
	fits_to_radplot.o 

FITS_TO_UVPLOT = 		\
	fits_to_uvplot.o

FITS_TO_CFD = 			\
	fits_to_cfd.o

EXTRA_GAIN = 			\
	extra_gain.o

CORR_GAIN = 			\
	corr_gain.o

FITS_CHECK_CLS = 		\
	fits_check_cls.o 

ACTA_PLOT = 			\
	acta_plot.o 

LIBS =						\
	$(GVH_LIB) 				\
	$(VTD_LIB) 				\
	$(FOURPACK_LIB) 			\
	$(SPC_LIB)				\
	$(NERS_LIB)				\
	$(FFTW_LIB) 				\
	$(CFITSIO_LIB)				\
	$(CURL_LIB)				\
	$(SOLVE_LIB_PGPLOT)			\
	$(PETOOLS_LIB)				\
	$(SOLVE_LIB_X11)			\
	$(SOLVE_LIB_XT)				\
	$(SOLVE_LIB_XHP11)			\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_M) 				\

all: 	pima_static_lib pima_shared_lib pima_bin


pima_static_lib:	  $(OBJS) 
	ar  r $(PIMA_LIB) $(OBJS)

pima_shared_lib:	$(OBJS) 
        ifeq ($(PETOOLS_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libpima.so.1.0 -o $(PIMA_SLIB) $(OBJS)
	      if ( -l $(PIMA_ROOT)/lib/libpima.so ) rm -f $(PIMA_ROOT)/lib/libpima.so
	      ln -s $(PIMA_ROOT)/lib/libpima.so.1.0    $(PIMA_ROOT)/lib/libpima.so
        endif
        ifeq ($(PETOOLS_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(PIMA_ROOT)/lib/libpima.1.dylib \
                            -o $(PIMA_SLIB) $(OBJS) \
                             $(LIBS)
	      if ( -l $(PIMA_ROOT)/lib/libpima.dylib    ) rm -f $(PIMA_ROOT)/lib/libpima.dylib
	      ln -s $(PIMA_ROOT)/lib/libpima.1.dylib    $(PIMA_ROOT)/lib/libpima.dylib
        endif

pima_bin: pima_static_bin pima pima_inq bandpass_mask_gen get_freq_table_from_fits \
          get_source_table_from_fits get_station_table_from_fits log_to_antab \
          fitsd fitsh uva_merge fft_test fits_to_map \
          fits_to_radplot fits_to_uvplot fits_to_cfd \
          extra_gain corr_gain fits_check_cls acta_plot \
          fits_tim_avr


pima_static_bin:	pima_static_lib $(OBJ1) 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/pima_static $(OBJ1) \
                       $(PIMA_ROOT)/lib/libpima.a \
                       $(VTD_PREFIX)/lib/libvtd.a \
                       $(VTD_PREFIX)/lib/libfitslib.a \
                       $(GVH_PREFIX)/lib/libgvh.a \
                       $(GVH_PREFIX)/lib/libvcat.a \
                       $(SPC_PREFIX)/lib/libspc.a \
                       $(NERS_PREFIX)/lib/libners.a \
		       $(CURL_LIB) \
		       $(FFTW_LIB) \
                       $(FOURPACK_LIB_STATIC) \
                       $(CFITSIO_PREFIX)/lib/libcfitsio.a \
                       $(PETOOLS_LIB_A) \
                       $(SOLVE_LIB_PGPLOT_A) \
		       $(FFTW_LIB) \
		       $(CURL_LIB) \
                       $(SOLVE_LIB_BLAS) \
	               $(SOLVE_LIB_X11)	\
	               $(SOLVE_EXTRA_LIB) \
                       $(SOLVE_LIB_M)

pima: 	$(OBJ1) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/pima $(OBJ1) -L$(PIMA_ROOT)/lib -lpima $(LIBS)

pima_inq: 	$(PIMA_INQ) pima_static_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/pima_inq $(PIMA_INQ) 

bandpass_mask_gen: $(BANDPASS_MASK_GEN) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/bandpass_mask_gen $(BANDPASS_MASK_GEN) $(PIMA_SLIB) $(LIBS);

get_freq_table_from_fits: $(GET_FREQ_TABLE_FROM_FITS) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/get_freq_table_from_fits $(GET_FREQ_TABLE_FROM_FITS) $(PIMA_SLIB)  $(LIBS);

get_source_table_from_fits: $(GET_SOURCE_TABLE_FROM_FITS) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/get_source_table_from_fits $(GET_SOURCE_TABLE_FROM_FITS) $(PIMA_SLIB)  $(LIBS);

get_station_table_from_fits: $(GET_STATION_TABLE_FROM_FITS) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/get_station_table_from_fits $(GET_STATION_TABLE_FROM_FITS) $(PIMA_SLIB)  $(LIBS);

log_to_antab: $(LOG_TO_ANTAB) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/log_to_antab $(LOG_TO_ANTAB) $(PIMA_SLIB) $(LIBS);

fitsd: 	$(FITSD) $(OBJS) 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fitsd $(FITSD) fits_lib.o $(LIBS);

fitsh:	$(FITSH) $(OBJS)
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fitsh $(FITSH) fits_lib.o $(LIBS);

uva_merge:	$(UVA_MERGE) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/uva_merge $(UVA_MERGE) $(PIMA_SLIB) $(LIBS);

fft_test: $(FFT_TEST) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fft_speed_test $(FFT_TEST) $(PIMA_SLIB) $(LIBS);

fits_tim_avr: $(FITS_TIM_AVR) pima_shared_lib 
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_tim_avr $(FITS_TIM_AVR) $(PIMA_SLIB) $(LIBS);

fits_to_map: $(FITS_TO_MAP) pima_shared_lib 
        ifeq ($(PETOOLS_OS),Linux)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_map $(FITS_TO_MAP) $(PIMA_SLIB) $(LIBS);
        endif
#
        ifeq ($(PETOOLS_OS),Darwin)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_map $(FITS_TO_MAP) \
                gen_mapplot.opt_o get_band.o io_jet_ang.o find_image_noise.o gen_maptxt.o get_fits_map.o fits_lib.o sou_free.o \
                $(PETOOLS_LIB_A) 	 \
                $(SOLVE_LIB_PGPLOT_A)    \
	        $(CFITSIO_LIB)	   	 \
	        $(CURL_LIB)		 \
                $(SOLVE_LIB_X11)         \
                $(SOLVE_LIB_BLAS)        \
                $(SOLVE_LIB_X11)	 \
                $(SOLVE_EXTRA_LIB)       \
                $(SOLVE_LIB_M);
        endif

fits_to_radplot: $(FITS_TO_RADPLOT) pima_shared_lib  
        ifeq ($(PETOOLS_OS),Linux)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_radplot $(FITS_TO_RADPLOT) $(PIMA_SLIB) $(LIBS);
        endif
#
        ifeq ($(PETOOLS_OS),Darwin)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_radplot $(FITS_TO_RADPLOT) \
                get_fits_vis.o get_uvlen_amp.o get_band.o gen_radplot.o gen_maptxt.o fits_lib.o sou_free.o \
                $(PETOOLS_LIB_A) 	 \
                $(SOLVE_LIB_PGPLOT_A)    \
	        $(CFITSIO_LIB)	   	 \
	        $(CURL_LIB)		 \
                $(SOLVE_LIB_X11)         \
                $(SOLVE_LIB_BLAS)        \
                $(SOLVE_LIB_X11)	 \
                $(SOLVE_EXTRA_LIB)       \
                $(SOLVE_LIB_M);
        endif

fits_to_uvplot: $(FITS_TO_UVPLOT) pima_shared_lib  
        ifeq ($(PETOOLS_OS),Linux)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_uvplot $(FITS_TO_UVPLOT) $(PIMA_SLIB) $(LIBS);
        endif
#
        ifeq ($(PETOOLS_OS),Darwin)
	     $(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_uvplot $(FITS_TO_UVPLOT) \
                get_fits_vis.o get_uvlen_amp.o get_band.o gen_uvplot.o get_uv_avr.o fits_lib.o sou_free.o \
                $(PETOOLS_LIB_A) 	 \
                $(SOLVE_LIB_PGPLOT_A)    \
	        $(CFITSIO_LIB)	   	 \
	        $(CURL_LIB)		 \
                $(SOLVE_LIB_X11)         \
                $(SOLVE_LIB_BLAS)        \
                $(SOLVE_LIB_X11)	 \
                $(SOLVE_EXTRA_LIB)       \
                $(SOLVE_LIB_M);
        endif

fits_to_cfd: $(FITS_TO_CFD) pima_shared_lib  
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_to_cfd $(FITS_TO_CFD) $(PIMA_SLIB)  $(LIBS);

extra_gain: $(EXTRA_GAIN) pima_shared_lib  
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/extra_gain $(EXTRA_GAIN) $(PIMA_SLIB) $(LIBS);

corr_gain: $(CORR_GAIN) pima_shared_lib  
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/corr_gain $(CORR_GAIN) $(PIMA_SLIB) $(LIBS);

fits_check_cls: $(FITS_CHECK_CLS) pima_shared_lib  
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/fits_check_cls $(FITS_CHECK_CLS) $(PIMA_SLIB) $(LIBS);

acta_plot:	$(ACTA_PLOT) pima_shared_lib  
	$(MK5_LINK) -o $(PIMA_ROOT)/bin/acta_plot $(ACTA_PLOT) $(PIMA_SLIB) $(LIBS);

$(OBJS): 		$(PIMA_ROOT)/include/pima.i
$(OBJ1): 		$(PIMA_ROOT)/include/pima.i
$(BANDPASS_MASK_GEN):	$(PIMA_ROOT)/include/pima.i
$(GET_SOURCE_TABLE_FROM_FITS): 	$(PIMA_ROOT)/include/pima.i
$(LOG_TO_ANTAB): 	$(PIMA_ROOT)/include/pima.i
$(PIMA_CONTROL_UPDATE): $(PIMA_ROOT)/include/pima.i
$(OBJS): 		$(PIMA_ROOT)/include/pima_db.i

clean:
	rm -f $(OBJS)  					\
	      $(OBJ1)  					\
              $(PIMA_ROOT)/lib/libpima.a 		\
	      $(BANDPASS_MASK_GEN) 			\
	      $(GET_SOURCE_TABLE_FROM_FITS) 		\
	      $(LOG_TO_ANTAB) 				\
	      $(PIMA_CONTROL_UPDATE) 			\
              $(GET_FREQ_TABLE_FROM_FITS) 		\
              $(GET_STATION_TABLE_FROM_FITS) 		\
	      $(FITSD) 					\
	      $(FITSH) 					\
	      $(UVA_MERGE) 				\
	      $(FFT_TEST)                               \
	      $(FITS_TO_MAP)                            \
	      $(FITS_TO_RADPLOT) 			\
	      $(FITS_TO_UVPLOT) 			\
	      $(FITS_TO_CFD)  				\
	      $(EXTRA_GAIN) 				\
	      $(CORR_GAIN)  				\
	      $(FITS_CHECK_CLS)  			\
	      $(ACTA_PLOT)  				\
	      $(FITS_TIM_AVR)  				\
	      $(PIMA_INQ)  				\
	      $(PIMA_ROOT)/bin/pima 	 		\
	      $(PIMA_ROOT)/bin/pima_inq			\
	      $(PIMA_ROOT)/bin/pima_static 	 	\
	      $(PIMA_ROOT)/bin/bpass_est    		\
	      $(PIMA_ROOT)/bin/bandpass_mask_gen 	\
	      $(PIMA_ROOT)/bin/get_freq_table_from_fits \
	      $(PIMA_ROOT)/bin/get_source_table_from_fits \
	      $(PIMA_ROOT)/bin/get_station_table_from_fits \
	      $(PIMA_ROOT)/bin/log_to_antab 		\
	      $(PIMA_ROOT)/bin/fitsd 			\
	      $(PIMA_ROOT)/bin/fitsh 			\
	      $(PIMA_ROOT)/bin/uva_merge 		\
	      $(PIMA_ROOT)/bin/fft_speed_test 		\
	      $(PIMA_ROOT)/bin/fits_to_map 		\
	      $(PIMA_ROOT)/bin/fits_tim_avr 		\
	      $(PIMA_ROOT)/bin/fits_to_radplot 		\
	      $(PIMA_ROOT)/bin/fits_to_uvplot 		\
	      $(PIMA_ROOT)/bin/fits_to_cfd 		\
	      $(PIMA_ROOT)/bin/extra_gain 		\
	      $(PIMA_ROOT)/bin/corr_gain 		\
	      $(PIMA_ROOT)/bin/fits_check_cls 		\
	      $(PIMA_ROOT)/bin/acta_plot
	if ( -l $(PIMA_ROOT)/lib/libpima.so      ) rm -f $(PIMA_ROOT)/lib/libpima.so
	if ( -f $(PIMA_ROOT)/lib/libpima.so.1.0  ) rm -f $(PIMA_ROOT)/lib/libpima.so.1.0
	if ( -l $(PIMA_ROOT)/lib/libpima.dylib   ) rm -f $(PIMA_ROOT)/lib/libpima.dylib
	if ( -f $(PIMA_ROOT)/lib/libpima.1.dylib ) rm -f $(PIMA_ROOT)/lib/libpima.1.dylib
