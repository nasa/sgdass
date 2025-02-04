# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  malo  executable              *
# *                                                                      *
# ************************************************************************
#
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:     	.f     	.c	.o	.opt_o	.optest_o .noopt_o	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -g -I ../include -I $(PETOOLS_PREFIX) -I $(FOURPACK_PREFIX) -I $(NETCDF_PREFIX) -I $(HDF5_PREFIX) -I $(NERS_INC) -c -o $*.o $*.c

.f.o:
	$(MK5_F95_NOOPT)        -fopenmp -I ../include -I $(PETOOLS_PREFIX)/include -I $(FOURPACK_PREFIX)/include -I $(NETCDF_PREFIX)/include -I $(HDF5_PREFIX)/include -I $(NERS_INC) -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT)    -fopenmp -I ../include -I $(PETOOLS_PREFIX)/include -I $(FOURPACK_PREFIX)/include -I $(NETCDF_PREFIX)/include -I $(HDF5_PREFIX)/include -I $(NERS_INC) -c -o $*.opt_o $*.f

.f.optest_o:
	$(MK5_F95_OPTEST) -fopenmp -I ../include -I $(PETOOLS_PREFIX)/include -I $(FOURPACK_PREFIX)/include -I $(NETCDF_PREFIX)/include -I $(HDF5_PREFIX)/include -I $(NERS_INC) -c -o $*.optest_o $*.f

.f.noopt_o:
	$(MK5_F95_NOOPT)  -fopenmp -I ../include -I $(PETOOLS_PREFIX)/include -I $(FOURPACK_PREFIX)/include -I $(NETCDF_PREFIX)/include -I $(HDF5_PREFIX)/include -I $(NERS_INC) -g -c -o $*.noopt_o $*.f


SRC  =  			 \
        aam_fcs_spl.f 		 \
	agra_read.f 		 \
	agra_write.f 		 \
	average_loading.f 	 \
        bds_util.f		 \
	comp_spr_model.f 	 \
	ckeck_listing_gap.f 	 \
	eop_fcs.f                \
	eop_fcs_io.f             \
	eop_init.f               \
	gen_aam.f 		 \
	get_evec.f 		 \
	gen_trg_dens.f 		 \
	eop_fcs_mod.f 		 \
	get_geoid.f 		 \
	get_pres_mod.f 		 \
	free_heb.f 		 \
        harpos_keep_s1_only.f 	 \
	heb_read.c 		 \
	hei_gd_to_ell.f 	 \
	loa_io.f 		 \
	learn_bds_start_intrv.f  \
	loading_model_to_nc.f 	 \
	malo_clean.f 		 \
	malo_comp_aam.f 	 \
	malo_comp_acp.f 	 \
	malo_comp_dewt.f 	 \
	malo_comp_ppwtem.f 	 \
	malo_comp_spr.f 	 \
	malo_config.f 		 \
	malo_ephedisp_read.f 	 \
	malo_ephedisp_write.f 	 \
	malo_ephe_monthly.f 	 \
	malo_harpos_read.f 	 \
	malo_free.f 		 \
	malo_init.f 		 \
	malo_inp_sta.f 		 \
	malo_intrp_ppwtem.f      \
	malo_intrp_pwtuv.f       \
	malo_intrp_pres.f        \
	malo_intrp_spr.f         \
	malo_loading.f 	 	 \
	malo_load_eop.f 	 \
	malo_load_leapsec.f 	 \
	malo_modc_parse.f 	 \
	malo_ond_proc.f 	 \
	malo_read_aod1b.f	 \
	malo_refra.f 		 \
	malo_regrid.f 		 \
	malo_shc_vgep.f 	 \
	malo_sphe.f 	 	 \
	malo_test.f 		 \
	malo_units_converter.f 	 \
	malo_upgrid.f 		 \
	malo_utc_to_tai.f 	 \
	malo_wave_table.f	 \
	malo_write_aod1b_love.f	 \
	move_lock.o 		 \
	omct_to_spr.f 		 \
	otide_upgrid.f 		 \
	parse_aam.f 		 \
	parse_eop_conf.f 	 \
	presan.f 		 \
        plot_eop_fcs.f 		 \
	plot_grid_r4.f 		 \
	plot_leap_r4.f 	 	 \
	plot_region_r4.f 	 \
	plot_region_coa.f 	 \
	read_dqt_heb.f           \
	read_dqtuv_heb.f         \
	read_geos_flux.f         \
	read_geos_lnd.f          \
	read_geos_rad.f          \
	read_geos_slv.f          \
	read_geos_trg.f           \
	read_heb.f               \
	read_heb_sect.f          \
	read_htw_heb.f           \
	read_love.f 	 	 \
	read_ls_mask_nc.f 	 \
	read_merranv_dqtuv.f 	 \
	sc_contr.f 		 \
	shc_io.f		 \
	spr_read_nc.f 		 \
	spr_write_nc.f 		 \
	write_heb.f              \
	write_ls_mask_nc.f 	 \
	write_ls_mask_pgm.f

LIB  =  $(MALO_ROOT)/lib/libmalo

OBJS =                  	 \
        aam_fcs_spl.o 		 \
	agra_read.o 		 \
	agra_write.o 		 \
        bds_util.o 		 \
	comp_spr_model.o 	 \
	eop_fcs.o                \
	eop_fcs_io.o             \
	eop_fcs_mod.o 		 \
	eop_init.o               \
	get_evec.o 		 \
	get_geoid.o 		 \
	gen_trg_dens.opt_o 	 \
	get_pres_mod.o 	 	 \
	free_heb.o 		 \
        harpos_keep_s1_only.o 	 \
	heb_read.o		 \
	loa_io.o 		 \
	learn_bds_start_intrv.o  \
	malo_clean.opt_o 	 \
	malo_comp_aam.opt_o 	 \
	malo_comp_acp.opt_o 	 \
	malo_comp_dewt.o 	 \
	malo_comp_ppwtem.o 	 \
	malo_comp_spr.opt_o 	 \
	malo_config.o 		 \
	malo_ephedisp_read.o 	 \
	malo_ephedisp_write.o 	 \
	malo_ephe_monthly.o 	 \
	malo_free.o 		 \
	malo_harpos_read.o 	 \
	malo_init.o 		 \
	malo_inp_sta.o 		 \
	malo_intrp_ppwtem.o      \
	malo_intrp_pwtuv.o 	 \
	malo_intrp_spr.o         \
	malo_intrp_pres.opt_o    \
	malo_loading.opt_o       \
	malo_load_eop.o 	 \
	malo_load_leapsec.o 	 \
	malo_modc_parse.o 	 \
	malo_ond_proc.o 	 \
	malo_read_aod1b.opt_o 	 \
	malo_refra.opt_o 	 \
	malo_regrid.opt_o 	 \
	malo_shc_vgep.o 	 \
	malo_sphe.opt_o 	 \
	malo_test.o 		 \
	malo_units_converter.o 	 \
	malo_upgrid.optest_o     \
	malo_utc_to_tai.o 	 \
	malo_wave_table.o	 \
	malo_write_aod1b_love.o	 \
	move_lock.o 		 \
	omct_to_spr.o 		 \
	otide_upgrid.o 		 \
	parse_aam.o 		 \
        parse_eop_conf.o 	 \
        plot_eop_fcs.o 		 \
	plot_grid_r4.o 		 \
	plot_leap_r4.o 	 	 \
	plot_region_r1.o 	 \
	plot_region_r4.o 	 \
	plot_region_coa.o 	 \
	read_geos_dgqtuv.o 	 \
	read_merranv_dqtuv.o 	 \
	read_heb.opt_o           \
	read_heb_sect.opt_o      \
	read_htw_heb.o           \
	read_dqt_heb.o           \
	read_dqtuv_heb.o         \
	read_geos_flux.o         \
	read_geos_lnd.o          \
	read_geos_rad.o          \
	read_geos_trg.o          \
	read_geos_slv.o          \
	read_love.o 	 	 \
	read_ls_mask_nc.o 	 \
	sc_contr.opt_o 		 \
	shc_io.o		 \
	spr_read_nc.o 		 \
	spr_write_nc.o 		 \
	write_heb.o              \
	write_ls_mask_nc.o 	 \
	write_ls_mask_pgm.o

OBJ1 =                  	 \
	malo.opt_o 

OBJ2 =                  	 \
	gtopo30_assemble.o

OBJ3 =                  	 \
	gtopo30_regrid.o

OBJ4 =                  	 \
	transform_ls_pgm.o

OBJ5 =                  	 \
	spr_from_reanal.o 

OBJ6 =                  	 \
	malo_spr_mod.o 

OBJ7 =                  	 \
	malo_regrid_main.o 

OBJ8 =                  	 \
	malo_spr_zero.o

OBJ9 =                  	 \
	malo_test_main.o

OBJ10 =                  	 \
	malo_load_dif.o

OBJ11 =                  	 \
	geos57nv_to_dgqtuv.o

OBJ12 =                  	 \
	geos_flx_to_heb.o

OBJ13 =                  	 \
	geos_rad_to_heb.o

OBJ14 =                  	 \
	merranv_to_dqtuv.o

OBJ15 =                  	 \
	geos_gen_eh.opt_o

OBJ16 =                  	 \
	gen_spr.o 

OBJ17 =                  	 \
	spr_model.opt_o 

OBJ18 =                  	 \
	heb_read_example.o 

OBJ19 =                  	 \
	load_map_view.o

OBJ20 =                  	 \
	malo_loading_model.o 

OBJ21 =                  	 \
	gen_dewt.o

OBJ22 =                  	 \
	grib1_to_heb.opt_o

OBJ23 =                  	 \
	lws_gen_ls_mask.o 

OBJ24 =                  	 \
	geos_lnd_to_heb.o

OBJ25 =                  	 \
	gen_acp.o

OBJ26 =                  	 \
	geos_slv_to_heb.o

OBJ27 =                  	 \
	malo_ondemand.o

OBJ28 =                  	 \
	heb_view.o

OBJ29 =                  	 \
	loading_model_to_nc.o

OBJ30 =                  	 \
	loading_model_to_harpos.o

OBJ31 =                  	 \
	fnl_to_heb.o

OBJ32 =                  	 \
	gen_egm2008_geoid.o

OBJ33 =                  	 \
	heb_to_heb.o

OBJ34 =                  	 \
	malo_inq.o

OBJ35 =                  	 \
	malo_eph_to_monthly.o

OBJ36 =                  	 \
	geosfpit_to_dgqtuv.o

OBJ37 =                  	 \
	test_01.o

OBJ38 =                  	 \
	heb_diff.o 

OBJ39 =                  	 \
	ephedisp_to_bin.o

OBJ40 =                  	 \
	malo_upgrid_main.o 

OBJ41 =                  	 \
	ls_regrid.o 

OBJ42 =                  	 \
	pres_nc_to_heb.o

OBJ43 =                  	 \
	sta_acp.o 

OBJ44 =                  	 \
	heb_2d_sta.opt_o

OBJ45 =                  	 \
	plot_sta_table.o 

OBJ46 =                  	 \
	gen_otide.o

OBJ47 =                  	 \
	equilibrium_tide_loading.o 

OBJ48 =  			 \
	gen_toc_equil.o

OBJ49 =  			 \
	bds_add.o

OBJ50 =  			 \
	malo_latency_plot.o

OBJ51 =  			 \
	proc_stafil.o

OBJ52 =  			 \
	gen_surface_meteo.o

OBJ53 =  			 \
	loading_integral.o

OBJ54 =  			 \
	average_loading.o

OBJ55 =  			 \
	bds_plot.o

OBJ56 =  			 \
	loading_int_time_series.o

OBJ57 =  			 \
	check_sta_list.o

OBJ58 =  			 \
	meph_to_bdsp.o

OBJ59 =  			 \
	gen_aam.o

OBJ60 =  			 \
	aam_ser.o

OBJ61 =  			 \
	aam_fcs_intrp.o

OBJ62 =  			 \
	hei_gd_to_ell.o

OBJ63 =  			 \
	modis_lw_gen.o

OBJ64 =  			 \
	lw_transform.opt_o

OBJ65 =  			 \
	gen_mask.o

OBJ66 =  			 \
	mask_diff.o

OBJ67 =  			 \
	toc_big.o

OBJ68 =  			 \
	sc_gen.o

OBJ69 =  			 \
	ls_dist.opt_o

OBJ70 =  			 \
	sc_transform.o

OBJ71 =  			 \
	sc_apply.opt_o

OBJ72 =  			 \
	eop_fcs_main.o

OBJ73 =  			 \
	show_eop_fcs.o

OBJ74 =  			 \
	eop_fcs_test.o

OBJ75 =  			 \
	loading_heb_to_nc.o

OBJ76 =  			 \
	loading_nc_to_heb.o

OBJ77 =  			 \
	loading_nc_to_eph.o

OBJ78 =  			 \
	loading_nc_to_spl_heb.o

OBJ79 =  			 \
	loading_spl_heb_to_sta.opt_o

OBJ80 =                  	 \
	loading_heb_to_spl_heb.opt_o

OBJ81 =                  	 \
	gen_cont_mask.opt_o

OBJ82 =                  	 \
	get_pres_mod_main.o

OBJ83 =                  	 \
	presan.o

OBJ84 =                  	 \
	heb_smooth.optest_o

OBJ85 =                  	 \
	geosfpit_to_trg.o

OBJ86 =                  	 \
	get_trg_dens.o

OBJ87 =                  	 \
	geos_nc4_to_heb.o

OBJ88 =                  	 \
	check_listing_gaps.o

OBJ89 =  			 \
	bds_util_main.o

LIBS_SHARED =					\
	-L$(MALO_ROOT)/lib -lmalo 		\
        -L$(VTD_PREFIX)/lib -lvtd               \
        -L$(NERS_PREFIX)/lib -lners             \
        -L$(SPD_CLIENT_DIR)/lib -lspc           \
        -L$(FOURPACK_PREFIX)/lib -lfourpack     \
        -L$(PETOOLS_PREFIX)/lib -lpetools       \
        -L$(CFITSIO_PREFIX)/lib -lcfitsio       \
	-L$(W3_PREFIX)/lib -lw3 		\
        -L$(HDF5_PREFIX)/lib -lhdf5 -lhdf5_hl   \
        -L$(HDF4_PREFIX)/lib -lmfhdf -ldf       \
	-L$(NETCDF_PREFIX)/lib -lnetcdff -lnetcdf \
	$(FFTW_LIB) 				\
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_M) 				\
        -lz -lreadline -lncurses

LIBS_STATIC =					\
	$(MALO_ROOT)/lib/libmalo.a 		\
	$(MALO_PREFIX)/lib/libvtd.a 		\
	$(MALO_PREFIX)/lib/libfitslib.a 	\
	$(MALO_PREFIX)/lib/libspc.a 		\
	$(MALO_PREFIX)/lib/libfourpack.a 	\
        $(MALO_PREFIX)/lib/libners.a            \
        $(MALO_PREFIX)/lib/libpetools.a         \
        $(MALO_PREFIX)/lib/libcfitsio.a         \
	$(FFTW_LIB) 				\
	-L$(W3_PREFIX)/lib -lw3 		\
        -L$(HDF5_PREFIX)/lib -lhdf5 -lhdf5_hl   \
        -L$(HDF4_PREFIX)/lib -lmfhdf -ldf       \
	-L$(NETCDF_PREFIX)/lib -lnetcdff -lnetcdf \
        $(MALO_PREFIX)/lib/libpgplot.a          \
	$(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_M) 				\
        -lz -lreadline -lncurses -ljpeg -ltirpc

static: LIBS     = $(LIBS_STATIC)
static: MALO_BIN = $(MALO_ROOT)/bin_static
static: all

shared: LIBS = $(LIBS_SHARED)
shared: MALO_BIN = $(MALO_ROOT)/bin
shared: all

All: 	loading_heb_to_nc
all: 	eop_fcs_test malo check_sta_list plot_sta_table gtopo30_assemble \
        transform_ls_pgm gtopo30_regrid spr_from_reanal malo_spr_mod malo_regrid \
        malo_spr_zero malo_test malo_load_dif geos57nv_to_dgqtuv geos_flx_to_heb \
        geos_rad_to_heb merranv_to_dqtuv geos_gen_eh gen_spr spr_model heb_read_example \
        load_map_view malo_loading_model gen_dewt grib1_to_heb lws_gen_ls_mask \
        geos_lnd_to_heb gen_acp geos_slv_to_heb malo_ondemand heb_view \
        loading_model_to_nc loading_model_to_harpos fnl_to_heb gen_egm2008_geoid \
        heb_to_heb malo_inq malo_eph_to_monthly geosfpit_to_dgqtuv test_01 heb_diff \
        malo_upgrid ls_regrid pres_nc_to_heb heb_2d_sta sta_acp ephedisp_to_bin \
        gen_otide equilibrium_tide_loading gen_toc_equil bds_add malo_latency_plot \
        proc_stafil gen_surface_meteo loading_integral average_loading \
        loading_int_time_series bds_plot check_sta_list gen_aam meph_to_bdsp \
        aam_ser aam_fcs_intrp hei_gd_to_ell modis_lw_gen lw_transform \
        gen_mask mask_diff toc_big sc_apply sc_gen ls_dist sc_transform \
        eop_fcs show_eop_fcs eop_fcs_test show_eop_fcs \
        sc_gen loading_heb_to_nc loading_nc_to_heb loading_nc_to_eph \
        loading_nc_to_spl_heb loading_spl_heb_to_sta loading_heb_to_spl_heb \
        gen_cont_mask get_pres_mod heb_smooth geosfpit_to_trg get_trg_dens \
        geos_nc4_to_heb check_listing_gaps bds_util  s1_hps_filter.py

malo_lib: $(OBJS)
	ar  r $(LIB).a $(OBJS)
        ifeq ($(MALO_OS),Linux)
		$(MK5_C_LINK) -shared -Wl,-soname,$(MALO_SOLIB)  \
                              -o $(MALO_ROOT)/lib/$(MALO_SOLIB).1.0  $(OBJS)
		if ( -f  $(MALO_ROOT)/lib/$(MALO_SOLIB).1 ) rm $(MALO_ROOT)/lib/$(MALO_SOLIB).1
		if ( -f  $(MALO_ROOT)/lib/$(MALO_SOLIB)   ) rm $(MALO_ROOT)/lib/$(MALO_SOLIB)
		cd $(MALO_ROOT)/lib; ln -sf $(MALO_SOLIB).1.0   $(MALO_SOLIB).1
		cd $(MALO_ROOT)/lib; ln -sf $(MALO_SOLIB).1.0   $(MALO_SOLIB)
        endif
        ifeq ($(MALO_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(MALO_ROOT)/lib/$(MALO_SOLIB) \
                            -o $(MALO_ROOT)/lib/$(MALO_SOLIB)  $(OBJS) $(LIBS_SHARED)
        endif
	@echo '--- Library $(LIB) $(MALO_SOLIB) updated ---'

malo:	$(OBJ1) $(OBJ1) 
	$(MK5_LINK) -o $(MALO_BIN)/malo             $(OBJ1) $(LIBS);

gtopo30_assemble: 	$(OBJ2) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gtopo30_assemble $(OBJ2) $(LIBS);

gtopo30_regrid: 	$(OBJ3) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gtopo30_regrid   $(OBJ3) $(LIBS);

transform_ls_pgm:	$(OBJ4) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/transform_ls_pgm     $(OBJ4) $(LIBS);

spr_from_reanal: 	$(OBJ5) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/spr_from_reanal  $(OBJ5) $(LIBS);

malo_spr_mod: 		$(OBJ6) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_spr_mod     $(OBJ6) $(LIBS);

malo_regrid: 		$(OBJ7) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_regrid      $(OBJ7) $(LIBS);

malo_spr_zero: 		$(OBJ8) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_spr_zero    $(OBJ8) $(LIBS);

malo_test: 		$(OBJ9) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_test        $(OBJ9) $(LIBS);

malo_load_dif: 		$(OBJ10) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_load_dif    $(OBJ10) $(LIBS);

geos57nv_to_dgqtuv: 	$(OBJ11) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos57nv_to_dgqtuv $(OBJ11) $(LIBS);

geos_flx_to_heb: 	$(OBJ12) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_flx_to_heb  $(OBJ12) $(LIBS);

geos_rad_to_heb: 	$(OBJ13) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_rad_to_heb  $(OBJ13) $(LIBS);

merranv_to_dqtuv: 	$(OBJ14) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/merranv_to_dqtuv $(OBJ14) $(LIBS);

geos_gen_eh:		$(OBJ15) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_gen_eh      $(OBJ15) $(LIBS);

gen_spr: 		$(OBJ16) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_spr       	 $(OBJ16) $(LIBS);

spr_model: 		$(OBJ17) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/spr_model        $(OBJ17) $(LIBS);

heb_read_example: 	$(OBJ18) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_read_example $(OBJ18) $(LIBS);

load_map_view:		$(OBJ19) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/load_map_view 	 $(OBJ19) $(LIBS);

malo_loading_model: 	$(OBJ20) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_loading_model $(OBJ20) $(LIBS);

gen_dewt: 		$(OBJ21) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_dewt       	 $(OBJ21) $(LIBS);

grib1_to_heb: 		$(OBJ22) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/grib1_to_heb     $(OBJ22) $(LIBS);

lws_gen_ls_mask: 	$(OBJ23) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/lws_gen_ls_mask   $(OBJ23) $(LIBS);

geos_lnd_to_heb:	$(OBJ24) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_lnd_to_heb  $(OBJ24) $(LIBS);

gen_acp:		$(OBJ25) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_acp          $(OBJ25) $(LIBS);

geos_slv_to_heb:	$(OBJ26) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_slv_to_heb  $(OBJ26) $(LIBS);

malo_ondemand:		$(OBJ27) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_ondemand $(OBJ27) $(LIBS);

heb_view: 		$(OBJ28) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_view $(OBJ28) $(LIBS);

loading_model_to_nc: $(OBJ29) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_model_to_nc $(OBJ29) $(LIBS);

loading_model_to_harpos: $(OBJ30) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_model_to_harpos $(OBJ30) $(LIBS);

fnl_to_heb:	         $(OBJ31) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/fnl_to_heb $(OBJ31) $(LIBS);

gen_egm2008_geoid: 	 $(OBJ32) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_egm2008_geoid $(OBJ32) $(LIBS);

heb_to_heb: 		$(OBJ33) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_to_heb $(OBJ33) $(LIBS);

malo_inq:	$(OBJ34) 
	$(MK5_LINK) -o $(MALO_BIN)/malo_inq $(OBJ34);

malo_eph_to_monthly: 	$(OBJ35)
	$(MK5_LINK) -o $(MALO_BIN)/malo_eph_to_monthly $(OBJ35) $(LIBS);

geosfpit_to_dgqtuv: 	$(OBJ36) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geosfpit_to_dgqtuv $(OBJ36) $(LIBS);

test_01:		$(OBJ37) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/test_01 $(OBJ37) $(LIBS);

heb_diff:		$(OBJ38) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_diff $(OBJ38) $(LIBS);

ephedisp_to_bin:	$(OBJ39) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/ephedisp_to_bin $(OBJ39) $(LIBS);

malo_upgrid:		$(OBJ40) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_upgrid $(OBJ40) $(LIBS);

ls_regrid:		$(OBJ41) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/ls_regrid $(OBJ41) $(LIBS);

pres_nc_to_heb:		$(OBJ42) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/pres_nc_to_heb $(OBJ42) $(LIBS);

sta_acp: 		$(OBJ43) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/sta_acp $(OBJ43) $(LIBS);

heb_2d_sta: 		$(OBJ44) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_2d_sta $(OBJ44) $(LIBS);

plot_sta_table: 	$(OBJ45) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/plot_sta_table $(OBJ45) $(LIBS);

gen_otide:		$(OBJ46) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_otide $(OBJ46) $(LIBS);

equilibrium_tide_loading:	$(OBJ47) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/equilibrium_tide_loading $(OBJ47) $(LIBS);

gen_toc_equil: 		$(OBJ48) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_toc_equil $(OBJ48) $(LIBS);

bds_add: 		$(OBJ49) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/bds_add     $(OBJ49) $(LIBS);

malo_latency_plot: 	$(OBJ50) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/malo_latency_plot $(OBJ50) $(LIBS);

proc_stafil: 		$(OBJ51) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/proc_stafil $(OBJ51) $(LIBS);

gen_surface_meteo: 	$(OBJ52) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_surface_meteo $(OBJ52) $(LIBS);

loading_integral: 	$(OBJ53) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_integral $(OBJ53) $(LIBS);

average_loading: 	$(OBJ54) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/average_loading $(OBJ54) $(LIBS);

bds_plot: 		$(OBJ55) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/bds_plot $(OBJ55) $(LIBS);

loading_int_time_series:	$(OBJ56) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_int_time_series $(OBJ56) $(LIBS);

check_sta_list: 	$(OBJ57) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/check_sta_list $(OBJ57) $(LIBS);

meph_to_bdsp:		$(OBJ58) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/meph_to_bdsp $(OBJ58) $(LIBS);

gen_aam:		$(OBJ59) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_aam $(OBJ59) $(LIBS);

aam_ser:		$(OBJ60) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/aam_ser $(OBJ60) $(LIBS);

aam_fcs_intrp:		$(OBJ61) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/aam_fcs_intrp   $(OBJ61) $(LIBS);

hei_gd_to_ell:		$(OBJ62) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/hei_gd_to_ell $(OBJ62) $(LIBS);

modis_lw_gen: $(OBJ63) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/modis_lw_gen $(OBJ63) $(LIBS);

lw_transform: $(OBJ64) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/lw_transform $(OBJ64) $(LIBS);

gen_mask:		$(OBJ65) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_mask $(OBJ65) $(LIBS);

mask_diff:		$(OBJ66) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/mask_diff $(OBJ66) $(LIBS);

toc_big:		$(OBJ67) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/toc_big $(OBJ67) $(LIBS);

sc_gen:			$(OBJ68) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/sc_gen $(OBJ68) $(LIBS);

ls_dist:		$(OBJ69) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/ls_dist $(OBJ69) $(LIBS);

sc_transform:		$(OBJ70) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/sc_transform $(OBJ70) $(LIBS);

sc_apply:		$(OBJ71) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/sc_apply     $(OBJ71) $(LIBS);

eop_fcs:		$(OBJ72) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/eop_fcs      $(OBJ72) $(LIBS);

show_eop_fcs:		$(OBJ73) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/show_eop_fcs  $(OBJ73) $(LIBS);

eop_fcs_test: 		$(OBJ74) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/eop_fcs_test $(OBJ74)  $(LIBS);

loading_heb_to_nc:  	$(OBJ75) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_heb_to_nc      $(OBJ75) $(LIBS);

loading_nc_to_heb:  	$(OBJ76) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_nc_to_heb      $(OBJ76) $(LIBS);

loading_nc_to_eph:  	$(OBJ77) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_nc_to_eph      $(OBJ77) $(LIBS);

loading_nc_to_spl_heb:  $(OBJ78) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_nc_to_spl_heb  $(OBJ78) $(LIBS);

loading_spl_heb_to_sta:  	$(OBJ79) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_spl_heb_to_sta $(OBJ79) $(LIBS);

loading_heb_to_spl_heb:  	$(OBJ80) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/loading_heb_to_spl_heb $(OBJ80) $(LIBS);

gen_cont_mask:  	$(OBJ81) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/gen_cont_mask $(OBJ81) $(LIBS);

get_pres_mod:  		$(OBJ82) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/get_pres_mod $(OBJ82) $(LIBS);

presan:  		$(OBJ83) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/presan        $(OBJ83) $(LIBS);

heb_smooth:  		$(OBJ84) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/heb_smooth    $(OBJ84) $(LIBS);

geosfpit_to_trg:  	$(OBJ85) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geosfpit_to_trg $(OBJ85) $(LIBS);

get_trg_dens: 		$(OBJ86) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/get_trg_dens $(OBJ86) $(LIBS);

geos_nc4_to_heb:	$(OBJ87) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/geos_nc4_to_heb $(OBJ87) $(LIBS);

check_listing_gaps: 	$(OBJ88) $(OBJ)
	$(MK5_LINK) -o $(MALO_BIN)/check_listing_gaps $(OBJ88) $(LIBS);

bds_util: 		$(OBJ89) $(OBJS)
	$(MK5_LINK) -o $(MALO_BIN)/bds_util     $(OBJ89) $(LIBS);


s1_hps_filter.py:
	cp $(MALO_ROOT)/support/s1_hps_filter.py $(MALO_BIN)

$(OBJS): 		$(MALO_ROOT)/include/malo.i
$(OBJ1): 		$(MALO_ROOT)/include/malo.i
$(OBJ2): 		$(MALO_ROOT)/include/malo.i
$(OBJ3): 		$(MALO_ROOT)/include/malo.i
$(OBJ4): 		$(MALO_ROOT)/include/malo.i
$(OBJ5): 		$(MALO_ROOT)/include/malo.i
$(OBJ6): 		$(MALO_ROOT)/include/malo.i
$(OBJ7): 		$(MALO_ROOT)/include/malo.i
$(OBJ8): 		$(MALO_ROOT)/include/malo.i
$(OBJ9): 		$(MALO_ROOT)/include/malo.i
$(OBJ10): 		$(MALO_ROOT)/include/malo.i
$(OBJ11): 		$(MALO_ROOT)/include/malo.i
$(OBJ12): 		$(MALO_ROOT)/include/malo.i
$(OBJ13): 		$(MALO_ROOT)/include/malo.i
$(OBJ14): 		$(MALO_ROOT)/include/malo.i
$(OBJ15): 		$(MALO_ROOT)/include/malo.i
$(OBJ16): 		$(MALO_ROOT)/include/malo.i
$(OBJ17): 		$(MALO_ROOT)/include/malo.i
$(OBJ18): 		$(MALO_ROOT)/include/malo.i
$(OBJ19): 		$(MALO_ROOT)/include/malo.i
$(OBJ20): 		$(MALO_ROOT)/include/malo.i
$(OBJ21): 		$(MALO_ROOT)/include/malo.i
$(OBJ22): 		$(MALO_ROOT)/include/malo.i
$(OBJ23): 		$(MALO_ROOT)/include/malo.i
$(OBJ24): 		$(MALO_ROOT)/include/malo.i
$(OBJ25): 		$(MALO_ROOT)/include/malo.i
$(OBJ26): 		$(MALO_ROOT)/include/malo.i
$(OBJ27): 		$(MALO_ROOT)/include/malo.i
$(OBJ28): 		$(MALO_ROOT)/include/malo.i
$(OBJ29): 		$(MALO_ROOT)/include/malo.i
$(OBJ30): 		$(MALO_ROOT)/include/malo.i
$(OBJ31): 		$(MALO_ROOT)/include/malo.i
$(OBJ32): 		$(MALO_ROOT)/include/malo.i
$(OBJ35): 		$(MALO_ROOT)/include/malo.i
$(OBJ36): 		$(MALO_ROOT)/include/malo.i
$(OBJ37): 		$(MALO_ROOT)/include/malo.i
$(OBJ38): 		$(MALO_ROOT)/include/malo.i
$(OBJ39): 		$(MALO_ROOT)/include/malo.i
$(OBJ40): 		$(MALO_ROOT)/include/malo.i
$(OBJ41): 		$(MALO_ROOT)/include/malo.i
$(OBJ42): 		$(MALO_ROOT)/include/malo.i
$(OBJ43): 		$(MALO_ROOT)/include/malo.i
$(OBJ44): 		$(MALO_ROOT)/include/malo.i
$(OBJ46): 		$(MALO_ROOT)/include/malo.i
$(OBJ47): 		$(MALO_ROOT)/include/malo.i
$(OBJ48): 		$(MALO_ROOT)/include/malo.i
$(OBJ49): 		$(MALO_ROOT)/include/malo.i
$(OBJ50): 		$(MALO_ROOT)/include/malo.i
$(OBJ52): 		$(MALO_ROOT)/include/malo.i
$(OBJ53): 		$(MALO_ROOT)/include/malo.i
$(OBJ54): 		$(MALO_ROOT)/include/malo.i
$(OBJ55): 		$(MALO_ROOT)/include/malo.i
$(OBJ56): 		$(MALO_ROOT)/include/malo.i
$(OBJ57): 		$(MALO_ROOT)/include/malo.i
$(OBJ58): 		$(MALO_ROOT)/include/malo.i
$(OBJ59): 		$(MALO_ROOT)/include/malo.i
$(OBJ60): 		$(MALO_ROOT)/include/malo.i
$(OBJ61): 		$(MALO_ROOT)/include/malo.i
$(OBJ62): 		$(MALO_ROOT)/include/malo.i
$(OBJ63): 		$(MALO_ROOT)/include/malo.i
$(OBJ64): 		$(MALO_ROOT)/include/malo.i
$(OBJ65): 		$(MALO_ROOT)/include/malo.i
$(OBJ66): 		$(MALO_ROOT)/include/malo.i
$(OBJ67): 		$(MALO_ROOT)/include/malo.i
$(OBJ68): 		$(MALO_ROOT)/include/malo.i
$(OBJ69): 		$(MALO_ROOT)/include/malo.i
$(OBJ71): 		$(MALO_ROOT)/include/malo.i
$(OBJ72): 		$(MALO_ROOT)/include/malo.i
$(OBJ73): 		$(MALO_ROOT)/include/malo.i
$(OBJ74): 		$(MALO_ROOT)/include/malo.i
$(OBJ75): 		$(MALO_ROOT)/include/malo.i
$(OBJ76): 		$(MALO_ROOT)/include/malo.i
$(OBJ77): 		$(MALO_ROOT)/include/malo.i
$(OBJ78): 		$(MALO_ROOT)/include/malo.i
$(OBJ79): 		$(MALO_ROOT)/include/malo.i
$(OBJ85): 		$(MALO_ROOT)/include/malo.i
$(OBJ86): 		$(MALO_ROOT)/include/malo.i
$(OBJ87): 		$(MALO_ROOT)/include/malo.i

$(OBJS): 		$(MALO_ROOT)/include/heb.i
$(OBJ11): 		$(MALO_ROOT)/include/heb.i
$(OBJ12): 		$(MALO_ROOT)/include/heb.i
$(OBJ13): 		$(MALO_ROOT)/include/heb.i
$(OBJ14): 		$(MALO_ROOT)/include/heb.i
$(OBJ15): 		$(MALO_ROOT)/include/heb.i
$(OBJ16): 		$(MALO_ROOT)/include/heb.i
$(OBJ17): 		$(MALO_ROOT)/include/heb.i
$(OBJ18): 		$(MALO_ROOT)/include/heb.i
$(OBJ19): 		$(MALO_ROOT)/include/heb.i
$(OBJ20): 		$(MALO_ROOT)/include/heb.i
$(OBJ21): 		$(MALO_ROOT)/include/heb.i
$(OBJ22): 		$(MALO_ROOT)/include/heb.i
$(OBJ23): 		$(MALO_ROOT)/include/heb.i
$(OBJ24): 		$(MALO_ROOT)/include/heb.i
$(OBJ25): 		$(MALO_ROOT)/include/heb.i
$(OBJ26): 		$(MALO_ROOT)/include/heb.i
$(OBJ28): 		$(MALO_ROOT)/include/heb.i
$(OBJ29): 		$(MALO_ROOT)/include/heb.i
$(OBJ30): 		$(MALO_ROOT)/include/heb.i
$(OBJ31): 		$(MALO_ROOT)/include/heb.i
$(OBJ32): 		$(MALO_ROOT)/include/heb.i
$(OBJ33): 		$(MALO_ROOT)/include/heb.i
$(OBJ42): 		$(MALO_ROOT)/include/heb.i
$(OBJ43): 		$(MALO_ROOT)/include/heb.i
$(OBJ44): 		$(MALO_ROOT)/include/heb.i
$(OBJ46): 		$(MALO_ROOT)/include/heb.i
$(OBJ47): 		$(MALO_ROOT)/include/heb.i
$(OBJ48): 		$(MALO_ROOT)/include/heb.i
$(OBJ51): 		$(MALO_ROOT)/include/heb.i
$(OBJ52): 		$(MALO_ROOT)/include/heb.i
$(OBJ54): 		$(MALO_ROOT)/include/heb.i
$(OBJ63): 		$(MALO_ROOT)/include/heb.i
$(OBJ64): 		$(MALO_ROOT)/include/heb.i
$(OBJ65): 		$(MALO_ROOT)/include/heb.i
$(OBJ66): 		$(MALO_ROOT)/include/heb.i
$(OBJ67): 		$(MALO_ROOT)/include/heb.i
$(OBJ68): 		$(MALO_ROOT)/include/heb.i
$(OBJ69): 		$(MALO_ROOT)/include/heb.i
$(OBJ70): 		$(MALO_ROOT)/include/heb.i
$(OBJ71): 		$(MALO_ROOT)/include/heb.i
$(OBJ74): 		$(MALO_ROOT)/include/heb.i
$(OBJ78): 		$(MALO_ROOT)/include/heb.i
$(OBJ79): 		$(MALO_ROOT)/include/heb.i
$(OBJ80): 		$(MALO_ROOT)/include/heb.i
$(OBJ82): 		$(MALO_ROOT)/include/heb.i
$(OBJ84): 		$(MALO_ROOT)/include/heb.i
$(OBJ85): 		$(MALO_ROOT)/include/heb.i
$(OBJ86): 		$(MALO_ROOT)/include/heb.i
$(OBJ87): 		$(MALO_ROOT)/include/heb.i

$(OBJ34): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ35): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ50): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ56): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ59): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ66): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ68): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ74): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ75): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ76): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ77): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ78): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ79): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ81): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ85): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ86): 		$(MALO_ROOT)/include/malo_local.i
$(OBJ87): 		$(MALO_ROOT)/include/malo_local.i

$(OBJS): 		$(NERS_INC)/ners.i
$(OBJ72): 		$(NERS_INC)/ners.i
$(OBJ73): 		$(NERS_INC)/ners.i
$(OBJ74): 		$(NERS_INC)/ners.i

$(OBJ89): 		$(MALO_ROOT)/include/bindisp.i

distclean: 	clean

clean: MALO_BIN = $(MALO_ROOT)/bin_static
clean: clean_all
clean: MALO_BIN = $(MALO_ROOT)/bin
clean: clean_all

clean_static: MALO_BIN = $(MALO_ROOT)/bin_static
clean_static: clean_all

clean_shared: MALO_BIN = $(MALO_ROOT)/bin
clean_shared: clean_all

clean_all:
	rm -f $(OBJS)  					\
	      $(OBJ1)  					\
	      $(OBJ2)  					\
	      $(OBJ3)  					\
	      $(OBJ4)  					\
	      $(OBJ5)  					\
	      $(OBJ6)  					\
	      $(OBJ7)  					\
	      $(OBJ8)  					\
	      $(OBJ9)  					\
	      $(OBJ10)  				\
	      $(OBJ11)  				\
	      $(OBJ12)  				\
	      $(OBJ13)  				\
	      $(OBJ14)  				\
	      $(OBJ15)  				\
	      $(OBJ16)  				\
	      $(OBJ17)  				\
	      $(OBJ18)  				\
	      $(OBJ19)  				\
	      $(OBJ20)  				\
	      $(OBJ21)  				\
	      $(OBJ22)  				\
	      $(OBJ23)  				\
	      $(OBJ24)  				\
	      $(OBJ25)  				\
	      $(OBJ26)  				\
	      $(OBJ27)  				\
	      $(OBJ28)  				\
	      $(OBJ29)  				\
	      $(OBJ30)  				\
	      $(OBJ31)  				\
	      $(OBJ32)  				\
	      $(OBJ33)  				\
	      $(OBJ34)  				\
	      $(OBJ35)  				\
	      $(OBJ36)  				\
	      $(OBJ37)  				\
	      $(OBJ38)  				\
	      $(OBJ39)  				\
	      $(OBJ40)  				\
	      $(OBJ41)  				\
	      $(OBJ42)  				\
	      $(OBJ43)  				\
	      $(OBJ44)  				\
	      $(OBJ45)  				\
	      $(OBJ46)  				\
	      $(OBJ47)  				\
	      $(OBJ48)  				\
	      $(OBJ49)  				\
	      $(OBJ50)  				\
	      $(OBJ51)  				\
	      $(OBJ52)  				\
	      $(OBJ53)  				\
	      $(OBJ54)  				\
	      $(OBJ55)  				\
	      $(OBJ56)  				\
	      $(OBJ57)  				\
	      $(OBJ58)  				\
	      $(OBJ59)  				\
	      $(OBJ60)  				\
	      $(OBJ61)  				\
	      $(OBJ62)  				\
	      $(OBJ63)  				\
	      $(OBJ64)  				\
	      $(OBJ65)  				\
	      $(OBJ66)  				\
	      $(OBJ67)  				\
	      $(OBJ68)  				\
	      $(OBJ69)  				\
	      $(OBJ70)  				\
	      $(OBJ71)  				\
	      $(OBJ72)  				\
	      $(OBJ73)  				\
	      $(OBJ74)  				\
	      $(OBJ75)  				\
	      $(OBJ76)  				\
	      $(OBJ77)  				\
	      $(OBJ78)  				\
	      $(OBJ79)  				\
	      $(OBJ80)  				\
	      $(OBJ81)  				\
	      $(OBJ82)  				\
	      $(OBJ83)  				\
	      $(OBJ84)  				\
	      $(OBJ85)  				\
	      $(OBJ86)  				\
	      $(OBJ87)  				\
	      $(OBJ88)  				\
	      $(OBJ89)  				

	rm -f $(MALO_BIN)/malo 		 		\
	      $(MALO_BIN)/malo_inq                      \
	      $(MALO_BIN)/gtopo30_assemble 		\
	      $(MALO_BIN)/gtopo3g0_regrid 		\
	      $(MALO_BIN)/transform_ls_pgm         	\
	      $(MALO_BIN)/spr_from_reanal 		\
	      $(MALO_BIN)/malo_spr_mod 			\
	      $(MALO_BIN)/malo_regrid 			\
	      $(MALO_BIN)/malo_spr_zero 		\
	      $(MALO_BIN)/malo_test 			\
	      $(MALO_BIN)/malo_load_dif 		\
	      $(MALO_BIN)/geos57nv_to_dgqtuv 		\
	      $(MALO_BIN)/geos_flx_to_heb 		\
	      $(MALO_BIN)/geos_rad_to_heb 		\
	      $(MALO_BIN)/geos_slv_to_heb 		\
	      $(MALO_BIN)/gtopo30_regrid                \
	      $(MALO_BIN)/merranv_to_dqtuv 		\
	      $(MALO_BIN)/geos_gen_eh 			\
	      $(MALO_BIN)/gen_spr 			\
	      $(MALO_BIN)/spr_model 			\
	      $(MALO_BIN)/heb_read_example 		\
	      $(MALO_BIN)/load_map_view 		\
	      $(MALO_BIN)/malo_loading_model 		\
	      $(MALO_BIN)/gen_dewt 			\
	      $(MALO_BIN)/grib1_to_heb 			\
	      $(MALO_BIN)/lws_gen_ls_mask 		\
	      $(MALO_BIN)/geos_lnd_to_heb 		\
	      $(MALO_BIN)/gen_acp 			\
	      $(MALO_BIN)/malo_ondemand 		\
	      $(MALO_BIN)/heb_view 			\
	      $(MALO_BIN)/loading_model_to_nc      	\
	      $(MALO_BIN)/loading_model_to_harpos  	\
	      $(MALO_BIN)/malo_eph_to_monthly           \
	      $(MALO_BIN)/fnl_to_heb 			\
	      $(MALO_BIN)/gen_egm2008_geoid 		\
	      $(MALO_BIN)/heb_to_heb 			\
	      $(MALO_BIN)/heb_diff 			\
	      $(MALO_BIN)/geosfpit_to_dgqtuv 		\
	      $(MALO_BIN)/ephedisp_to_bin 		\
	      $(MALO_BIN)/malo_upgrid 			\
	      $(MALO_BIN)/ls_regrid 			\
	      $(MALO_BIN)/pres_nc_to_heb 		\
	      $(MALO_BIN)/sta_acp 			\
	      $(MALO_BIN)/heb_2d_sta 			\
	      $(MALO_BIN)/plot_sta_table 		\
	      $(MALO_BIN)/gen_otide 			\
	      $(MALO_BIN)/equilibrium_tide_loading 	\
	      $(MALO_BIN)/gen_toc_equil                 \
	      $(MALO_BIN)/bds_add 			\
	      $(MALO_BIN)/malo_latency_plot  		\
	      $(MALO_BIN)/proc_stafil  	        	\
	      $(MALO_BIN)/gen_surface_meteo		\
	      $(MALO_BIN)/loading_integral		\
	      $(MALO_BIN)/average_loading	   	\
	      $(MALO_BIN)/loading_int_time_series  	\
	      $(MALO_BIN)/bds_plot		   	\
	      $(MALO_BIN)/check_sta_list           	\
	      $(MALO_BIN)/meph_to_bdsp             	\
	      $(MALO_BIN)/gen_aam                  	\
	      $(MALO_BIN)/aam_ser                  	\
	      $(MALO_BIN)/aam_fcs_intrp            	\
	      $(MALO_BIN)/hei_gd_to_ell            	\
	      $(MALO_BIN)/modis_lw_gen            	\
	      $(MALO_BIN)/lw_transform            	\
	      $(MALO_BIN)/gen_mask                 	\
	      $(MALO_BIN)/mask_diff                	\
	      $(MALO_BIN)/toc_big                  	\
	      $(MALO_BIN)/sc_gen                   	\
	      $(MALO_BIN)/ls_dist                  	\
	      $(MALO_BIN)/sc_transform             	\
	      $(MALO_BIN)/sc_apply                 	\
	      $(MALO_BIN)/eop_fcs                  	\
	      $(MALO_BIN)/show_eop_fcs             	\
	      $(MALO_BIN)/eop_fcs_test             	\
	      $(MALO_BIN)/loading_heb_to_nc        	\
	      $(MALO_BIN)/loading_nc_to_heb        	\
	      $(MALO_BIN)/loading_nc_to_eph        	\
	      $(MALO_BIN)/loading_nc_to_spl_heb    	\
	      $(MALO_BIN)/loading_spl_heb_to_sta   	\
	      $(MALO_BIN)/loading_heb_to_spl_heb   	\
	      $(MALO_BIN)/gen_cont_mask                 \
	      $(MALO_BIN)/test_01                  	\
	      $(MALO_BIN)/get_pres_mod                  \
	      $(MALO_BIN)/presan                  	\
	      $(MALO_BIN)/heb_smooth                  	\
	      $(MALO_BIN)/geosfpit_to_trg               \
	      $(MALO_BIN)/get_trg_dens 			\
	      $(MALO_BIN)/geos_nc4_to_heb               \
	      $(MALO_BIN)/check_listing_gaps 		\
	      $(MALO_BIN)/bds_util                      \
	      $(MALO_BIN)/s1_hps_filter.py

	rm -f $(MALO_ROOT)/lib/libmalo.a
        ifeq ($(MALO_OS),Linux)
	      rm -f $(MALO_ROOT)/lib/libmalo.so.1.0        \
	            $(MALO_ROOT)/lib/libmalo.so.1          \
	            $(MALO_ROOT)/lib/libmalo.so; exit 0
        endif
        ifeq ($(MALO_OS),Darwin)
	      rm -f $(MALO_ROOT)/lib/libmalo.1.dylib       \
	            $(MALO_ROOT)/lib/libmalo.dylib; exit 0
        endif
	@echo '--- Cleaned $(MALO_ROOT)'
