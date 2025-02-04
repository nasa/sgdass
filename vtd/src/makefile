# ************************************************************************
# *                                                                      *
# *   Make file for compiling and linking VTD: VLBI Theoretical Delay.   *
# *                                                                      *
# *  ### 26-JAN-2004     vtd.mak   v1.7 (c)  L. Petrov  15-NOV-2016 ###  *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:      .f      .c      .o      .o_opt  .a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     $(VTD_OPT_NOSTRUC) -I$(PETOOLS_PREFIX)/include -I $(SPD_CLIENT_PREFIX)/include -I $(NERS_INC) -c -o $*.o     $*.f  

.f.o_opt:
	$(MK5_F95_OPT) $(VTD_OPT_NOSTRUC) -I$(PETOOLS_PREFIX)/include -I $(SPD_CLIENT_PREFIX)/include -I $(NERS_INC) -c -c -o $*.o_opt $*.f  


LIB     = libvtd.a
ifeq ($(VTD_OS),Linux)
	SOLIB   = libvtd.so.1
endif
ifeq ($(VTD_OS),Darwin)
	SOLIB   = libvtd.1.dylib
endif

OBJ = 				\
	anti_parse.o 		\
	agd_parse.o 		\
	comp_iono_spl.o 	\
	de_eph.o 		\
	del_isa.o 		\
	e3zt_dickman1993.o 	\
	e3zt_re2014.o 		\
	e3zt_yoder1981_all.o 	\
	e3zt_yoder1981_short.o 	\
	heo_empi.o 		\
	heo_iers1996.o 		\
	heo_mhb2000.o 		\
	heo_mhb2000_transf.o 	\
	heo_ren2000.o 		\
	heo_wahr1980.o 		\
	nmf.o 			\
	nut_geodesic.o 		\
	read_aem.o 		\
	read_erm.o 		\
	read_heo.o 		\
	sotid_dsp.o		\
	sotid_get_love.o	\
	sotid_inq.o		\
	sotid_pre.o		\
	sotid_tai_tdb.o		\
	sotid_tim.o		\
	sotid_set.o		\
	spd_gaussian_layer_mf.o \
	taitdb.o 		\
	vio_get_header.o 	\
	vtd_aem.o 		\
	vtd_ant_gravity.o 	\
	vtd_ant_thermal.o 	\
	vtd_apply_heo.o 	\
	vtd_axof.o 		\
	vtd_calc_pco.o 		\
	vtd_conf.o 		\
	vtd_cootrn.o 		\
	vtd_corr_amp.o 		\
	vtd_delay.o 		\
	vtd_doppler.o 		\
	vtd_erm_na.o 		\
	vtd_gal_abr.o 		\
	vtd_get_azel.o 		\
	vtd_get_erm.o 		\
	vtd_get_posvar.o 	\
	vtd_get_uvw.o 		\
	vtd_get_iono.o 		\
	vtd_gr_del_struc.o 	\
	vtd_init.o 		\
	vtd_iono_delay.o 	\
	vtd_iono_mf.o 		\
	vtd_ks1999.o 		\
	vtd_iono_stat.o 	\
	vtd_lt_orb.o 		\
	vtd_load.o 		\
	vtd_load_bindisp.o 	\
	vtd_load_bsppos.o 	\
	vtd_load_harpos.o 	\
	vtd_load_iono.o 	\
	vtd_load_leapsec.o 	\
	vtd_load_mf.o 		\
	vtd_load_nzo.o 		\
	vtd_load_obj_nzo.o 	\
	vtd_load_spd.o 		\
	vtd_load_stacoo.o 	\
	vtd_load_stadesc.o 	\
	vtd_load_staecc.o 	\
	vtd_load_stavel.o 	\
	vtd_load_soucoo.o 	\
	vtd_load_souprl.o 	\
	vtd_meteo_in.o 		\
	vtd_mmf.o 		\
	vtd_mmf_zen.o 		\
	vtd_moment.o 		\
	vtd_moment_struc.o 	\
	vtd_name_repair.o 	\
	vtd_nut_transform.o 	\
	vtd_jn2019.o 		\
	vtd_nzo_lt.o 		\
	vtd_nzo_sf2004.o 	\
	vtd_parang.o 		\
	vtd_ph_del_struc.o 	\
	vtd_pk2001.o 		\
	vtd_posvar_init.o 	\
	vtd_potid_dsp.o 	\
	vtd_quit.o 		\
	vtd_read_nzo.o 		\
	vtd_read_bsp.o 		\
	vtd_read_antex.o  	\
	vtd_read_struc.o 	\
        vtd_rel_orb.o 		\
	vtd_rotmat.o 		\
        vtd_set_bindisp.o 	\
        vtd_set_bsppos.o 	\
	vtd_set_harpos.o 	\
	vtd_set_posvar.o 	\
	vtd_sou_debias.o 	\
	vtd_sou_index.o 	\
	vtd_sta_index.o 	\
	vtd_sta_syn_repl.o 	\
	vtd_struc.o 		\
	vtd_tropdel.o 		\
	vtd_trop_axof_coupl.o 	\
	vtd_trop_geom_coupl.o 	\
	vtd_ueop_init.o 	\
	vtd_utc_to_tai.o 	\
	vtd_version.o 		\
	write_aem.o 		\
	zendel_saa.o 

lib:	$(OBJ)
	ar  r  $(LIB)  $(OBJ)
        ifeq ($(VTD_OS),Linux)
        	ifeq ($(VTD_CFITSIO_LIB),)
			$(MK5_C_LINK) -shared -Wl,-soname,libvtd.so.1 -o libvtd.so.1.0 $(OBJ)
	        	if ( -f libvtd.so ) rm libvtd.so 
	        	ln -s libvtd.so.1.0  libvtd.so
        	else
			$(MK5_C_LINK) -shared -Wl,-soname,libvtd.so.1 -o libvtd.so.1.0 $(OBJ) \
                              -L$(VTD_ROOT)/fitslib -lfitslib
	        	if ( -f libvtd.so ) rm libvtd.so 
	        	ln -s libvtd.so.1.0  libvtd.so
        	endif
        endif
        ifeq ($(VTD_OS),Darwin)
        	ifeq ($(VTD_CFITSIO_LIB),)
	      		$(MK5_C_LINK) -dynamiclib -install_name $(VTD_ROOT)/src/libvtd.1.dylib \
                            -o libvtd.1.dylib $(OBJ) \
                            -L$(SPD_CLIENT_LIB) -lspc \
                            -L$(NERS_LIB) -lners \
                            $(PETOOLS_LIB)
        	else
	      		$(MK5_C_LINK) -dynamiclib -install_name $(VTD_ROOT)/src/libvtd.1.dylib \
                            -o libvtd.1.dylib $(OBJ) \
                            -L$(VTD_ROOT)/fitslib -lfitslib \
                            -L$(SPD_CLIENT_LIB) -lspc \
                            -L$(NERS_LIB) -lners \
                            -L$(VTD_CFITSIO_DIR) -lcfitsio \
                            $(CURL_LIB) \
                            $(PETOOLS_LIB)
        	endif
	      if ( -f libvtd.dylib ) rm libvtd.dylib 
	      ln -s libvtd.1.dylib libvtd.dylib 
        endif
	@echo '--- Library $(LIB) $(SOLIB) updated ---'

$(OBJ):	$(VTD_ROOT)/include/vtd.i 		\
        $(VTD_ROOT)/include/de440_eph.i 	\
        $(VTD_ROOT)/include/ueop.i 		\
        $(VTD_ROOT)/include/viono.i 		\
        $(VTD_ROOT)/include/sotid_type.i 	\
        $(VTD_ROOT)/include/sotid_data.i

clean:
	rm -f $(OBJ) $(LIB) libvtd.so.1.0 libvtd.1.dylib libvtd.so libvtd.dylib 
