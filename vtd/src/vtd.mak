# ************************************************************************
# *                                                                      *
# *   Make file for compiling and linking VTD: VLBI Theoretical Delay.   *
# *                                                                      *
# *  ### 26-JAN-2004     vtd.mak   v1.1 (c)  L. Petrov  01-NOV-2004 ###  *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:      .f      .c      .o      .o_opt  .a      .d

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -I$(VTD)/include -c -o $*.o     $*.f  

.f.o_opt:
	$(MK5_F95_OPT) -I$(VTD)/include -c -o $*.o_opt $*.f  

.f.d:
	$(MK5_F95)     -I$(VTD)/include -c -o $*.o $*.f  

LIB     = vtd.a

OBJ = 				\
	de_eph.o 		\
	e3zt_dickman1993.o 	\
	e3zt_yoder1981_all.o 	\
	e3zt_yoder1981_short.o 	\
	heo_ren2000.o 		\
	heo_mhb2000.o 		\
	heo_iers1996.o 		\
	heo_wahr1980.o 		\
	heo_mhb2000_transf.o 	\
	nmf.o 			\
	nut_geodesic.o 		\
	read_heo.o 		\
	sotid_dsp.o		\
	sotid_get_love.o	\
	sotid_inq.o		\
	sotid_pre.o		\
	sotid_tai_tdb.o		\
	sotid_tim.o		\
	sotid_set.o		\
	taitdb.o 		\
	vtd_apply_heo.o 	\
	vtd_axof.o 		\
	vtd_conf.o 		\
	vtd_do.o 		\
	vtd_erm_na.o 		\
	vtd_get_posvar.o 	\
	vtd_init.o 		\
	vtd_ks1999.o 		\
	vtd_load.o 		\
	vtd_load_bindisp.o 	\
	vtd_load_harpos.o 	\
	vtd_load_leapsec.o 	\
	vtd_load_stacoo.o 	\
	vtd_load_stadesc.o 	\
	vtd_load_staecc.o 	\
	vtd_load_stavel.o 	\
	vtd_load_soucoo.o 	\
	vtd_meteo_in.o 		\
	vtd_moment.o 		\
	vtd_moment_struc.o 	\
	vtd_mpl_read.o 		\
	vtd_name_repair.o 	\
	vtd_pk2001.o 		\
	vtd_posvar_init.o 	\
	vtd_potid_dsp.o 	\
	vtd_read_struc.o 	\
        vtd_set_bindisp.o 	\
	vtd_set_harpos.o 	\
	vtd_set_posvar.o 	\
	vtd_sou_index.o 	\
	vtd_sta_index.o 	\
	vtd_struc.o 		\
	vtd_tropdel.o 		\
	vtd_trop_axof_coupl.o 	\
	vtd_trop_geom_coupl.o 	\
	vtd_ueop.o 		\
	vtd_utc_to_tai.o 	\
	zendel_saa.o 		

lib:	$(OBJ)
	ar  r  $(LIB)  $(OBJ)
	@echo '--- Library $(LIB) updated ---'

$(OBJ):	$(VTD)/include/vtd.i 		\
        $(VTD)/include/de_eph.i 	\
        $(VTD)/include/ueop.i 		\
        $(VTD)/include/sotid_type.i 	\
        $(VTD)/include/sotid_data.i

clean:
	rm -f $(OBJ) $(LIB)
