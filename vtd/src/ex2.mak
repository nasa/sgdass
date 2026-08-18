# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  dif_eop executable            *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:	.f	.c.	.o	.a	

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_OPT) -c -o $*.o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  


EXE_DIR = $(Ex)

RES     = vtd_example_11

OBJS =           \
        $(RES).o 

OBJ_LIB = 			\
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
	vtd_cootrn.o 		\
	vtd_do.o 		\
	vtd_erm_na.o 		\
	vtd_get_posvar.o 	\
	vtd_init.o 		\
	vtd_ks1999.o 		\
	vtd_load.o 		\
	vtd_load_bindisp.o 	\
	vtd_load_harpos.o 	\
	vtd_load_leapsec.o 	\
	vtd_load_nzo.o 		\
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
	vtd_nzo_lt.o 		\
	vtd_nzo_sf2004.o 	\
	vtd_pk2001.o 		\
	vtd_posvar_init.o 	\
	vtd_potid_dsp.o 	\
	vtd_read_nzo.o 		\
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


LIBS =						\
        $(MK5_ROOT)/libs/diagi/diagi.a          \
        $(MK5_ROOT)/libs/pet_util/pet_util.a	\
        $(MK5_ROOT)/libs/matvec/matvec.a        \
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
	$(SOLVE_LIB_XT)				\
	$(SOLVE_LIB_XHP11)			\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_EXTRA_LIB)

bin: 	$(OBJS) $(OBJ_LIB)
	$(MK5_LINK) -o $(EXE_DIR)/$(RES).e $(OBJS) $(OBJ_LIB) $(LIBS) ; \
        $(EXE_DIR)/$(RES).e

clean:
	rm -f $(OBJS) $(EXE_DIR)/$(RES).e
