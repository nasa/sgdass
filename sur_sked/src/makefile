# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  sur_sked executable            *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh -f -c 'umask 0002; shift; eval "$2"'

.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.a

.c.o:
	$(MK5_F95_OPT) -c -o $*.o $*.c

.f.o:
	$(MK5_F95_OPT) -I $(PETOOLS_PREFIX)/include -I $(TLE_PREFIX)/include -I ../include/ -I ../auxil/ -c -o $*.o $*.f


EXE_DIR = $(Ex)
SPC_LIB = -L/opt64/lib -lspc

SRC  =  			\
	get_lst.f 		\
	get_planet_coo.f 	\
	sur_astromet.f 		\
	sur_astro_cal.f 	\
	sur_astro_fill.f 	\
	sur_astro_first.f 	\
	sur_astro_score.f	\
	sur_astro_seq.f	 	\
	sur_astro_stat.f	\
	sur_astro_tropo.f	\
	sur_azel.f 		\
	sur_find_first.f 	\
	sur_find_seq.f 		\
	sur_get_elev_ref.f 	\
	sur_meridian.f  	\
	sur_print_res.f  	\
	sur_set_cur.f 		\
	sur_sked_main.f 	\
	sur_sked_conf.f 	\
	sur_sked_inq.f 	        \
	sur_slew_report.f 	\
	sur_slew_time.f 	\
	sur_sort.f 		\
	sur_source.f 		\
	sur_sta_conf.f 		\
	sur_transit.f


OBJS =                  		\
	get_lst.o 			\
	get_planet_coo.o 		\
	sur_astromet.o 			\
	sur_astro_cal.o 		\
	sur_astro_fill.o 		\
	sur_astro_first.o 		\
	sur_astro_score.o 		\
	sur_astro_seq.o 		\
	sur_astro_stat.o 		\
	sur_astro_tropo.o		\
	sur_azel.o 			\
	sur_check_vis.o 		\
	sur_find_first.o 		\
	sur_find_seq.o 			\
	sur_get_elev_ref.o 		\
	sur_knock_out_180_azim.o 	\
	sur_meridian.o 			\
	sur_print_res.o 		\
	sur_set_cur.o 			\
	sur_sked_conf.o 		\
	sur_sked_inq.o 		 	\
	sur_slew_report.o 		\
	sur_slew_time.o 		\
	sur_sort.o 			\
	sur_source.o 			\
	sur_sta_conf.o 			\
	sur_transit.o 			\
        ../auxil/read_sou.o

OBJ1 =                  		\
	sur_sked_main.o 		\

OBJ2 =                  		\
     	ast_to_snap.o

OBJ3 =                  		\
     	check_proc_snap.o

OBJ4 =                  		\
     	gen_seq_prc.o

OBJ5 =                  		\
     	tle_to_spind.o

OBJ6 =                  		\
     	refine_gnss_key.o

OBJ7 =                  		\
     	eval_gain.o

OBJ8 =                  		\
     	rxg_plot.o 

OBJ9 =                  		\
     	slew_adjust.o 

OBJ10 =                  		\
     	sur_sked_inq_main.o 

LIBS =  \
	$(VTD_LIB) 				\
	$(TLE_LIB) 				\
	$(SPC_LIB) 				\
	$(NERS_LIB) 				\
	$(CFITSIO_LIB)				\
	$(PETOOLS_LIB)				\
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_EXTRA_LIB)


bin: 	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8) $(OBJ9) $(OBJ10) 
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/sur_sked        $(OBJ1)  $(OBJS) $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/ast_to_snap     $(OBJ2)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/check_proc_snap $(OBJ3)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/gen_seq_prc     $(OBJ4)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/tle_to_spind    $(OBJ5)  $(OBJS) $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/refine_gnss_key $(OBJ6)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/eval_gain       $(OBJ7)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/rxg_plot        $(OBJ8)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/slew_adjust     $(OBJ9)  $(LIBS)
	$(MK5_LINK)  -o $(SUR_SKED_ROOT)/bin/sur_sked_inq    $(OBJ10) $(OBJS) $(LIBS)

$(OBJS): ../include/sur_sked.i

clean:
	rm -f $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8) $(OBJ9) $(OBJ10) $(OBJS)
	rm -f $(SUR_SKED_ROOT)/bin/sur_sked
	rm -f $(SUR_SKED_ROOT)/bin/ast_to_snap
	rm -f $(SUR_SKED_ROOT)/bin/check_proc_snap
	rm -f $(SUR_SKED_ROOT)/bin/gen_seq_prc
	rm -f $(SUR_SKED_ROOT)/bin/tle_to_spind
	rm -f $(SUR_SKED_ROOT)/bin/refine_gnss_key
	rm -f $(SUR_SKED_ROOT)/bin/eval_gain
	rm -f $(SUR_SKED_ROOT)/bin/rxg_plot
	rm -f $(SUR_SKED_ROOT)/bin/slew_adjust
	rm -f $(SUR_SKED_ROOT)/bin/sur_sked_inq
