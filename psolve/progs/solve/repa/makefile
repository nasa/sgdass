# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking repa executable                *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.noopt_o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_NOOPT) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_NOOPT) -c -o $*.opt_o $*.f  

.f.noopt_o:
	$(MK5_F95_NOOPT) -c -o $*.noopt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS =	mk4plot.o 		\
	pima_parse.o 		\
	repa_change_par.o 	\
	repa_com.o 		\
	repa_df_chmod.o 	\
	repa_df_fp.o 		\
	repa_df_friplo.o 	\
	repa_df_grpamb.o 	\
	repa_df_grptgl.o 	\
	repa_df_init.o 		\
	repa_df_inq.o 		\
	repa_df_quit.o 		\
	repa_df_setbox.o 	\
	repa_df_sngamb.o 	\
	repa_df_sngtgl.o 	\
	repa_df_undo.o 		\
	repa_df_cont.o 		\
	repa_doinq.o 		\
	repa_get_ambsp.o 	\
	repa_get_iono.o 	\
	repa_inqline.o 		\
	repa_load_obs.o 	\
	repa_load_res.o 	\
	repa_minmax.o 		\
	repa_parse_cnf.o 	\
	repa_plot_scale.o 	\
	repa_quit.o 		\
	repa_search_closest.o 	\
	repa_selsou.o 		\
	repa_setdiagi.o 	\
	repa_setfunc.o 		\
	repa_show_ms.o 		\
	repa_sngamb.o 		\
	repa_sngtgl.o 		\
	repa_updamb.o 		\
	repa_wristat.o 		

OBJM =	repa.o

OBJ2 	=               \
	$(SOLVE_ROOT)/progs/solve/elim/proc_m.o 

OBJ3 	=               \
	loc_ext.o 		

OBJ4 	=               \
	repa_int.o 	\
	repamb8.o 	\
	repbasl.o 	\
	repcabl.o 	\
	repconn.o 	\
	repepoc.o 	\
	repgood.o 	\
	rephead.o 	\
	repinni.o 	\
	repinse.o 	\
	repmvva.o 	\
	repnard.o 	\
	repobrd.o 	\
	repobwt.o 	\
	repparm.o 	\
	reprerd.o 	\
	reprewt.o 	\
	repscan.o 	\
	repsigm.o 	\
	repstat.o 	\
        repambi.o 	\
        repbott.o 	\
        repcclo.o 	\
        repcout.o 	\
        repdele.o 	\
        repexpa.o 	\
        repgrrd.o 	\
        repgrrs.o 	\
        repgrsh.o 	\
        repgrsu.o 	\
        repinfo.o 	\
        repptsh.o 	\
        repptsu.o 

OBJ5 =	repab.noopt_o 	


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

all:	repa repab

repa:	$(OBJM) $(OBJS) $(OBJ2) $(OBJ3)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/REPA  $(OBJS) $(OBJM) $(OBJ2) $(OBJ3) $(LIBS)

repab:	$(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) 
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/REPAB $(OBJ4) $(OBJ3) $(OBJ5) $(LIBS)

$(OBJS): $(SOLVE_ROOT)/include/repa.i
$(OBJM): $(SOLVE_ROOT)/include/repa.i

repab.noopt_o:	repa_version.i
repab.o:	repa_version.i

clean:  
	rm -f $(OBJS) $(OBJM) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(EXE_DIR)/REPA  $(EXE_DIR)/REPAB
