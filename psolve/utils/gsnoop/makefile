# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  gsnoop  executable            *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(MK5_ROOT)/bin
SUPPORT = $(MK5_ROOT)/support

OBJS 	=               		\
		as2nl.o 		\
		asnl.o 			\
		clrputstring.o 		\
		cons_levs.o 		\
		cplstat.o 		\
		default_epoch.o 	\
		deg_breakdown.o 	\
		epoch_form95.o 		\
		file_control.o 		\
		fit_line.o 		\
		gconst_gn.o 		\
		get_arc_info.o 		\
		get_cat_file.o 		\
		get_control_info.o 	\
		get_covn_files.o 	\
		get_file_gen.o 		\
		get_glob_source.o 	\
		get_gsnoop_envs.o 	\
		get_spool_list.o 	\
		get_stat_names.o 	\
		gflags_gn.o 		\
		global_info.o 		\
		gsn_main.o 		\
		gsn_menu1.o 		\
		gsn_menu1_1.o 		\
		gsn_menu1_2.o 		\
		gsnoop.o 		\
		gsuprs_gn.o 		\
		gvelcnst.o 		\
		hyper.o 		\
		id_cov_type.o 		\
		linft.o 		\
		make_cfile.o 		\
		make_dfile.o 		\
		make_plot.o 		\
		min_sig_out.o 		\
		next_nonblank.o 	\
		open_files.o 		\
		open_plot_files.o 	\
		output_annual.o 	\
		output_source.o 	\
		parminfo.o 		\
		plot_menu1.o 		\
		plot_scale.o 		\
		plot_uen.o 		\
		gsn_putchar.o 		\
		putstring.o 		\
		qant_type.o 		\
		qsitpl.o 		\
		quitter.o 		\
		read_data.o 		\
		return_to_continue.o 	\
		search_spool_list.o 	\
		sel_dev.o 		\
		site_id_sect.o 		\
		site_sort_prep.o 	\
		snx_head_hst.o 		\
		solest.o 		\
		solmat.o 		\
		solstat_sect.o 		\
		sort_snx_non.o 		\
		spool_top.o 		\
		topo_bin.o 		\
		write_cpl.o 		\
		ztime.o 

OBJ2 	= 				\
		$(MK5_ROOT)/progs/solve/batch/gstavelcnst.o	\
		$(MK5_ROOT)/progs/solve/batch/gorisp.o 		\
		$(MK5_ROOT)/progs/solve/batch/gstasp.o 		\
		$(MK5_ROOT)/progs/solve/batch/gstatie.o 	\
		$(MK5_ROOT)/progs/solve/batch/gvlosp.o 		\
		$(MK5_ROOT)/progs/solve/batch/gveltie.o 	\
		$(MK5_ROOT)/progs/solve/batch/gvelsp.o 		\
		$(MK5_ROOT)/progs/solve/batch/cfread.o 		\
		$(MK5_ROOT)/progs/solve/batch/cfeof.o 		\
		$(MK5_ROOT)/progs/solve/batch/cfunrd.o 		\
		$(MK5_ROOT)/progs/solve/batch/cfopen.o 		\
		$(MK5_ROOT)/progs/solve/batch/chkcmp.o

LIBS =						\
	$(MK5_ROOT)/libs/qcat4/qcat4.a 		\
	$(MK5_ROOT)/libs/cutil/cutil.a		\
	$(MK5_ROOT)/libs/newlib/newlib.a 	\
	$(MK5_ROOT)/libs/ftlib/ftlib.a 		\
	$(MK5_ROOT)/libs/curlib/curlib.a 	\
	$(MK5_ROOT)/libs/fclib/fclib.a 		\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(SOLVE_LIB_CURSES) 			\
	$(SOLVE_LIB_RTENSHD)                    \
	$(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ2)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/gsnoop    gsnoop.o $(OBJS) $(OBJ2) $(LIBS)

gsnoop.o:	gsnoop_version.i

clean:  
	rm -f $(OBJS) $(EXE_DIR)/gsnoop
