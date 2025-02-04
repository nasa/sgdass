#######################################################################
#								      #
#	Make file for recompiling library  pet_util.a     	      #
#						     		      #
#  %%%  27-DEC-96   makefile   v2.4  (c)  L. Petrov  23-JUL-2003 %%%  #
# 								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.Opt_o	.a	.d
#
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

LIB     = pet_util.a
NUL     = /null/dev

.c.o:
	$(MK5_C) $(MK5_C_OPENMP) -c -o $*.o $*.c

.f.o:
	$(MK5_F95) $(MK5_F_OPENMP) -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT) $(MK5_F_OPENMP) -c -o $*.opt_o $*.f

.f.Opt_o:
	$(MK5_F95_OPTEST) $(MK5_F_OPENMP) -c -o $*.Opt_o $*.f

.f.d:
	$(MK5_F95) $(MK5_F_OPENMP) -c -o $*.o $*.f


OBJ	=			\
	address_align.o 	\
	alg1.opt_o		\
        atan_cs.opt_o		\
	arclen.opt_o 		\
	b1950_to_j2000.o 	\
	binio.o			\
	big_read.o              \
	big_write.o             \
	bspl.opt_o		\
	bsple3.Opt_o		\
	bsple3_r4.Opt_o		\
	bsple3_1d.Opt_o		\
	bsple3_1d_r4.Opt_o	\
	bsple3_3d.Opt_o		\
	bsple3_3d_r4.Opt_o	\
	bsple3_4d.Opt_o		\
	bsple3_4d_r4.Opt_o	\
	bsple3_extend.o         \
	bsple4.Opt_o		\
	bspl4.opt_o		\
	bspl4_1d.opt_o 		\
	bspl4_2d.opt_o 		\
	bspl4_3d.opt_o 		\
	bspl4_4d.opt_o 		\
	bsplj4.opt_o 		\
	bspl_1d.opt_o 		\
	bspl_2d.opt_o 		\
	bspl_3d.opt_o 		\
	bspl_4d.opt_o 		\
	bsplj.opt_o 		\
	cha.opt_o		\
	che.opt_o		\
        check_port.o 		\
	command_line.o		\
	cpu_timer.o 		\
	date_to_time.o		\
	decpol.o 		\
	debug_mem.o		\
	disp8.opt_o		\
	ebspl.opt_o 		\
	ebspl_lsq_cns.opt_o 	\
	ebspl_lsq_cns3.opt_o 	\
	ebspl_lsq_cns3_vec.opt_o \
	endian_cnv.opt_o	\
	endian_pair_cnv.opt_o	\
	error.o			\
	error_ellipse.opt_o	\
	file_info.o 		\
	filters.opt_o 		\
	find_free_port.o 	\
	fr.opt_o		\
	free_mem.o		\
	geod_to_cfs.o           \
	getinfo.o		\
	get_cdate.o		\
        get_file_from_dir.o	\
        get_eclcoo.opt_o 	\
        get_galcoo.opt_o 	\
	get_hr_timer.o          \
	get_mem.o		\
	get_mem32.o		\
        get_memrss.o 		\
	get_memsize.o		\
        get_my_exe_path.o 	\
        get_name_from_dir.o	\
	get_swapsize.o		\
	get_system_constant.o	\
	get_termsize.o		\
	get_unit.o		\
	grab.o			\
	gr_tat.opt_o		\
	hist.opt_o 		\
	hr_tat.opt_o		\
	idigit.opt_o		\
	inang.opt_o		\
	inva.opt_o		\
	inv_spl.opt_o		\
	inode64.o 		\
	io64.o 			\
	ipsg.opt_o		\
	is_dir_exist.o		\
	is_inf.opt_o		\
	is_nan.opt_o		\
	itti.o			\
	itti_chr.o		\
	itti_chr_del.o		\
	ixmn4.opt_o 		\
	ixmn8.opt_o 		\
	j2000_to_b1950.o 	\
	jd_to_date.o		\
	jd_to_mjd_sec.o		\
	legendre_regr.opt_o 	\
	lib_callg.o		\
	list.opt_o		\
	list_to_line.opt_o	\
	lspline.opt_o		\
	lsq.opt_o		\
	lsqw_cns.opt_o		\
	make_xyz_to_uen.opt_o	\
	make_uen_to_xyz.opt_o	\
	match_regexp.o		\
	match_regexp_c.o	\
	match_wild.o		\
	matview.o		\
	mjd_sec_to_jd.o 	\
	mjdsec_to_date.o 	\
	mjdsec_to_tim.o 	\
	mjdsec_to_vex.o 	\
	multi_index.o		\
        nsleep.o		\
	parabolic_extremum.o    \
	petools_set_alarm_handler.o \
	petools_vers.o 		\
	petutil_trap.o		\
	poly_int.opt_o 		\
	poly_solve_stable.opt_o \
	phas_cmpl_r4.o		\
	phas_cmpl_r8.o		\
	ps_tat.opt_o		\
	probe_address.o 	\
	read_line.o 		\
	ref_ell.opt_o		\
	regr8.opt_o		\
	regrite.opt_o		\
	remove_tree.o		\
	reorder.opt_o		\
	resolve_env.o		\
	rgrw8.opt_o		\
	rg_tat.opt_o		\
	rh_tat.opt_o		\
	rssr.opt_o		\
	rwfil.o			\
	sanitize_logvar.o 	\
	set_file_lock.o 	\
	set_read_lock.o        \
	set_sigcld.o 		\
	set_signal_ctrlc.o 	\
	set_stacksize.o 	\
	set_write_lock.o        \
	show_text.o		\
        simtar.o		\
        sock_accept.o 		\
        sock_open_client.o 	\
        sock_open_server.o 	\
        sock_read.o 		\
        sock_read_poll.o 	\
        sock_select.o 		\
        sock_shutdown.o 	\
        sock_write.o 		\
        sort.opt_o		\
	soucoo_to_name.o 	\
	spl8.opt_o		\
	tai_to_tdb.o 		\
	tim_to_mjdsec.o 	\
	tim_to_date.o 		\
	titi.o	                \
	term_io.o		\
	tdb_to_tai.o 		\
	tran.opt_o		\
	trend_ite.o		\
	trm.o			\
	unweb_string.o		\
	unix_date_to_date.o 	\
	vex_to_date.o 		\
	wall_timer.o 		\
	write_list.o		\
	write_long.o		\
	wrapper.o

MOD	= module_pet_util.d

all:	mod lib

mod:	$(MOD)

lib:	$(OBJ)
	ar  r $(LIB) $(OBJ)
	@echo '--- Library $(LIB) updated ---'

clean:
	rm -f 	$(OBJ)
	rm -f 	$(LIB)
	rm -f 	$(MOD)
	rm -f   module_pet_util.o
