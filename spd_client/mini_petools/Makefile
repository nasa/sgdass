# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking a set of executables for        *
# *   computation of slanted path delay.                                 *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002; eval "$2"'

.c.o:
	$(SPC_C) -c -o $*.o $*.c

.f.o:
	$(SPC_F95) $(SPC_OPENMP) -D BASIC_PREFIX -D PETOOLS_LITE -I$(SPC_ROOT)/include -I$(PETOOLS_ROOT)/include -c -o $*.o $*.f

.f.opt_o:
	$(SPC_F95_OPT)   $(SPC_OPENMP) -D BASIC_PREFIX -D PETOOLS_LITE -I$(SPC_ROOT)/include -I$(PETOOLS_ROOT)/include -c -o $*.opt_o $*.f

OBJS =                  	\
	binio.o 		\
	bspl.opt_o 		\
	bspl_1d.opt_o  	        \
	bspl_2d.opt_o  	        \
	bspl_3d.opt_o  	        \
	bspl_4d.opt_o  	        \
	bspl4.opt_o  		\
	bspl4_1d.opt_o  	\
	bspl4_2d.opt_o  	\
	bspl4_3d.opt_o  	\
	bspl4_4d.opt_o  	\
        bsple3_r4.opt_o 	\
	cha.o 			\
	date_to_time.o 		\
	error.o 		\
	file_info.o 		\
	get_cdate.o 		\
	get_file_from_dir.o 	\
	get_hr_timer.o          \
	get_name_from_dir.o 	\
	get_system_constant.o  	\
	get_unit.o 		\
	inode64.o 		\
	is_dir_exist.o 		\
	is_nan.o 		\
	ixmn4.o                 \
	ixmn8.o                 \
	jd_to_date.o 		\
	list.o 			\
	match_wild.o 		\
	mjdsec_to_date.o 	\
        petools_set_alarm_handler.o	\
	probe_address.o 	\
	rwfil.o 		\
	set_read_lock.o 	\
	set_stacksize.o 	\
	set_write_lock.o 	\
	sock_accept.o 		\
	sock_open_client.o 	\
	sock_open_server.o 	\
	sock_read_poll.o 	\
	sock_read.o 		\
	sock_write.o 		\
	sort.o 			\
	spl8.o                  \
	tim_to_date.o		\
	tran.o 			\
	wrapper.o 		\
				\
	addc_vv.opt_o 		\
	cheb.opt_o 		\
	diad_cvt.opt_o		\
	dp_vv_v.opt_o		\
	invsf.opt_o		\
	mul_mv_sv_v.opt_o 	\
	mul_mm_ii_i.opt_o 	\
	mul_vc_v.opt_o 		\
	vec.opt_o

SPC_LIB   = $(SPC_ROOT)/lib/libspc.a

LIBS =						\
	$(SPC_LIB)

bin:	spc_lib 

spc_lib:	$(OBJS) 
		ar r $(SPC_LIB) $(OBJS) 


clean:
	rm -f $(OBJS) 
	if ( -f $(SPC_LIB) ) rm -f $(SPC_LIB)
