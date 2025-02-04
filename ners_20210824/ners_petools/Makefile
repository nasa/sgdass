# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking ners_petools                    *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.noopt	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95)        -D GENERIC -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPTEST) -D GENERIC -c -o $*.opt_o $*.f

.f.noopt:
	$(MK5_F95_NOOPT)  -D GENERIC -c -o $*.noopt $*.f


OBJS =                  		\
        atan_cs.o 			\
	addc_vv.o 			\
	binio.o 			\
	bspl.o 				\
	cha.o                   	\
	date_to_time.o 			\
	dp_vv_v.o 			\
	ebspl.opt_o 			\
	ebspl_lsq_cns3.opt_o 		\
	ebspl_lsq_cns3_vec.opt_o 	\
	error.o 			\
	get_cdate.o 			\
	get_hr_timer.o 			\
	get_system_constant.o 		\
	get_unit.o 	 		\
	file_info.o 			\
	is_nan.o 			\
	ixmn8.opt_o 			\
	jd_to_date.o 			\
	libwait.o 			\
	match_wild.o 			\
	mjdsec_to_date.o 		\
	mul_mm_ii_i.o 			\
	mul_mv_iv_v.o 			\
	mul_vc_v.o 			\
	nsleep.o 			\
        petools_set_alarm_handler.o 	\
	probe_address.o 		\
	rwfil.o 			\
	set_read_lock.o 		\
	set_write_lock.o 		\
	spl8.o 				\
	tai_to_tdb.o  			\
	tim_to_date.o 			\
	tran.o  			\
	wrapper.o 			\
	vec.o 				

NERS_PETOOLS_LIB   =  $(NERS_ROOT)/lib/libners_petools.a

ners_lib:		 $(OBJS) 
	ar r $(NERS_PETOOLS_LIB) $(OBJS) 

clean:
	rm -f $(OBJS) \
	      $(NERS_PETOOLS_LIB)
