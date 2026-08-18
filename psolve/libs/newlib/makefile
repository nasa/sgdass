#######################################################################
#								      #
#	Make file for recompiling library  newlib.a     	      #
#						     		      #
#  ###  15-MAY-2003  makefile   v1.1 (c)   L. Petrov 15-MAY-2003 %%%  #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.o	.oo	.c	.a	.noopt_o
#
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

LIB     = newlib.a
LIB_OO  = fitsio.a
NUL     = /null/dev

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.noopt_o:
	$(MK5_F95_NOOPT) -g -c -o $*.noopt_o $*.f  

.f.oo:
	$(MK5_F95) -c -o $*.oo $*.f  


OBJ=				\
	aoidmp10.o 		\
	backf.o 		\
	break.o		        \
	break_msg.o 		\
	break_vec.o 		\
	c_f_jkctl.o 		\
	c_fplot.o 		\
	c_fploth.o 		\
	c_mnt.o 		\
	casefold.o 		\
	char2hol.o 		\
	chk_lock.o 		\
	choose_dir.o 		\
	clndr.o			\
	cnumd.o 		\
	comp_key.o 		\
	decents.o 		\
	decimaltoint.o 		\
	decmltodint.o 		\
	dflfort.o 		\
	dflsig18.o 		\
	dinttodecml.o 		\
	dinttodecmlr.o 		\
	endian_flips.o 		\
	endian_swap.o		\
	equal.o 		\
	f_jkctl.o 		\
	fcntl_lock_check.o 	\
	fjldy.o 		\
	fjldy_err.o 		\
	fplot.o 		\
	fploth.o 		\
	funchout.o 		\
	get_dt_range.o 		\
	get_unix_sys_release.o 	\
	getunit.o 		\
	getunit_not.o 		\
	hol2char.o	        \
	i2_loc.o 		\
	iday0.o 		\
	ifill.o			\
	ignsig18.o 		\
	inkey.o 		\
	inttodecimal.o 		\
	inttodecimalr.o 	\
	ioidmp10.o 		\
	jchar.o			\
	lcasefold.o 		\
	loglu.o 		\
	lsmult.o 		\
	mail_message.o 		\
	mdyjl.o 		\
	mnt.o 			\
	mnt_names.o 		\
	movewords.o 		\
	newcents.o 		\
	numerictime.o 		\
	nvwmov1.o 		\
	ocdump6.o 		\
	octaltoint.o 		\
	path_break.o 		\
	pchar.o			\
	pname.o 		\
	purge.o			\
	randmax.o 		\
	rcpar.o 		\
	read_asc.o 		\
	read_endian_unformatted.o \
	read_systemf.o 		\
	read_to_limf.o 		\
	readf.o 		\
	reio.o 			\
	rmpar.o 		\
	secs_elapsed.o 		\
	sel_bds.o 		\
	sencr.o 		\
	silent_trace.o 		\
	sint2decr.o 		\
	sort_into_fl.o 		\
	splitstring.noopt_o 	\
	srch_circle.o 		\
	susp.o 			\
	system2_sh.o 		\
	tape.o 			\
	timef.o 		\
	timeget.o 		\
	timenow.o 		\
	trimlen.o 		\
	usecnow.o 		\
	var_i4get.o 		\
	var_intput.o 		\
	var_numbget.o 		\
	var_numget.o 		\
	write_asc.o 		\
	write_endian_unformatted.o \
	writf.o 		\
	xx_lock.o 		\
	ymday.o 		\
	zeroterm.o 		\
	zterm.noopt_o 

OBJ_OO=				\
	fitsio.oo

all:	lib 

lib:	$(OBJ) $(OBJ_OO) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Library $(LIB) updated ---'

clean:	
	rm -f $(OBJ) $(OBJ_OO) $(LIB) $(LIB_OO) 
