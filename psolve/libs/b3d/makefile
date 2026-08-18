######################################################################
#								     #
#	Make file for recompiling library  b3d.a     		     #
#						     		     #
#  %%%  27-JAN-97   makefile  v2.0  (c)  L. Petrov   04-MAY-99  %%%  #
#								     #
######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a
#
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

LIB     = b3d.a

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95) -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f


OBJ =				\
	b3d_cov_f.o		\
	b3d_cov_x.o		\
	b3d_dsp_x.o		\
	b3d_scl_x.o		\
	b3d_sol_x.o		\
	b3d_update.o		\
	b3d_usc_f.o		\
	b3d_usc_x.o		\
	b1b3d_sol.o		\
	b1b3d_cov.o		\
	b1b3d_scl.o		\
	fast.opt_o 		\
	fast_glo.opt_o		\
	fast_bypass.o

all:	lib

lib: 	$(OBJ)
	ar  r $(LIB) $(OBJ) 
	echo '--- Library $(LIB) updated ---'

clean:
	rm -f $(OBJ) $(LIB) $(MOD)
