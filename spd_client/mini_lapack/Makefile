# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking a set of executables for        *
# *   computation of slanted path delay.                                 *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.ff	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(SPC_C) -c -o $*.o $*.c

.f.opt_o:
	$(SPC_F95_PUNCH) $(SPC_OPENMP) -c -o $*.opt_o $*.f

OBJS =                  	\
	dcopy.opt_o 		\
	dgbtrf.opt_o 		\
	dgbtrs.opt_o 		\
	dgbtf2.opt_o 		\
	dger.opt_o 		\
	dgemm.opt_o 		\
	dgemv.opt_o 		\
	dlaswp.opt_o 		\
	dscal.opt_o 		\
	dswap.opt_o 		\
	dtbsv.opt_o 		\
	dtrsm.opt_o 		\
	idamax.opt_o		\
	ieeeck.opt_o		\
	ilaenv.opt_o 		\
	iparmq.opt_o		\
	isamax.opt_o 		\
	lsame.opt_o 		\
	scopy.opt_o 		\
	sgbtrf.opt_o 		\
	sgbtrs.opt_o 		\
	sgbtf2.opt_o 		\
	sger.opt_o 		\
	sgemm.opt_o 		\
	sgemv.opt_o 		\
	slaswp.opt_o 		\
	sscal.opt_o 		\
	sswap.opt_o 		\
	stbsv.opt_o 		\
	strsm.opt_o 		\
	xerbla.opt_o 		

all:	lib


LIB =	$(SPC_ROOT)/lib/libspc.a

lib:	$(OBJS) 
	ar  r $(LIB) $(OBJS) 

clean:
	rm -f $(OBJS) $(LIB)                    		\
