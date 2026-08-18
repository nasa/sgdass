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
	$(NERS_F95_PUNCH) $(SPC_OPENMP) -c -o $*.opt_o $*.f

OBJS =                  	\
	dcopy.opt_o 		\
	dger.opt_o 		\
	dgemm.opt_o 		\
	dgemv.opt_o 		\
	dgbtrf.opt_o 		\
	dgbtf2.opt_o 		\
	dgbtrs.opt_o 		\
	dlaswp.opt_o 		\
	dscal.opt_o 		\
	dswap.opt_o 		\
	dtbsv.opt_o 		\
	dtrsm.opt_o 		\
	idamax.opt_o 		\
	ieeeck.opt_o 		\
	ilaenv.opt_o 		\
	iparmq.opt_o 		\
	lsame.opt_o 		\
	xerbla.opt_o 		

all:	lib

lib:	$(OBJS) 
	ar  r $(NERS_LAPACK) $(OBJS) 

clean:
	rm -f $(OBJS) $(LIB)                    		\
