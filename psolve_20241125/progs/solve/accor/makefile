# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking accor executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=              			\
		modcor.o 		\
		obcor.o 		\
		scor_allgoal.o 		\
		scor_blnkmsg.o 		\
		scor_chall.o 		\
		scor_chind.o 		\
		scor_lkeyall.o 		\
		scor_statdat.o 		\
		scorf_center.o 		\
		scorf_chall.o 		\
		scorf_chind.o 		\
		scorf_lkyall.o 		\
		scorf_stadat.o 		\
		selcor.o 		\
		selcorf.o 		\
		selpart.o 

OBJM 	=       accor.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) 
#	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/ACCOR $(OBJM) $(OBJS) $(LIBS)

accor.o:	accor_version.i

clean:  
	rm -f $(OBJS) $(OBJM) $(EXE_DIR)/ACCOR
