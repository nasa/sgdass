# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   set_deltran  executable      *
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

OBJS 	=               	\
		set_deltran.o 	

OBJ1 	=               				\
		$(MK5_ROOT)/progs/phase_doctor/dbhget.o

LIBS =						\
	$(MK5_ROOT)/libs/dbase96/dbase.a 	\
	$(MK5_ROOT)/libs/qcat4/qcat4.a 		\
	$(MK5_ROOT)/libs/chain4/chain4.a 	\
	$(MK5_ROOT)/libs/cutil/cutil.a		\
	$(MK5_ROOT)/libs/newlib/newlib.a 	\
	$(MK5_ROOT)/libs/curlib/curlib.a	\
	$(MK5_ROOT)/libs/fclib/fclib.a 		\
	$(MK5_ROOT)/libs/ftlib/ftlib.a 		\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_LIB_VEC)                        \
	$(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ1) 
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/set_deltran  set_deltran.o \
                       $(OBJS) $(OBJ1) $(LIBS)

clean:  
	rm -f $(OBJS) $(EXE_DIR)/set_deltran  
