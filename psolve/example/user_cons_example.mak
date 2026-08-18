# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  user_cons_example  executable *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh

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
		user_cons_example.o

OBJ2 	=               			\
	$(MK5_ROOT)/progs/solve/norml/add_cns.o	\
	$(MK5_ROOT)/progs/solve/norml/io_cnstr.o	


LIBS =						\
	$(MK5_ROOT)/libs/cutil/cutil.a		\
	$(MK5_ROOT)/libs/newlib/newlib.a 	\
	$(MK5_ROOT)/libs/curlib/curlib.a	\
	$(MK5_ROOT)/libs/lnfch/lnfch.a 		\
	$(MK5_ROOT)/libs/fclib/fclib.a 		\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VEC)	     		\
	$(SOLVE_LIB_BLAS)

all:	bin

bin:	$(OBJS)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/user_cons_example  user_cons_example.o \
                       $(OBJS) $(OBJ2) $(LIBS)
clean:  
	rm -f $(OBJS) $(EXE_DIR)/user_cons_example  
