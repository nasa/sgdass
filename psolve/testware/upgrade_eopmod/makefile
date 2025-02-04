# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  upgrade_eopmod  executable    *
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
		upgrade_eopmod.o

LIBS =						\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VEC)                        \
	$(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/upgrade_eopmod  upgrade_eopmod.o \
        $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJS) $(EXE_DIR)/upgrade_eopmod  
