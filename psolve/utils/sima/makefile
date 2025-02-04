# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   sima  executable             *
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
	    check_supcat.o 	\
	    sima_config.o 	\
	    sima_do.o 		\
	    sima_main.o	 	

OBJ1 	=               				\
	    $(MK5_ROOT)/utils/opa/check_solve_initials.o

LIBS =						\
	$(MK5_ROOT)/libs/cutil/cutil.a		\
        $(MK5_ROOT)/libs/newlib/newlib.a        \
        $(MK5_ROOT)/libs/fclib/fclib.a          \
	$(MK5_ROOT)/libs/pet_util/pet_util.a    \
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_LIB_VEC)                        \
	$(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/sima sima_main.o $(OBJS) $(OBJ1) $(LIBS)

sima.o:	sima_version.i

clean:  
	rm -f $(OBJS) $(EXE_DIR)/sima
