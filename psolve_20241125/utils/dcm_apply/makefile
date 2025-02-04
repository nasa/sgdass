# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  dcm_apply  executable         *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_NOOPT) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(MK5_ROOT)/bin
SUPPORT = $(MK5_ROOT)/support

OBJS 	=               		\
	dcm_apply.o 			\
	dcm_parse_config.o 		\
	dcm_lists.o 			\
	dcm_perform.o 

OBJ2 	=               				\
	$(MK5_ROOT)/progs/solve/proc/edc_init.o 	\
	$(MK5_ROOT)/progs/solve/proc/edc_quit.o 	\
	$(MK5_ROOT)/progs/solve/proc/edc_read.o 	\
	$(MK5_ROOT)/progs/solve/proc/edc_update.o 	\
	$(MK5_ROOT)/progs/solve/proc/edc_write.o 	\

LIBS =						\
	$(MK5_ROOT)/libs/vtd/vtd.a 		\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(MK5_ROOT)/libs/matvec/matvec.a 	\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)		        \
	$(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ2)
	$(SUPPORT)/check_var;
	$(MK5_LINK) -o $(EXE_DIR)/dcm_apply dcm_apply.o $(OBJS) \
                       $(OBJ2) $(LIBS)

edc.i: 	$(OBJS) $(OBJ2)

clean:  
	rm -f $(OBJS) $(EXE_DIR)/dcm_apply
