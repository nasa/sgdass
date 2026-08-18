# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   b3d_exe  executable          *
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

EXE_DIR   = $(MK5_ROOT)/bin
SUPPORT   = $(MK5_ROOT)/support

OBJS =                  \
	b1b3d_stand.o 	\
	b3d_stand.o 

LIB =   $(MK5_ROOT)/libs/b3d/b3d.a   	       \
        $(MK5_ROOT)/libs/matvec/matvec.a       \
        $(MK5_ROOT)/libs/pet_util/pet_util.a   \
        $(MK5_ROOT)/libs/matvec/matvec.a       \
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_LIB_VEC) 			\
	$(SOLVE_EXTRA_LIB)

all:	$(OBJS)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/b3d_stand b3d_stand.o $(LIB) 
	$(MK5_LINK) -o $(EXE_DIR)/b1b3d_stand b1b3d_stand.o $(LIB) 

clean:
	rm -f $(OBJS) $(EXE_DIR)/b3d_stand $(EXE_DIR)/b1b3d_stand 
