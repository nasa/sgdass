# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking diagi executables              *
# *   (demonstration programms).                                         *
# *                                                                      *
# *  ###  23-OCT-97   diagi.mak    v2.2  (c)  L. Petrov 10-JUL-2000 ###  *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.o	.e	.a


INCLUDE_DIR1   = $(MK4_ROOT)/progs/solve/include 
INCLUDE_DIR2   = $(PGPLOT_DIR)/../src

F77_FLAGS = +es +U77 +O2 +T -C -c \
            -I $(INCLUDE_DIR1) -I $(INCLUDE_DIR2) -o

.f.o:
	f77   $(F77_FLAGS) $*.o $*.f 

EXE_DIR   = $(MK4_ROOT)/bin
SUPPORT   = $(MK4_ROOT)/support

LIBS =  $(MK4_ROOT)/libs/diagi/diagi.a		\
	$(MK4_ROOT)/libs/pet_util/pet_util.a	\
	$(MK4_ROOT)/libs/matvec/matvec.a	\
        $(SOLVE_LIB_PGPLOT)			\
        $(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_XT)				\
        $(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_XHP11)			\
        $(SOLVE_LIB_VEC)			\
        $(SOLVE_LIB_BLAS)		

demo:	diagi_demo.o diagi_dec.o diagi_rst.o md_demo.o diagi_batch.o \
        diagi_user.o
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	f77 +U77 -o $(EXE_DIR)/diagi_demo diagi_demo.o $(OBJ) $(LIBS)

	f77 +U77 -o $(EXE_DIR)/diagi_batch diagi_batch.o $(OBJ) $(LIBS)

	f77 +U77 -o $(EXE_DIR)/diagi_user diagi_user.o $(OBJ) $(LIBS)

	f77 +U77 -o $(EXE_DIR)/diagi_dec diagi_dec.o $(OBJ) $(LIBS)

	f77 +U77 -o $(EXE_DIR)/diagi_rst diagi_rst.o $(OBJ) $(LIBS)

	f77 +U77 -o $(EXE_DIR)/md_demo md_demo.o $(OBJ) $(LIBS)

