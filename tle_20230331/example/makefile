# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   example_tle_xx executables.  *
# *                                                                      *
# ************************************************************************
# 
# -I $(NERS_INC)  
#
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -Wstrict-prototypes -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -I../include -I$(PETOOLS_PREFIX)/include -I$(VTD_INC) -I$(NERS_INC) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -I../include -I$(PETOOLS_PREFIX)/include -I$(VTD_INC) -I$(NERS_INC) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95)     -c -o $*.o $*.f  

EXE_DIR = $(TLE_ROOT)/bin

OBJ1 	=               		\
		tle_example_to_coo.o

OBJ2 	=               		\
		tle_example_to_azel.o

OBJ3 	=               		\
		tle_dir_to_azel.o

LIBS =					\
        $(PETOOLS_LIB)			\
	$(VTD_LIB)			\
	$(NERS_LIB)			\
	$(SPC_LIB)			\
	$(CFISTIO_LIB)			\
        $(SOLVE_LIB_VEC)		\
        $(SOLVE_LIB_PGPLOT)		\
        $(SOLVE_LIB_X11) 		\
        $(SOLVE_LIB_BLAS)           	\
        -lpthread -lgomp -lm  		\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJ1) $(OBJ2) $(OBJ3)
	$(MK5_LINK)   -o $(EXE_DIR)/tle_example_to_coo  tle_example_to_coo.o  -L${TLE_ROOT}/src -ltle $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/tle_example_to_azel tle_example_to_azel.o -L${TLE_ROOT}/src -ltle $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/tle_dir_to_azel     tle_dir_to_azel.o     -L${TLE_ROOT}/src -ltle $(LIBS)

clean:  
	rm -f $(OBJ1) \
              $(OBJ2) \
              $(OBJ3) \
              $(EXE_DIR)/tle_example_to_coo   \
              $(EXE_DIR)/tle_example_to_azel  \
              $(EXE_DIR)/tle_dir_to_azel

distclean:  clean
	    rm -f tle_example_to_coo.f
	    rm -f tle_example_to_azel.f
	    rm -f tle_dir_to_azel.f
