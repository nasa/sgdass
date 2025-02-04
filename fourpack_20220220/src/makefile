# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking fourpack                       *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.o_opt	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_OPT)    $(MK5_F_OPENMP) -I$(FFTW_DIR)/include -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o $*.f  

.f.o_opt:
	$(MK5_F95_OPTEST) $(MK5_F_OPENMP) -I$(FFTW_DIR)/include -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o_opt $*.f  


RES1    = create_fftw_plan
RES2    = aj_comp

LIB 	= libfourpack.a
ifeq ($(FOURPACK_OS),Linux)
         SLIB 	= libfourpack.so.1.0
endif
ifeq ($(FOURPACK_OS),Darwin)
	 SLIB   = libfourpack.1.dylib
endif

OBJS 	=           			\
	create_fftw_plan.o 		\
	fast_fft.o 			\
	filter_fft.o 			\
	power_spe.o 			\
	sphe_aj.o  			\
	sphe_comp_val.o 		\
	sphe_comp_vec.o 		\
	sphe_dir_2nn.o  		\
	sphe_dir_2nn_f.o_opt 		\
	sphe_dir_2nn_x.o_opt  		\
	sphe_dir_lsq.o  		\
	sphe_ff.o  			\
	sphe_init.o 			\
	sphe_inv_2nn.o 			\
	sphe_inv_2nn_f.o_opt 		\
	sphe_inv_2nn_x.o_opt 		\
	sphe_inv_2nn_vec.o 		\
	sphe_inv_2nn_vec_f.o_opt 	\
	sphe_inv_2nn_vec_x.o_opt 	\
	sphe_leg.o 		        \
	sphe_leg_f.o_opt 		\
	sphe_leg_x.o_opt 		\
	sphe_quit.o 

OBJ1 	= create_fftw_plan_main.o

OBJ2 	= aj_comp.o

LIBS =					\
	$(PETOOLS_LIB) 			\
	$(FFTW_LIB) 			\
	$(SOLVE_LIB_PGPLOT)		\
	$(SOLVE_LIB_X11)		\
	$(SOLVE_LIB_VEC)		\
	$(SOLVE_LIB_BLAS)		\
	$(MKL_LIBS)			\
	$(SOLVE_EXTRA_LIB)

all:	lib shared bin

lib:	$(OBJS)
	ar  r $(LIB) $(OBJS)

shared:	$(OBJS)
        ifeq ($(FOURPACK_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libfourpack.so.1 -o $(SLIB) $(OBJS)
        endif
        ifeq ($(FOURPACK_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(FOURPACK_LIB)/libfourpack.1.dylib \
                            -o $(SLIB) $(OBJS) \
                            $(FFTW_LIB) \
                            -L$(PETOOLS_PREFIX)/lib/ -lpetools
        endif

bin: 	$(OBJS) $(OBJ1) $(OBJ2) 
	$(MK5_LINK) -o $(RES1) $(OBJ1) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(RES2) $(OBJ2) $(OBJS) $(LIBS)

$(OBJS): ../include/fourpack.i ../include/fourpack_constants.i

clean:
	rm -f $(OBJS)
	rm -f $(OBJ1)
	rm -f $(OBJ2)
	rm -f $(LIB)
	rm -f $(SLIB)
	rm -f $(RES1)
	rm -f $(RES2)
	rm -f libfourpack.*
	rm -f *.o
