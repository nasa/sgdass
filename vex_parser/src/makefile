# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking vex_parser                     *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.o_opt	.a
SHELL = /bin/csh -f -c 'umask 0002; shift; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)        $(MK5_F_OPENMP) -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o     $*.f  

.f.o_opt:
	$(MK5_F95_OPT)    $(MK5_F_OPENMP) -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o_opt $*.f  


LIB 	= libvex_parser.a
ifeq ($(VEX_PARSER_OS),Linux)
         SLIB 	= libvex_parser.so.1.0
endif
ifeq ($(VEX_PARSER_OS),Darwin)
	 SLIB   = libvex_parser.1.dylib
endif

OBJS 	=           			\
	get_band_range.o 		\
	matsub_nh.o			\
	vex_ang_fmts.o                  \
        vex_compar.o			\
	vex_flags.o			\
        vex_lists.o                     \
	vex_parser.o 			\
	vex_sou_idx.o			\
	vex_sta_idx.o			\
	sou_struc_fil_gen.o		\
	souivs_to_sourfc.o		\
	sourfc_to_souivs.o		\
	stp_compar.o			\
	stp_dir_parser.o		\
        stp_fil_parser.o		\
	stp_obs_err.o                   \
	stp_sefd.o			\
	stp_snr.o			\
	stp_sta_idx.o

OBJ1    = vex_parser_dump.o 		

OBJ2    = stp_fil_parser_dump.o

OBJ3    = gen_ima_fil.o

LIBS =					\
	$(PETOOLS_LIB) 			\
	$(FFTW_LIB) 			\
	$(SOLVE_LIB_PGPLOT)		\
	$(SOLVE_LIB_X11)		\
	$(SOLVE_LIB_VEC)		\
	$(SOLVE_LIB_BLAS)		\
	$(MKL_LIBS)			\
	$(SOLVE_EXTRA_LIB) 		\

all:	lib shared bin

lib:	$(OBJS)
	ar  r $(LIB) $(OBJS)

        ifeq ($(VEX_PARSER_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libvex_parser.so.1 -o $(SLIB) $(OBJS)
        endif
        ifeq ($(VEX_PARSER_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(VEX_PARSER_LIB)/libvex_parser.1.dylib \
                            -o $(SLIB) $(OBJS) \
                            -L$(PETOOLS_PREFIX)/lib/ -lpetools
        endif

bin: 	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3)
	$(MK5_LINK) -o $(VEX_PARSER_ROOT)/bin/vex_parser_dump      $(OBJ1) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(VEX_PARSER_ROOT)/bin/stp_fil_parser_dump  $(OBJ2) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(VEX_PARSER_ROOT)/bin/gen_ima_fil          $(OBJ3) $(OBJS) $(LIBS)

$(OBJS): ../include/vex.i		\
	 ../include/stp.i

clean:
	rm -f $(VEX_PARSER_ROOT)/bin/vex_parser_test
	rm -f $(VEX_PARSER_ROOT)/bin/stp_fil_parser_dump
	rm -f $(VEX_PARSER_ROOT)/bin/gen_ima_fil
	rm -f $(OBJS)
	rm -f $(OBJ1)
	rm -f $(OBJ2)
	rm -f $(OBJ3)
	rm -f $(LIB)
	rm -f $(SLIB)
	rm -f libvex_parser.*
