# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  stp_parser_dump programs      *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o

.c.o:
	$(MK5_C) -I /opt64/include -c -o $*.o $*.c

.f.o:
	$(MK5_F95_OPT) -I /opt64/include -I /progs/vlbi -c -o $*.o $*.f
#
#.f.opt_o:
#	$(MK5_F95_NOOPT) $(MK5_F_OPENMP) -I /opt64/include -I ../include -I /progs/vlbi -c -o $*.opt_o $*.f

EXE_DIR = $(Ex)

RES  =  stp_fil_parser_dump

OBJS = 	$(RES).o               \
        stp_fil_parser.o           \
        stp_compar.o
        
#LIBS =					\
#        -L/opt64/lib  -lfourpack	\
#	-L /opt64/lib -lfftw3 -lfftw3f  -lfftw3_omp -lfftw3f_omp  \
#        $(SOLVE_LIB_PGPLOT)             \
#        $(PETOOLS_LIB)                  \
#        -L/usr/X11R6/lib               \
#        $(FFTW_LIB) -lX11               \
#        $(SOLVE_LIB_BLAS)               \
#        $(SOLVE_EXTRA_LIB) 

LIBS =						\
	-L /opt64/lib -lvex_parser              \
	$(PETOOLS_LIB)				\
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
	$(SOLVE_LIB_XT)				\
	$(SOLVE_LIB_XHP11)			\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_M) 				\



all:	bin

bin: 	$(OBJS)
	$(MK5_LINK) -o  $(EXE_DIR)/$(RES).e $(OBJS) $(LIBS); $(EXE_DIR)/$(RES).e

clean:
	rm  -f $(OBJS)  $(EXE_DIR)/$(RES).e
