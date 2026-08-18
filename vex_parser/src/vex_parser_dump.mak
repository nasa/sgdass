# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  rfc_outcat executable         *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:	.f	.c	.opt_o	.a

.c.o:
	$(MK5_C) -c -I /opt64/include -o $*.opt_o $*.c  

.f.opt_o:
	$(MK5_F95_OPT) -c -I /opt64/include -I /progs/vlbi -o $*.opt_o $*.f
#
#.f.o:
#	$(MK5_F95_NOOPT) $(MK5_F_OPENMP) -I /opt64/include -I ../include -I /progs/#vlbi -c -o $*.o $*.f

EXE_DIR = $(Ex)

RES     = vex_parser_dump

OBJS =						\
        $(RES).opt_o				\

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

bin: 	$(OBJS)
	$(MK5_LINK) -o $(EXE_DIR)/$(RES).e $(OBJS) $(LIBS) ; \
        $(EXE_DIR)/$(RES).e

clean:
	rm -f $(OBJS) $(EXE_DIR)/$(RES).e
