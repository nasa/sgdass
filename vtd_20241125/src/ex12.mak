# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  dif_eop executable            *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.a	

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_OPT) -c -o $*.o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  


EXE_DIR = $(Ex)

RES     = vtd_example_12

OBJS =           \
        $(RES).o 

LIBS =						\
        $(MK5_ROOT)/libs/vtd/vtd.a          	\
        $(MK5_ROOT)/libs/diagi/diagi.a          \
	$(MK5_ROOT)/libs/fitslib/fitslib.a 	\
	$(MK5_ROOT)/libs/cfitsio/libcfitsio.a 	\
        $(MK5_ROOT)/libs/pet_util/pet_util.a	\
        $(MK5_ROOT)/libs/matvec/matvec.a        \
        $(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
	$(SOLVE_LIB_XT)				\
	$(SOLVE_LIB_XHP11)			\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_EXTRA_LIB)

bin: 	$(OBJS) $(OBJ_LIB)
	$(MK5_C_LINK) -o $(EXE_DIR)/$(RES).e $(OBJS) $(OBJ_LIB) $(LIBS) ; \
        $(EXE_DIR)/$(RES).e

clean:
	rm -f $(OBJS) $(EXE_DIR)/$(RES).e
