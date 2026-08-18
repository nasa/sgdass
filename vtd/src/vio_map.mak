# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  dif_eop executable            *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh
.SUFFIXES:
.SUFFIXES:	.f	.c.	.o	.a	

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_NOOPT) -c -o $*.o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  


EXE_DIR = $(Ex)

RES     = vio_map

OBJS =				\
        $(RES).o		\
	viono_grid_plot.o 

LIBS =						\
        $(MK5_ROOT)/libs/vtd/vtd.a 		\
        $(MK5_ROOT)/libs/diagi/diagi.a          \
        $(MK5_ROOT)/libs/pet_util/pet_util.a	\
        $(MK5_ROOT)/libs/matvec/matvec.a        \
        $(MK5_ROOT)/libs/pet_util/pet_util.a	\
        $(SOLVE_LIB_PGPLOT)			\
        $(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_XT)				\
        $(SOLVE_LIB_XHP11)			\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)			\
	$(SOLVE_EXTRA_LIB)			\
	-lz

bin: 	$(OBJS) $(OBJ_LIB)
	$(MK5_LINK) -o $(EXE_DIR)/$(RES).e $(OBJS) $(LIBS) ; \
        $(EXE_DIR)/$(RES).e

clean:
	rm -f $(OBJS) $(EXE_DIR)/$(RES).e
