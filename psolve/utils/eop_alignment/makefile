# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking eop_alignment  executable      *
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

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=               		\
		eop_alignment.o	

OBJ1 	=               						\
		$(SOLVE_ROOT)/utils/get_apriori_eop/rd_iers_c04.o	\
		$(SOLVE_ROOT)/utils/get_apriori_eop/rd_finals.o 

OBJ2 	=               						\
		$(SOLVE_ROOT)/utils/getpar/parse_erm.o			\
		$(SOLVE_ROOT)/utils/getpar/read_eob.o			\
		$(SOLVE_ROOT)/utils/getpar/read_sta.o			\
		$(SOLVE_ROOT)/utils/getpar/read_vel.o			

OBJ3 	=               						\
		$(SOLVE_ROOT)/utils/erm_to_eop/erm_to_eop.o 


LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
	$(SOLVE_LIB_CURSES)			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) 
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/eop_alignment $(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(LIBS)

eop_alignment.o: 	eop_alignment_version.i

clean:  
	rm -f $(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(EXE_DIR)/eop_alignment
