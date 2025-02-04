# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  vcat_info  executable         *
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

EXE_DIR = $(MK5_ROOT)/bin
SUPPORT = $(MK5_ROOT)/support

OBJS 	=              				\
		vcat_info.o 	        

LIBS =						\
	$(MK5_ROOT)/libs/vcat/vcat.a 		\
	$(MK5_ROOT)/libs/cutil/cutil.a	 	\
	$(MK5_ROOT)/libs/newlib/newlib.a 	\
	$(MK5_ROOT)/libs/fclib/fclib.a 		\
	$(MK5_ROOT)/libs/matvec/matvec.a	\
	$(MK5_ROOT)/libs/pet_util/pet_util.a	\
	$(MK5_ROOT)/libs/matvec/matvec.a	\
        $(SOLVE_LIB_PGPLOT)			\
        $(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_XT)				\
        $(SOLVE_LIB_XHP11)			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_LIB_VEC)		        \
	$(SOLVE_EXTRA_LIB) 			

all:	bin

bin:	$(OBJS) 
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/vcat_info vcat_info.o $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJS) $(EXE_DIR)/vcat_info
