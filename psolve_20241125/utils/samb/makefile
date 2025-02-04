# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  samb  executable              *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS 	=              		\
		samb_do.o 				

OBJ1 	= 			\
		samb.o 			

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/ftlib/ftlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS) $(OBJ1) $(OBJ2)
	$(MK5_LINK) -o $(EXE_DIR)/samb   $(OBJ1) $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJS) $(OBJ1) $(OBJ2)
	rm -f $(EXE_DIR)/samb
