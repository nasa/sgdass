#
#  Makefile for program f90_test
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh
#
EXE_DIR	= $(PETOOLS_ROOT)/bin
EXEC	= $(EXE_DIR)/f90_test

.f.o: 	
	$(MK5_F95) -c -o  $*.o $<

OBJS    =       f90_test.o

LIBS    =	$(SOLVE_LIB_PW) 	\
		$(SOLVE_LIB_M) 		\
		$(SOLVE_LIB_LCL) 	\
		$(SOLVE_LIB_XHP11) 	\
		$(SOLVE_LIB_RTENSHD) 	\
		$(SOLVE_LIB_NSIPC) 	\
		$(SOLVE_LIB_X11) 	\
		$(SOLVE_LIB_XT) 	\
		$(SOLVE_LIB_CL) 	\
		$(SOLVE_LIB_CAT) 	\
		$(SOLVE_LIB_LFSYS) 	\
		$(SOLVE_LIB_U77) 	\
		$(SOLVE_LIB_VEC) 	\
		$(SOLVE_LIB_BLAS) 	\
		$(SOLVE_LIB_CURSES) 	\
		$(SOLVE_EXTRA_LIB) 

all:    f90_test.o
	$(MK5_LINK) -o $(EXEC) $(OBJS) $(LIBS)

clean:	
	rm -f $(OBJS) $(EXEC)
