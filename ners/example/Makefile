# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking ners                            *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.noopt	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95)        -I$(PETOOLS_INC) -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPTEST) -I$(PETOOLS_INC) -c -o $*.opt_o $*.f

.f.noopt:
	$(MK5_F95_NOOPT)  -I$(PETOOLS_INC) -c -o $*.noopt $*.f


OBJS =                  		\
	ners_fortran_example_01.o 	\
	ners_fortran_example_02.o 	\
	ners_fortran_example_03.o 	\
	ners_fortran_example_04.o 	\
	ners_fortran_example_05.o 	\
	ners_c_example_01.o 		\
	ners_c_example_02.o 		\
	ners_c_example_03.o 		\
	ners_c_example_04.o 		\
	ners_c_example_05.o

NERS_LIB   =  $(NERS_ROOT)/lib/libners.a

LIBS =				\
	$(NERS_LIB) 		\
	$(PETOOLS_LIB) 		\
	$(SPC_LIB) 		\
	$(SOLVE_LIB_BLAS) 	\
	$(NERS_LAPACK) 		\
	$(LDFLAGS) 

bin:	ners_fortran_example_01 \
        ners_fortran_example_02 \
        ners_fortran_example_03 \
        ners_fortran_example_04 \
        ners_fortran_example_05 \
        ners_c_example_01       \
        ners_c_example_02       \
        ners_c_example_03       \
        ners_c_example_04       \
        ners_c_example_05

ners_fortran_example_01: $(OBJS)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_fortran_example_01 ners_fortran_example_01.o $(LIBS);

ners_fortran_example_02: $(OBJS)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_fortran_example_02 ners_fortran_example_02.o $(LIBS);

ners_fortran_example_03: $(OBJS)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_fortran_example_03 ners_fortran_example_03.o $(LIBS);

ners_fortran_example_04: $(OBJS)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_fortran_example_04 ners_fortran_example_04.o $(LIBS);

ners_fortran_example_05: $(OBJS)
	    $(MK5_LINK) -o $(NERS_ROOT)/bin/ners_fortran_example_05 ners_fortran_example_05.o $(LIBS);

ners_c_example_01: $(OBJS)
	    $(MK5_C_LINK) -o $(NERS_ROOT)/bin/ners_c_example_01 ners_c_example_01.o $(LIBS);

ners_c_example_02: $(OBJS)
	    $(MK5_C_LINK) -o $(NERS_ROOT)/bin/ners_c_example_02 ners_c_example_02.o $(LIBS);

ners_c_example_03: $(OBJS)
	    $(MK5_C_LINK) -o $(NERS_ROOT)/bin/ners_c_example_03 ners_c_example_03.o $(LIBS);

ners_c_example_04: $(OBJS)
	    $(MK5_C_LINK) -o $(NERS_ROOT)/bin/ners_c_example_04 ners_c_example_04.o $(LIBS);

ners_c_example_05: $(OBJS)
	    $(MK5_C_LINK) -o $(NERS_ROOT)/bin/ners_c_example_05 ners_c_example_05.o $(LIBS);

    

$(OBJS):			$(NERS_ROOT)/include/ners.i
ners_fortran_example_01: 	$(NERS_ROOT)/include/ners.i
ners_fortran_example_02: 	$(NERS_ROOT)/include/ners.i
ners_fortran_example_03: 	$(NERS_ROOT)/include/ners.i
ners_fortran_example_04: 	$(NERS_ROOT)/include/ners.i
ners_fortran_example_05: 	$(NERS_ROOT)/include/ners.i
ners_c_example_01: 		$(NERS_ROOT)/include/ners.h
ners_c_example_02: 		$(NERS_ROOT)/include/ners.h
ners_c_example_03: 		$(NERS_ROOT)/include/ners.h
ners_c_example_04: 		$(NERS_ROOT)/include/ners.h
ners_c_example_05: 		$(NERS_ROOT)/include/ners.h

clean:
	rm -f $(OBJS) \
              $(NERS_ROOT)/bin/ners_fortran_example_01 \
              $(NERS_ROOT)/bin/ners_fortran_example_02 \
              $(NERS_ROOT)/bin/ners_fortran_example_03 \
              $(NERS_ROOT)/bin/ners_fortran_example_04 \
              $(NERS_ROOT)/bin/ners_fortran_example_05 \
              $(NERS_ROOT)/bin/ners_c_example_01       \
              $(NERS_ROOT)/bin/ners_c_example_02       \
              $(NERS_ROOT)/bin/ners_c_example_03       \
              $(NERS_ROOT)/bin/ners_c_example_04       \
              $(NERS_ROOT)/bin/ners_c_example_05
