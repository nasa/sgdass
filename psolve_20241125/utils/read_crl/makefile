# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking read_crl  executable           *
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

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=               	\
		read_crl.opt_o	

OBJS 	=               	\
		read_crl_main.o	

LIBS =						\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/read_crl $(OBJM) $(OBJS) $(LIBS)

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/read_crl
#
#  Dependencies
#
read_crl_main.o:	read_crl_version.i
read_crl_main.o:	$(SOLVE_ROOT)/include/corel.i
read_crl.o:		$(SOLVE_ROOT)/include/corel.i
