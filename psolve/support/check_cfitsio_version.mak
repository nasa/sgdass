#
#  Makefile for program check_cfitsio_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -I $(SOLVE_CFITSIO_DIR)/include -o   $*.o $<


EXE_DIR	= $(PSOLVE_ROOT)/bin
EXEC	= $(EXE_DIR)/check_cfitsio_version.e

#

OBJECTS =		        	\
	$(PSOLVE_ROOT)/support/check_cfitsio_version.o 

LIBS = $(SOLVE_CFITSIO_LIB)

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	
	@rm -f $(OBJECTS) $(EXEC) 
