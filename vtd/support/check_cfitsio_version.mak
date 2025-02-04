#
#  Makefile for program check_cfitsio_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -I $(CFITSIO_INC) -o   $*.o $<


EXE_DIR	= $(VTD_ROOT)/bin
EXEC	= $(EXE_DIR)/check_cfitsio_version.e

#

OBJECTS =		        	\
	$(VTD_ROOT)/support/check_cfitsio_version.o 

LIBS = $(CFITSIO_LIB) $(CURL_LIB)

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	rm -f $(OBJECTS) 

clean_exe:	
	rm -f $(EXEC) 
