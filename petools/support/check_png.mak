#
#  Makefile for program check_cfitsio_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.c.o:
	@$(MK5_CC) -c -I $(PNG_INC_DIR) -o   $*.o $<


EXE_DIR	= $(PETOOLS_ROOT)/bin
EXEC	= $(EXE_DIR)/png_test.e
#

OBJECTS =		        	\
	$(PETOOLS_ROOT)/support/png_test.o

all:	$(OBJECTS) 
	@$(MK5_CC) -o $(EXEC) $(OBJECTS) -L $(PNG_LIB_DIR) -lpng -L $(PREFIX/lib) -lz

clean:	clean_obj  clean_exe

clean_obj:	
	@rm -f $(OBJECTS) 

clean_exe:	
	@rm -f $(EXEC) 
