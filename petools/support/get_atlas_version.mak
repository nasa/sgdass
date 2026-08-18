#
#  Makefile for program readline_check
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_C) -c -o   $*.o $<


EXE_DIR	= $(PETOOLS_ROOT)/bin
EXEC	= $(EXE_DIR)/get_atlas_version

#

OBJECTS =		        	\
		$(PETOOLS_ROOT)/support/get_atlas_version.o

LIBS = $(PETOOLS_BLAS) 

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	rm -f $(OBJECTS) 

clean_exe:	
	rm -f $(EXEC) 
