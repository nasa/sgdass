#
#  Makefile for program ncurses_check
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o   $*.o $<


EXE_DIR	= $(PETOOLS_ROOT)/bin
EXEC	= $(EXE_DIR)/ncurses_check.e
#

OBJECTS =		        	\
		$(PETOOLS_ROOT)/support/ncurses_check.o 

LIBS = $(PETOOLS_NCURSES_LIBDIR) -lncurses

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	rm -f $(OBJECTS) 

clean_exe:	
	rm -f $(EXEC) 
