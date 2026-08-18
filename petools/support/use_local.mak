#
#  Makefile for program use_local
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o   $*.o $<


EXE_DIR	= $(PETOOLS_ROOT)/bin
EXEC	= $(EXE_DIR)/use_local

#

OBJECTS =		        	\
		use_local.o		\
		use_local_support.o	


all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) 

clean:	clean_obj  clean_exe

clean_obj:	
	rm -f $(OBJECTS) 

clean_exe:	
	rm -f $(EXEC) 
