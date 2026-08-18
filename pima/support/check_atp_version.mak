#
#  Makefile for program check_atp_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0002; eval "$2"'

.f.o:
	$(MK5_F95) -c -I $(ATP_PREFIX)/include -o   $*.o $<


EXE_DIR	= $(PIMA_ROOT)/bin
EXEC	= $(EXE_DIR)/check_atp_version.e

#

OBJECTS =		        	\
	$(PIMA_ROOT)/support/check_atp_version.o 

LIBS = -L $(ATP_PREFIX)/lib -latp -L $(SPC_PREFIX)/lib -lspc -L $(NERS_PREFIX)/lib -lners $(PETOOLS_LIB) $(SOLVE_LIB_PGPLOT) 

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	@rm -f $(OBJECTS) 

clean_exe:	
	@rm -f $(EXEC) 
