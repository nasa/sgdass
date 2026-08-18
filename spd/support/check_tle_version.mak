#
#  Makefile for program check_tle_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.f.o:
	$(MK5_F95) -c -I $(SPD_TLE)/include -o   $*.o $<

EXE_DIR	= $(SPD_ROOT)/bin
EXEC	= $(EXE_DIR)/check_tle_version.e

OBJECTS =		        	\
	$(SPD_ROOT)/support/check_tle_version.o 

LIBS = -L $(SPD_TLE)/lib -ltle -L $(SPD_NERS)/lib -lners $(PETOOLS_LIB)

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	@rm -f $(OBJECTS) 

clean_exe:	
	@rm -f $(EXEC) 
