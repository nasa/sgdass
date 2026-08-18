#
#  Makefile for program check_tle_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0002; eval "$2"'

.f.o:
	$(MK5_F95) -c -I $(TLE_PREFIX)/include -o   $*.o $<


EXE_DIR	= $(SUR_SKED_ROOT)/bin
EXEC	= $(EXE_DIR)/check_tle_version.e

#

OBJECTS =		        	\
	$(SUR_SKED_ROOT)/support/check_tle_version.o 

LIBS = -L $(TLE_PREFIX)/lib -ltle -L $(NERS_PREFIX)/lib -lners $(PETOOLS_LIB)

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	@rm -f $(OBJECTS) 

clean_exe:	
	@rm -f $(EXEC) 
