#
#  Makefile for program check_tle_version.mak
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.f.o:
	$(MK5_F95) -c -I $(VTD_TLE_DIR)/include -o   $*.o $<


EXE_DIR	= $(VTD_ROOT)/bin
EXEC	= $(EXE_DIR)/check_tle_version.e

#

OBJECTS =		        	\
	$(VTD_ROOT)/support/check_tle_version.o 

LIBS = -L $(VTD_TLE_DIR)/lib -ltle -L $(VTD_NERS_DIR)/lib -lners -L $VTD_SPD_CLIENT_DIR/lib -lspc $(PETOOLS_LIB)

all:	$(OBJECTS) 
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(LIBS)

clean:	clean_obj  clean_exe

clean_obj:	
	@rm -f $(OBJECTS) 

clean_exe:	
	@rm -f $(EXEC) 
