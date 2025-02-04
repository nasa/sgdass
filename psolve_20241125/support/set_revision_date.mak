#
#  Makefile for program set_revision_date
#
.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh
#
.f.o:
	$(MK5_F95) -c -o   $*.o $*.f  
#

EXEC	= $(MK5_ROOT)/bin/set_revision_date

OBJECTS =		        	\
		set_revision_date.o	\
		use_local_support.o	

all:	$(OBJECTS) 
#	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXEC) $(OBJECTS) $(SOLVE_EXTRA_LIB)

clean:	
	rm -f $(OBEJCTS) $(EXEC) 
