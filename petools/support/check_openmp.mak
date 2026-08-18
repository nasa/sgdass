.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o  $*.o $*.f  

EXE_DIR	= $(PETOOLS_ROOT)/bin
NAME    = check_openmp
EXEC	= $(EXE_DIR)/check_openmp

OBJECT  = $(PETOOLS_ROOT)/support/$(NAME).o  

all:    $(OBJECT)
	$(MK5_LINK) -o $(EXEC) $(OBJECT)

clean:	
	rm -f $(OBJECT) $(EXEC)
