.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o  $*.o $*.f  

EXE_DIR	= $(PETOOLS_ROOT)/bin
NAME    = check_libblas
EXEC	= $(EXE_DIR)/check_libblas

OBJECT  = $(PETOOLS_ROOT)/support/$(NAME).o  

LIBS  	= $(SOLVE_LIB_BLAS)

all:    $(OBJECT)
	$(MK5_LINK) -o $(EXEC) $(OBJECT) $(LIBS) 

clean:	
	rm -f $(OBJECT) $(EXEC)
