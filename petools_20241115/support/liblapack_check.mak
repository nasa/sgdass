.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o  $*.o $*.f  

NAME    = liblapack_check

OBJECT  = $(NAME).o  

LIBS  	= $(SOLVE_LIB_BLAS) $(SOLVE_EXTRA_LIB) 

all:    $(OBJECT)
	$(MK5_LINK) -o $(liblapack_check_exec) $(OBJECT) $(LIBS) 

clean:	
	rm -f $(OBJECT) $(liblapack_check_exec) 
