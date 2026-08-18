.SUFFIXES:
.SUFFIXES:	.f	.o      .c	.a
SHELL=/bin/csh

.f.o:
	$(MK5_F95) -c -o   $*.o $*.f  


NAME    = libvec_check
OBJECT  = $(NAME).o  

LIBS  	= $(SOLVE_LIB_VEC)

all:    $(OBJECT)
	$(MK5_LINK) -o $(libvec_check_exec) $(OBJECT) $(LIBS) 

clean:	
	rm -f $(OBJECT) $(libvec_check_exec) 
