#######################################################################
#								      #
#	Make file for recompiling library  blas_stub                  #
#						     		      #
#  %%%  15-DEC-2006  makefile   v1.1  (c)  L. Petrov  24-JUN-2015 %%% #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
#
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

LIB     = libblas_stub.a
NUL     = /null/dev

.c.o:
	$(MK5_C) -c -o $*.o $*.c  
	ar  r $(LIB)    $*.o

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  
	ar  r $(LIB)     $*.o

.f.a:
	$(MK5_F95) -c -o $*.o $*.f  
	ar  r $(LIB)     $*.o

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  
	ar  r $(LIB)         $*.opt_o

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  


OBJ	=			\
	blas_stub.o

all:	lib 

lib:	$(OBJ) $(LIB)
	@echo '--- Library $(LIB) updated ---'

clean:	
	rm -f 	$(OBJ) 
	rm -f 	$(LIB) 
