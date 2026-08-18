#######################################################################
#								      #
#	Make file for recompiling library  ftlib.a     	      	      #
#						     		      #
#  ###  15-MAY-2003  makefile   v1.10 (c)  L. Petrov 15-MAY-2003 %%%  #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.o	.c	.a	.d
#
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

LIB     = ftlib.a
NUL     = /null/dev

.c.o:
	$(MK5_C) -c -I $(SOLVE_ROOT)/include -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

OBJ=				\
		ftdup.o 	\
		ftread.o 	\
		ftpiputil.o 	\
		ftperror.o 	\
		ftwrite.o 	\
		ftsopen.o 	\
		ftclose.o 	\
		ftcreate.o 	\
		ftunlink.o 	\
		ftqsort.o 	\
		ftsleep.o 	\
		ftseopenx.o 	\
		ftexecvp.o 	\
		ftlseek.o 	\
		ftmknod.o 	\
		ftopen.o 	\
		ftseopen.o 

all:	lib 

lib:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Library $(LIB) updated ---'

clean:	
	rm -f $(OBJ) $(LIB) 
