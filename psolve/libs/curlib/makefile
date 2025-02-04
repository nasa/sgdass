#######################################################################
#								      #
#	Make file for recompiling library  curlib.a     	      #
#						     		      #
#  ###  16-MAY-2003  makefile   v1.0  (c)  L. Petrov 16-MAY-2003 %%%  #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
#
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

LIB = curlib.a

OBJ =					\
		addstr_f.o 		\
		get_curlib.o 		\
		senkr_mn.o 		\
		set_curlib.o 		\
		term_flush_fort.o 	\
		save_term.o 		\
		start_mn.o 

all:	lib 

lib:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Library $(LIB) updated ---'

clean:	
	rm -f $(OBJ) $(LIB) 
