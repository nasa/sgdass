.SUFFIXES:
.SUFFIXES:	.f	.o	.c	.a
SHELL = /bin/csh

.f.o:
	$(FCOMPL) $(FFLAGC) -I../src -c -o   $*.o $*.f  
	ar  r libpgplot.a $*.o

.c.o:
	$(CCOMPL) $(CFLAGC) -I../src -I../include -I../../include -I$(MK5_X11_INCLUDE) -c -o   $*.o $*.c  
	ar  r libpgplot.a $*.o

OBJECT =			\
		pgcloq.o	\
		pgendq.o 	

all:	$(OBJECT)

clean:	
	rm -f $(OBJECT)
