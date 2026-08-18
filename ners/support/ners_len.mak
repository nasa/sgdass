# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  ners_len program.             *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.noopt	.a
SHELL = /bin/csh

.c.o:
	$(MK5_C) -c -o $*.o $*.c

.f.o:
	$(MK5_F95) -I$(NERS_ROOT)/include -c -o $*.o $*.f

RES  =  ners_len
        
OBJS = 	$(NERS_ROOT)/support/$(RES).o

all:	bin

bin: 	$(OBJS)
	$(MK5_LINK) -o  $(NERS_ROOT)/support/$(RES) $(OBJS) $(LIBS)

clean:
	rm  -f $(OBJS)  $(NERS_ROOT)/support/$(RES)
