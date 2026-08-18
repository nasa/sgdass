# ************************************************************************
# *                                                                      *
# *   Makefile  for copiling a set of executable s for compuation of the *
# *   atmospheric pressure loading.                                      *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.a	.d
SHELL = /bin/csh

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(APLO_ROOT)/bin

RES  =  sun_preproc

OBJS =  ${RES}.o

bin:	$(OBJS) 
	$(MK5_LINK) -o $(EXE_DIR)/$(RES) $(OBJS) 

clean:
	rm -f $(OBJS) $(EXE_DIR)/$(RES)

