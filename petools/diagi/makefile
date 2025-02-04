# ************************************************************************
# *                                                                      *
# *   Makefile  for creation of diagi library.                           *
# *                                                                      *
# *   NB: library diagi should be compiled and linked from the           *
# *   directory where source code is located.                            *
# *                                                                      *
# *  ###  23-OCT-97   makefile    v3.1  (c)  L. Petrov 25-MAR-2004  ###  *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

LIB       = diagi.a

MOD       = module_diagi.d

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o   $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

OBJS =                  	\
	diagi.o			\
	diagi_bst.o		\
	diagi_ccl.o		\
	diagi_chf.o		\
	diagi_dra.o		\
	diagi_hlp.o		\
	diagi_inq.o		\
	diagi_lst.o		\
	diagi_met.o		\
	diagi_mul.o		\
	diagi_ost.o		\
	diagi_prn.o		\
	diagi_pst.o		\
	diagi_res.o		\
	diagi_sav.o		\
	diagi_vie.o		\
	diagi_wst.o		\
	define_palette.o 	\
	hammer_transform.o 	\
	multi_diagi.o


lib:    $(OBJS)
	ar r  $(LIB) $(OBJS)
	@echo '--- Library $(LIB) updated ---'

$(OBJS): $(PETOOLS_ROOT)/include/diagi.i

clean:
	rm -f $(OBJS) 
	rm -f $(LIB) 
	rm -f $(MOD)
