# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling  vcat  library                             *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.o	.gen_o	.opt_o	.c	.a	.d
SHELL=/bin/csh

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR	= $(Ex)

LIB     = vcat.a

OBJ  =                          \
	mark3_to_gvf_name.o 	\
	gvf_to_mark3_name.o 	\
	vcat_get_conf.o 	\
	vcat_resolve_dbname.o   

lib:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Library is $(LIB) updated ---'

clean:	
	rm -f $(OBJ) $(LIB)

#
vcat_resolve_dbname.o: 	$(SOLVE_ROOT)/include/vcat.i
