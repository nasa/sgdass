# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling  fitslib library                           *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.o	.gen_o	.opt_o	.c	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR	= $(Ex)

LIB     = libfitslib.a

ifeq ($(VTD_OS),Linux)
	SOLIB   = libfitslib.so.1
endif
ifeq ($(VTD_OS),Darwin)
	SOLIB   = libfitslib.1.dylib 
endif

OBJ  =                          	\
		ffits_lib.o 		\
		read_fits_vis.o 	\
		read_fits_map.o 	\
		fitslib_sou_free.o 


lib:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
        ifeq ($(VTD_OS),Linux)
		$(MK5_C_LINK) -shared -Wl,-soname,libfitslib.so.1 -o libfitslib.so.1.0 $(OBJ)
        endif
        ifeq ($(VTD_OS),Darwin)
		$(MK5_C_LINK) -dynamiclib -install_name libfitslib.1.dylib -o libfitslib.1.dylib $(OBJ) -L$(VTD_CFITSIO_DIR) -lcfitsio -L$(PETOOLS_PREFIX)/lib/ -lpetools
        endif
	@echo '--- Library is $(LIB) $(SOLIB) updated ---'

clean:	
	rm -f $(OBJ) $(LIB) libfitslib.so.1.0 libfitslib.1.dylib 

#
$(OBJ):	$(VTD_ROOT)/include/sou_map.i
