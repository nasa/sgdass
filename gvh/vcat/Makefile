# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling  vcat  library                             *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'
.SUFFIXES:
.SUFFIXES:	.f	.o	.opt_o	.c	.a

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -D GVH_STANDALONE -I$(GVH_ROOT)/include -I$(PETOOLS_ROOT)/include -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -D GVH_STANDALONE -I$(GVH_ROOT)/include -I$(PETOOLS_ROOT)/include -c -o $*.opt_o $*.f  


EXE_DIR	= $(Ex)

LIB     = $(GVH_ROOT)/lib/libvcat.a
ifeq ($(GVH_OS),Linux)
        SLIB 	= $(GVH_ROOT)/lib/libvcat.so.1.0
endif
ifeq ($(GVH_OS),Darwin)
        SLIB 	= $(GVH_ROOT)/lib/libvcat.1.dylib
endif

OBJ  =                          \
	vcat_get_conf.o 	\
	mark3_to_gvf_name.o 	\
	gvf_to_mark3_name.o 	\
	vcat_resolve_dbname.o   

lib:	$(OBJ)

	ar  r $(LIB) $(OBJ) 
	@echo '--- Library is $(LIB) updated ---'
        ifeq ($(GVH_OS),Linux)
	      $(MK5_C) -shared -shared -Wl,-soname,libvcat.so.1 -o $(SLIB) $(OBJ)
	      if ( -l $(GVH_ROOT)/lib/libvcat.so  ) rm -f $(GVH_ROOT)/lib/libvcat.so
	      ln -s $(GVH_ROOT)/lib/libvcat.so.1.0 $(GVH_ROOT)/lib/libvcat.so
        endif
        ifeq ($(GVH_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(GVH_ROOT)/lib/libvcat.1.dylib \
                            -o $(SLIB) $(OBJ) \
                             $(VTD_LIB) \
                            -L$(PETOOLS_LIB)
	      if ( -l $(GVH_ROOT)/lib/libvcat.dylib ) rm -f $(GVH_ROOT)/lib/libvcat.dylib
	      ln -s $(GVH_ROOT)/lib/libvcat.1.dylib $(GVH_ROOT)/lib/libvcat.dylib 
        endif
	@echo '--- Shared library is $(SLIB) created ---'

clean:	
	rm -f $(OBJ) $(LIB) $(SLIB)
	if ( -l $(GVH_ROOT)/lib/libvcat.so    ) rm -f $(GVH_ROOT)/lib/libvcat.so
	if ( -l $(GVH_ROOT)/lib/libvcat.dylib ) rm -f $(GVH_ROOT)/lib/libvcat.dylib

#
vcat_resolve_dbname.o: 	$(GVH_ROOT)/include/vcat.i
