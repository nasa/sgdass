# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking a set of executables for        *
# *   computation of slanted path delay.                                 *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(SPC_C) -c -o $*.o $*.c

.f.o:
	$(SPC_F95) $(SPC_OPENMP) -I$(SPC_ROOT)/include -I$(SPC_PETOOLS_INC) -D SPC -D $(SPC_PETOOLS_USE) -c -o $*.o $*.f

.f.opt_o:
	$(SPC_F95_OPT)   $(SPC_OPENMP) -I$(SPC_ROOT)/include -I$(SPC_PETOOLS_INC) -D SPC -D $(SPC_PETOOLS_USE) -c -o $*.opt_o $*.f

OBJS =                  		\
					\
	bspd_move_lock.o 		\
	bspd_util.o 	        	\
	create_bspd_summary.o   	\
	del_isa.o 			\
	get_opa_tatm_spectrum.o 	\
	get_opa_tatm_tipping.o 	        \
	get_spd_res.o 	                \
	get_spd_res_azel.o 	        \
	nmf.o                   	\
	spd_3d_bin_read.o 		\
	spd_3d_bin_write.o 		\
	spd_3d_read.o 			\
	spd_3d_write.o 			\
	spd_alias.o 			\
	spd_cli.o 			\
	spd_client_version.o 		\
	spd_client_version_check.o 	\
	spd_del_init.o          	\
	spd_del_quit.o          	\
	spd_free.o 			\
        spd_gaussian_layer_mf.o 	\
	spd_init.o 			\
        spd_intrp_delay.o 		\
        spd_load_bspd.o                 \
	spd_load_leapsec.o 		\
	spd_load_mf.o           	\
	spd_res_intrp.o 		\
        spd_tai_to_utc.o 		
         

OBJ1 =  \
	spd_show.o 		

OBJ2 =  \
	sob_show.o

OBJ3 =  \
	spd_3d_toser.o 

OBJ4 =  \
	spc_inq.o

OBJ5 =  \
	opa_tatm_spectrum.o

OBJ6 =  \
	opa_tatm_tipping.o

OBJ7 =  \
	spd_res_azel.o

OBJ8 =  \
	spd_res_ts.o

OBJ9 =  \
	bspd_util_main.o

LIB = libspc
SPC_LIB   = $(SPC_ROOT)/lib/$(LIB).a

LIBS =				\
	$(SPC_LIB) 		\
	$(SPC_PETOOLS_LIB) 	\
	$(SPC_LAPACK_LIB) 	\
	$(SPC_LDFLAGS) 

all:	spc_shared_lib bin

spc_lib:		$(OBJS) 
	ar r $(SPC_LIB) $(OBJS) 

spc_shared_lib:		spc_lib
        ifeq ($(SPC_OS),Linux)
		$(SPC_C_LINK) -shared -Wl,-soname,libspc.so.1 -o $(SPC_ROOT)/lib/$(LIB).so.1.0  $(OBJS) $(LIBS)
		if ( -f  $(LIB).so.1 ) rm $(LIB).so.1   
		if ( -f  $(LIB).so   ) rm $(LIB).so
		cd $(SPC_ROOT)/lib; ln -sf libspc.so.1.0  $(LIB).so.1
		cd $(SPC_ROOT)/lib; ln -sf libspc.so.1.0  $(LIB).so
		$(SPC_ROOT)/support/gen_libspc_vars.csh   $(SPC_ROOT)/lib/$(LIB).so.1 $(SPC_PREFIX)/lib/$(LIB).so.1
        endif
        ifeq ($(SPC_OS),Darwin)
	      $(SPC_C_LINK) -dynamiclib -install_name $(SPC_ROOT)/lib/$(LIB).1.dylib \
                            -o $(SPC_ROOT)/lib/$(LIB).1.dylib   $(OBJS) $(LIBS)
	      cd $(SPC_ROOT)/lib; if ( -f libspc.dylib ) rm libspc.dylib 
	      cd $(SPC_ROOT)/lib; ln -s $(SPC_ROOT)/lib/$(LIB).1.dylib libspc.dylib 
	      $(SPC_ROOT)/support/gen_libspc_vars.csh  $(SPC_ROOT)/lib/$(LIB).1.dylib $(SPC_PREFIX)/lib/$(LIB).1.dylib
        endif

bin:	sob_show spc_inq spd_res_azel spd_res_ts spd_show spd_3d_toser \
        opa_tatm_spectrum opa_tatm_tipping spc_inq bspd_util

spd_show: 	$(OBJS) $(OBJ1)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/spd_show $(OBJ1) $(OBJS) $(LIBS) 

sob_show: 	$(OBJS) $(OBJ2)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/sob_show $(OBJ2) $(OBJS) $(LIBS) 

spd_3d_toser: 	$(OBJS) $(OBJ3)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/spd_3d_toser $(OBJ3) $(OBJS) $(LIBS) 

spc_inq: 	$(OBJ4)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/spc_inq $(OBJ4) $(LIBS) 

opa_tatm_spectrum:  $(OBJ5)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/opa_tatm_spectrum $(OBJ5) $(LIBS) 

opa_tatm_tipping:  $(OBJ6)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/opa_tatm_tipping $(OBJ6) $(LIBS) 

spd_res_azel:	    $(OBJ7)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/spd_res_azel $(OBJ7) $(LIBS) 

spd_res_ts:	    $(OBJ8)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/spd_res_ts $(OBJ8) $(LIBS) 

bspd_util:	    $(OBJ9)
	$(SPC_LINK) -o $(SPC_ROOT)/bin/bspd_util $(OBJ9) $(LIBS) 

clean:
	rm -f \
		$(SPC_ROOT)/bin/opa_tatm_spectrum \
		$(SPC_ROOT)/bin/opa_tatm_tipping  \
		$(SPC_ROOT)/bin/spc_inq           \
		$(SPC_ROOT)/bin/spd_3d_toser      \
		$(SPC_ROOT)/bin/spd_res_azel  	  \
                $(SPC_ROOT)/bin/spd_res_ts  	  \
                $(SPC_ROOT)/bin/spd_show          \
		$(SPC_ROOT)/bin/sob_show      	  \
		$(SPC_ROOT)/bin/bspd_util      	  \
		$(OBJ1) \
		$(OBJ2) \
		$(OBJ3) \
		$(OBJ4) \
		$(OBJ5) \
		$(OBJ6) \
		$(OBJ7) \
		$(OBJ8) \
		$(OBJ9) \
		$(OBJS) \
		$(SPC_LIB)
        ifeq ($(SPC_OS),Linux)
		if ( -f $(SPC_ROOT)/lib/libspc.so.1.0 )	rm $(SPC_ROOT)/lib/libspc.so.1.0
		if ( -f $(SPC_ROOT)/lib/libspc.so.1   )	rm $(SPC_ROOT)/lib/libspc.so.1
		if ( -f $(SPC_ROOT)/lib/libspc.so     )	rm $(SPC_ROOT)/lib/libspc.so
        endif
        ifeq ($(SPC_OS),Darwin)
		if ( -f $(SPC_ROOT)/lib/libspc.1.dylib ) rm $(SPC_ROOT)/lib/libspc.1.dylib 
		if ( -f $(SPC_ROOT)/lib/libspc.dylib   ) rm $(SPC_ROOT)/lib/libspc.dylib 
        endif
