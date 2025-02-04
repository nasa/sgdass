# ************************************************************************
# *                                                                      *
# *   Makefile for compiling and linking a set of executables for        *
# *   computation of slant path delay.                                   *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.noopt	.a	.d
SHELL = /bin/csh

.c.o:
	$(MK5_C) -c -o $*.o $*.c

# 
.f.o:
	$(MK5_F95)       $(SPD_OPENMP) -I$(SPD_ROOT)/include -I$(PETOOLS_PREFIX)/include -c -o $*.o $*.f

.f.opt_o:
	$(MK5_F95_OPT)   $(SPD_OPENMP) -I$(SPD_ROOT)/include -I$(PETOOLS_PREFIX)/include -c -o $*.opt_o $*.f

.f.noopt:
	$(MK5_F95_NOOPT) $(SPD_OPENMP) -I$(SPD_ROOT)/include -I$(PETOOLS_PREFIX)/include -c -o $*.noopt $*.f


OBJS =                  	\
	anti_parse.o 		\
	att_itu_r13.opt_o       \
	bspd_move_lock.o 	\
	bspd_util.o 	        \
	comp_pd_2point.opt_o 	\
	create_bspd_summary.o   \
	del_isa.opt_o 		\
	hlp_to_xi_der.opt_o 	\
	get_refr_der.opt_o 	\
	nmf.o                   \
	pd_proc.opt_o 		\
	sob_comp.noopt          \
	spd_2d_zpd_wri.o 	\
	spd_2d_mzpd_wri.o 	\
	spd_3d_bin_read.o 	\
	spd_3d_bin_write.o 	\
	spd_3d_conf.o 		\
	spd_3d_comp.opt_o 	\
	spd_3d_del.opt_o 	\
	spd_3d_inp_sta.o 	\
	spd_3d_load.o 		\
	spd_3d_read.o 		\
	spd_3d_refra.opt_o 	\
	spd_3d_slant.opt_o	\
	spd_3d_write.o 		\
	spd_3d_zen.opt_o	\
	spd_4d_get_inds.opt_o 	\
	spd_att_intr.opt_o      \
 	spd_cli.o 		\
	spd_daemon_config.o     \
	spd_del_init.o          \
	spd_del_quit.o          \
	spd_free.o 		\
	spd_gaussian_layer_mf.o \
	spd_get_att.opt_o       \
	spd_get_inds.opt_o 	\
	spd_get_temp.opt_o 	\
	spd_init.o 		\
	spd_intrp_delay.opt_o 	\
	spd_load_bspd.o 	\
	spd_load_leapsec.o 	\
        spd_load_mf.o 	        \
	spd_quit.o 		\
	spd_ref.opt_o           \
	spd_sig_proc.o 		\
        spd_tai_to_utc.o	\
        spd_utc_to_tai.o 	\
	units_convert.o 	\
	vtd_rotmat.opt_o 	\
	xyz_to_hlp.opt_o 	\
	zpd_to_ascii.o

OBJE =  \
        bspd_util_main.o        \
	merge_spd.o 		\
	pd_2point.o		\
        sob_show.o              \
	spd_3d_inq.o 		\
	spd_3d_main.o 		\
	spd_3d_toser.o 		\
	spd_cli_example.o 	\
	spd_inq.o 		\
	spd_resp.o              \
	spd_show.o 		\
	spd_daemon.o		\
	spd_server.o 		\
	spd_smooth.o            \
	zendel_grid_heb.o 	\
	zpd_to_ascii_main.o 

LIB  =  $(SPD_ROOT)/lib/libspd

LIBS =						\
	-L $(SPD_ROOT)/lib -lspd 		\
	-L $(SPD_MALO)/lib -lmalo 		\
	-L $(SPD_VTD)/lib -lvtd 		\
	-L $(SPD_NERS)/lib -lners 		\
	-L $(SPD_CFITSIO)/lib -lcfitsio		\
	$(FOURPACK_LIB) 			\
	$(FFTW_LIB) 				\
	$(PETOOLS_LIB)				\
        -L$(SPD_HDF4_LIB) -lmfhdf -ldf          \
        -L$(SPD_HDF5_LIB) -lhdf5 -lhdf5_hl      \
	-L$(SPD_NETCDF_LIB) -lnetcdff -lnetcdf  \
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_M) 				\
        $(SPD_JPEG) 				\
        $(SPD_Z) 

SPD_LIB   = $(SPD_ROOT)/lib/libspd.a

LIBS_STATIC = $(SPD_LIB)			\
	      $(SPD_MALO)/lib/libmalo.a		\
              $(SPD_VTD)/lib/libvtd.a           \
              $(SPD_VTD)/lib/libfitslib.a       \
              $(SPD_NERS)/lib/libners.a         \
	      $(SPD_CFITSIO)/lib/libcfitsio.a   \
	      $(SPD_CFITSIO)/lib/libcfitsio.a   \
	      $(SPD_FOURPACK)/lib/libfourpack.a \
	      $(FFTW_LIB) 			\
	      $(PETOOLS_LIB)			\
              -L$(SPD_HDF4_LIB) -lmfhdf -ldf    \
              -L$(SPD_HDF5_LIB) -lhdf5 -lhdf5_hl      \
	      -L$(SPD_NETCDF_LIB) -lnetcdff -lnetcdf  \
	      $(SOLVE_LIB_PGPLOT) 		\
	      $(SOLVE_LIB_X11)			\
              $(SOLVE_LIB_VEC)			\
	      $(SOLVE_LIB_BLAS) 		\
	      $(SOLVE_EXTRA_LIB) 		\
	      $(SOLVE_LIB_M) 			\
              $(SPD_JPEG) 			\
              $(SPD_Z) 

all:	      spd_lib bin

spd_lib:		$(OBJS) 
	ar r $(SPD_LIB) $(OBJS) 
        ifeq ($(SPD_OS),Linux)
		$(MK5_C_LINK) -shared -Wl,-soname,libspd.so.1 -o $(LIB).so.1.0  $(OBJS) 
		if ( -f  $(LIB).so.1 ) rm $(LIB).so.1   
		if ( -f  $(LIB).so   ) rm $(LIB).so
		cd $(SPD_ROOT)/lib; ln -sf libspd.so.1.0  $(LIB).so.1
		cd $(SPD_ROOT)/lib; ln -sf libspd.so.1.0  $(LIB).so
        endif
        ifeq ($(SPD_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(SPD_ROOT)/lib/$(SPD_SOLIB) \
                            -o $(SPD_ROOT)/lib/$(SPD_SOLIB)  $(OBJS) $(LIBS)
	      cd $(SPD_ROOT)/lib; if ( -f libspd.dylib ) rm libspd.dylib 
	      cd $(SPD_ROOT)/lib; ln -s $(SPD_SOLIB)        libspd.dylib 
        endif

bin:	spd_lib spd_smooth spd_3d spd_3d_static spd_3d_toser \
        spd_inq spd_3d_inq sob_show spd_show spd_resp pd_2point spd_daemon  \
        spd_server spd_cli_example merge_spd zendel_grid_heb zpd_to_ascii   \
        bspd_util


bspd_util: $(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/bspd_util \
	bspd_util_main.o $(OBJS) $(LIBS) 

merge_spd:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/merge_spd \
        merge_spd.o $(OBJS) $(LIBS) 

pd_2point:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/pd_2point \
        pd_2point.o $(OBJS) $(LIBS) 

spd_3d:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_3d \
        spd_3d_main.o $(OBJS) $(LIBS) 

spd_3d_static:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_3d_static \
        spd_3d_main.o $(OBJS) $(LIBS_STATIC)

spd_3d_toser:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_3d_toser \
        spd_3d_toser.o $(OBJS) $(LIBS) 

spd_3d_inq:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_3d_inq \
        spd_3d_inq.o $(OBJS) $(LIBS) 

spd_inq:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_inq \
        spd_inq.o $(OBJS) $(LIBS) 

sob_show: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/sob_show \
        sob_show.o $(OBJS) $(LIBS) 

spd_show: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_show \
        spd_show.o $(OBJS) $(LIBS) 

spd_resp: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_resp \
	spd_resp.o $(OBJS) $(LIBS) 

spd_daemon: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_daemon \
	spd_daemon.o $(OBJS) $(LIBS) 

spd_server: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_server \
	spd_server.o $(OBJS) $(LIBS) 

spd_cli_example: 	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_cli_example \
	spd_cli_example.o $(OBJS) $(LIBS) 

spd_smooth: 		$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/spd_smooth \
	spd_smooth.o $(OBJS) $(LIBS) 

zendel_grid_heb:	$(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/zendel_grid_heb \
	zendel_grid_heb.o $(OBJS) $(LIBS) 

zpd_to_ascii:	  $(OBJS) $(OBJE) spd_lib
	$(MK5_LINK) -o $(SPD_ROOT)/bin/zpd_to_ascii \
	zpd_to_ascii_main.o $(OBJS) $(LIBS) 

$(OBJS): 		$(SPD_ROOT)/include/spd.i
$(OBJS): 		$(SPD_ROOT)/include/spd_local.i
$(OBJE): 		$(SPD_ROOT)/include/spd.i
$(OBJE): 		$(SPD_ROOT)/include/spd_local.i

clean:
	rm -f $(SPD_ROOT)/bin/bspd_util         \
	      $(SPD_ROOT)/bin/merge_spd         \
              $(SPD_ROOT)/bin/spd_3d       	\
              $(SPD_ROOT)/bin/spd_3d_inq   	\
              $(SPD_ROOT)/bin/spd_3d_static     \
              $(SPD_ROOT)/bin/spd_3d_toser 	\
              $(SPD_ROOT)/bin/spd_inq           \
              $(SPD_ROOT)/bin/spd_resp     	\
              $(SPD_ROOT)/bin/spd_show     	\
              $(SPD_ROOT)/bin/spd_smooth     	\
              $(SPD_ROOT)/bin/sob_show     	\
              $(SPD_ROOT)/bin/pd_2point    	\
              $(SPD_ROOT)/bin/spd_daemon   	\
              $(SPD_ROOT)/bin/spd_server   	\
	      $(SPD_ROOT)/bin/spd_cli_example 	\
              $(SPD_ROOT)/bin/zendel_grid_heb   \
              $(SPD_ROOT)/bin/zpd_to_ascii      \
	      $(OBJM) $(OBJS) $(OBJE) $(SPD_LIB) $(SPD_SHLIB)   \
	      $(SPD_ROOT)/lib/libspd.a
        ifeq ($(SPD_OS),Linux)
		rm	$(SPD_ROOT)/lib/libspd.so.1.0     \
                        $(SPD_ROOT)/lib/libspd.so.1       \
                        $(SPD_ROOT)/lib/libspd.so ; set status = 0
        endif
        ifeq ($(SPD_OS),Darwin)
		rm	$(SPD_ROOT)/lib/libspd.1.dylib \
                        $(SPD_ROOT)/lib/libspd.dylib ; set status = 0
        endif
