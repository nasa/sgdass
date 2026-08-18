# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking atp                            *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.o_opt	.a
SHELL = /bin/csh -f -c 'umask 0002; shift; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)        $(MK5_F_OPENMP) -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o     $*.f  

.f.o_opt:
	$(MK5_F95_NOOPT)    $(MK5_F_OPENMP) -I$(PETOOLS_PREFIX)/include -I../include -c -o $*.o_opt $*.f  


LIB 	= libatp.a
ifeq ($(ATP_OS),Linux)
         SLIB 	= libatp.so.1.0
endif
ifeq ($(ATP_OS),Darwin)
	 SLIB   = libatp.1.dylib
endif

OBJS 	=           			\
	anc_clean.o                     \
	anc_parse.o_opt                 \
        anc_read.o                      \
        anc_type.o                      \
        anc_write.o                     \
        anc_scav.o                      \
        atp_array_compare.o             \
        atp_inq.o                       \
        atp_lists.o                     \
        atp_scans.o                     \
        anc_compute_tatm_opa.o          \
        bnc_parse.o                     \
        bnc_write.o                     \
        fmtgps_filter.o                 \
        get_pol.o                       \
        get_spl_tatm.o                  \
        get_sta.o                       \
        mode_stat.o                     \
        pcal_filter.o                   \
        pcal_frq_pol_idx.o              \
        pcal_stats.o                    \
        sefd_filter.o                   \
        sort2_i8.o                      \
        tsys_azel_plot.o                \
        tsys_filter.o                   \
        tsys_frq_pol_idx.o              \
        tsys_stats.o                    \
        tpi_filter.o                    \
        tpi_stats.o

OBJ1    = anc_to_bnc.o

OBJ2    = bnc_plot.o

OBJ3    = bnc_scav.o

OBJ4    = atp_get_tones.o

OBJ5    = atp_inq_main.o

OBJ6    = anc_compute_tatm_opa_main.o

OBJ7    = anc_scav_main.o

LIBS =					\
	$(PETOOLS_LIB) 			\
	$(NERS_LIB) 			\
	$(SPD_CLIENT_LIB) 		\
	$(SOLVE_LIB_PGPLOT)		\
	$(SOLVE_LIB_X11)		\
	$(SOLVE_LIB_VEC)		\
	$(SOLVE_LIB_BLAS)		\
	$(SOLVE_EXTRA_LIB) 		\

all:	lib shared bin

lib:	$(OBJS)
	ar  r $(LIB) $(OBJS)

        ifeq ($(ATP_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libatp.so.1 -o $(SLIB) $(OBJS)
        endif
        ifeq ($(ATP_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(ATP_LIB)/libatp.1.dylib \
                            -o $(SLIB) $(OBJS) $(LIBS)                            
        endif

bin: 	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/anc_to_bnc            $(OBJ1) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/bnc_plot              $(OBJ2) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/bnc_scav              $(OBJ3) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/atp_get_tones         $(OBJ4) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/atp_inq               $(OBJ5) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/anc_compute_tatm_opa  $(OBJ6) $(OBJS) $(LIBS)
	$(MK5_LINK) -o $(ATP_ROOT)/bin/anc_scav              $(OBJ7) $(OBJS) $(LIBS)


$(OBJS): ../include/atp.i
$(OBJ8): ../include/atp_local.i

clean:
	rm -f $(ATP_ROOT)/bin/anc_to_bnc
	rm -f $(ATP_ROOT)/bin/bnc_plot
	rm -f $(ATP_ROOT)/bin/bnc_scav
	rm -f $(ATP_ROOT)/bin/atp_get_tones
	rm -f $(ATP_ROOT)/bin/atp_inq
	rm -f $(ATP_ROOT)/bin/anc_compute_tatm_opa
	rm -f $(ATP_ROOT)/bin/anc_scav
	rm -f $(OBJS)
	rm -f $(OBJ1)
	rm -f $(OBJ2)
	rm -f $(OBJ3)
	rm -f $(OBJ4)
	rm -f $(OBJ6)
	rm -f $(OBJ7)
	rm -f $(LIB)
	rm -f $(SLIB)
	rm -f libatp.*
