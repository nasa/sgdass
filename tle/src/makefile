# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking tle                            *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.o_opt	.a
SHELL = /bin/csh -f -c 'umask 0002; shift; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)        $(MK5_F_OPENMP) -I../include -I$(PETOOLS_PREFIX)/include -I$(VTD_INC) -I$(NERS_INC) -c -o $*.o $*.f  

.f.o_opt:
	$(MK5_F95_OPT)    $(MK5_F_OPENMP) -I../include -I$(PETOOLS_PREFIX)/include -I$(VTD_INC) -I$(NERS_INC) -c -o $*.o_opt $*.f  


LIB 	= libtle.a
ifeq ($(TLE_OS),Linux)
         SLIB 	= libtle.so.1.0
endif
ifeq ($(TLE_OS),Darwin)
	 SLIB   = libtle.1.dylib
endif

OBJS =               		     \
        dpper.o_opt                  \
        dscom.o_opt                  \
        dsinit.o_opt                 \
        dspace.o_opt                 \
        getgravconst.o_opt           \
        gstime.o_opt                 \
        initl.o_opt                  \
        newtonnu.o_opt               \
        sgp4.o_opt                   \
        sgp4init.o_opt               \
        tle_angle.o_opt              \
        tle_asinh.o_opt              \
        tle_cross.o_opt              \
        tle_days2mdhms.o_opt         \
        tle_dot.o_opt                \
        tle_invjday.o_opt            \
        tle_jday.o_opt               \
        tle_mag.o_opt                \
        tle_parser.o_opt             \
        tle_to_crs.o_opt             \
        tle_to_trs.o_opt             \
        tledate_to_mjdsec.o_opt      \
        twoline2rvsgp4.o

LIBS =					\
        $(PETOOLS_LIB)      		\
	$(VTD_LIB)			\
	$(NERS_LIB)			\
	$(SPC_LIB)			\
	$(CFISTIO_LIB)			\
        $(SOLVE_LIB_VEC)		\
        $(SOLVE_LIB_PGPLOT)		\
        $(SOLVE_LIB_X11) 		\
        $(SOLVE_LIB_BLAS)           	\
        $(SOLVE_EXTRA_LIB)

all:	lib shared bin

lib:	$(OBJS)
	ar  r $(LIB) $(OBJS)

        ifeq ($(TLE_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libtle.so.1 -o $(SLIB) $(OBJS)
        endif
        ifeq ($(TLE_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(TLE_LIB)/libtle.1.dylib \
                            -o $(SLIB) $(OBJS) $(LIBS)
        endif

bin:

clean:
	rm -f $(OBJS)
	rm -f $(LIB)
	rm -f $(SLIB)
	rm -f libtle.*
