# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   example_vtd_xx executables.  *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -Wstrict-prototypes -I$(VTD_ROOT)/include -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -I$(VTD_ROOT)/include  -I$(SPD_CLIENT_PREFIX)/include $(VTD_OPT_NOSTRUC) -I $(NERS_INC) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -I$(VTD_ROOT)/include  -I$(SPD_CLIENT_PREFIX)/include $(VTD_OPT_NOSTRUC) -I $(NERS_INC) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95)     -I$(VTD_ROOT)/include  -I$(SPD_CLIENT_PREFIX)/include $(VTD_OPT_NOSTRUC) -I $(NERS_INC) -c -o $*.o $*.f  

EXE_DIR = $(VTD_ROOT)/bin

OBJS 	=               		\
		pd_iono.o 		\
		vtd_example_01.o 	\
		vtd_example_02.o 	\
		vtd_example_03.o 	\
		vtd_example_04.o 	\
		vtd_example_05.o 	\
		vtd_example_06.o

LIBS =						\
	$(VTD_LIB_EXAMPLE)                      \
        -L$(NERS_LIB)         -lners            \
        -L$(SPD_CLIENT_LIB)   -lspc             \
        $(PETOOLS_LIB)      -lpetools   	\
        $(VTD_FITSLIB_LIB)  			\
        $(VTD_CFITSIO_LIB)    			\
        $(CURL_LIB)    				\
        $(SOLVE_LIB_VEC)			\
        $(SOLVE_LIB_PGPLOT)			\
        $(SOLVE_LIB_X11) 			\
        $(SOLVE_LIB_BLAS)           		\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJS)
	$(MK5_LINK)   -o $(EXE_DIR)/pd_iono        pd_iono.o        $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_example_01 vtd_example_01.o $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_example_02 vtd_example_02.o $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_example_03 vtd_example_03.o $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_example_04 vtd_example_04.o $(LIBS)
	$(MK5_C_LINK) -o $(EXE_DIR)/vtd_example_05 vtd_example_05.o $(LIBS)
	$(MK5_C_LINK) -o $(EXE_DIR)/vtd_example_06 vtd_example_06.o $(LIBS)

clean:  
	rm -f $(OBJS) $(EXE_DIR)/vtd_example_01 vtd_example_01.o \
		      $(EXE_DIR)/vtd_example_02 vtd_example_02.o \
		      $(EXE_DIR)/vtd_example_03 vtd_example_03.o \
		      $(EXE_DIR)/vtd_example_04 vtd_example_04.o \
		      $(EXE_DIR)/vtd_example_05 vtd_example_05.o \
		      $(EXE_DIR)/vtd_example_06 vtd_example_06.o \
		      $(EXE_DIR)/pd_iono             pd_iono.o
distclean:  clean
	rm -f vtd_example_01.f \
	      vtd_example_02.f \
	      vtd_example_03.f \
	      vtd_example_04.f \
	      vtd_example_05.f \
	      vtd_example_06.f
