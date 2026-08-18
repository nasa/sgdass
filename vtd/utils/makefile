# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking   vtd utils executables.       *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -Wstrict-prototypes -I$(VTD_ROOT)/include -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)     -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(PETOOLS_PREFIX)/include -I $(NERS_INC) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(PETOOLS_PREFIX)/include -I $(NERS_INC) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95)     -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(PETOOLS_PREFIX)/include -I $(NERS_INC) -c -o $*.o $*.f  

EXE_DIR = $(VTD_ROOT)/bin

OBJS 	=               		\
		gti_update.o 		\
		parse_stapos.o 		\
		stapos_to_bindisp.o     \
		viono_merge.o

OBJ1    =       bindisp_merge.o

OBJ2    =       difxvtd.o

OBJ3    =       gti_update_main.o

OBJ4    =       viono_merge_main.o 	

OBJ5    =       viono_show_header.o

OBJ6    =       viono_to_heb.o 		

OBJ7    =       vtd_control_update.o 

OBJ8    =       vtd_inq_main.o

OBJ9    =       stapos_to_bin.o

OBJ10    =      vtd_antex_to_bintex.o

LIBS =						\
	$(VTD_ROOT)/src/libvtd.a 		\
        -L$(SPD_CLIENT_LIB)   -lspc             \
        -L$(NERS_LIB) -lners                    \
        -L$(TLE_LIB) -ltle                      \
        $(VTD_FITSLIB_LIB)  			\
        $(VTD_CFITSIO_LIB)    			\
	$(PETOOLS_LIB)				\
        $(CURL_LIB)    				\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	-lz

all:	bin

bin:	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6)  $(OBJ7) $(OBJ8) $(OBJ9) $(OBJ10)
	$(MK5_LINK)   -o $(EXE_DIR)/bindisp_merge       $(OBJ1)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/difxvtd             $(OBJ2)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/gti_update          $(OBJ3)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_merge         $(OBJ4)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_show_header   $(OBJ5)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_to_heb        $(OBJ6)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_control_update  $(OBJ7)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_inq             $(OBJ8)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/stapos_to_bin       $(OBJ9)  $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_antex_to_bintex $(OBJ10) $(OBJS) $(LIBS)

$(OBJ1): 	$(VTD_ROOT)/include/difxvtd.i

clean:  
	rm -f $(OBJS) $(OBJS)			\
	      	      $(OBJ1)			\
	      	      $(OBJ2)			\
	      	      $(OBJ3)			\
	      	      $(OBJ4)			\
	      	      $(OBJ5)			\
	      	      $(OBJ6)			\
	      	      $(OBJ7)			\
	      	      $(OBJ8)			\
	      	      $(OBJ9)			\
                      $(EXE_DIR)/bindisp_merge      \
	      	      $(EXE_DIR)/difxvtd	    \
	      	      $(EXE_DIR)/gti_update	    \
                      $(EXE_DIR)/viono_show_header  \
                      $(EXE_DIR)/viono_merge        \
                      $(EXE_DIR)/viono_to_heb	    \
		      $(EXE_DIR)/vtd_control_update \
                      $(EXE_DIR)/vtd_inq            \
                      $(EXE_DIR)/stapos_to_bin      \
                      $(EXE_DIR)/vtd_antex_to_bintex
