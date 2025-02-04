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
	$(MK5_F95)     -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(NERS_INC) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(NERS_INC) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95)     -I$(VTD_ROOT)/include  $(VTD_OPT_NOSTRUC) -I$(SPD_CLIENT_PREFIX)/include -I $(NERS_INC) -c -o $*.o $*.f  

EXE_DIR = $(VTD_ROOT)/bin

OBJS 	=               		\
		gti_update.o 		\
		viono_merge.o

OBJ1    =       bindisp_merge.o

OBJ2    =       difxvtd.o

OBJ3    =       gti_update_main.o

OBJ4    =       viono_merge_main.o 	

OBJ5    =       viono_show_header.o

OBJ6    =       viono_to_heb.o 		

OBJ7    =       vtd_control_update.o 

OBJ8    =       vtd_inq.o



LIBS =						\
	$(VTD_ROOT)/src/libvtd.a 		\
        -L$(SPD_CLIENT_LIB)   -lspc             \
        -L$(NERS_LIB) -lners                    \
        $(VTD_FITSLIB_LIB)  			\
        $(VTD_CFITSIO_LIB)    			\
	$(PETOOLS_LIB)				\
        $(CURL_LIB)    				\
        $(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS) 			\
	$(SOLVE_EXTRA_LIB) 			\
	-lz

all:	bin

bin:	$(OBJS) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(OBJ7) $(OBJ8) 
	$(MK5_LINK)   -o $(EXE_DIR)/bindisp_merge       $(OBJ1) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/difxvtd             $(OBJ2) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/gti_update          $(OBJ3) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_merge         $(OBJ4) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_show_header   $(OBJ5) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/viono_to_heb        $(OBJ6) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_control_update  $(OBJ7) $(OBJS) $(LIBS)
	$(MK5_LINK)   -o $(EXE_DIR)/vtd_inq             $(OBJ8) $(OBJS) $(LIBS)

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
                      $(EXE_DIR)/bindisp_merge  \
	      	      $(EXE_DIR)/difxvtd	\
	      	      $(EXE_DIR)/gti_update	\
                      $(EXE_DIR)/viono_show_header \
                      $(EXE_DIR)/viono_merge 	\
                      $(EXE_DIR)/viono_to_heb	\
		      $(EXE_DIR)/vtd_control_update \
                      $(EXE_DIR)/vtd_inq 
