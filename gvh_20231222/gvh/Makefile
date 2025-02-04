# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  gvh library and executables   *
# *                                                                      *
# ************************************************************************
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.o_opt	.a

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95)      -D GVH_STANDALONE -I$(GVH_ROOT)/include -I $(NERS_INC) -I$(PETOOLS_ROOT)/include -c -o $*.o     $*.f  

.f.o_opt:
	$(MK5_F95_OPT)  -D GVH_STANDALONE -I$(GVH_ROOT)/include -I $(NERS_INC) -I$(PETOOLS_ROOT)/include -c -o $*.o_opt $*.f  


EXE_DIR = $(GVH_ROOT)/bin

LIB     = $(GVH_ROOT)/lib/libgvh.a
ifeq ($(GVH_OS),Linux)
        SLIB 	= $(GVH_ROOT)/lib/libgvh.so.1.0
endif
ifeq ($(GVH_OS),Darwin)
        SLIB 	= $(GVH_ROOT)/lib/libgvh.1.dylib
endif

OBJ  =                          \
	gvh_allocate.o		\
	gvh_cache_free.o 	\
	gvh_compar_lcd.o 	\
	gvh_compar_lct.o 	\
	gvh_dtoc.o		\
	gvh_exch_lcode1.o	\
	gvh_exch_lcode2.o	\
	gvh_free.o		\
	gvh_from_mark3.o	\
	gvh_from_nusolve.o	\
	gvh_get_lcode_list.o	\
	gvh_glcode.o		\
	gvh_gprea.o		\
	gvh_gtext_chp.o		\
	gvh_init.o		\
	gvh_inq_lcode.o 	\
	gvh_lcode_tab_inq.o	\
	gvh_obs_tab_inq.o	\
	gvh_phlcode.o		\
	gvh_plcode.o		\
	gvh_pprea.o		\
	gvh_preget.o		\
	gvh_preput.o		\
	gvh_ptext_chp.o		\
	gvh_ptoc.o		\
        gvh_put_inline.o	\
        gvh_put_obs_tab.o	\
	gvh_read_agv.o_opt	\
	gvh_read_bgv.o		\
	gvh_read_bin.o		\
	gvh_read_db.o		\
	gvh_release.o 		\
	gvh_skip_bin.o		\
	gvh_version.o		\
	gvh_version_check.o	\
	gvh_wri_db.o		\
	gvh_write_agv.o_opt	\
	gvh_write_bgv.o		\
	gvh_write_bin.o		\
	gvh_write_pgv.o	

OBJ1  =                          \
	gvf_transform.o

OBJ2  =                         \
	gvf_db.o 		\
	gvf_db_read.o 		\
	gvf_db_get.o 		\
	gvf_db_get_env_dir.o 	\
	gvf_db_write.o 		

OBJ3  =                         \
	gvf_supr_promote.o 

OBJ4  =                         \
	gvf_grambig_reset.o 

OBJ5  =                         \
	gvf_repair_db.o

OBJ6  =                         \
	gvf_set_par.o

LIBS =						\
	-L $(GVH_ROOT)/lib -lvcat -lgvh         \
	$(NERS_LIB)				\
	$(PETOOLS_LIB)                          \
	$(SOLVE_LIB_PGPLOT)			\
	$(SOLVE_LIB_X11)			\
	$(SOLVE_EXTRA_LIB) 			\
	$(SOLVE_LIB_VEC)			\
	$(SOLVE_LIB_BLAS)

lib:	static shared

static:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Static library $(LIB) is updated ---'

shared:	$(OBJ)
        ifeq ($(GVH_OS),Linux)
	      $(MK5_C_LINK) -shared -Wl,-soname,libgvh.so.1 -o $(SLIB) $(OBJ)
	      if ( -l $(GVH_ROOT)/lib/libgvh.so    ) rm -f $(GVH_ROOT)/lib/libgvh.so
	      ln -s $(GVH_ROOT)/lib/libgvh.so.1.0 $(GVH_ROOT)/lib/libgvh.so
        endif
        ifeq ($(GVH_OS),Darwin)
	      $(MK5_C_LINK) -dynamiclib -install_name $(GVH_PREFIX)/lib/libgvh.1.dylib \
                            -o $(SLIB) $(OBJ) 	\
                             $(VTD_LIB) 	\
                             $(NERS_LIB) 	\
                             $(PETOOLS_LIB)
	      if ( -l $(GVH_ROOT)/lib/libgvh.dylib ) rm -f $(GVH_ROOT)/lib/libgvh.dylib
	      ln -s $(GVH_ROOT)/lib/libgvh.1.dylib $(GVH_ROOT)/lib/libgvh.dylib 
        endif
	@echo '--- Shared library $(SLIB) is created ---'


bin:    gvf_transform gvf_db gvf_supr_promote gvf_grambig_reset gvf_repair_db gvf_set_par

gvf_transform: 		$(OBJ1)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_transform     $(OBJ1) $(LIBS)

gvf_db: 		$(OBJ2)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_db            $(OBJ2)  $(LIBS)

gvf_supr_promote: 	$(OBJ3)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_supr_promote  $(OBJ3) $(LIBS)

gvf_grambig_reset: 	$(OBJ4)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_grambig_reset $(OBJ4) $(LIBS)

gvf_repair_db:	     	$(OBJ5)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_repair_db     $(OBJ5) $(LIBS)

gvf_set_par:	     	$(OBJ6)
	$(MK5_LINK)  -o $(EXE_DIR)/gvf_set_par      $(OBJ6) $(LIBS)


clean:	
	rm -f $(OBJ) $(OBJ1) $(OBJ2) $(OBJ3) $(OBJ4) $(OBJ5) $(OBJ6) $(LIB) $(SLIB) \
        $(EXE_DIR)/gvf_transform 	\
	$(EXE_DIR)/gvf_db 		\
	$(EXE_DIR)/gvf_supr_promote 	\
	$(EXE_DIR)/gvf_grambig_reset    \
	$(EXE_DIR)/gvf_repair_db        \
	$(EXE_DIR)/gvf_set_par
	if ( -l $(GVH_ROOT)/lib/libgvh.so    ) rm -f $(GVH_ROOT)/lib/libgvh.so
	if ( -l $(GVH_ROOT)/lib/libgvh.dylib ) rm -f $(GVH_ROOT)/lib/libgvh.dylib

#
gvh_allocate.o:		$(GVH_ROOT)/include/gvh.i
gvh_exch_lcode1.o:	$(GVH_ROOT)/include/gvh.i
gvh_exch_lcode2.o:	$(GVH_ROOT)/include/gvh.i
gvh_free.o:		$(GVH_ROOT)/include/gvh.i
gvh_glcode.o:           $(GVH_ROOT)/include/gvh.i
gvh_init.o:		$(GVH_ROOT)/include/gvh.i
gvh_obs_tab_inq.o:	$(GVH_ROOT)/include/gvh.i
gvh_phlcode.o:          $(GVH_ROOT)/include/gvh.i
gvh_plcode.o:           $(GVH_ROOT)/include/gvh.i
gvh_pprea.o:		$(GVH_ROOT)/include/gvh.i
gvh_preput.o:		$(GVH_ROOT)/include/gvh.i
gvh_ptext_buf.o:	$(GVH_ROOT)/include/gvh.i
gvh_ptoc.o:		$(GVH_ROOT)/include/gvh.i
gvh_put_obs_tab.o:	$(GVH_ROOT)/include/gvh.i
gvh_write_agv.o:	$(GVH_ROOT)/include/gvh.i
gvh_write_bgv.o:	$(GVH_ROOT)/include/gvh.i
gvf_set_par.o:          $(GVH_ROOT)/include/gvh.i

gvf_db.o: 		$(GVH_ROOT)/include/gvf_db.i
gvf_db_read.o: 		$(GVH_ROOT)/include/gvf_db.i
gvf_db_get.o: 		$(GVH_ROOT)/include/gvf_db.i
gvf_db_write.o:		$(GVH_ROOT)/include/gvf_db.i

gvf_supr_promote.o:	$(GVH_ROOT)/include/gvf_db.i
grambig_reset.o:	$(GVH_ROOT)/include/gvf_db.i
gvh_from_nusolve.o:	$(GVH_ROOT)/include/gvf_db.i

gvf_set_par.o:          $(GVH_ROOT)/include/gvf_db.i
