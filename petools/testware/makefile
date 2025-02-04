# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  diagi_exe  executable         *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0022; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(PETOOLS_ROOT)/bin
SUPPORT = $(PETOOLS_ROOT)/support

OBJS =				\
	diagi_batch.o		\
	diagi_color_demo.o 	\
	diagi_dec.o 		\
	diagi_demo.o 		\
	diagi_key.o 		\
	diagi_rst.o 		\
	diagi_user.o 		\
        matvec_test.o 		\
	md_demo.o 

LIBS  = 					\
	$(PETOOLS_ROOT)/diagi/diagi.a		\
	$(PETOOLS_ROOT)/pet_util/pet_util.a	\
        $(PETOOLS_ROOT)/matvec/matvec.a		\
	$(PETOOLS_ROOT)/lib/libpgplot.a         \
        $(SOLVE_LIB_X11)			\
        $(SOLVE_LIB_XT)				\
        $(SOLVE_LIB_XHP11)			\
        $(SOLVE_EXTRA_LIB)			\
        $(SOLVE_LIB_VEC)			\
        $(SOLVE_LIB_BLAS)		

LIB1  = 					\
	$(PETOOLS_ROOT)/pet_util/pet_util.a	\
        $(PETOOLS_ROOT)/matvec/matvec.a		\
	$(PETOOLS_ROOT)/pet_util/pet_util.a	\
        $(SOLVE_EXTRA_LIB)			\
        $(SOLVE_LIB_VEC)			\
        $(SOLVE_LIB_BLAS)		


all:	matvec diagi 

matvec:	$(OBJS)
	$(MK5_LINK) -o $(EXE_DIR)/matvec_test  matvec_test.o  $(LIB1)

diagi:	$(OBJS)
	$(MK5_LINK) -o $(EXE_DIR)/diagi_batch       diagi_batch.o      $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_color_demo  diagi_color_demo.o $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_demo        diagi_demo.o       $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_dec         diagi_dec.o        $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_key         diagi_key.o        $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_rst         diagi_rst.o        $(LIBS) 
	$(MK5_LINK) -o $(EXE_DIR)/diagi_user        diagi_user.o       $(LIBS)
	$(MK5_LINK) -o $(EXE_DIR)/md_demo           md_demo.o          $(LIBS)


clean:
	rm -f $(OBJS)                \
        $(EXE_DIR)/diagi_batch	     \
        $(EXE_DIR)/diagi_color_demo  \
        $(EXE_DIR)/diagi_demo        \
	$(EXE_DIR)/diagi_dec         \
        $(EXE_DIR)/diagi_key         \
	$(EXE_DIR)/diagi_rst         \
        $(EXE_DIR)/diagi_user        \
	$(EXE_DIR)/matvec_test       \
	$(EXE_DIR)/md_batch
