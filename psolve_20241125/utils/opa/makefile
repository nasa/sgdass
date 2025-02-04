# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  opa  executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=               		\
		opa_head.o 		

OBJS 	=               		\
		check_ivsdir.o 		\
		check_solve_initials.o 	\
		create_opc.o 		\
		line_stsact.o 		\
		lisbas_from_spool.o 	\
		opa_action.o 		\
		opa_baw.o 		\
		opa_config.o 		\
		opa_eopi.o 		\
		opa_eopk.o 		\
		opa_eopm.o 		\
		opa_eops.o 		\
		opa_gal.o 		\
		opa_vdb.o 		\
		opa_menu.o 		\
		opa_snranal.o 		\
		opa_standalone.o 	\
		opa_stsinq.o 		\
		opa_stw.o 		\
		opa_submit.o 		\
		opa_sup.o 		\
		read_opc.o 		\
		spool_to_eopb.o 	\
		write_opc.o 

OBJP    = 							\
		$(SOLVE_ROOT)/utils/getpar/getpar_parse.o	\
		$(SOLVE_ROOT)/utils/getpar/getpar_to_eob.o	\
		$(SOLVE_ROOT)/utils/getpar/read_eob.o		\
		$(SOLVE_ROOT)/utils/getpar/write_eops.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_LIB_GVH) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) $(OBJP)
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/opa       $(OBJM) $(OBJS) $(OBJP) $(LIBS)

opa.o:	opa_version.i
(OBJS): $(SOLVE_ROOT)/inclide/opa.i
(OBJM): $(SOLVE_ROOT)/inclide/opa.i
(OBJP): $(SOLVE_ROOT)/inclide/opa.i

clean:  
	rm -f $(OBJM) $(OBJS) $(OBJP) $(EXE_DIR)/opa
