# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking setfl executable               *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c.	.o	.opt_o	.a	.d
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

OBJS 	=              			\
		autoa.o 		\
		autoc.o 		\
		change_datatype.o 	\
		change_supmet.o 	\
		check_4_eop_options.o 	\
		clatm.o 		\
		clclk.o 		\
		conpg.o 		\
		datm.o 			\
		dclk.o 			\
		eoyfl.o 		\
		get_epoch.o 		\
		get_gclock_deg.o 	\
		intorcon.o 		\
		j2000.o 		\
		more_conpg.o 		\
		nutat.o 		\
		ofst_sta.o 		\
		pick_refclo.o 		\
		posn.o 			\
		reset_b_at.o 		\
		reset_b_cl.o 		\
		rmflg.o 		\
		rotfl.o 		\
		rotin.o 		\
		set_acm.o 		\
		set_cursor.o 		\
		set_el_cut.o 		\
		set_sngchk.o 		\
		setcl_sta.o 		\
		setep_sta.o 		\
		sldb.o 			\
		srflg.o 		\
		stflg.o 		\
		toggle_eop_poly_flags.o \
		tstcl_sta.o 		\
		write_eop_menu.o 	\
		write_screen.o 

OBJM 	=              			\
		setfl.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_ROOT)/libs/b3d/b3d.a 		\
	$(SOLVE_LIB_CURSES)			\
        $(SOLVE_LIB_VTD)    			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	$(OBJM) $(OBJS) 
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/SETFL $(OBJM) $(OBJS) $(LIBS)

setfl.o:	setfl_version.i

clean:  
	rm -f $(OBJM) $(OBJS) $(EXE_DIR)/SETFL
