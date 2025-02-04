# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking 24 procpar executable files    *
# *                                                                      *
# ************************************************************************
.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.opt_o	.a	.d
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

.c.o:
	$(MK5_C) -c -o $*.o $*.c  

.f.o:
	$(MK5_F95_NOOPT) -c -o $*.o $*.f  

.f.opt_o:
	$(MK5_F95_OPT) -c -o $*.opt_o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJM 	=				\
		axof_update.o		\
		bas_spline.o		\
		basplo.o		\
		basrep.o		\
		bsp_plot.o		\
		c04_to_erp.o		\
		check_bnames.o		\
		check_jnames.o		\
		dif_c04_usno.o		\
		dif_eop.o		\
		dif_eop_ext.o		\
		dif_erm.o		\
		dif_erp_c04.o		\
		dif_polu_c04.o		\
		eob_to_eops.o		\
		eob_to_erp.o		\
		eops_to_eob.o		\
		find_badeop.o		\
		find_badnut.o		\
		local_cat.o		\
		remove_badeop.o		\
		smooth_eop.o 		\
		sort_eob.o		\
		sou_list.o		\
		src_stat.o		\
		up_sitmod.o		\
		up_velmod.o		\
		update_apr_cat.o	\
		update_source_names.o 

OBJS 	=			\
		bas_stuff.o	\
		basspl_plo.o	\
		comp_spline.o 	\
		rd_gps_eop.o

GAE_OBJECTS =							  \
	        $(SOLVE_ROOT)/utils/get_apriori_eop/rd_iers_c04.o \
	        $(SOLVE_ROOT)/utils/get_apriori_eop/rd_finals.o   \
	        $(SOLVE_ROOT)/utils/get_apriori_eop/wri_erp.o

EOE_OBJECTS =							 \
	        $(SOLVE_ROOT)/utils/erm_to_eop/erm_to_eop.o	 \


GETPAR_OBJECTS =						\
	        $(SOLVE_ROOT)/utils/getpar/parse_erm.o		\
	        $(SOLVE_ROOT)/utils/getpar/read_eob.o		\
	        $(SOLVE_ROOT)/utils/getpar/read_eops.o		\
	        $(SOLVE_ROOT)/utils/getpar/read_sou.o		\
	        $(SOLVE_ROOT)/utils/getpar/write_eob.o		\
	        $(SOLVE_ROOT)/utils/getpar/write_eops.o

BATCH_OBJECTS =			\
	        $(SOLVE_ROOT)/progs/solve/batch/parse_spe.o 	\
		$(SOLVE_ROOT)/progs/solve/batch/parse_hpe.o

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_ROOT)/libs/curlib/curlib.a	\
	$(SOLVE_ROOT)/libs/newlib/newlib.a 	\
	$(SOLVE_ROOT)/libs/fclib/fclib.a 	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_VTD) 			\
	$(SOLVE_LIB_GVH) 			\
	$(SOLVE_FITSLIB_LIB) 			\
	$(SOLVE_CFITSIO_LIB) 			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

all:	bin

bin:	axof_update basplo bas_spline basrep bsp_plot check_bnames check_jnames \
        c04_to_erp  dif_c04_usno dif_eop dif_eop_ext dif_erm dif_erp_c04 \
	dif_polu_c04 eob_to_eops eob_to_erp eops_to_eob find_badeop find_badnut \
	local_cat remove_badeop smooth_eop sort_eob sou_list src_stat up_velmod \
        up_sitmod update_apr_cat update_source_names

basplo:		    $(OBJM) $(OBJS)   
		    $(MK5_LINK) -o $(EXE_DIR)/basplo basplo.o $(OBJS) $(LIBS)

axof_update:	    $(OBJM) $(OBJS)   
		    $(MK5_LINK) -o $(EXE_DIR)/axof_update axof_update.o $(OBJS) $(LIBS)

bas_spline:	    $(OBJM) $(OBJS) $(BATCH_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/bas_spline bas_spline.o $(OBJS) $(BATCH_OBJECTS) $(LIBS)

basrep:		    $(OBJM) $(OBJS)	    
		    $(MK5_LINK) -o $(EXE_DIR)/basrep basrep.o $(OBJS) $(LIBS)

bsp_plot:	    $(OBJM) $(OBJS)
		    $(MK5_LINK) -o $(EXE_DIR)/bsp_plot bsp_plot.o $(OBJS) $(LIBS)

c04_to_erp:	    $(OBJM) $(OBJS) $(GAE_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/c04_to_erp c04_to_erp.o $(OBJS) $(GAE_OBJECTS) $(LIBS)

check_bnames:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/check_bnames check_bnames.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

check_jnames:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/check_jnames check_jnames.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

dif_c04_usno:	    $(OBJM) $(OBJS) $(GAE_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_c04_usno dif_c04_usno.o $(OBJS) $(GAE_OBJECTS) $(LIBS)

dif_eop:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_eop dif_eop.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

dif_eop_ext:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_eop_ext dif_eop_ext.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

dif_erm:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(EOE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_erm dif_erm.o $(OBJS) $(GAE_OBJECTS) $(EOE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

dif_erp_c04:	    $(OBJM) $(OBJS) $(GAE_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_erp_c04 dif_erp_c04.o $(OBJS) $(GAE_OBJECTS) $(LIBS)

dif_polu_c04:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/dif_polu_c04 dif_polu_c04.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

eob_to_eops:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/eob_to_eops eob_to_eops.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

eob_to_erp:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/eob_to_erp eob_to_erp.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

eops_to_eob:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/eops_to_eob eops_to_eob.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

find_badeop:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/find_badeop find_badeop.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

find_badnut:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/find_badnut find_badnut.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

local_cat:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/local_cat local_cat.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

remove_badeop: 	    $(OBJM) $(OBJS)
		    $(MK5_LINK) -o $(EXE_DIR)/remove_badeop remove_badeop.o $(OBJS) $(LIBS)

smooth_eop:         $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/smooth_eop smooth_eop.o $(OBJS) $(LIBS)

sort_eob:	    $(OBJM) $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/sort_eob sort_eob.o $(OBJS) $(GAE_OBJECTS) $(GETPAR_OBJECTS) $(LIBS)

sou_list:	    $(OBJM) $(OBJS) 
		    $(MK5_LINK) -o $(EXE_DIR)/sou_list sou_list.o $(OBJS) $(LIBS)

src_stat:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/src_stat src_stat.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

up_sitmod:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/up_sitmod	up_sitmod.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

up_velmod:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/up_velmod	up_velmod.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

update_apr_cat:	    $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/update_apr_cat update_apr_cat.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)

update_source_names:   $(OBJM) $(OBJS) $(GETPAR_OBJECTS)
		    $(MK5_LINK) -o $(EXE_DIR)/update_source_names update_source_names.o $(OBJS) $(GETPAR_OBJECTS) $(LIBS)


clean:  
	rm -f $(OBJS) $(OBJM)			\
              $(EXE_DIR)/axof_update            \
              $(EXE_DIR)/bas_spline		\
              $(EXE_DIR)/basplo			\
              $(EXE_DIR)/basrep			\
              $(EXE_DIR)/bsp_plot		\
              $(EXE_DIR)/c04_to_erp		\
              $(EXE_DIR)/check_bnames		\
              $(EXE_DIR)/check_jnames		\
              $(EXE_DIR)/c04_to_erp		\
              $(EXE_DIR)/dif_c04_usno		\
              $(EXE_DIR)/dif_eop		\
              $(EXE_DIR)/dif_eop_ext		\
              $(EXE_DIR)/dif_erm		\
              $(EXE_DIR)/dif_erp_c04		\
              $(EXE_DIR)/dif_polu_c04		\
              $(EXE_DIR)/eob_to_eops		\
              $(EXE_DIR)/eob_to_erp		\
              $(EXE_DIR)/eops_to_eob		\
              $(EXE_DIR)/find_badeop		\
              $(EXE_DIR)/find_badnut		\
              $(EXE_DIR)/local_cat              \
              $(EXE_DIR)/remove_badeop		\
              $(EXE_DIR)/smooth_eop             \
              $(EXE_DIR)/sort_eob		\
              $(EXE_DIR)/sou_list		\
              $(EXE_DIR)/src_stat		\
              $(EXE_DIR)/up_sitmod 		\
              $(EXE_DIR)/up_velmod		\
              $(EXE_DIR)/update_apr_cat		\
              $(EXE_DIR)/update_source_names
