.SUFFIXES:
.SUFFIXES:	.f	.c	.o	.e

SHELL = /bin/csh -f -c 'umask 0002; eval "$2"'

.c.o:	
	cc                  -c -o $*.o $<
.f.o: 	
	$(MK5_F95) -DSOLVE -c -o $*.o $*.f

EXE_DIR = $(SOLVE_ROOT)/bin
SUPPORT = $(SOLVE_ROOT)/support

OBJS =                  	\
	cex_config.o		\
	cex_build_command.o 	\
	cex_check.o 		\
	cex_mk4dir_check.o 	\
	cex_submit.o 		\
	get_master.o 

OBJM =                  	\
        corel_export.o 		\

LIBS =						\
	$(SOLVE_ROOT)/libs/cutil/cutil.a	\
	$(SOLVE_LIB_CURSES)			\
	$(SOLVE_LIB_PETOOLS) 			\
        $(SOLVE_LIB_BLAS)			\
        $(SOLVE_EXTRA_LIB)

bin:	corel_export

corel_export: $(OBJM) $(OBJS) 
	      $(MK5_LINK) -o $(EXE_DIR)/corel_export  $(OBJM) $(OBJS) $(LIBS)

$(OBJS):  $(SOLVE_ROOT)/include/corel_export.i $(SOLVE_ROOT)/include/corel_export_help.i
$(OBJ1):  $(SOLVE_ROOT)/include/corel_export.i 

clean:
	rm -f $(OBJS) $(OBJM) $(EXE_DIR)/corel_export
