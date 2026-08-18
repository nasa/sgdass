# ************************************************************************
# *                                                                      *
# *   Makefile  for compiling and linking  dclient  executable           *
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

EXE_DIR = $(MK5_ROOT)/bin
SUPPORT = $(MK5_ROOT)/support

OBJS 	=               		\
	dbh_command_file.o 		\
	dbh_request.o 			\
	dclient.o 			\
	dclient_config.o 		\
	dclient_ping.o 			\
	dsnx_command_file.o 		\
	eop_command_file.o 		\
	execute_command.o 		\
	get_dbname_file.o 		

LIBS = 						\
	$(MK5_ROOT)/libs/cutil/cutil.a 		\
	$(MK5_ROOT)/libs/qcat4/qcat4.a		\
	$(MK5_ROOT)/libs/chain4/chain4.a	\
	$(MK5_ROOT)/libs/newlib/newlib.a 	\
	$(MK5_ROOT)/libs/ftlib/ftlib.a 		\
	$(MK5_ROOT)/libs/fclib/fclib.a		\
	$(MK5_ROOT)/libs/cutil/cutil.a 		\
	$(MK5_ROOT)/libs/pet_util/pet_util.a 	\
	$(SOLVE_LIB_VEC)                        \
	$(SOLVE_EXTRA_LIB) 			

all:	bin

bin:	$(OBJS) $(OBJS1) $(OBJS2)
	$(SUPPORT)/check_var;
	$(SUPPORT)/set_revision_date.csh;
	$(MK5_LINK) -o $(EXE_DIR)/dclient dclient.o $(OBJS) $(LIBS)
	cd $(@D); \
        $(SHELL) -f ./dclient_install 

dclient.o:	dclient_version.i
$(OBJS):	$(MK5_ROOT)/include/dclient.i

clean:  
	rm -f $(OBJS) $(EXE_DIR)/dclient 
