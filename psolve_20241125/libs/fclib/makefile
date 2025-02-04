#######################################################################
#								      #
#	Make file for recompiling library  fclib.a     	      	      #
#						     		      #
#								      #
#  pet  2003.07.11   removed fc_tell.o from the makefile.             #
#								      #
#  ###  15-MAY-2003  makefile   v1.11 (c)  L. Petrov 11-JUL-2003 ###  #
#								      #
#######################################################################
.SUFFIXES:
.SUFFIXES:	.f	.o	.c	.a	.d
#
SHELL = /bin/csh -f -c 'umask 0002 ; eval "$2"'

LIB     = fclib.a
NUL     = /null/dev

.c.o:
	$(MK5_C) -c -I $(SOLVE_ROOT)/include -o $*.o $*.c  

.f.o:
	$(MK5_F95) -c -o $*.o $*.f  

.f.d:
	$(MK5_F95) -c -o $*.o $*.f  

OBJ=					\
		civerr.o 		\
		execute.o 		\
		execute_cls.o 		\
		execute_d.o 		\
		fc__exit.o 		\
		fc_catchsigs.o 		\
		fc_clock.o 		\
		fc_close.o 		\
		fc_const_g.o 		\
		fc_creat.o 		\
		fc_dup.o 		\
		fc_execvp.o 		\
		fc_exit.o 		\
		fc_fcntl.o 		\
		fc_flock_g.o 		\
		fc_flock_p.o 		\
		fc_freesp.o 		\
		fc_fsize.o 		\
		fc_fsreal8.o 		\
		fc_fstat.o 		\
		fc_getenv.o 		\
		fc_gethostname.o 	\
		fc_gmtime.o 		\
		fc_link.o 		\
		fc_lseek.o 		\
		fc_memcpy.o 		\
		fc_memcpy_from.o 	\
		fc_memcpy_to.o 		\
		fc_nice.o 		\
		fc_open.o 		\
		fc_pause.o 		\
		fc_perror.o 		\
		fc_pipe.o 		\
		fc_read.o 		\
		fc_shmget.o 		\
		fc_sleep.o 		\
		fc_stat64.o 		\
		fc_statfs.o 		\
		fc_statfs_kbytes.o 	\
		fc_system.o 		\
		fc_tell.o 		\
		fc_time.o 		\
		fc_tm_g.o 		\
		fc_umask.o 		\
		fc_unlink.o 		\
		fc_wait.o 		\
		fc_write.o 		\
		fciv_open.o 		\
		ferrx.o			\
		null_term.o		\
		ptr_ch.o		\
		ptr_nc.o 		\
		syserr.o 		\
		writff.o 

all:	lib 

lib:	$(OBJ) 
	ar  r $(LIB) $(OBJ) 
	@echo '--- Library $(LIB) updated ---'

clean:	
	rm -f $(OBJ) $(LIB) 
