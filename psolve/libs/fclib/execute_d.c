/* execute_d - execute command and wait for return with optional xdb

   input:
           com - (pointer to) command line, null terminated
          argc - (pointer to) number of elements in array argv
                 must be 2 greater that actual number of arguments
                 to command (1 extra for command and 1 extra for NULL)
          argv - (pointer to) array of int to hold pointers to arguments
          device - (pointer to) debug i/o file, = NULL if no debug
   output:
             com - command line destroyed
     status_chld - the status which the child process returned via exit() or
                   _exit()
                   
 
   return value:  0 (succesful)
                 -1 bad args
                 -2 fork (or vfork) failed
                 -3 wait failed
                 -4 xdb fork failed

   FORTRAN CALL (replace '?'s with appropriate values):

   INCLUDE '/mk3/src/fclib.i'
   INTEGER*4 IRET,ARGC,ARGV(ARGC),STATUS
   PARAMETER (ARGC=?)
   CHARACTER*? COM,NTERM,NDEV
   IRET=NULL_TERM(NTERM,'command line with args')
   IRET=NULL_TERM(NDEV,'debug i/o file')
   IRET=EXECUTE_D(PTR_CH(NTERM),ARGC,ARGV,STATUS,PTR_CH(NDEV))

   behavior:

   This routine carves up com to make acceptable arguments to 
   execvp. A (v)fork is executed, Then the command is execvp'd.
   The forked process is waited for. The termination status is
   returned. The termination status other childern that complete
   while waiting is lost. If NDEV is not a null pointer, then xdb
   will be exec'd with stdin, stdout, and stderr all open to device.

   The reason for the existence of this routine is provide a
   fortran callable routine that will use vfork() correctly.
   This requires that the data segment, particularly the stack,
   not be disturbed during the [vfork(),...execvp()] window.

*/          
#include <fcntl.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>
extern int errno;

#ifdef _NEEDED
int execute_d_(com,argc,argv,status,device)
#else
int execute_d(com,argc,argv,status,device)
#endif
char **com,*argv[],**device;
int *argc,*status;
{
    int  chpid, i, wpid, fdr, fdw, dbgid, status_exec;
    char *argd[5],chrpid[13];

    if(*argc < 1 ) return -1;

    if((argv[0]=strtok(*com," "))==NULL) return -1;

    for (i=1; i < *argc && ( (argv[i]=strtok(NULL," ")) != NULL ); i++)
        ;

    switch(chpid=vfork()){
      case -1:
        if(errno == ENOMEM) return -5;
        return -2;
      case 0:
        i=execvp(argv[0],argv);
        fprintf(stderr,"execvp failed on %s\n",argv[0]); 
	fflush(stderr);
        _exit(-1);
    }
    if(*device!=NULL) {
      switch(dbgid=fork()){ /* must use regular fork() for this */
        case -1:
          return -4;
        case 0:
          if((fdr=open(*device,O_RDONLY,0))==-1) _exit(-2);
          if((fdw=open(*device,O_WRONLY,0))==-1) _exit(-3);
          if(close(0))                                 _exit(-4);
          if(close(1))                                 _exit(-5);
          if(close(2))                                 _exit(-6);
          if(dup(fdr)!=0)                              _exit(-7);
          if(dup(fdw)!=1)                              _exit(-8);
          if(dup(fdw)!=2)                              _exit(-9);
          for (i=3;i<=19;i++) close(i);
          argd[0]="xdb";
          argd[1]="-P";
          sprintf(chrpid,"%d",chpid);
          argd[2]=chrpid;
          argd[3]=argv[0];
          argd[4]=NULL;
          i=execvp(argd[0],argd);
          fprintf(stderr,"xdb exec failed on %s\n",argv[0]);
          _exit(-1);
      }
    }
    while((wpid=wait(&status_exec))!=chpid && wpid != -1)
        ;

    /* printf ( " execute_d: status_exec: %d wpid %d \n", status_exec, wpid ); */

    if ( wpid == -1 ) {
         *status = errno ;
         return -3;
    }

    if (      WIFEXITED(status_exec)        ) *status = WEXITSTATUS(status_exec) ;
    else if ( WIFSIGNALED(status_exec) == 0 ) *status = WTERMSIG(status_exec) + 10000 ;
    else if ( WIFSTOPPED(status_exec)       ) *status = WSTOPSIG(status_exec) + 10000 ;
    else  *status = 0;

    /* printf ( " execute_d: status %d  \n", *status );  */

    return 0;
}
