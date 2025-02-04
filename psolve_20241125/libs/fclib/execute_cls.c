/* execute_cls  - execute command and wait for return, closing file ids
                  iclose, ..., iclose + (inum_close - 1)

   input:
           com - (pointer to) command line, null terminated
          argc - (pointer to) number of elements in array argv
                 must be 2 greater that actual number of arguments
                 to command (1 extra for command and 1 extra for NULL)
          argv - (pointer to) array of int to hold pointers to arguments
          iclose (pointer to) first file id to close
          inum_close (pointer to) number of file ids to close
                  (e.g., choose iclose = 0, inum_close = 3 to close 0-2
                     (standard input/output/error))
   output:
             com - command line destroyed
          status - returned (through pointer) termination status of program
 
   return value:  0 (successful)
                 -1 bad args
                 -2 fork (or vfork) failed
                 -3 wait failed

   FORTRAN CALL (replace '?'s with appropriate values):


   INCLUDE  'fclib.i'
   INTEGER*4 IRET,ARGC,ARGV(ARGC),STATUS,ICLOSE,INUM_CLOSE
   PARAMETER (ARGC=?)
   CHARACTER*? COM,NTERM
   IRET=NULL_TERM(NTERM,'command line with args')
   IRET=EXECUTE(PTR_CH(NTERM),ARGC,ARGV,ICLOSE,INUM_CLOSE,STATUS)

   behavior:

   This routine carves up com to make acceptable arguments to 
   execl. A (v)fork is executed, the requested file ids are closed,
   Then the command is execvp'd.
   The forked process is waited for. The termination status is
   returned. The termination status of any other childern will
   be lost.

   The reason for the existence of this routine is provide a
   fortran callable routine that will use vfork() correctly.
   This requires that the data segment, particularly the stack,
   not be disturbed during the [vfork(),...execvp()] window.

*/          
#include <fcntl.h>
#include <string.h>
#include <stdio.h>

#ifdef _NEEDED
int execute_cls_(com,argc,argv,iclose,inum_close,status)
#else
int execute_cls(com,argc,argv,iclose,inum_close,status)
#endif
char **com,*argv[];
int *argc,*status;
int *iclose,*inum_close;
{
    int chpid,i,wpid,iloop;

    if(*argc < 1 ) return -1;

    if((argv[0]=strtok(*com," "))==NULL) return -1;

    for (i=1; i < *argc && ( (argv[i]=strtok(NULL," ")) != NULL ); i++)
        ;

    switch(chpid=vfork()){
      case -1:
        return -2;
      case 0:
/*      fcntl will close the given file ids when execvp is called.  */
        for (iloop=*iclose+*inum_close-1; iloop >= *iclose; iloop--) {
           fcntl(iloop,F_SETFD,1);
        }
        i=execvp(argv[0],argv);
/*      fprintf(3,"exec failed on %s\n",argv[0]); */
        _exit(-1);
    }
    while((wpid=wait(status))!=chpid && wpid != -1)
      ;

    if(wpid == -1) return -3;

    return 0;
}

