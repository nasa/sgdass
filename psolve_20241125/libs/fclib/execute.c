/* execute - execute command and wait for return

   input:
           com - (pointer to) command line, null terminated
          argc - (pointer to) number of elements in array argv
                 must be 2 greater that actual number of arguments
                 to command (1 extra for command and 1 extra for NULL)
          argv - (pointer to) array of int to hold pointers to arguments
   output:
             com - command line destroyed
          status - returned (through pointer) termination status of program
 
   return value:  0 (successful)
                 -1 bad args
                 -2 fork (or vfork) failed
                 -3 wait failed

   FORTRAN CALL (replace '?'s with appropriate values):

   INCLUDE '/mk3/src/fclib.i'
   INTEGER*4 IRET,ARGC,ARGV(ARGC),STATUS
   PARAMETER (ARGC=?)
   CHARACTER*? COM,NTERM
   IRET=NULL_TERM(NTERM,'command line with args')
   IRET=EXECUTE(PTR_CH(NTERM),ARGC,ARGV,STATUS)

   behavior:

   This routine carves up com to make acceptable arguments to 
   execvp. A (v)fork is executed, Then the command is execvp'd.
   The forked process is waited for. The termination status is
   returned. The termination status of any other childern will
   be lost.

   The reason for the existence of this routine is provide a
   fortran callable routine that will use vfork() correctly.
   This requires that the data segment, particularly the stack,
   not be disturbed during the [vfork(),...execvp()] window.

*/          
#include <string.h>
#include <stdio.h>

#ifdef _NEEDED
int execute_(com,argc,argv,status)
#else
int execute(com,argc,argv,status)
#endif
char **com,*argv[];
int *argc,*status;
{
    int chpid,i,wpid;

    if(*argc < 1 ) return -1;

    if((argv[0]=strtok(*com," "))==NULL) return -1;

    for (i=1; i < *argc && ( (argv[i]=strtok(NULL," ")) != NULL ); i++)
        ;

    switch(chpid=vfork()){
      case -1:
        return -2;
      case 0:
        i=execvp(argv[0],argv);
        fprintf(3,"exec failed on %s\n",argv[0]);
        _exit(-1);
    }
    while((wpid=wait(status))!=chpid && wpid != -1)
      ;

    if(wpid == -1) return -3;

    return 0;
}

