/***************************************************************************/
/*                                                                         */
/* AEE 910614                                                              */
/* fc_pause echos a pause; If job is running in the back ground it         */
/* terminates all the background processes and quits. If job is running    */
/* in the foreground it waits until <CR> is hit and continues.             */
/*                                                                         */
/* Modifications:                                                          */
/*                                                                         */
/* AEM&BA  930712  "fflush" call added to make sure error messages sent    */
/*                 before kill.                                            */
/*                                                                         */
/* pet     980623  Commented out call of system (kill_of_current_process)  */
/*                 since it causes system error under HP-10.20             */
/*                                                                         */
/* pet     2000.05.25  Removed call of getstr_bf from                      */
/*                     ../solve/curses/start_mn.c since it violates        */
/*                     libraries structure                                 */
/*                                                                         */
/* pet     2003.07.25  Fixed a bug: fflush was omitted before getchar      */
/*                                                                         */
/***************************************************************************/

#include <stdio.h>
#include <sys/utsname.h>
#include <signal.h>

#ifdef _NEEDED
fc_pause_(BGROUND,msg,len)
#else
fc_pause(BGROUND,msg,len)
#endif
int *BGROUND;
char *msg;
int len;
{
   char tmpstr[80];
   int getstr_bp();

/*   printf("\n in fc_pause, bground= %d\n",*BGROUND); i*/
   if (*BGROUND == 0) {
      printf("\nError caused the termination of background processes.\n");
      fflush(stdout);
      sprintf(tmpstr,"kill %d",getpid());
      /*      system(tmpstr); */
      exit(1);
   }
   else if (*BGROUND == 1) {
     printf("\nPAUSE. Hit <CR> to continue...");
     fflush(stdout);
     getchar();
   }
   else {
     printf("\nPAUSE. Hit <CR> to continue... ");
     tmpstr[1] = 7;
     printf("%s", &tmpstr[1]);
     fflush(stdout);
     sleep (1);
     printf("%s", &tmpstr[1]);
     fflush(stdout);
     getchar();
     printf("\n%s\r", &tmpstr[1]);
     fflush(stdout);
   }
   return(0);
}
