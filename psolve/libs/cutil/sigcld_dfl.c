#include <signal.h>
/*************************************************************************
 *                                                                       *
 *   Routine  sigcld_dfl  sets default behaviour of unix for handling   *
 *   signals for waiting for child process.                              *
 *                                                                       *
 *  ###  05-JAN-2001   sigcld_dfl   v1.1 (c) L. Petrov  24-APR-2020 ###  *
 *                                                                       *
 *************************************************************************
*/
void sigcld_dfl ()
{
#ifdef DARWIN
  signal ( SIGCHLD, SIG_DFL );
#else 
  signal ( SIGCLD, SIG_DFL );
#endif
}
