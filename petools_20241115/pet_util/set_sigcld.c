#include <signal.h>
/*************************************************************************
 *                                                                       *
 *   Routine  set_sigcld  changes behavior of HP-UX on termination of    *
 *   a child process: system removes PID of the terminated process and   *
 *   does not create a zombie.                                           *
 *                                                                       *
 *  ###  07-SEP-1999   set_sigcld   v1.1 (c) L. Petrov  01-APR-2014 ###  *
 *                                                                       *
 *************************************************************************
*/
void set_sigcld ()
{
#ifdef DARWIN
  signal ( SIGCHLD, SIG_IGN );
#else 
  signal ( SIGCLD, SIG_IGN );
#endif
}
