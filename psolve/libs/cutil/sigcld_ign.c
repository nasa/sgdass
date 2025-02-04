#include <signal.h>
/*************************************************************************
 *                                                                       *
 *   Routine  sigcld_ign  changes behavior of HP-UX on termination of    *
 *   a child process: system removes PID of the terminated process and   *
 *   does not create a zombie.                                           *
 *                                                                       *
 *   This routine causes ignoring SIGCLD signal.                         *
 *                                                                       *
 *   Cautiuon: as a by-effect U77 routine SYSTEM always returns          *
 *             completion code 255 even if the subprocess completed      *
 *             correctly. Use sigcld_dfl before calling SYSTEM in order  *
 *             to get rid of this by-effect.                             *
 *                                                                       *
 *  ###  07-SEP-99    sigcld_ign   v1.1 (c)  L. Petrov  24-APR-2020 ###  *
 *                                                                       *
 *************************************************************************
*/
void sigcld_ign ()
{
#ifdef DARWIN
  signal ( SIGCHLD, SIG_IGN );
#else 
  signal ( SIGCLD,  SIG_IGN );
#endif
}
