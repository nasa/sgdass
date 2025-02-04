#include <signal.h>

#ifdef _NEEDED
dflsig18_()
#else
dflsig18()
#endif
{
#ifdef DARWIN
  signal ( SIGCHLD, SIG_IGN );
#else 
  signal ( SIGCLD, SIG_IGN );
#endif
}
