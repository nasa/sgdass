#include <signal.h>

#ifdef _NEEDED
ignsig18_()
#else
ignsig18()
#endif
{
#ifdef DARWIN
  signal ( SIGCHLD, SIG_IGN );
#else 
  signal ( SIGCLD, SIG_IGN );
#endif
}
