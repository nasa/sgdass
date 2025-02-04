#include <fcntl.h>

#ifdef _NEEDED
int fc_fcntl_(fildes,cmd,arg)
#else
int fc_fcntl(fildes,cmd,arg)
#endif
int *fildes,*cmd;
union {
      int val;
      struct flock *lockdes;
      } *arg;
{
     return(fcntl(*fildes,*cmd,*arg));
}
