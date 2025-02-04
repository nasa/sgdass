#include <sys/types.h>
#ifdef DARWIN
#include <sys/param.h>
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#endif
#include <errno.h>

#ifdef _NEEDED
int fc_freesp_(fildes)
#else
int fc_freesp(fildes)
#endif

      int *fildes;
{
      struct statfs buf;
      int err;
      long long blocks_free;
      extern int errno;

      err = fstatfs(*fildes,&buf);
      if ( err==-1) { printf("errno= %d \n",errno); }
      blocks_free = (long long)buf.f_bavail * (long long)(buf.f_bsize/1024);
      if ( blocks_free > 2147483647 ) {blocks_free = 2147483647;}
      return (blocks_free);
}
