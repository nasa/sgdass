#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef DARWIN
#include <sys/param.h>
#include <sys/mount.h>
#else
#include <sys/vfs.h>
#endif
#include <errno.h>
#include <string.h>

#ifdef _NEEDED
long int fc_statfs_(fname,flen)
#else
long int fc_statfs(fname,flen)
#endif

 char **fname;
 int *flen;
{
      struct statfs status;
      int err,i;
      long int num_blocks; 
      extern int errno;

      err = statfs(*fname ,&status);
      if (err==-1) {
	printf("errno= %d \n",errno);
        perror( " " );
        return (-1);
      }
      num_blocks = status.f_bavail;
      return (num_blocks);
}
