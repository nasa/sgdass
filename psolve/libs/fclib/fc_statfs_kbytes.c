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
long int fc_statfs_kbytes_ ( fname )
#else
long int fc_statfs_kbytes  ( fname )
#endif

 char *fname;
{
      struct statfs status;
      int err,i;
      long int num_blocks; 
      extern int errno;

      err = statfs ( fname, &status );
      if ( err==-1) {
           perror   ( "fc_statfs_kbytes (statfs)" );
	   printf ( "fc_statfs_kbytes ( %s ) \n", fname );
           return (-1);
      }
      num_blocks = status.f_bavail*(status.f_bsize/1024);
      return (num_blocks);
}
