#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <string.h>

#ifdef _NEEDED
long int fc_fstat_(fname,flen)
#else
long int fc_fstat(fname,flen)
#endif

 char **fname;
 int *flen;
{
      struct stat status;
      int err,i;
      long int num_bytes; 
      extern int errno;

      err = stat(*fname ,&status);
      if (err==-1) {
        printf("errno= %d \n",errno);
        perror( " " );
        return (-1);
      }
      num_bytes = status.st_size;
      return (num_bytes);
}
