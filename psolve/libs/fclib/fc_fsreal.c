#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#ifdef _NEEDED
float fc_fsreal_(fname,len)
#else
float fc_fsreal(fname,len)
#endif

 char *fname;
 int len;
{
      struct stat status;
      int err,i;
      long num_bytes; 

      if (stat(fname, &status) == -1) {
         printf("\nErr: couldn't stat %s.\n",fname);
         return(-1.0);
      }
      num_bytes = status.st_size;
      return ((float)num_bytes);
}
