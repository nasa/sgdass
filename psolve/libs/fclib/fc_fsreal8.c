#define _LARGEFILE_SOURCE 
#define _FILE_OFFSET_BITS 64 
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
//
//  pet 2004.10.18 Added Large Files Support. Replaced everywhere float with 
//                 double. Changed name from fc_fsreal to fc_fsreal8
//
//  Call from Fortran
//
//  CHARACTER  FILE_NAME*128
//  REAL*8  RES
//  REAL*8  FC_FSREAL8
//  ...
//  RES = FC_FSREAL8 ( FILE_NAME )
//

#ifdef _NEEDED
double fc_fsreal8_ ( fname, len )
#else
double fc_fsreal8 ( fname, len )
#endif

char *fname;
int len;
{
      struct stat status;
      int err, i;

      if (stat64(fname, &status) == -1) {
         printf("FC_FSREAL8: Error: couldn't stat64 %s.\n", fname );
         return(-1.0);
      }
      
      return ( (double) status.st_size );
}
