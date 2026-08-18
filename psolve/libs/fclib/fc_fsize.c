#include <sys/types.h>
#include <sys/stat.h>

#ifdef _NEEDED
int fc_fsize_(fname)
#else
int fc_fsize(fname)
#endif
char *fname;
{
   struct stat status;
   if(stat (fname, &status) == -1){
    printf(" err= -1\n");
   }
    printf("%d bytes is the file size\n",status.st_size);
   return (status.st_size);
}
