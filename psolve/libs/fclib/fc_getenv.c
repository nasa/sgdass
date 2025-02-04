#include <stdlib.h>

#ifdef _NEEDED
int fc_getenv_(name,path)
#else
int fc_getenv(name,path)
#endif

 char **name;
 char **path;
{
 char *p1;
 int len;
      p1 = getenv(*name);
      if ( p1 == 0 ) return(0);
      len = strlen(p1);
      if(len > 0) strncpy(*path,p1,len);
      return (len);
}
