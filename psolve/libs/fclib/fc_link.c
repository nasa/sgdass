#include <stdio.h>

#ifdef _NEEDED
int fc_link_(argv)
#else
int fc_link(argv)
#endif
char **argv;
{
  char *path1,*path2;

     path1 = strtok(*argv," ");
     path2 = strtok(NULL," ");
     return(link(path1,path2));
}
