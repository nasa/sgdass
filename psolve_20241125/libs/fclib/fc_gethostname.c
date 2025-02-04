#include <stdlib.h>

#ifdef _NEEDED
int fc_gethostname_(name,size)
#else
int fc_gethostname(name,size)
#endif

/* 970612 JFC (John Chandler): fix declaration of gethostname arguments
                               for Sun compatibility */
 

 char *name;
 unsigned int *size;
{return(gethostname(name,*size));}
