
#include <memory.h>

#ifdef _NEEDED
char *fc_memcpy_(s1,s2,n)
#else
char *fc_memcpy(s1,s2,n)
#endif

    char **s1,**s2;
    int *n;
{
    return(memcpy(*s1,*s2,*n));
}
