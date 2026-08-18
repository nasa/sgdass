#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

#ifdef _NEEDED
int fc_unlink_(fpath)
#else
int fc_unlink(fpath)
#endif
char **fpath;
{
     return(unlink(*fpath));
}
