#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef _NEEDED
void syserr_(msg)
#else
void syserr(msg)
#endif
char *msg;
{
  
    fprintf ( stderr, "ERROR: %s (%d", msg, errno );
    fprintf ( stderr,"; %s)\n", strerror(errno) ); 
    exit(1);
}
