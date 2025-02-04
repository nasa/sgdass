#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef _NEEDED
void civerr_(msg)
#else
void civerr(msg)
#endif
/*  variant of syserr which does not abend with cryptic error message
    of the form error (2; no such file), but proceed
    created kdb 11/2/95       */
char *msg;
{
    fprintf ( stderr, "ERROR: %s (%d", msg, errno );
    fprintf ( stderr,"; %s)\n", strerror(errno) ); 
}
