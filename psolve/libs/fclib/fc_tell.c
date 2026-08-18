#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>

#ifdef _NEEDED
long fc_tell_(fd)
#else
long fc_tell(fd)
#endif
    int *fd;
{
    unsigned long int iret;
    iret = tell ( *fd );
    if ( iret == -1 ) perror ("tell");
    return(iret);
}
