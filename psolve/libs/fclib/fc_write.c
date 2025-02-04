#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef _NEEDED
int fc_write_(fd,buf,nbyte)
#else
int fc_write(fd,buf,nbyte)
#endif
    int *fd;
    unsigned int *nbyte;
    char **buf;
{
    ssize_t iret;
    iret = write ( *fd, *buf, (unsigned int) *nbyte );
    if ( iret == -1 ) perror ("fc_write: write");
    return(iret);
    
}
