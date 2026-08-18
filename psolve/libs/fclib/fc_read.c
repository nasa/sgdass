#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef _NEEDED
int fc_read_(fd,buffer,num)
#else
int fc_read(fd,buffer,num)
#endif
    int *fd;
    unsigned long *num;
    char **buffer;
{
    ssize_t iret;
    iret = read ( *fd, *buffer, (unsigned int) *num );
    if ( iret == -1 ) perror ("read");
    return(iret);
}
