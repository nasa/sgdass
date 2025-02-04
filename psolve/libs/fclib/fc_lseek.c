#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

#ifdef _NEEDED
int fc_lseek_(fd,offset,whence)
#else
int fc_lseek(fd,offset,whence)
#endif
    int *fd;
    unsigned int *offset;
    int *whence;
{
    off_t offset_lseek, iret;
    offset_lseek = *offset;
    iret = lseek( *fd, offset_lseek, *whence ); 
    if ( iret == -1 ) perror ("lseek");
    return(iret);
}
