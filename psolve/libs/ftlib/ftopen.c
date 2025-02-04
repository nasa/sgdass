/*
	Ftopen provides a means for Fortran programs to call the write(2)
	system call.

	fdesc = ftopen(fpath,string,mode)

	where:
		fd     - return value; if >0, it is the file descriptor
			 if <0 an error has occurred.
		fpath  - UNIX file pathname
		type   - 0 for read; 1 for write; 2 for both
		mode   - file protection mode bits for file creation
			 (0 means default file creation mask)
	Written by EJH.  Modified by LEF
*/
#include <fcntl.h>
#ifdef _NEEDED
int ftopen_(fpath,type,mode)
#else
int ftopen(fpath,type,mode)
#endif
    char *fpath;	/* mode is mode bits for file creation */
    int *type, *mode; /* type=0,1,2 for read, write, and read+write resp. */
{
    void perror();
    int oflag, fd ; /* open(), removed by L. Petrov 2003.11.17 */
    if (*type==0) oflag = O_RDONLY;
    else if( *type==1) oflag = O_WRONLY;
    else oflag = O_RDWR;
    oflag |= O_CREAT;
    if (*mode==0) fd = open(fpath,oflag);
    else fd = open(fpath,oflag,*mode);
    if( fd < 0 ) perror("ftopen");
    return(fd);
}
