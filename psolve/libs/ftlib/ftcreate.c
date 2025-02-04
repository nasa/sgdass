/*
	Ftcreate provides a means for Fortran programs to call the create(2)
	system call.

	fd = ftcreate(fpath,mode)

	where:
		fd     - return value; if >0, it is the file descriptor
			 if <0 an error has occurred.
		fpath  - UNIX file pathname
		mode   - file creation mode (see chmod)
*/

#include <stdio.h>

#ifdef _NEEDED
int ftcreate_(fpath,mode)
#else
int ftcreate(fpath,mode)
#endif
    char *fpath;
    int *mode;
{
    void perror();
    int fd, l_mode;
    if(*mode==0) l_mode=0664;
    else l_mode=*mode;
    fd=creat(fpath,l_mode);
    if(fd<0) perror("ftcreate");
    return(fd);
}
