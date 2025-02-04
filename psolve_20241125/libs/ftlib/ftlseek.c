/*
	Ftlseek provides a means for Fortran programs to call the write(2)
	system call.

	ierr = ftlseek(fd,num)

	where:
		ierr   - return value; if ierr>0, it is the number of bytes
			 written; if ierr<0 an error has occurred.
		fd     - UNIX file descriptor
		buffer - character buffer to be written
		num    - number of bytes to be written
	written by EJH
*/
#include <unistd.h>

#ifdef _NEEDED
ftlseek_(fd,offset,whence)
#else
ftlseek(fd,offset,whence)
#endif
    int *fd, *whence;
    long *offset;
{
    return(lseek(*fd,*offset,*whence));
}
