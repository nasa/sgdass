/*
	Ftwrite provides a means for Fortran programs to call the write(2)
	system call.

	ierr = ftwrite(fd,buffer,num)

	where:
		ierr   - return value; if ierr>0, it is the number of bytes
			 written; if ierr<0 an error has occurred.
		fd     - UNIX file descriptor
		buffer - character buffer to be written
		num    - number of bytes to be written
*/
#ifdef _NEEDED
int ftwrite_(fd,buffer,num)
#else
int ftwrite(fd,buffer,num)
#endif
    int *fd, *num;
    char *buffer;
{
    void perror();
    unsigned unum;
    int nbytes;
    unum = *num;
    nbytes = write(*fd,buffer,unum);
    if(nbytes<0) perror("ftwrite");
    return(nbytes);
}
