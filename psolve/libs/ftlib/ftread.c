/*
	FTREAD provides a way for a Fortran program to access the read(2)
	call.  It can be used to read pipes or files.  It will continue to
	attempt to read until a) the pipe is closed for writing, or b) an
	end-of-file is encountered.

	iret = ftread(fd,buffer,num)

	where:
	    iret   - the return value for ftread:
		iret > 0 is the number of bytes read
		iret = 0 indicates an end of file was encountered
		iret < 0 indicates an error
	    fd     - open file descriptor for reading
	    buffer - character buffer where the input is placed
	    num    - number of bytes read

	Note:  As with most of the ftlib routines, it will run correctly only
	on machines with 32 bit int's.
*/
#ifdef _NEEDED
int ftread_(fd,buffer,num)
#else
int ftread(fd,buffer,num)
#endif
    int *fd, *num;
    char *buffer;
{
    unsigned num_left; 
    char *pt_buf;
    int iret, iret2, read();
    num_left = *num;
    pt_buf = buffer;
    while(1) {
	iret = read(*fd,pt_buf,num_left);
	iret2 = *num-num_left;
	if(iret == 0) return(iret2);
	else if(iret < 0) return(iret);
	pt_buf += iret;
	num_left -= iret;
    }
}
