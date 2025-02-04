/*
	ftunlink(fpath)

	where:
	    iret   - the return value for ftunlink:
		iret > 0 is the number of bytes read
		iret = 0 indicates an end of file was encountered
		iret < 0 indicates an error
	    fpath     - open file descriptor for reading

	Note:  As with most of the ftlib routines, it will run correctly only
	on machines with 32 bit int's.
	Written by EJH; modified by LEF
*/
#ifdef _NEEDED
int ftunlink_(fpath)
#else
int ftunlink(fpath)
#endif
    char *fpath;
{
    int unlink();
    return(unlink(*fpath));
}
