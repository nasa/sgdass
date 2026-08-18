/*
	ftdup provides a Fortran interface to dup(2).
*/
#ifdef _NEEDED
int ftdup_(fd)
#else
int ftdup(fd)
#endif
    int *fd;
{
    int dup();
    return(dup(*fd));
}    
