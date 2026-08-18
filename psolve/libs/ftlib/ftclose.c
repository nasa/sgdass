/*
	ftclose provides a way for Fortran to call close(2).
*/
#ifdef _NEEDED
int ftclose_(fd)
#else
int ftclose(fd)
#endif
    int *fd;
{
    return(close(*fd));
}
