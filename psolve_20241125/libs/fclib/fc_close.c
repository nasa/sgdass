#ifdef _NEEDED
int fc_close_(fd)
#else
int fc_close(fd)
#endif
int *fd;
{
    return(close(*fd));
}
