#ifdef _NEEDED
int fc_umask_(fd)
#else
int fc_umask(fd)
#endif
int *fd;
{
    return(umask(*fd));
}
