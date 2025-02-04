#ifdef _NEEDED
int fc_dup_(fd)
#else
int fc_dup(fd)
#endif
    int *fd;
{
    return(dup(*fd));
}    
