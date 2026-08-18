#ifdef _NEEDED
int fc_pipe_(fd)
#else
int fc_pipe(fd)
#endif
    int *fd[2];
{
    return(pipe(*fd));
}
