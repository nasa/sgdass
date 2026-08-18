#ifdef _NEEDED
int fc_creat_(fpath,mode)
#else
int fc_creat(fpath,mode)
#endif
    char **fpath;
    int *mode;
{
    return(creat(*fpath,*mode));
}
