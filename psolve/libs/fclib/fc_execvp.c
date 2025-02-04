#ifdef _NEEDED
int fc_execvp_(file,argv)
#else
int fc_execvp(file,argv)
#endif
char **file,**argv[ ];
{
    return(execvp(*file,*argv));
}
