#ifdef _NEEDED
int fc_nice_(pchn)
#else
int fc_nice(pchn)
#endif

    int *pchn;
{
    int nice();

    return(nice(*pchn));
}
