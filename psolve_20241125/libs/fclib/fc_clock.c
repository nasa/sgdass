#ifdef _NEEDED
long fc_clock_()
#else
long fc_clock()
#endif
{
    long clock();
    return(clock());
}
