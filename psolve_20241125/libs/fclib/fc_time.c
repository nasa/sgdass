#ifdef _NEEDED
int fc_time_(tloc)
#else
int fc_time(tloc)
#endif

    int **tloc;
{
    long time();

    return((int)time(*tloc));
}
