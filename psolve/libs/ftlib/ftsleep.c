#ifdef _NEEDED
void ftsleep_(psecs) /* Allows Fortran to access sleep(2) */
#else
void ftsleep(psecs) /* Allows Fortran to access sleep(2) */
#endif
    short *psecs;
{
    unsigned long vsecs, sleep();
    vsecs = *psecs;
    sleep(vsecs);
    return;
}   
