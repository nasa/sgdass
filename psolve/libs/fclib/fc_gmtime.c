#include <time.h>

#ifdef _NEEDED
struct tm *fc_gmtime_(clock)
#else
struct tm *fc_gmtime(clock)
#endif
    int **clock;
{
    struct tm *gmtime();
/*     printf ( "fc_gmtime: %d  %ld \n", **clock, gmtime( clock) ); %%%%%%%%%%%% */
    return ( gmtime( clock) );
}
