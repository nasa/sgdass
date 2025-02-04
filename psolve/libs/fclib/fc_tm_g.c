#include <time.h>
#include <string.h>

#ifdef _NEEDED
int fc_tm_g_(tm_ptr,str,ret_ptr)
#else
int fc_tm_g(tm_ptr,str,ret_ptr)
#endif

    struct tm **tm_ptr;
    char **str;
    int *ret_ptr;
{

    printf ( "YEAR: %d \n", (*tm_ptr) -> tm_year ) ; /* %%%%%%%%%%%%% */
         if(!strcmp(*str,"sec"))   *ret_ptr = (*tm_ptr) -> tm_sec;
    else if(!strcmp(*str,"min"))   *ret_ptr = (*tm_ptr) -> tm_min;
    else if(!strcmp(*str,"hour"))  *ret_ptr = (*tm_ptr) -> tm_hour;
    else if(!strcmp(*str,"mday"))  *ret_ptr = (*tm_ptr) -> tm_mday;
    else if(!strcmp(*str,"mon"))   *ret_ptr = (*tm_ptr) -> tm_mon;
    else if(!strcmp(*str,"year"))  *ret_ptr = (*tm_ptr) -> tm_year;
    else if(!strcmp(*str,"wday"))  *ret_ptr = (*tm_ptr) -> tm_wday;
    else if(!strcmp(*str,"yday"))  *ret_ptr = (*tm_ptr) -> tm_yday;
    else if(!strcmp(*str,"isdst")) *ret_ptr = (*tm_ptr) -> tm_isdst;
    else return(-1);

    return(0);
}
