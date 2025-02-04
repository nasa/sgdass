#include <fcntl.h>
#include <string.h>

#ifdef _NEEDED
int fc_flock_g_(arg,str,value)
#else
int fc_flock_g(arg,str,value)
#endif

    struct flock **arg;
    union {
           long longv;
           short shortv;
           int intv;
          } *value;
    char **str;
{

         if(!strcmp(*str,"l_type"  )) (*value).shortv= (*arg) -> l_type;
    else if(!strcmp(*str,"l_whence")) (*value).shortv= (*arg) -> l_whence;
    else if(!strcmp(*str,"l_start"))  (*value).longv=  (*arg) -> l_start;
    else if(!strcmp(*str,"l_len"))    (*value).longv=  (*arg) -> l_len;
    else if(!strcmp(*str,"l_pid"))    (*value).intv=   (*arg) -> l_pid;
    else return -1;

    return 0;
}
