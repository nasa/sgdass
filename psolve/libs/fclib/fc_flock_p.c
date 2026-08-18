#include <fcntl.h>
#include <string.h>

#ifdef _NEEDED
int fc_flock_p_(arg,str,value)
#else
int fc_flock_p(arg,str,value)
#endif

    struct flock **arg;
    union {
           long longv;
           short shortv;
           int intv;
          } *value;
    char **str;
{
    static struct flock lockdes;

         if(!strcmp(*str,"l_type"  )) lockdes.l_type   = (*value).shortv;
    else if(!strcmp(*str,"l_whence")) lockdes.l_whence = (*value).shortv;
    else if(!strcmp(*str,"l_start"))  lockdes.l_start  = (*value).longv;
    else if(!strcmp(*str,"l_len"))    lockdes.l_len    = (*value).longv;
    else if(!strcmp(*str,"l_pid"))    lockdes.l_pid    = (*value).intv;
    else return -1;

    *arg = &lockdes;
    return 0;
}
