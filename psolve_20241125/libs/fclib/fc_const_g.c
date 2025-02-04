#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef _NEEDED
int fc_const_g_(str,val)
#else
int fc_const_g(str,val)
#endif
int *val;
char **str;
{
/* control flags - see fcntl(2) */
         if(!strcmp(*str,"F_DUPFD" ))  *val = F_DUPFD ;
    else if(!strcmp(*str,"F_GETFD" ))  *val = F_GETFD ;
    else if(!strcmp(*str,"F_SETFD" ))  *val = F_SETFD ;
    else if(!strcmp(*str,"F_GETFL" ))  *val = F_GETFL ;
    else if(!strcmp(*str,"F_SETFL" ))  *val = F_SETFL ;
    else if(!strcmp(*str,"F_GETLK" ))  *val = F_GETLK ;
    else if(!strcmp(*str,"F_SETLK" ))  *val = F_SETLK ;
    else if(!strcmp(*str,"F_SETLKW"))  *val = F_SETLKW;

/* open flags - see open(2) */
    else if(!strcmp(*str,"O_RDONLY"))  *val = O_RDONLY;
    else if(!strcmp(*str,"O_WRONLY"))  *val = O_WRONLY;
    else if(!strcmp(*str,"O_RDWRCR"))  *val = O_RDWR|O_CREAT;
    else if(!strcmp(*str,"O_RDWR"  ))  *val = O_RDWR  ;
    else if(!strcmp(*str,"O_NDELAY"))  *val = O_NDELAY;
    else if(!strcmp(*str,"O_CREAT" ))  *val = O_CREAT ;
    else if(!strcmp(*str,"O_TRUNC" ))  *val = O_TRUNC ;
    else if(!strcmp(*str,"O_EXCL"  ))  *val = O_EXCL  ;
    else if(!strcmp(*str,"O_SYNC"  ))  *val = O_SYNC  ;
/*    else if(!strcmp(*str,"O_SYNCIO"))  *val = O_SYNCIO; */

/* mode flags - see chmod(2) */
    else if(!strcmp(*str,"S_ISUID" ))  *val = S_ISUID ;
    else if(!strcmp(*str,"S_ISGID" ))  *val = S_ISGID ;
#ifdef HPUX
    else if(!strcmp(*str,"S_ENFMT" ))  *val = S_ENFMT ;
#else
    else if(!strcmp(*str,"S_ENFMT" ))  *val = -1 ;
#endif
    else if(!strcmp(*str,"S_ISVTX" ))  *val = S_ISVTX ;
    else if(!strcmp(*str,"S_IRUSR" ))  *val = S_IRUSR ;
    else if(!strcmp(*str,"S_IWUSR" ))  *val = S_IWUSR ;
    else if(!strcmp(*str,"S_IXUSR" ))  *val = S_IXUSR ;
    else if(!strcmp(*str,"S_IRGRP" ))  *val = S_IRGRP ;
    else if(!strcmp(*str,"S_IWGRP" ))  *val = S_IWGRP ;
    else if(!strcmp(*str,"S_IXGRP" ))  *val = S_IXGRP ;
    else if(!strcmp(*str,"S_IROTH" ))  *val = S_IROTH ;
    else if(!strcmp(*str,"S_IWOTH" ))  *val = S_IWOTH ;
    else if(!strcmp(*str,"S_IXOTH" ))  *val = S_IXOTH ;

/* errors - see errno(2) */

    else if(!strcmp(*str,"E2BIG"        ))  *val = E2BIG        ;
    else if(!strcmp(*str,"EACCES"       ))  *val = EACCES       ;
    else if(!strcmp(*str,"EADDRINUSE"   ))  *val = EADDRINUSE   ;
    else if(!strcmp(*str,"EADDRNOTAVAIL"))  *val = EADDRNOTAVAIL;
    else if(!strcmp(*str,"EAFNOSUPPORT" ))  *val = EAFNOSUPPORT ;
    else if(!strcmp(*str,"EAGAIN"       ))  *val = EAGAIN       ;
    else if(!strcmp(*str,"EALREADY"     ))  *val = EALREADY     ;
    else if(!strcmp(*str,"EBADF"        ))  *val = EBADF        ;
    else if(!strcmp(*str,"EBUSY"        ))  *val = EBUSY        ;
    else if(!strcmp(*str,"ECHILD"       ))  *val = ECHILD       ;
    else if(!strcmp(*str,"ECONNABORTED" ))  *val = ECONNABORTED ;
    else if(!strcmp(*str,"ECONNREFUSED" ))  *val = ECONNREFUSED ;
    else if(!strcmp(*str,"ECONNRESET"   ))  *val = ECONNRESET   ;
    else if(!strcmp(*str,"EDEADLK"      ))  *val = EDEADLK      ;
    else if(!strcmp(*str,"EDESTADDRREQ" ))  *val = EDESTADDRREQ ;
    else if(!strcmp(*str,"EDOM"         ))  *val = EDOM         ;
    else if(!strcmp(*str,"EEXIST"       ))  *val = EEXIST       ;
    else if(!strcmp(*str,"EFAULT"       ))  *val = EFAULT       ;
    else if(!strcmp(*str,"EFBIG"        ))  *val = EFBIG        ;
    else if(!strcmp(*str,"EHOSTDOWN"    ))  *val = EHOSTDOWN    ;

    else return -2;

    return 0;
}
