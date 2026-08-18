#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#ifdef _NEEDED
int fc_shmget_(str,val)
#else
int fc_shmget(str,val)
#endif
    char **str;
    int *val;
{
    int retval;
    retval = shmget(IPC_PRIVATE,*val,0600);
    return(retval);
}
