
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#ifdef _NEEDED
int fc_memcpy_to_(shmid,buf,nbytes)
#else
int fc_memcpy_to(shmid,buf,nbytes)
#endif
    int *shmid, *nbytes;
    char **buf;
{
    char *addr;
    addr = shmat(*shmid,0,0);
    memcpy(addr,*buf,*nbytes);
    (void)shmdt(addr);
}
