
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#ifdef _NEEDED
int fc_memcpy_from_(buf,shmid,nbytes)
#else
int fc_memcpy_from(buf,shmid,nbytes)
#endif
    int *shmid, *nbytes;
    char **buf;
{
    char *addr;
    addr = shmat(*shmid,0,0);
    memcpy(*buf,addr,*nbytes);
    (void)shmdt(addr);
    (void)shmctl(*shmid,IPC_RMID,0);
}
