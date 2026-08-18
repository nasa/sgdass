/*
	This file contains the routines used for locking: rd_lock, wrt_lock
	un_lock which lock for reading, lock for writing, and unlock a named
	file, respectively.  These routines leave the file pointer in an
	unknown state.  Note: the timeout only applies (currently) to the 
	waiting for the "write" lock.
	Error returns:
	    -1  -  some serious error from fcntl
	    -2  -  timeout
*/

#include <fcntl.h>
#include <unistd.h>
#include <sys/errno.h>

#define SLEEP_TIME 1
#define MAX_SLEEP 10000

union {
    int val;
    struct flock *lockdes;
} f_lock;
int xx_lock_first=1;
struct flock lock_target;

#ifdef _NEEDED
int un_lock_(fd)
#else
int un_lock(fd)
#endif
    int *fd;		/* file descriptor provided by rd_lock, or wrt_lock */
{
    void perror();
    int iret;
    if(xx_lock_first) {
	xx_lock_first = 0;
	f_lock.lockdes = &lock_target;
	f_lock.lockdes->l_start = 0;
	f_lock.lockdes->l_len = 0;
    }
    f_lock.lockdes->l_type = F_UNLCK;
    if((iret=fcntl(*fd,F_SETLK,f_lock)==-1))
	perror("unl_fcntl");
    return(iret);
}

#ifdef _NEEDED
int rd_lock_(fd,timeout)
#else
int rd_lock(fd,timeout)
#endif
    int *fd, *timeout;	/* file descriptor and timeout value in seconds */
{
    void perror();
    if(xx_lock_first) {
	xx_lock_first = 0;
	f_lock.lockdes = &lock_target;
	f_lock.lockdes->l_start = 0;
	f_lock.lockdes->l_len = 0;
    }
    f_lock.lockdes->l_type = F_RDLCK;
#ifdef _NEEDED
    return(xx_lock_(fd,*timeout));
#else
    return(xx_lock(fd,*timeout));
#endif
}
	
#ifdef _NEEDED
int wrt_lock_(fd,timeout)
#else
int wrt_lock(fd,timeout)
#endif
    int *fd, *timeout;	/* file descriptor and timeout value in seconds */
{
    void perror();
    int iret;
    if(xx_lock_first) {
	xx_lock_first = 0;
	f_lock.lockdes = &lock_target;
	f_lock.lockdes->l_start = 0;
	f_lock.lockdes->l_len = 0;
    }
    f_lock.lockdes->l_type = F_WRLCK;
#ifdef _NEEDED
    iret=xx_lock_(fd,*timeout);
#else
    iret=xx_lock(fd,*timeout);
#endif
    return(iret);
}

#ifdef _NEEDED
int xx_lock_(fd,timeout)
#else
int xx_lock(fd,timeout)
#endif
    int *fd, timeout;
{
    extern int errno;
    int iret, to;
   /* long lseek();  Removed by L. Petrov on 2003.11.18 */
   /* unsigned long sleep();*/
    unsigned int sleep();
    void perror();

    if(timeout < 0) to = MAX_SLEEP;
    else if(timeout==0) to = 1;   /* to contains the # of times to sleep */
    else to = (timeout/SLEEP_TIME)+1; /* need to try at least once */

    while(1) {
	if((iret=fcntl(*fd,F_SETLK,f_lock))==-1) {
	    if(errno==EACCES || errno==EAGAIN) {
		if((--to) <= 0) return(-2);  /* timed out */
		else sleep((unsigned) SLEEP_TIME);
	    }
	    else {
		perror("fcntl");
		return(-1);
	    }
	}
	else return(0);		/* locked it. Go home. */
    }
}
