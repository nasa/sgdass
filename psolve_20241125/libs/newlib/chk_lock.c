/* WARNING:  DO NOT INCORPORATE THIS SUBROUTINE INTO XX_LOCK.C.  
             THE ROUTINES IN THAT FILE EXPECT TO SHARE A FILE DESCRIPTOR 
             AND WORK TOGETHER, RETAINING THE DESCRIPTOR INFORMATION BETWEEN
             CALLS.  THIS ROUTINE WILL OVERWRITE ITS FILE DESCRIPTOR 
             INFORMATION WHEN IT CHECKS THE LOCK STATUS, SO IT NEEDS TO BE 
             PASSED A SEPARATE FILE DESCRIPTOR.
	This file contains a routine used to check whether or not the input
        file pointer has been locked via an fcntl lock.  If so, the type of lock
        is also reported.
        (Warning: This routine leaves the file pointer in an unknown state.)

        Returned values:
          0 - input file descriptor has no fcntl lock
          1 - descriptor has read lock
          2 - descriptor has write lock
         -1 - error checking lock
*/

#include <fcntl.h>
#include <unistd.h>
#include <sys/errno.h>


#ifdef _NEEDED
int chk_lock_(fdd)
#else
int chk_lock(fdd)
#endif

    int *fdd;		/* input file descriptor to be checked */
{
    union {
        int value;
        struct flock *lock_des;
    } ff_lock;
    struct flock locking_target;
    void perror();
    int iret;
    ff_lock.lock_des = &locking_target;
    ff_lock.lock_des->l_start = 0;
    ff_lock.lock_des->l_len = 0;
/*  Check to see if a write lock can be placed on the input file descriptor.
    If so, there is no lock.  Otherwise, there is either a read or a write 
    lock.  The actual lock type can be read from the description of the 
    existing lock that will be placed in the ff_lock structure. */
    ff_lock.lock_des->l_type = F_WRLCK;
    if((iret=fcntl(*fdd,F_GETLK,ff_lock)<0)) {
	perror("chk_lock");
        return (-1);
    }
    if (ff_lock.lock_des->l_type == F_UNLCK) return (0);
    if (ff_lock.lock_des->l_type == F_RDLCK) return (1);
    if (ff_lock.lock_des->l_type == F_WRLCK) return (2);
}
