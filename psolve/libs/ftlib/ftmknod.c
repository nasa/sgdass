/* ftmknod - fortran interface to mknod(2)     */

#include <sys/types.h>

#ifdef _NEEDED
int ftmknod_(path, p_mode, p_dev)
#else
int ftmknod(path, p_mode, p_dev)
#endif
    char *path;
    int *p_mode;
    dev_t *p_dev;  /* This translates to type int */
{
    return(mknod(path, *p_mode, *p_dev));
}
