extern long set_stacksize ( long stacksize_in_bytes ) ;
#ifdef LINUX
#include <sys/resource.h>
/*
# ************************************************************************
# *                                                                      *
# *   This routine sets stacksize in bytes. When the argument is -1,     *
# *   SET_STACKSIZE returns the current stack size in bytes.             *
# *                                                                      *
# *   Usage:                                                             *
# *                                                                      *
# *      IS = SET_STACKSIZE ( %VAL( STACK_SIZE ) )                       *
# *                                                                      *
# * _________________________ Input parameters: ________________________ *
# *                                                                      *
# *                                                                      *
# *   STACK_SIZE ( INTEGER*8 ) -- Requested stacksize in bytes.          *
# *                                                                      *
# * _________________________ Output parameters: _______________________ *
# *                                                                      *
# *  <SET_STACKSIZE> ( INTEGER*8 ) -- New stack size in bytes.           *
# *                                                                      *
# * ### 22-APR-2019 sets stacksize  v1.0 (c)  L. Petrov 22-APR-2019 ###  *
# *                                                                      *
# ************************************************************************
*/
long set_stacksize ( long stacksize_in_bytes )
{
    struct rlimit rl;
    int result;

    result = getrlimit(RLIMIT_STACK, &rl);
    if (result == 0)
    {
        if (rl.rlim_cur < stacksize_in_bytes)
        {
            rl.rlim_cur = stacksize_in_bytes;
            result = setrlimit(RLIMIT_STACK, &rl);
            if (result != 0 ) { 
	        return result;
            }
            result = getrlimit(RLIMIT_STACK, &rl);
        }
    }
    return rl.rlim_cur;
}

#else
long set_stacksize ( long stacksize_in_bytes ){
  return 0;
}
#endif
