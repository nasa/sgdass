#include <time.h>
#include <errno.h>

/* ************************************************************************ */
/* *                                                                      * */
/* *   Function nsleep causes the current process to be suspended         * */
/* *   from execution until either the time interval specified by the     * */
/* *   TIEM_TO_SLEEP argument has elapsed, or a signal is delivered to    * */
/* *   the calling process and its action is to invoke a signal-catching  * */
/* *   function or to terminate the process.  The suspension time may be  * */
/* *   longer than that requested because the argument value is rounded   * */
/* *   up to an integer multiple of the sleep resolution or because of    * */
/* *   the scheduling of other activity by the system.  But, except for   * */
/* *   the case of being interrupted by a signal, the suspension time     * */
/* *   will not be less than the time specified by TIME_TO_SLEEP,         * */
/* *   as measured by the system clock, CLOCK_REALTIME.                   * */
/* *   The use of the nanosleep() function has no effect on the action or * */
/* *   blockage of any signal.                                            * */
/* *                                                                      * */
/* *                                                                      * */
/* * ________________________ Input paramters: __________________________ * */
/* *                                                                      * */
/* *    TIME_TO_SLEEP ( REAL*8     ) -- Time to sleep in seconds.         * */
/* *                                                                      * */
/* * ________________________ Output parameters. ________________________ * */
/* *                                                                      * */
/* *    TIME_REMINDED ( REAL*8     ) -- Time reminded to sleep. It is not * */
/* *                                    zero onlly if the process was     * */
/* *                                    invoked by a signal.              * */
/* *                                                                      * */
/* *  ### 14-AUG-2003      NSLEEP   v1.0 (c)  L. Petrov  14-AUG-2003 ###  * */
/* *                                                                      * */
/* ************************************************************************ */

int nsleep ( double *time_to_sleep, double *time_remainded )
{
   struct timespec interval, remainder;
   int cond ;

   interval.tv_sec = *time_to_sleep;
   interval.tv_nsec = (*time_to_sleep-interval.tv_sec)*1000000000.0 ;

   cond = nanosleep(&interval, &remainder) ;

   *time_remainded = remainder.tv_sec + remainder.tv_nsec*1000000000.0 ;

   return ( cond ) ;
}
