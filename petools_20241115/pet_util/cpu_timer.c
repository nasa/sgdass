#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

double cpu_timer ( char str[], long len_str )
/*
# ************************************************************************
# *                                                                      *
# *  Routine cpu_timer  returns used CPU time with nanosecond            *
# *  resolution.                                                         *
# *                                                                      *
# *  Usage from Fortran:                                                 *
# *  1) CALL CPU_TIMER ( %VAL(0) ) -- resets to zero used CPU time.      *
# *                                   should be called at the beginning  *
# *                                   of the block, which CPU used time  *
# *                                   is measured.                       *
# *                                                                      *
# *  2) CALL CPU_TIMER ( %VAL(1) ) -- prints on the screen used CPU time.*
# *                                                                      *
# *  3) R = CPU_TIMER ( %VAL(2) )  -- returns the CPU time as REAL*8     *
# *                                   number (in seconds).               *
# *                                                                      *
# *  4) CALL CPU_TIMER ( STR )     -- returns used CPU time into the     *
# *                                   character string STR of length at  *
# *                                   least 28 bytes.                    *
# *                                                                      *
# * ###  28-MAR-2009  cpu_timer  v3.0  (c) L. Petrov   18-FEB-2014  ###  *
# *                                                                      *
**************************************************************************
*/

{
  static struct timespec glob_cpu_timer;
  struct timespec now_cpu_timer;
  char   temp_str[28];
  double cpu_time;

#ifdef __MACH__ // OS X does not have clock_gettime, use clock_get_time
  clock_serv_t cclock;
  mach_timespec_t mts;
#else
  // clockid_t clock_to_use = CLOCK_PROCESS_CPUTIME_ID;
  clockid_t clock_to_use = CLOCK_THREAD_CPUTIME_ID ;
#endif

  if ( str == NULL ){
#ifndef __MACH__ 
       clock_gettime ( clock_to_use, &glob_cpu_timer);
#else
       host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
       clock_get_time(cclock, &mts);
       mach_port_deallocate(mach_task_self(), cclock);
       glob_cpu_timer.tv_sec = mts.tv_sec;
       glob_cpu_timer.tv_nsec = mts.tv_nsec;
#endif
       return 0.0e0 ;
  }
 
#ifndef __MACH__ 
  clock_gettime ( clock_to_use, &now_cpu_timer);
#else
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);
  now_cpu_timer.tv_sec = mts.tv_sec;
  now_cpu_timer.tv_nsec = mts.tv_nsec;
#endif


  cpu_time = ( now_cpu_timer.tv_sec  - glob_cpu_timer.tv_sec ) +
             ( now_cpu_timer.tv_nsec - glob_cpu_timer.tv_nsec )*1.0e-9;

  if ( (long) &*str == 1 ){
       printf ( "cpu_time: %17.9f\n", cpu_time );
       return cpu_time ;
  } else if ( (long) &*str == 2 ){
       return cpu_time ;
  } else {
    snprintf ( temp_str, sizeof(temp_str), "cpu_time: %17.9f\n", cpu_time );
    strncpy  ( str,  temp_str, len_str );
    if ( len_str > sizeof(temp_str)-1 ) {
	 bzero ( str+sizeof(temp_str)-1, len_str - sizeof(temp_str) + 1 );
    }
    return cpu_time ;
  }
  return 0.0e0 ;
}
