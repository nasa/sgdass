#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#endif

#define UNIX__J2000_UTC  946684800.0
#define UNIX__J2000_TAI  946684832.0

double get_utc ( )
/*
# ************************************************************************
# *                                                                      *
# *   Routine get_utc returns current UTC time tag with resepect to      *
# *   2000.01.01_00:00:00.0 UTC with precision to one nanosecond.        *
# *   (Though the accuracy of clock is more likely be at a level of      *
# *   one millisecond).                                                  *
# *                                                                      *
# *   Usage:                                                             *
# *                                                                      *
# *   UTC = GET_UTC ( )                                                  *
# *                                                                      *
# *   GET_UTC ( REAL*8     )                                             *
# *                                                                      *
# * ###  11-MAY-2016     get_utc   v1.0 (c)  L. Petrov 11-MAY-2016  ###  *
# *                                                                      *
# ************************************************************************
*/

{
  struct tm    *now_tm;
#ifndef DARWIN
//
// --- Linux
//
  clockid_t clock_to_use = CLOCK_REALTIME ;
  struct timespec   timer;
 
  clock_gettime ( clock_to_use, &timer);
  return ( (timer.tv_sec - UNIX__J2000_UTC) + timer.tv_nsec/1.0E9 ) ;

#else
//
// --- Darwin
//

  clock_serv_t cclock;
  mach_timespec_t mts;
  host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
  clock_get_time(cclock, &mts);
  mach_port_deallocate(mach_task_self(), cclock);

  return ( (mts.tv_sec - UNIX__J2000_UTC) + mts.tv_nsec/1.0E9 );

#endif

}
