#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

void get_hr_time ( uint32_t  * time_info )
/*
# ************************************************************************
# *                                                                      *
# *   Routinbe get_hr_time returns system time in a broken format        *
# *   with precision to one nanosecond. (Though the accuracy of clock    *
# *   is more liklely be at a lelvel of one millisecond).                *
# *                                                                      *
# *   Usage:                                                             *
# *                                                                      *
# *   CALL GET_HR_TIME ( TIME_INFO )                                     *
# *                                                                      *
# *                                                                      *
# * _________________________ Input parameters: ________________________ *
# *                                                                      *
# *    TIME_INFO ( INTEGER*4 ) -- Adday of date. Dimension: 7.           *
# *                            TIME_INFO(1) -- year                      *
# *                            TIME_INFO(2) -- month                     *
# *                            TIME_INFO(3) -- day                       *
# *                            TIME_INFO(4) -- hour                      *
# *                            TIME_INFO(5) -- minute                    *
# *                            TIME_INFO(6) -- second                    *
# *                            TIME_INFO(7) -- nanosecond                *
# *                                                                      *
# * ###  31-AUG-2014  get_hr_time  v1.1 (c)  L. Petrov 31-AUG-2014  ###  *
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
  struct timespec        now_wall_timer;
 
  clock_gettime ( clock_to_use, &now_wall_timer);
  now_tm = localtime ( (time_t *) &now_wall_timer.tv_sec ) ;

#else
//
// --- Darwin
//
  static clock_serv_t    cclock;
  mach_timespec_t        now_wall_timer;
  time_t      *now;

  host_get_clock_service ( mach_host_self(), CALENDAR_CLOCK, &cclock );
  clock_get_time ( cclock, &now_wall_timer );
  mach_port_deallocate ( mach_task_self(), cclock );

  now = (time_t *) time (NULL) ;
  now_tm = localtime ( (time_t *) &now ) ;

#endif

  time_info[0] = now_tm->tm_year  + 1900 ;
  time_info[1] = now_tm->tm_mon + 1 ;
  time_info[2] = now_tm->tm_mday  ;
  time_info[3] = now_tm->tm_hour  ;
  time_info[4] = now_tm->tm_min  ;
  time_info[5] = now_tm->tm_sec  ;
  time_info[6] = now_wall_timer.tv_nsec  ;
  //   printf ( "time_info(1) = %d %d %d %d %d %d %d \n ", time_info[0], time_info[1], time_info[2], time_info[3],  time_info[4],  time_info[5],  time_info[6] ) ; /* %%%%%%%%%%% */
}
