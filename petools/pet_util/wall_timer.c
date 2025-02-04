#include <stdio.h>
#include <string.h>
#include <time.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

double wall_timer ( char str[], long len_str )
/*
# ************************************************************************
# *                                                                      *
# *  Routine wall_timer  returns elapsed wall time with nanosecond       *
# *  resolution.                                                         *
# *                                                                      *
# *  Usage from Fortran:                                                 *
# *  1) CALL WALL_TIMER ( %VAL(0) ) -- resets to zero used WALL time.    *
# *                                    should be called at the beginning *
# *                                    of the block, which WALL used     *
# *                                    time is measured.                 *
# *                                                                      *
# *  2) CALL WALL_TIMER ( %VAL(1) ) -- prints on the screen used WALL    *
# *                                    time.                             *
# *                                                                      *
# *  3) R = WALL_TIMER ( %VAL(2) )  -- returns the WALL time as REAL*8   *
# *                                    number (in seconds).              *
# *                                                                      *
# *  3) CALL WALL_TIMER ( STR )     -- returns used WALL time into the   *
# *                                    character string STR of length at *
# *                                    least 28 bytes.                   *
# *                                                                      *
# * ###  04-JUN-2009    wall_timer 2.1 (c) L. Petrov     03-APR-2014 ### *
# *                                                                      *
**************************************************************************
*/


{
  char   temp_str[28];
  double wall_time;
#ifndef DARWIN
//
// --- Linux
//
  clockid_t clock_to_use = CLOCK_MONOTONIC_RAW;
  static struct timespec glob_wall_timer = {0, 0};
  struct timespec        now_wall_timer;

  if ( str == NULL ){
       clock_gettime ( clock_to_use, &glob_wall_timer);
       return ;
  }
 
  clock_gettime ( clock_to_use, &now_wall_timer);

#else
//
// --- Darwin
//
  static clock_serv_t    cclock;
  static mach_timespec_t glob_wall_timer = {0, 0};
  mach_timespec_t        now_wall_timer;
  host_get_clock_service ( mach_host_self(), CALENDAR_CLOCK, &cclock );
  
  if ( str == NULL ){
       clock_get_time ( cclock, &glob_wall_timer );
       mach_port_deallocate ( mach_task_self(), cclock );
       return ;
  }

  clock_get_time ( cclock, &now_wall_timer );
  mach_port_deallocate ( mach_task_self(), cclock );

#endif

  wall_time = ( now_wall_timer.tv_sec  - glob_wall_timer.tv_sec ) +
              ( now_wall_timer.tv_nsec - glob_wall_timer.tv_nsec )*1.0e-9;

  if ( (long) &*str == 1 ){
       printf ( "wall_time: %16.9f\n", wall_time );
       return wall_time ;
  } else if ( (long) &*str == 2 ){
       return wall_time ;
  } else {
    snprintf ( temp_str, sizeof(temp_str), "wall_time: %17.9f\n", wall_time );
    strncpy  ( str,  temp_str, len_str );
    if ( len_str > sizeof(temp_str)-1 ) {
	 bzero ( str+sizeof(temp_str)-1, len_str - sizeof(temp_str) + 1 );
    }
    return wall_time ;
  }
  return 0.0e0 ;
}
