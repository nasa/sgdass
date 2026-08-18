#include <stdio.h>
#include <stdlib.h>
#include "ners.h"

// ************************************************************************
// *                                                                      *
// *   Test program NERS_C_EXAMPLE_02 demonstrates how to compute         *
// *   TAI time that corresponds to UTC time tag. The UTC time tag        *
// *   is expressed as sum of days elapsed since 2000.01.01_00:00:00 UTC  *
// *   epoch multiplied by 86400.0 and the UTC time tag with respect to   *
// *   the midnight.                                                      *
// *                                                                      *
// * ### 10-MAY-2017 NERS_C_EXAMPLE_02 v1.0 (c) L. Petrov 12-MAY-2017 ### *
// *                                                                      *
// ************************************************************************

void main(){

     struct   ners_struct ners ; /* internal NERS structure defined in ners.h */
     int      iuer ;
     int      lpar ;
     int      mjd_beg, mjd_end, mjd_obs;
     double   time_tai_beg, time_tai_end, utc_tim_obs, time_tai_obs;
     double   tai_beg, tai_end, utc_obs;
     double   utc_mtai;
//
// --- Define dates of the interval and the observation date
//
     mjd_beg = 57700 ; tai_beg = 12000.0;
     mjd_end = 57702 ; tai_end = 64200.0;
//
// --- Observation epoch MJD_OBS, UTC_OBS
//
     mjd_obs = 57701 ; utc_obs = 28923.4823534;

//
// --- Transform dates to time since epoch 2000.01.01_00:00:00.0 tai
//
     time_tai_beg = (mjd_beg - J2000__MJD)*86400.0 + tai_beg ;
     time_tai_end = (mjd_end - J2000__MJD)*86400.0 + tai_end ;
//
// --- NB: TIME_UTC_OBS is *not* interval of time elapsed since 2000.0 epoch!
//
     utc_tim_obs  = (mjd_obs - J2000__MJD)*86400.0 + utc_obs ;

//
// --- 1) Initialize NERS. We use default NERS__CONFIG file created during installation
// ---    NERS__CONFIG is defined in ners.h
// 
      iuer = -1 ;
      cners_init ( NERS__CONFIG, &ners, time_tai_beg, time_tai_end, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

//
// --- 2) Return the UTC_MTAI that corresponds to UTC timetag
//
      iuer = -1 ;
      cners_get_utcmtai ( &ners, &utc_tim_obs, &utc_mtai, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;
//
//--- Compute time TAI that corresponds to UTC time tag TIME_UTC_OBS
//
      time_tai_obs =  utc_tim_obs - utc_mtai ;

//
// --- Print UTC_MTAI
//
      printf ( "utc= %20.6f  utc_mtai= %f\n", utc_tim_obs, utc_mtai ) ;

//
// --- 3) Deallocate memory for all ners internal data structure
// ---    NERS__ALL is defined in ners.h
//
      cners_quit ( NERS__ALL, &ners );
}
