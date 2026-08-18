#include <stdio.h>
#include <stdlib.h>
#include "ners.h"

// ************************************************************************
// *                                                                      *
// *   Test program NERS_C_EXAMPLE_06 demonstrates how to compute         *
// *   right ascenstion and declination, as well as their rates for       *
// *   station observing a source at a given azimuth and elevation        *
// *   at the specified moment of time using NERS.                        *
// *                                                                      *
// * ### 22-JUL-2025 NERS_C_EXAMPLE_05 v1.0 (c) L. Petrov 22-JUL-2025 ### *
// *                                                                      *
// ************************************************************************

void main(){

     double   PI__NUM = 3.141592653589793 ;
     double   DEG__TO__RAD  = PI__NUM/180.0 ;
     struct  ners_struct ners ;
     int     iuer ;
     int     lpar ;
     int     mjd_beg, mjd_end, mjd_obs;
     double  time_tai_beg, time_tai_end;
     double  tai_beg, tai_end, tai_obs, tim_tai_obs, tim_tai, 
             az, el, ra, dec, ra_rate, dec_rate ;
     double  coo_trs[3]; 
     char * NERS__REFR_NONE  = "none"  ;
     char * NERS__REFR_RADIO = "radio" ;
     char * NERS__REFR_OPTIC = "optic" ;

//
// --- Define dates of the interval
//
     mjd_beg = 57600 ; tai_beg = 0.0;
     mjd_end = 57700 ; tai_end = 0.0;

//
//-- Transform dates to time
//
     time_tai_beg = (mjd_beg - J2000__MJD)*86400.0 + tai_beg ;
     time_tai_end = (mjd_end - J2000__MJD)*86400.0 + tai_end ;

//
// --- 1) Initialize NERS. We use default NERS__CONFIG file created during installation
// ---    NERS__CONFIG is defined in ners.h
// 
     iuer = -1 ;
     cners_init ( NERS__CONFIG, &ners, time_tai_beg, time_tai_beg, &iuer ) ;
     if  ( iuer != 0 ) exit ( 1 ) ;
//

     mjd_obs     = 57600 ;
     tim_tai_obs = 20000.0 ;
     tim_tai     = (mjd_obs - J2000__MJD)*86400.0 + tim_tai_obs ;
     coo_trs[0]  =  1446375.114  ; /* HN-VLBA */
     coo_trs[1]  = -4447939.660  ;
     coo_trs[2]  =  4322306.122  ;
     ra          =  3.545084643  ;
     dec         =  1.349095330  ; 

     iuer = -1 ;
     cners_radec_comp ( &ners, tim_tai, coo_trs, az, el, "radio", 
                        &ra, &dec, &ra_rate, &dec_rate, &iuer ) ;

     printf  ( "iuer %d = \n", iuer ) ;
//     if ( iuer != 0 ) exit ( 1 ) ;

     printf ( "Right ascension: %14.9f deg,  Declination: %14.9f deg deg  \n", 
               ra/DEG__TO__RAD, dec/DEG__TO__RAD ) ;
     printf ( "Right_ascension_rate: %14.7g rad/s,  Declination_rate: %14.7g rad/s \n", 
               ra_rate, dec_rate ) ;
 
//
// -- 4) Deallocate memory for all ners internal data structure
// --    NERS__ALL is defined in ners.h
//
     cners_quit ( NERS__ALL, &ners ) ;
}
