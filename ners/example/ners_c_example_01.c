#include <stdio.h>
#include <stdlib.h>
#include "ners.h"

// ************************************************************************
// *                                                                      *
// *   Test program NERS_C_EXAMPLE_01 demonstrates how to compute         *
// *   the Earth rotation matrix that transform a vector in the rotating  *
// *   terrestrial coordinate system fixed with respect the Earth's       *
// *   crust to the inertial non-rotating celestial coordinate system.    *
// *                                                                      *
// * ### 10-MAY-2017 NERS_C_EXAMPLE_01 v1.0 (c) L. Petrov 12-MAY-2017 ### *
// *                                                                      *
// ************************************************************************

void main(){

     struct  ners_struct ners ; /* This sructure is defined in ners.h */
     int     iuer ;
     int     lpar ;
     int     mjd_beg, mjd_end, mjd_obs;
     double  time_tai_beg, time_tai_end, time_tai_obs;
     double  tai_beg, tai_end, tai_obs;
     double  eops[27];
     double  ermat[3][3];
//
// --- Define dates of the interval and the observation date
//
     mjd_beg = 56700 ; tai_beg = 12000.0;
     mjd_end = 56702 ; tai_end = 64200.0;
     mjd_obs = 56701 ; tai_obs = 28923.9284260001;

//
//-- Transform dates to time
//
     time_tai_beg = (mjd_beg - J2000__MJD)*86400.0 + tai_beg ;
     time_tai_end = (mjd_end - J2000__MJD)*86400.0 + tai_end ;
     time_tai_obs = (mjd_obs - J2000__MJD)*86400.0 + tai_obs ;

//
// --- 1) Initialize NERS. We use default NERS__CONFIG file created during installation
//        NERS__CONFIG is defined in ners.h
// 
      iuer = -1 ;
      cners_init ( NERS__CONFIG, &ners, time_tai_beg, time_tai_end, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

//
// --- 2) Compute the EOP, the rotation matrix from the rotating terrestrial coordinate
// ---   to the intertial celestial coordinte system
//
      iuer = -1 ;
      cners_get_eop ( &ners, time_tai_obs, "mat", 9, &lpar, ermat, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

//
// --- print the metrix elements
//
      printf ( "MAT1= %20.12f %20.12f %20.12f \n", ermat[0][0], ermat[0][1], ermat[0][2] ) ;
      printf ( "MAT2= %20.12f %20.12f %20.12f \n", ermat[1][0], ermat[1][1], ermat[1][2] ) ;
      printf ( "MAT3= %20.12f %20.12f %20.12f \n", ermat[2][0], ermat[2][1], ermat[2][2] ) ;

//
// --- 3) Deallocate memory for all ners internal data structure
// ---    NERS__ALL is defined in ners.h
//
      cners_quit ( NERS__ALL, &ners );
}
