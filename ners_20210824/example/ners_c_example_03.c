#include <stdio.h>
#include <stdlib.h>
#include "ners.h"

// ************************************************************************
// *                                                                      *
// *   Test program NERS_C_EXAMPLE_03 demonstrates how to compute         *
// *   the time series of 8 Earth orientation parameters with a specified *
// *   time step for the specified time range and print them as a table.  *
// *   NB: the units in the table are not SI units, but the non-standard  *
// *   units historically used in the past.                               *
// *                                                                      *
// * ### 10-MAY-2017 NERS_C_EXAMPLE_03 v1.0 (c) L. Petrov 12-MAY-2017 ### *
// *                                                                      *
// ************************************************************************

void main(){

     struct  ners_struct ners ;
     int     iuer ;
     int     lpar ;
     int     mjd_beg, mjd_end, mjd_obs;
     double  time_tai_beg, time_tai_end;
     double  tai_beg, tai_end, tai_obs, tim_step;
     int     m_par =   8 ;
     int     m_ser = 256 ;
     int     i, ns, iday ;
     double  eopser[m_par][m_ser], timser[m_ser];
//
// --- Time step of the EOP time series table
//
     tim_step = 43200.0 ;
//
// --- Ddefine dates of the interval for the EOP table
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
      cners_init ( NERS__CONFIG, &ners, time_tai_beg, time_tai_beg + tim_step, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

//
// --- 2) Compute the EOP time series. Parameter "eops" tells to NERS linrary
// ---    that cners_get_series should generate a table with 8 EOPS:
// ---    Xpole, Ypole, UT1, X pole rate, Y pole rate, UT1 rate,
// ---    nutation offset in longitude and nutation offset in obliquity with 
// ---    in accordance to a model. NB: these parametdrs do not include empirical
// ---    harmonic variations in the Earth rotation parameters with respect to the
// ---    model
//
      iuer = -1 ;
      cners_get_series ( &ners, time_tai_beg, time_tai_end, tim_step, "eops", m_par, 
                         m_ser, &ns, timser, eopser, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

//
// --- print the eop series. NB: cners_get_series returns the EOP
// --- in non-SI units
//
      for ( i=0; i<ns; i++ ){
          iday = (int )(timser[i]/86400.0) ;
          printf ( "mjd= %5d Tai: %7.1f," \
                   " X_pole= %10.6f arcsec," \
                   " Y_pole= %10.6f arcsec," \
                   " UT1= %10.6f s," \
                   " Xp_rate= %11.8f arcsec/day," \
                   " Yp_rate= %11.8f arcsec/day," \
                   " UT1_rate= %11.8f s/day," \
                   " Dpsi= %10.6f arcsec," \
                   " Deps= %10.6f arcsec \n", \
                   mjd_beg + iday, timser[i] -86400.0*iday,  \
                   eopser[0][i], eopser[1][i], eopser[2][i], \
                   eopser[3][i], eopser[4][i], eopser[5][i], \
                   eopser[6][i], eopser[7][i] );
      }      

//
// --- 3) Deallocate memory for all ners internal data structure
// ---    NERS__ALL is defined in ners.h
//
      cners_quit ( NERS__ALL, &ners );
}
