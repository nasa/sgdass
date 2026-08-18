#include <stdio.h>
#include <stdlib.h>
#include "ners.h"

// ************************************************************************
// *                                                                      *
// *   Test program NERS_C_EXAMPLE_04 demonstrates how to learn           *
// *   the range of dates for which the NERS provides the Earth           *
// *   orientation parameters. NERS routine NERS_INQ returns either two   *
// *   ranges:  1) the range of the EOP forecast which is based on        *
// *   measurements, also called the data assimilation range and          *
// *   2) the long-term prediction that is based in extrapolation, or     *
// *   the time epoch of the forecast generation. It is assumed that the  *
// *   EOP long-term prediction will be used only in a case of either     *
// *   NERS servers failure or a failure of the NERS client to establish  *
// *   Internet connection.                                               *
// *                                                                      *
// * ### 10-MAY-2017 NERS_C_EXAMPLE_04 v1.0 (c) L. Petrov 12-MAY-2017 ### *
// *                                                                      *
// ************************************************************************

void main(){

     struct  ners_struct ners ;
     int     iuer ;
     int     lpar ;
     int     mjd_beg, mjd_end, mjd_obs;
     double  time_tai_beg, time_tai_end;
     double  tai_beg, tai_end, tai_obs;
     int     m_par =   3 ;
     int     l_par, iday ;
     double  pars[m_par] ;
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
// --- 2) Inquire the EOP range. When cners_inq is called with 'range' parameter,
// ---    it returns three time epochs for two ranges
//
      iuer = -1 ;
      cners_inq ( &ners, "range", m_par, &l_par, pars, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

      printf ( "Data assimilation range:    %14.2f, %14.2f\n" \
               "Long-term prediction range: %14.2f, %14.2f\n", \
                pars[0], pars[1], pars[1], pars[2] ) ;

//
// --- 3) Inquire the time epoch of the forecast generation.
// ---    When cners_inq is called with 'fcn_gen_time' parameter, 
// ---    it returns the epoch of the forecast generation.
//
      iuer = -1 ;
      cners_inq ( &ners, "fcs_gen_time", m_par, &l_par, pars, &iuer ) ;
      if  ( iuer != 0 ) exit ( 1 ) ;

      printf ( "Forecast generation time:   %14.2f \n", pars[0] ) ;

//
// --- 4) Deallocate memory for all ners internal data structure
// ---    NERS__ALL is defined in ners.h
//
      cners_quit ( NERS__ALL, &ners );
}
