#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vtd.h"

int
main (int argc, char **argv)
{
  char *vtd ;
  int  iuer ;

  vtd__obs_type obs_typ ;
  char   date_beg[21] ;
  char   date_end[21] ;
  char   sou_nam[8] ;
  char   sta_nam[2][8] ;
  char   date_obs[21] ;
  char   vtd_version[128];
  int    l_sta = 3;
  int    l_sou = 4;
  char   c_sta[l_sta][8] ;
  char   c_sou[l_sta][8] ;
  char   vtd_config_file[128] ;
  int    mjd_beg, mjd_end, mjd_obs ;
  double tai_beg, tai_end, tai_obs ;
  double pres[2] ;
  double temp[2] ;
  double der_del[VTD__NDER], der_rat[VTD__NDER], tau, rate_ph ;
  long   is;
  long   stack_size_in_bytes = 4LL*1024LL*1024LL*1024LL;

/*
//
// ---- Set stacksize. Alterntive is to set stacksize in shell:
// ---- commands limit stacksize 4000000 or limit -s 4000000
// ---- and set evironment variable GOMP_STACKSIZE
// ---- Program will crash in attempt to use default stacksize,
// ---- because fortran uses stack for storing variables
//
*/
  is = set_stacksize ( stack_size_in_bytes );

/*  ========== Beginning of definitions for computing path delay ==== */
//  definitions below are neede donly for computing contributin of ionosphere
//  to group and phase delays


  strncpy ( (char *) obs_typ.plrz, (const char *) "RR", 2 ) ;
  obs_typ.frq_ref[0] = 2.2E9 ;
  obs_typ.frq_ref[1] = 8.2E9 ;
  obs_typ.n_bnd      = 2 ;
  obs_typ.delay_type = VTD__MLMH__DTP ;
  obs_typ.frq_ion_eff[0] = 2.3E9 ;
  obs_typ.frq_ion_eff[1] = 8.5E9 ;
  obs_typ.status         = VTD__BND ;
  strncpy ( (char *) obs_typ.exp_name,  (const char *) "Test_05\0",
            strlen("Test_05")+1 ) ;
  strncpy ( (char *) obs_typ.scan_name, (const char *) "Scan_0001\0",
            strlen("Scan_0001")+1 ) ;
//

  strncpy ( (char *) c_sta[0], (const char *) "WESTFORD", 8 ) ;
  strncpy ( (char *) c_sta[1], (const char *) "GEOCENTR", 8 ) ;
  strncpy ( (char *) c_sta[2], (const char *) "WETTZELL", 8 ) ;

  strncpy ( (char *) c_sou[0], (const char *) "0955+476", 8 ) ;
  strncpy ( (char *) c_sou[1], (const char *) "2318+049", 8 ) ;
  strncpy ( (char *) c_sou[2], (const char *) "1749+096", 8 ) ;
  strncpy ( (char *) c_sou[3], (const char *) "2234+282", 8 ) ;
  strncpy ( (char *) date_beg, (const char *) "2002.12.11-18:00:00.0", 21 ) ;
  strncpy ( (char *) date_end, (const char *) "2002.12.11-19:00:00.0", 21 ) ;


  strncpy ( (char *) sta_nam[0],  (const char *) "WESTFORD", 8 ) ;
  strncpy ( (char *) sta_nam[1],  (const char *) "WETTZELL", 8 ) ;
  strncpy ( (char *) sou_nam,     (const char *) "2318+049", 8 ) ;
  strncpy ( (char *) date_obs,    (const char *) "2002.12.11-18:24:32.0", 21 ) ;

  pres[0] = 100170.0E0  ; // Pa
  pres[1] =  95430.0E0  ; // Pa
  temp[0] =    275.55E0 ; // K
  temp[1] =    278.55E0 ; // K

  strncpy ( (char *) vtd_config_file,  
            (const char *) "/vlbi/vtd_data/vtd_test_01.cnf\0", 
            strlen("/vlbi/vtd_data/vtd_test_01.cnf")+1 ) ;

/*  ========== End of definitions for computing path delay ==== */

//
// --- Convert calendar dates to pair MJD/TAI
//

  iuer = -1 ;
  date_to_time ( (char *)date_beg, &mjd_beg, &tai_beg, &iuer, sizeof(date_beg) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in decording begin date %s  iuer= %d\n", date_beg, iuer );
       exit ( 1 ) ;
  }

  iuer = -1 ;
  date_to_time ( (char *)date_end, &mjd_end, &tai_end, &iuer, sizeof(date_end) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in decording end date %s \n", date_end );
       exit ( 1 ) ;
  }

  iuer = -1 ;
  date_to_time ( (char *)date_obs, &mjd_obs, &tai_obs, &iuer, sizeof(date_obs) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in decording observing date %s \n", date_obs );
       exit ( 1 ) ;
  }

//
// --- Allocate memory for vtd. NB: the size of vtd object
// --- is inquired by a call of auxilliary routine vtd_size
//
  vtd = (char *) malloc ( vtd_size() ) ;

//
// --- Initiale internal structure of VTD
//
  iuer = -1 ;
  vtd_init ( vtd, &iuer ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in VTD initialization \n" );
       exit ( 1 ) ;
  }

//
// --- Parse VTD configuratrion file
//
  iuer = -1 ;
  vtd_conf ( (char *)vtd_config_file, vtd, &iuer, strlen(vtd_config_file) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in parsing VTD configuration file %s \n", 
                          vtd_config_file );
       exit ( 1 ) ;
  }

//
// --- Load the list sources and statsion that will be observing, 
// --- as well as various catalogues specified in the configuration file
//
  iuer = -1 ;
  vtd_load ( vtd, &l_sta, (char *)c_sta, &l_sou, (char *)c_sou, &mjd_beg, 
             &tai_beg, &mjd_end, &tai_end, &iuer, sizeof(c_sta[0]), 
             sizeof(c_sou[0]) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in loading VTD auxilliary data files \n" );
       exit ( 1 ) ;
  }

//
// --- Load meteorological parameters for station #1
//
  iuer = -1 ;
  vtd_meteo_in ( vtd, (char *)sta_nam[0], &pres[0], &temp[0], &temp[0], 
                 &iuer, sizeof(sta_nam[0]) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in putting in VTD meteorological information "
                         "about the first station %.8s \n", sta_nam[0] );
       exit ( 1 ) ;
  }

//
// --- Load meteorological parameters for station #2
//
  iuer = -1 ;
  vtd_meteo_in ( vtd, (char *)sta_nam[1], &pres[1], &temp[1], &temp[1], 
                 &iuer, sizeof(sta_nam[1]) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in putting in VTD meteorological information "
		 "about the second station %.8s \n", sta_nam[0] );
       exit ( 1 ) ;
  }

//
// --- Compute VLBI path delay, its rate of change and partial derivatives
// --- of path delay with respect to some parameters
//
  iuer = -1 ;
  vtd_delay ( (char *)sou_nam, (char *)sta_nam[0], (char *)sta_nam[1], 
              &mjd_obs, &tai_obs, &obs_typ, vtd, &tau, &rate_ph, 
              (double *)der_del, (double *)der_rat, &iuer,
              sizeof(sou_nam), sizeof(sta_nam[0]), sizeof(sta_nam[1]) ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in computing path delay \n" ) ;
       exit ( 1 ) ;
  }

//
// --- Now print results
//
  vtd_get_version ( (char *)vtd_version, sizeof(vtd_version) ) ;

  printf ( "%s\n", vtd_version );
  printf ( "VTD Configuratin file: %s\n", vtd_config_file ) ; 
  printf ( "Observing stations:    %.8s  %.8s\n", sta_nam[0], sta_nam[1] );
  printf ( "Observing source:      %.8s\n", sou_nam );
  printf ( "Time_tai:              %.21s\n", date_obs );
  printf ( "Group_delay= %19.12g s,  Phase_delay_rate= %20.13E \n", tau, rate_ph ) ;

//
// --- Deallocate memory allocated by VTD
//
  iuer = -1 ;
  vtd_quit ( vtd, &iuer ) ;
  if ( iuer != 0 ){
       fprintf ( stderr, "Error in an attempt to quit VTD \n" );
       exit ( 1 ) ;
  }

//
// --- Deallocate VTD object. NB: vtd_quit should preceed free(vtd),
// --- otherwise a memory leakage will occur
//
  free(vtd) ;
  exit ( 0 ) ;
}
