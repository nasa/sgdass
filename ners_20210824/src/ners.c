#include <string.h>
#include <stdio.h>
#include "ners.h"

void cners_init ( char * config_file, struct ners_struct * ners, double time_tai_beg, 
                  double time_tai_end, int * iuer )
{
    ners_init ( config_file, ners, &time_tai_beg, &time_tai_end, iuer, strlen(config_file) ) ;
  
}

void cners_get_eop ( struct ners_struct * ners, double time_tai_obs, char * cpar, 
                     int mpar, int * lpar, double eops[], int * iuer )
{
    ners_get_eop ( ners, &time_tai_obs, cpar, &mpar, lpar, eops, iuer, strlen(cpar) ) ;
}

void cners_get_utcmtai ( struct ners_struct * ners, double * utc_obs, double * utc, int * iuer )
{
    ners_get_utcmtai ( ners, utc_obs, utc, iuer ) ;
}

void cners_quit ( int quit_code, struct ners_struct * ners )
{
    ners_quit ( &quit_code, ners ) ;
}

void cners_get_series  ( struct ners_struct * ners, double time_tai_beg, double time_tai_end, 
                         double tim_step, char * cpar, int m_par, int m_ser, int * ns, 
                         double tim[], double ser[], int * iuer )
{
    ners_get_series  ( ners, &time_tai_beg, &time_tai_end, &tim_step, cpar, &m_par, &m_ser, ns, 
                       tim, ser, iuer, strlen(cpar)  ) ;
}

void cners_inq ( struct ners_struct * ners, char * req, int m_par, int * l_par, 
                         double pars[], int * iuer )
{
    ners_inq  ( ners, req, &m_par, l_par, pars, iuer, strlen(req) ) ;
}

void cners_azelha_comp ( struct ners_struct * ners, double tim_tai, double coo_trs[], double ra, double dec,
                         char * refr_mode, double * az, double * el, double * ha, 
                         double * az_rate, double * el_rate, double * ha_rate, int * iuer )
{
    ners_azelha_comp   ( ners, &tim_tai, coo_trs, &ra, &dec,
                         refr_mode, az, el, ha, az_rate, el_rate, ha_rate, iuer, strlen(refr_mode) ) ;
}
