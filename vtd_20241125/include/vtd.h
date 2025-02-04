//
//   Include-block for C-binding for VTD routines
//
//   Last update: 2010.06.15_14:39:14
//

#define VTD__M_STA   32 // ! Maximal number of stations
#define VTD__M_SOU 1024 // ! Maximal number of sources
#define VTD__M_BND    2 // ! Maximum number of frequency bands
#define VTD__M_FRQ   32 // ! Maximum channels in one band for ionospheric delay computations

#define VTD__PL__DTP   7001
#define VTD__PH__DTP   7002
#define VTD__SL__DTP   7003
#define VTD__SH__DTP   7004
#define VTD__ML__DTP   7005
#define VTD__MH__DTP   7006
#define VTD__PLPH__DTP 7007
#define VTD__SLSH__DTP 7008
#define VTD__MLMH__DTP 7009
#define VTD__PHML__DTP 7010
#define VTD__PHMH__DTP 7011
#define VTD__PLML__DTP 7012
#define VTD__PLMH__DTP 7013

#define VTD__BND  6001
#define VTD__CHN  6002
#define VTD__NDER   64    // ! Number of partial derviatvies and misc parameters in one record

#define VTD__DER_E1     1 // ! Partial derivative wrt Earth rotation Euler angle 1
#define VTD__DER_E2     2 // ! Partial derivative wrt Earth rotation Euler angle 2
#define VTD__DER_E3     3 // ! Partial derivative wrt Earth rotation Euler angle 3
#define VTD__DER_ST1X   4 // ! Partial derivative wrt Station 1, X coordinate
#define VTD__DER_ST1Y   5 // ! Partial derivative wrt Station 1, Y coordinate
#define VTD__DER_ST1Z   6 // ! Partial derivative wrt Station 1, Z coordinate
#define VTD__DER_ST2X   7 // ! Partial derivative wrt Station 2, X coordinate
#define VTD__DER_ST2Y   8 // ! Partial derivative wrt Station 2, Y coordinate
#define VTD__DER_ST2Z   9 // ! Partial derivative wrt Station 2, Z coordinate
#define VTD__DER_RA    10 // ! Partial derivative wrt Right ascension
#define VTD__DER_DL    11 // ! Partial derivative wrt Declination
#define VTD__DER_AT1   12 // ! Partial derivative wrt Station 1, atmospheric zenith path delay
#define VTD__DER_AT2   13 // ! Partial derivative wrt Station 2, atmospheric zenith path delay
#define VTD__DER_ATN1  14 // ! Partial derivative wrt Station 1, atmospheric north tilt
#define VTD__DER_ATE1  15 // ! Partial derivative wrt Station 1, atmospheric east  tilt
#define VTD__DER_ATN2  16 // ! Partial derivative wrt Station 2, atmospheric north tilt
#define VTD__DER_ATE2  17 // ! Partial derivative wrt Station 2, atmospheric east  tilt
#define VTD__DER_POS1  18 // ! Partial derivative wrt Position of the object, X coordinate
#define VTD__DER_POS2  19 // ! Partial derivative wrt Position of the object, Y coordinate
#define VTD__DER_POS3  20 // ! Partial derivative wrt Position of the object, Z coordinate
#define VTD__DER_VEL1  21 // ! Partial derivative wrt Velocity of the object, X coordinate
#define VTD__DER_VEL2  22 // ! Partial derivative wrt Velocity of the object, Y coordinate
#define VTD__DER_VEL3  23 // ! Partial derivative wrt Velocity of the object, Z coordinate
#define VTD__DER_AXF1  24 // ! Partial derivative wrt Antenna axis offset length 1st antenna
#define VTD__DER_AXF2  25 // ! Partial derivative wrt Antenna axis offset length 2nd antenna
#define VTD__ELEV1     33 // ! Elevation angle of the 1st antenna
#define VTD__ELEV2     34 // ! Elevation angle of the 2nd antenna
#define VTD__AZIM1     35 // ! Aximuth angle of the 1st antenna
#define VTD__AZIM2     36 // ! Aximuth angle of the 2nd antenna
#define VTD__TROP1     35 // ! Slanted troposphere path at the 1st antenna
#define VTD__TROP2     36 // ! Slanted troposphere path at the 2nd antenna
#define VTD__IONO1     37 // ! Slanted ionosphere  path at the 1st antenna
#define VTD__IONO2     38 // ! Slanted ionosphere  path at the 2nd antenna
#define VTD__TRP_HZD1  39 // ! Hydrostatic troposphere path delay in zentith at the 1st antenna
#define VTD__TRP_HZD2  40 // ! Hydrostatic troposphere path delay in zentith at the 2nd antenna
#define VTD__TRP_WZD1  41 // ! Non-hydrostatic troposphere path delay in zentith at the 1st antenna
#define VTD__TRP_WZD2  42 // ! Non-hydrostatic troposphere path delay in zentith at the 1nd antenna
#define VTD__PARAL1    43 // ! Parallactic agle at the 1st antenna
#define VTD__PARAL2    44 // ! Parallactic agle at the 2nd antenna
#define VTD__STRUC     45 // ! Contribution of the source structure to time delay


typedef struct {
        long    delay_type ;
//
 	long    l_frq      ; // ! The number of frequency channels
        char    plrz[2]    ; // ! Polarization: RR, LL, RL, LR or NO
        long    status     ; // ! Status: VTD__UNDF, VTD__BND, VTD__CHN
        long    n_bnd      ; // ! Number of bands
        char    exp_name[16]  ; // ! Experiment name
        char    scan_name[16] ; // ! Scan name
//
        double  frq[VTD__M_BND][VTD__M_FRQ] ; //! Array of cyclic frequencies
        double  wei[VTD__M_BND][VTD__M_FRQ] ; //! Array of weights per channel
//
	double  frq_ref[VTD__M_BND]        ; // ! Reference frequency per band
        double  frq_ion_eff[VTD__M_BND]    ; // ! Effective ionosphere frequency
//                                           // ! for this type of delay
} vtd__obs_type ;

/*******************************************************************/
/*                                                                 */
/*  Function prototypes of aomw top level VTD routines             */
/*                                                                 */
/*******************************************************************/

extern long vtd_size ( void ) ;

extern void vtd_init ( 
              char * vtd, 
              int  * iuer  
            ) ;

extern void vtd_conf ( 
               char * vtd_config_file, 
               char * vtd, 
               int  * iuer, 
               int    sizeof_vtd_config_file 
            ) ;

extern void vtd_load ( 
               char   * vtd, 
               int    * l_sta, 
               char   * c_sta, 
               int    * l_sou, 
               char   * c_sou,
               int    * mjd_beg, 
	       double * tai_beg, 
               int    * mjd_end, 
	       double * tai_end, 
               int    * iuer, 
               int      sizeof_c_sta_element,
               int      sizeof_c_sou_element
            ) ;

extern void vtd_meteo_in ( 
               char   * vtd, 
               char   * sta_nam, 
               double * pres, 
               double * temp, 
               double * humid, 
               int    * iuer, 
               int      sizeof_sta_nam 
            ) ;

extern void vtd_delay ( 
               char          * sou_nam, 
               char          * sta_nam1, 
               char          * sta_nam2, 
               int           * mjd_obs, 
               double        * tai_obs, 
	       vtd__obs_type * obs_typ, 
               char          * vtd, 
               double        * tau, 
               double        * rate_ph, 
               double        * der_del, 
               double        * der_rat, 
               int           * iuer,
               int             sizeof_sou_nam, 
               int             sizeof_sta_nam1, 
               int             sizeof_sta_nam2 
              ) ;

extern void   vtd_get_version ( 
                char * vtd_version, 
                int    sizeof_vtd_version
              ) ;

extern void vtd_quit ( 
              char * vtd, 
              int  * iuer  
            ) ;

/*******************************************************************/
/*                                                                 */
/*  Function prototypes of some handy PETOOLS routines             */
/*                                                                 */
/*******************************************************************/

//extern int  ilen ( 
//              char * str, 
//              int    sizeof_str 
//            ) ;

extern int  i_len ( 
              char * str, 
              int    sizeof_str 
            ) ;


extern void date_to_time ( 
              char   * date_beg, 
              int    * mjd_beg, 
              double * tai_beg, 
              int    * iuer, 
              int      sizeof_date_beg 
       ) ;

extern char *  mjdsec_to_date ( 
              char   * res,
              int      res_len,
              int    * mjd, 
              double * tai, 
              int    * iuer
       ) ;
