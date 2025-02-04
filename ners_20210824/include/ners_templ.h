#define J2000__MJD     51544
#define NERS__FCS  820148234 
#define NERS__EXP  612907562 
#define NERS__ALL  126054782 
#define NERS__LEN     @NERS__LEN@
#define NERS__CONFIG "@NERS__CONFIG@"

struct ners_struct {
    char body[NERS__LEN] ;
};

extern void cners_init ( char * config_file, struct ners_struct * ners, double time_tai_beg, double time_tai_end, int * iuer ) ;

extern void cners_get_eop ( struct ners_struct * ners, double time_tai_obs, char * cpar, int mpar, int * lpar, double eops[], int * iuer ) ;

extern void cners_get_utcmtai ( struct ners_struct * ners, double * utc_obs, double * utc, int * iuer ) ;

extern void cners_quit ( int quit_code, struct ners_struct * ners ) ;

extern void cners_get_series  ( struct ners_struct * ners, double time_tai_beg, double time_tai_end, 
                         double tim_step, char * cpar, int m_par, int m_ser, int * ns, 
                         double tim[], double ser[], int * iuer ) ;

extern void cners_azel_comp ( struct ners_struct * ners, double time_tai_beg, double coo_trs[],
                              double ra, double dec, char * refr, double * az, double * el, 
                              double * ha, double * az_rate, double * el_rate, double * ha_rate, 
                              int * iuer ) ;

extern void cners_inq ( struct ners_struct * ners, char * req, int m_par, int * l_par, 
                         double pars[], int * iuer ) ;
