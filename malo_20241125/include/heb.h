//
//  Header block for heb.i   Lat updated on 2013.03.01
//
#define HEB__MATT    64
#define HEB__HDL   2048
#define HEB__LATT   128
#define HEB__LABEL "HEB Format version of 2013.01.30"
#define HEB__LEN_MAX 134217728

#define heb_2d_r4_elem(heb$$,lon,lat) heb$$->r4_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_r4_elem(heb$$,lon,lat,slice) heb$$->r4_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_r4_elem(heb$$,lon,lat,slice,cube) heb$$->r4_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

#define heb_2d_r8_elem(heb$$,lon,lat) heb$$->r8_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_r8_elem(heb$$,lon,lat,slice) heb$$->r8_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_r8_elem(heb$$,lon,lat,slice,cube) heb$$->r8_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

#define heb_2d_i1_elem(heb$$,lon,lat) heb$$->i1_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_i1_elem(heb$$,lon,lat,slice) heb$$->i1_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_i1_elem(heb$$,lon,lat,slice,cube) heb$$->i1_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

#define heb_2d_i2_elem(heb$$,lon,lat) heb$$->i2_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_i2_elem(heb$$,lon,lat,slice) heb$$->i2_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_i2_elem(heb$$,lon,lat,slice,cube) heb$$->i2_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

#define heb_2d_i4_elem(heb$$,lon,lat) heb$$->i4_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_i4_elem(heb$$,lon,lat,slice) heb$$->i4_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_i4_elem(heb$$,lon,lat,slice,cube) heb$$->i4_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

#define heb_2d_i8_elem(heb$$,lon,lat) heb$$->i8_ptr[lat*heb$$->dims[0]+lon]
#define heb_3d_i8_elem(heb$$,lon,lat,slice) heb$$->i8_ptr[slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0] + lon]
#define heb_4d_i8_elem(heb$$,lon,lat,slice,cube) heb$$->i8_ptr[cube*heb$$->dims[0]*heb$$->dims[1]*heb$$->dims[2] + slice*heb$$->dims[0]*heb$$->dims[1] + lat*heb$$->dims[0]+lon]

struct heb_struct {
       char      att_name[HEB__LATT][HEB__MATT];
       char      att_val[HEB__LATT][HEB__MATT];
       long      dims[4];
       int       natts;

       double    *r8_ptr;
       long      *i8_ptr;
       float     *r4_ptr;
       long      *i4_ptr;
       short     *i2_ptr;
       char      *i1_ptr;

       long long i8_fill_value;
       double    r8_fill_value;
       float     r4_fill_value;
       long      i4_fill_value;
       short     i2_fill_value;
       char      i1_fill_value;
};

int find_char   ( char string[], char chr, int offset, int len );
int find_nochar ( char string[], char chr, int offset, int len );
