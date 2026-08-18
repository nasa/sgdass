#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>

#include "heb.h"

int heb_read ( char * filin, struct heb_struct  *heb, char * out_type ) {
// ************************************************************************
// *                                                                      *
// *   Routine heb_read reads a file filin in heb format, parses the      *
// *   header, the data section, and fills the field of structure heb.    *
// *   It returns 1 on successful completion and -1 in a case of failure. *
// *                                                                      *
// *    filin -- input file.                                              *
// *      heb -- structure where results of parsing will be put.          *
// * out_type -- type ofthe ouotput data:                                 *
// *             I1 --  8-bit integer;                                    *
// *             I2 -- 16-bit integer;                                    *
// *             I4 -- 32-bit integer;                                    *
// *             I8 -- 64-bit integer;                                    *
// *             R4 -- 32-bit float;                                      *
// *             R8 -- 64-bit float;                                      *
// *                                                                      *
// *   Public elements of the heb structure:                              *
// *                                                                      *
// *   heb->natts      int       -- number of attributes                  *
// *   heb->dims       long long -- dimensions. Heb value is              *
// *                                a four-dimensional array.             *
// *   heb->attr_name  char      -- Array of attribute names.             *
// *   heb->attr_val   char      -- Array of attribute values.            *
// *   heb->xx_fill_value        -- fill_value. "xx" is the prefix that   *
// *                                determines data type: i1, i2, i4, r4, *
// *                                or r8.                                *
// *   heb->xx_i1_ptr            -- pointer to output array. "xx" is the  *
// *                                prefix that determines data type:     *
// *                                i1, i2, i4, r4, or r8.                *
// *                   One of these six pointers is allocated and points  *
// *                   to the contents of retrieved data section          *
// *                   depending on out_type. The pointer points to the   *
// *                   flat array of values. The 4D array is presented as *
// *                   a continuous chunk of memory. NB: Fortran memory   *
// *                   layoyt is used: first dimension runs first.        *
// *                   A given elemet can be accessed using macros:       *
// *                                                                      *
// *                   heb_2d_xx_elem(heb,i0,i1)                          *
// *                   heb_3d_xx_elem(heb,i0,i1,i2)                       *
// *                   heb_4d_xx_elem(heb,i0,i1,i2,i3)                    *
// *                                                                      *
// *                   where i1, i2, i3, i4 are indices over dimensions   *
// *                   0,1,2,3, and xx is the data type (low case).       *
// *                                                                      *
// *  ### 28-FEB-2013    heb_read   v1.0 (c)  L. Petrov  01-MAR-2013 ###  *
// *                                                                      *
// ************************************************************************

    int  fd, i, k, len, ir, j, m, data_offset, elem_len;
    long is, nel;
    long long data_len, data_chunk, got_bytes, ind;
    char buf[HEB__HDL+1];
    char colon = 58 ;
    char blank = 32 ;
    char cr    = 10 ;
    char  *i1_ptr     = NULL;
    short *i2_ptr     = NULL;
    int   *i4_ptr     = NULL;
    long long *i8_ptr = NULL;
    float  *r4_ptr    = NULL;
    double *r8_ptr    = NULL;
    float  scale_r4  = 1.0 ;
    float  offset_r4 = 0.0 ;
    double scale_r8  = 1.0 ;
    double offset_r8 = 0.0 ;
    char att[128], str[128], data_transform[8], data_format[8];
//
    heb->r8_fill_value = 0;
    heb->i8_fill_value = 0;
    heb->i4_fill_value = 0;
    heb->i2_fill_value = 0;
    heb->i1_fill_value = 0;
//
    heb->r8_ptr = NULL; 
    heb->i8_ptr = NULL;
    heb->i4_ptr = NULL; 
    heb->i2_ptr = NULL; 
    heb->i1_ptr = NULL; 
//
// --- Open and read the file header
//
    fd = open ( filin, O_RDONLY );
    if ( fd < 0 ){
         perror ( "heb_read open: " );
         fprintf ( stderr, "Cannot open input file %s\n", filin );
 	 return -1;
    }
    is = read ( fd, buf, HEB__HDL );
    if ( is < HEB__HDL ){
         perror ( "heb_read read_header:" );
 	 return -1;
    }
    buf[HEB__HDL] = 0;

//
// --- Check the label
//
    if ( strncmp ( &buf[0], HEB__LABEL, sizeof(HEB__LABEL)-1 ) != 0 ) {
         strncpy ( (char *)str, &buf[0], sizeof(HEB__LABEL)-1 );
         fprintf ( stderr, "Wrong first line of input file %s: %s while label %s was expected",
		   filin, &str, HEB__LABEL );
         return -2;
    }

    data_offset = HEB__HDL; /* Initialize data offset */
//
// --- Parse the header of the file and create the hashe table (att, par)
//
    k = sizeof(HEB__LABEL); 
    heb->natts = 0;
    for ( i=0; i<HEB__MATT; i++ ){
          is = find_char ( (char *)buf, colon, k, HEB__HDL); /* Find colon */
	  if ( is < 1 ) break;

	  len = MIN ( is-k, HEB__LATT-1 );
          strncpy ( (char *)heb->att_name[heb->natts], &buf[k], len ); /* Extract attribute name */
          heb->att_name[heb->natts][len] = 0;
	  k  = is;
          k  = find_nochar ( (char *)buf, blank, k+1, HEB__HDL); /* Find the first non-blank character */
          is = find_char ( (char *)buf, cr, k, HEB__HDL); /* find carriage return characger */
	  len = MIN ( is-k, HEB__LATT-1 ); 
          strncpy ( (char *)heb->att_val[heb->natts], &buf[k], len ); /* Extract attribute value */
          heb->att_val[heb->natts][len] = 0;
         
          if ( strncmp ( heb->att_name[heb->natts], "Dims", sizeof("Dims")-1 ) == 0 ){
//
// ----------- Decode dimensions
//
	       m = 0 ;
	       for ( j=0; j<4; j++ ){
		    ir = find_char ( heb->att_val[heb->natts], blank, m, HEB__LATT );
		    if ( ir < 0 ) ir = strlen(heb->att_val[heb->natts]);
                    strncpy ( (char *)str, &heb->att_val[heb->natts][m], ir-m );
	            str[ir-m] = 0;
		    heb->dims[j] = atoll(str) ;
	            m = ir;
	            m = find_nochar ( heb->att_val[heb->natts], blank, m, HEB__LATT );
              }
          }
          // printf ( "==> %s: || %s\n", (char *)heb->att_name[heb->natts], (char *)heb->att_val[heb->natts] ); /* %%%%%% */
//
// ------ Decode data format and determine the element length in bytes 
//
	  if ( strncmp ( heb->att_name[heb->natts], "Data_Format", sizeof("Data_Format")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], blank, 0, HEB__LATT );
	       strncpy ( (char *)data_format, &heb->att_val[heb->natts][0], ir );
               data_format[ir] = 0;
	       if ( strncmp (      data_format, "I1", sizeof("I1")-1 ) == 0 )
		    elem_len = 1;
	       else if ( strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0 )
		    elem_len = 2;
	       else if ( strncmp ( data_format, "I4", sizeof("I4")-1 ) == 0 )
		    elem_len = 4;
	       else if ( strncmp ( data_format, "R4", sizeof("R4")-1 ) == 0 )
		    elem_len = 4;
	       else if ( strncmp ( data_format, "I8", sizeof("R8")-1 ) == 0 )
		    elem_len = 8;
	       else if ( strncmp ( data_format, "R8", sizeof("I8")-1 ) == 0 )
		    elem_len = 8;
	  }
//
// ------- Decode fill Value
//
	  if ( strncmp ( heb->att_name[heb->natts], "Fill_Value", sizeof("Fill_Value")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       strncpy ( (char *) str, &heb->att_val[heb->natts][0], ir );
               str[ir] = 0;
	       if ( strncmp (      out_type, "I1", sizeof("I1")-1 ) == 0 )
		 heb->i1_fill_value = (char )atoi ( str );
	       else if ( strncmp ( out_type, "I2", sizeof("I2")-1 ) == 0 )
                    heb->i2_fill_value = atoi ( str );
	       else if ( strncmp ( out_type, "I4", sizeof("I4")-1 ) == 0 )
                    heb->i4_fill_value = atol ( str );
	       else if ( strncmp ( out_type, "R4", sizeof("R4")-1 ) == 0 )
                    heb->r4_fill_value = strtof ( str, NULL );
	       else if ( strncmp ( out_type, "I8", sizeof("R8")-1 ) == 0 )
                    heb->r8_fill_value = strtod ( str, NULL );
	       else if ( strncmp ( out_type, "R8", sizeof("I8")-1 ) == 0 )
                    heb->i8_fill_value = atoll ( str );
	  }
//
// ------ Decode data length in bytes
//
	  if ( strncmp ( heb->att_name[heb->natts], "Data_Length", sizeof("Data_Length")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       strncpy ( (char *) str, &heb->att_val[heb->natts][0], ir );
               str[ir] = 0;
	       data_len = atoll(str) ;
	  }
//
// ------ Decode offset of the data section in bytes
//
	  if ( strncmp ( heb->att_name[heb->natts], "Data_Offset", sizeof("Data_Offset")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       strncpy ( (char *) str, &heb->att_val[heb->natts][0], ir );
               str[ir] = 0;
	       data_offset = atoi(str) ;
	  }
//
// ------ Decode the data transformation code
//
	  if ( strncmp ( heb->att_name[heb->natts], "Data_Transform", sizeof("Data_Transform")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       strncpy ( (char *) data_transform, &heb->att_val[heb->natts][0], ir );
               data_transform[ir] = 0;
	  }
//
// ------ Decode offset that is to be added to the value
//
	  if ( strncmp ( heb->att_name[heb->natts], "Offset", sizeof("Offset")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       if ( ir > 0 ){
	            strncpy ( (char *)str, &heb->att_val[heb->natts][0], ir );
                    str[ir] = 0;
                    if ( strncmp ( out_type, "R4", sizeof("R4")-1 ) == 0 )
	                 offset_r4 = strtof ( str, NULL ) ;
                    else if ( strncmp ( out_type, "R8", sizeof("R8")-1 ) == 0 )
	                 offset_r8 = strtod ( str, NULL ) ;
               }
	  }
//
// ------ Decode the scale factor that is to be multiplied by the value
//
	  if ( strncmp ( heb->att_name[heb->natts], "Scale_Factor", sizeof("Scale_Factor")-1 ) == 0 ){
	       ir = find_char ( heb->att_val[heb->natts], 0, 0, HEB__LATT );
	       if ( ir > 0 ){
	            strncpy ( (char *)str, &heb->att_val[heb->natts][0], ir );
                    str[ir] = 0;
                    if ( strncmp ( out_type, "R4", sizeof("R4")-1 ) == 0 )
	                 scale_r4 = strtof ( str, NULL ) ;
                    else if ( strncmp ( out_type, "R8", sizeof("R8")-1 ) == 0 )
	                 scale_r8 = strtod ( str, NULL ) ;
               }
	  }
          // printf ( "NAM: %s\n", heb->att_name[heb->natts] ) ; /* %%%%%%%%%%%%%% */

          heb->natts = heb->natts + 1;
          k = is + 1;
	  if ( is > HEB__HDL ) break;
    }
    if ( strncmp ( data_transform, "none", sizeof("none")-1 ) == 0 ){
         if ( scale_r4 != 0.0 || offset_r4 != 0.0 )
              strcpy ( data_transform, "scof" ) ;
    }

    if ( data_offset != HEB__HDL ){
//
// ----- Position the file pointer to the beginning of the data section
//
         is = lseek ( fd, data_offset, SEEK_SET );
         if ( is < 0 ){
              perror ( "heb_read lseek: " );
	      return -1;
         }
    }
//
// --- Allocate memory for the output pointer
//
    if ( strncmp (      out_type, "I1", sizeof("I1")-1 ) == 0 ){
         heb->i1_ptr = malloc ( heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
         if ( heb->i1_ptr == NULL ){
              perror ( "@1 heb_read malloc" );
              return -1;
	 } }
    else if ( strncmp ( out_type, "I2", sizeof("I2")-1 ) == 0 ){
         heb->i2_ptr = malloc ( heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
         if ( heb->i2_ptr == NULL ){
              perror ( "@2 heb_read malloc" );
              return -1;
	 } }
    else if ( strncmp ( out_type, "I4", sizeof("I4")-1 ) == 0 ){
         heb->i4_ptr = malloc ( 4*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
         if ( heb->i4_ptr == NULL ){
              perror ( "@3 heb_read malloc" );
              return -1;
	 } }
    else if ( strncmp ( out_type, "I8", sizeof("I8")-1 ) == 0 ){ 
         heb->i8_ptr = malloc ( 8*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
         if ( heb->i8_ptr == NULL ){
              perror ( "@4 heb_read malloc" );
              return -1;
	 } }
    else if ( strncmp ( out_type, "R4", sizeof("R4")-1 ) == 0 ){
         heb->r4_ptr = malloc ( 4*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
         if ( heb->r4_ptr == NULL ){
              perror ( "@5 heb_read malloc" );
              return -1;
	 } }
    else if ( strncmp ( out_type, "R8", sizeof("I1")-1 ) == 0 ){
        heb->r8_ptr = malloc ( 8*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] );
        if ( heb->r8_ptr == NULL ){
              perror ( "@6 heb_read malloc: " );
              return -1;
	} }
//
// --- Determine the data chunk size and allocate temopary pointer for a chunk if needed
//
    if ( strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0 ){
         data_len = heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] ;
         data_chunk = data_len ;
         if ( data_chunk > HEB__LEN_MAX ) data_chunk = HEB__LEN_MAX;
         if ( strncmp ( out_type, "I1", sizeof("I1")-1 ) != 0 )
              i1_ptr = malloc ( data_chunk ); /* the output data type is not the same as input data type */
         }
       else if ( strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0 ){
         data_len = 2*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] ;
         data_chunk = data_len;
         if ( data_chunk > HEB__LEN_MAX ) data_chunk = HEB__LEN_MAX;
         if ( strncmp ( out_type, "I2", sizeof("I2")-1 ) != 0 )
              i2_ptr = malloc ( data_chunk ) ;
         }
       else if ( strncmp ( data_format, "R4", sizeof("I4")-1 ) == 0 ){
         data_len = 4*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] ;
         data_chunk = data_len;
         if ( data_chunk > HEB__LEN_MAX ) data_chunk = HEB__LEN_MAX;
         }
       else if ( strncmp ( data_format, "R8", sizeof("R8")-1 ) == 0 ){
         data_len = 8*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] ;
         data_chunk = data_len;
         if ( data_chunk > HEB__LEN_MAX ) data_chunk = HEB__LEN_MAX;
         }
       else if ( strncmp ( data_format, "I8", sizeof("I8")-1 ) == 0 ){
         data_len = 8*heb->dims[0]*heb->dims[1]*heb->dims[2]*heb->dims[3] ;
         data_chunk = data_len;
         if ( data_chunk > HEB__LEN_MAX ) data_chunk = HEB__LEN_MAX;
    }
//
// --- Now read the data. If the data length is less than the size of a chunk,
// --- we read the data in one operation. Otherewise we read data chunk by chunk
//
    got_bytes = 0;
    ind = 0;
    do {
       if ( strncmp ( out_type, "I1", sizeof("I1")-1 ) == 0 ) {
            if ( strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->i1_ptr[got_bytes/elem_len]), data_chunk ) ;
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } }
         else if ( strncmp ( out_type, "I2", sizeof("I2")-1 ) == 0 ){
            if ( strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->i2_ptr[got_bytes/elem_len]), data_chunk );
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } }
         else if ( strncmp ( out_type, "I4", sizeof("I4")-1 ) == 0 ){
            if ( strncmp ( data_format, "I4", sizeof("I4")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->i4_ptr[got_bytes/elem_len]), data_chunk );
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } }
         else if ( strncmp ( out_type, "I8", sizeof("I8")-1 ) == 0 ){
            if ( strncmp ( data_format, "I8", sizeof("I8")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->i8_ptr[got_bytes/elem_len]), data_chunk );
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } }
         else if ( strncmp ( out_type, "R4", sizeof("R4")-1 ) == 0 ){
//
// --------- If the output data type is R4, we accept input data as I1 or I2.
// --------- Then we tranform them to R4 type
//
            if ( strncmp ( data_format, "R4", sizeof("R4")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->r4_ptr[got_bytes/elem_len]), data_chunk );
            else if ( strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0 )
	         is = read ( fd, i2_ptr, data_chunk );
            else if ( strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0 )
	         is = read ( fd, i1_ptr, data_chunk );
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } }
         else if ( strncmp ( out_type, "R8", sizeof("R8")-1 ) == 0 ){
//
// --------- If the output data type is R8, we accept input data as I1 or I2.
// --------- Then we tranform them to R8 type
//
            if ( strncmp ( data_format, "R8", sizeof("R8")-1 ) == 0 )
	         is = read ( fd, (float *)&(heb->r8_ptr[got_bytes/elem_len]), data_chunk );
            else if ( strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0 )
	         is = read ( fd, i2_ptr, data_chunk );
            else if ( strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0 )
	         is = read ( fd, i1_ptr, data_chunk );
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported\n", 
                                      data_format, out_type );
                   return -1;
            } 
       }
       // printf ( "read is= %d  data_chunk: %lld data_len: %lld \n", is, data_chunk, data_len ) ; /* %%%%%%%%%%%%%%%%% */
       if ( is < 1 ){
	    perror ( "heb_read read_data" );
 	    return -1;
       }
//
// --- Peform data transform if needed and if supported
//
       if ( strncmp ( data_transform, "scof", sizeof("scof")-1 ) == 0 ){
//
//--------- Scale and offset
//
	    nel = is/elem_len ; /* the number of elelmets in the retrieved data chunk */
            if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "R4", sizeof("R4")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = offset_r4 + scale_r4*heb->r4_ptr[ind];
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = offset_r4 + scale_r4*heb->i1_ptr[i];
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = offset_r4 + scale_r4*i2_ptr[i];
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R8")-1 ) == 0 && 
                 strncmp ( data_format, "R8", sizeof("R8")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = offset_r4 + scale_r4*heb->r8_ptr[ind];
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R8")-1 ) == 0 && 
                 strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = offset_r4 + scale_r4*heb->i1_ptr[i];
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = offset_r4 + scale_r4*heb->i2_ptr[i];
                       ind++ ;
                 } }
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported for scale/offset transformation\n", 
                                      data_format, out_type );
                   return -1;
            } 
       }         
       if ( strncmp ( data_transform, "log", sizeof("log")-1 ) == 0 ){
//
//--------- Logariphimcval Scale and offset
//
	    nel = is/elem_len ; /* the number of elelmets in the retrieved data chunk */
            if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "R4", sizeof("R4")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = expf(offset_r4 + scale_r4*(float )heb->r4_ptr[ind]);
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = expf(offset_r4 + scale_r4*heb->i1_ptr[i]);
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R4", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r4_ptr[ind] = expf(offset_r4 + scale_r4*i2_ptr[i]);
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R8")-1 ) == 0 && 
                 strncmp ( data_format, "R8", sizeof("R8")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = expf(offset_r4 + scale_r4*heb->r8_ptr[ind]);
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R8")-1 ) == 0 && 
                 strncmp ( data_format, "I1", sizeof("I1")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = exp(offset_r4 + scale_r4*heb->i1_ptr[i]);
                       ind++ ;
                 } }
            else if ( strncmp ( out_type,    "R8", sizeof("R4")-1 ) == 0 && 
                 strncmp ( data_format, "I2", sizeof("I2")-1 ) == 0   ){
	         for ( i=0; i<nel; i++ ){
	               heb->r8_ptr[ind] = exp(offset_r4 + scale_r4*heb->i2_ptr[i]);
                       ind++ ;
                 } }
            else { fprintf ( stderr, "Combination of internal data format %s and external data format %s is not supported for logariphmic scale/offset transformation\n", 
                                      data_format, out_type );
                   return -1;
            } 
       }         

       got_bytes = got_bytes + is ; /* increment the number of bytes read */

    } while ( got_bytes < data_len );

    close ( fd ) ;
//
// --- Collect garbage
//
    if ( i1_ptr != NULL ) free (i1_ptr) ;
    if ( i2_ptr != NULL ) free (i2_ptr) ;
    if ( i4_ptr != NULL ) free (i4_ptr) ;
    if ( r4_ptr != NULL ) free (r4_ptr) ;
    if ( r8_ptr != NULL ) free (r8_ptr) ;

    return 1;
}

void heb_free ( struct heb_struct  *heb, char * out_type ){
// ************************************************************************
// *                                                                      *
// *   Routine heb_free frees dynamic memory acquired by previous calls   *
// *   of heb_read and initializes fields of heb structure.               *
// *                                                                      *
// *  ### 28-FEB-2013    heb_read   v1.0 (c)  L. Petrov  28-FEB-2013 ###  *
// *                                                                      *
// ************************************************************************
  bzero ( heb->att_name,    sizeof(heb->att_name)    ) ;
  bzero ( heb->att_val,     sizeof(heb->att_val)     ) ;
  bzero ( heb->dims,        sizeof(heb->dims)        ) ;
  heb->i1_fill_value = 0 ;
  heb->i2_fill_value = 0 ;
  heb->i4_fill_value = 0 ;
  heb->i8_fill_value = 0 ;
  heb->r4_fill_value = 0.0 ;
  heb->r8_fill_value = 0.0 ;
  heb->natts = 0 ;
  if ( strncmp ( out_type, "I1", sizeof("I1") ) == 0 )
       if ( heb->i1_ptr != NULL ) free ( heb->i1_ptr ) ;
     else if ( strncmp ( out_type, "I2", sizeof("I2") ) == 0 )
       if ( heb->i2_ptr != NULL ) free ( heb->i2_ptr ) ;
     else if ( strncmp ( out_type, "I4", sizeof("I4") ) == 0 )
       if ( heb->i4_ptr != NULL ) free ( heb->i4_ptr ) ;
     else if ( strncmp ( out_type, "R4", sizeof("R4") ) == 0 )
       if ( heb->i4_ptr != NULL ) free ( heb->r4_ptr ) ;
     else if ( strncmp ( out_type, "I8", sizeof("I8") ) == 0 )
       if ( heb->i8_ptr != NULL ) free ( heb->i8_ptr ) ;
     else if ( strncmp ( out_type, "R8", sizeof("R8") ) == 0 )
       if ( heb->r8_ptr != NULL ) free ( heb->r8_ptr ) ;
}

int find_char ( char string[], char chr, int offset, int len ){
//
// --- Aixilliary routine find_char returns the index of the first occurence of 
// --- character chr in a character array string of length len with respect to the specified offset
//
    int i ;
    for ( i=offset; i<len; i++ ){
          if ( string[i] == chr ){
	       return i;
          }
    }
    return -1;     
}

int find_nochar ( char string[], char chr, int offset, int len ){
//
// --- Aixilliary routine nofind_char returns the index of the first character that
// --- is not chr in a character array sting of length len with respect to the specified offset
//
    int i ;
    for ( i=offset; i<len; i++ ){
	  if ( string[i] != chr )
	       return i;
    }
    return -1;     
}
