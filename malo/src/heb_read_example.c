#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

#include "heb.h"

main(){
    char filin[128] ;
    char out_type[2] ;
     struct heb_struct *heb = malloc ( sizeof(*heb) ) ;

    strcpy ( filin, "/tmp/spr_20121212_1200.heb" ) ;
    strcpy ( filin, "/tmp/d_20130226_0300.heb" ) ;
    strcpy ( out_type, "R4" ) ;
    int i, is;
    long ind;

    heb->natts = -1 ;
    is = heb_read ( (char *) filin, (struct heb_struct *) heb, (char *) out_type ) ;
    if ( is < 0 ){
         fprintf ( stderr, "Failure to read input file %s\n", filin );
         exit ( 1 );        
    }
    
    for ( i=0; i < heb->natts; i++ ){
          if ( strncmp ( heb->att_name[i], "SDS_name", sizeof("SDS_name") ) == 0 )
	       printf ( "sds_name: %s\n", heb->att_val[i] );
    }
    printf ( "dims: %4d %4d %4d %4d \n", heb->dims[0], heb->dims[1], heb->dims[2], heb->dims[3] );
    printf ( "fill_value: %f\n",      heb->r4_fill_value ) ;
    printf ( "val[1][1]: %f\n",       heb->r4_ptr[1] ) ;
    printf ( "val[1707][341]: %f\n",  heb->r4_ptr[341*4096+1707] ) ;
    printf ( "VAL[1707][341]: %f\n",  heb_2d_r4_elem(heb,341,1707) );
    printf ( "VAL[1][1][1]: %f\n",    heb_3d_r4_elem(heb,1,1,1) );  /* %%%%%%%%%%%%%%%%%%% */
    fflush ( stdout );
    // printf ( "val[2048][4096]: %f\n", heb_2d_r4_elem(heb,341,1707) ) ;
    printf ( "ind_el: %ld \n",                       71*heb->dims[0]*heb->dims[1] + 700*heb->dims[0] + 341  ); /* %%%% */
    printf ( "vaL[341][700][71]: %f\n",  heb->r4_ptr[71*heb->dims[0]*heb->dims[1] + 700*heb->dims[0] + 341] ) ;
    ind = 71*heb->dims[0]*heb->dims[1] + 700*heb->dims[0] + 341 ;
    printf ( "VaL[341][700][71]: %f\n", heb->r4_ptr[ind] ) ;
    printf ( "Va2              : %f\n", heb->r4_ptr[71*1152*721 + 700*1152 + 341] ) ;
    printf ( "Va3              : %f\n", heb->r4_ptr[71*heb->dims[0]*heb->dims[1] + 700*heb->dims[0] + 341] ) ;


//    printf ( "@@ vaL[341][700][71]: %f %ld\n",  heb->r4_ptr[72*heb->dims[0]*heb->dims[1]-1] , 
//             72*heb->dims[0]*heb->dims[1]-1 ) ;

    // printf ( "VAL[341][700][71]: %f\n",  heb_3d_r4_elem(heb,341,700,71) );
    printf ( "ELEL %f\n", heb->r4_ptr[59778773] );
    heb_free ( heb, (char *) out_type ) ;
}
