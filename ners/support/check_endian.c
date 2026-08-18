#include <string.h>
#include <stdlib.h>
#include <stdio.h>
/*
* ************************************************************************
* *                                                                      *
* *   Program for detremination whether the processor is BIG_ENDIAN or   *
* *   LITTLE_ENDIAN. It prints in stdour either BIG_ENDIAN or            *
* *   LITTLE_ENDIAN                                                      *
* *                                                                      *
* *  ### 28-OCT-2003 check_endian  v1.1 (c)  L. Petrov  04-MAY-2004 ###  *
* *                                                                      *
* ************************************************************************
*/
int main (int argc, char *const *argv)
{
   short arr_i2[2];
   long  i4;

   arr_i2[0] =  1;
   arr_i2[1] = -1;

   memcpy ( &i4, &arr_i2, 4); 
   if ( i4 == 131071 ) 
        printf ("BIG_ENDIAN\n" );
      else 
        printf ("LITTLE_ENDIAN\n" );
   exit ( 0 ) ;
}
