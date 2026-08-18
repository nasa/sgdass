#include <stdlib.h>
#include <stdio.h>
#include "fitsio.h"
void main()
{
   float    version;
   ffvers ( &version) ;  /* IO - version number */
   printf ( "cfitsio version: %.2f\n", version );
   exit ( 0 );
}
