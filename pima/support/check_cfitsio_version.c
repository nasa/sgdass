#include <stdlib.h>
#include <stdio.h>
#include "fftw3.h"
main()
{
   float    version;
   ffvers ( &version) ;  /* IO - version number */
   printf ( "cfitsion version: %.2f\n", version );
   exit ( 0 );
}
