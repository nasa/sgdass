#include <stdio.h>
#include <stdlib.h>
void main()
{
  int vers_major, vers_minor, vers_patch;
  ATL_buildinfo();
  ilaver( &vers_major, &vers_minor, &vers_patch );
  printf ( "Lapack version: %d.%d.%d \n", vers_major, vers_minor, vers_patch );
  exit ( 0 );
}
