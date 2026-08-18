#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "mk4_data.h"

char progname[] = "size_dfio";
int msglev = 1;

int
size_dfio ( type, version )

int *type, *version;

{
    switch (*type)
      {
      case 0:
        return ( sizeof ( struct type_000 ) ) ;
      case 200:
        return ( sizeof ( struct type_200 ) ) ;
      case 201:
        return ( sizeof ( struct type_201 ) ) ;
      case 202:
        return ( sizeof ( struct type_202 ) ) ;
      case 203:
        return ( sizeof ( struct type_203 ) ) ;
      case 204:
        return ( sizeof ( struct type_204 ) ) ;
      case 205:
        return ( sizeof ( struct type_205 ) ) ;
      case 206:
	switch (*version)
          {
	  case 0:
               return ( sizeof ( struct type_206_v0 ) ) ;
	  case 1:
               return ( sizeof ( struct type_206_v1 ) ) ;
          default:
               return ( -206 );
	  }
      case 207:
        return ( sizeof ( struct type_207 ) ) ;
      case 208:
        return ( sizeof ( struct type_208 ) ) ;
      case 210:
        return ( sizeof ( struct type_210 ) ) ;
      case 212:
        return ( sizeof ( struct type_212 ) ) ;
      case 220:
        return ( sizeof ( struct type_220 ) ) ;
      case 221:
        return ( sizeof ( struct type_221 ) ) ;
      case 230:
        return ( sizeof ( struct type_230 ) ) ;
      default:
        return ( -1 );
      }
}
