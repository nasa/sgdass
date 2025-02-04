#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int find_free_port ( int * port_min, int * port_max )

/*
# ************************************************************************
# *                                                                      *
# *   Routine find_free_port  scans the table of tcp state, available    *
# *   at /proc/net/tcp under Linux and checks which port in the range    *
# *   [port_min, port_max] is not present in this table as a local or    *
# *   remote port. It returns the minimum port in the range              *
# *   [port_min, port_max] which is not present in this table.           *
# *                                                                      *
# *   If all ports are in use, it returns 0. It returns -1, or -2 in     *
# *   the case of system call failures.                                  *
# *                                                                      *
# *   Caveat: This routine is supposed to work only under Linux.         *
# *                                                                      *
# *  ### 2009.05.22 find_free_port v1.2 (c)  L. Petrov   2009.06.12 ###  *
# *                                                                      *
# ************************************************************************
*/

{
  char   tcp_file_name[64] ;
  char   buf[512] ;
  int    i ;
  int    loc_port, rem_port ;
  int    *arr ;
  FILE   *TCP_STAT_FILE ;

  strncpy ( tcp_file_name, "/proc/net/tcp", sizeof(tcp_file_name)-1 ) ;

//
// --- Open the Unis "file" that keeps the current tcp status
//
  TCP_STAT_FILE = fopen ( tcp_file_name, "r" );
  if ( TCP_STAT_FILE == 0 ){
       return -1 ;
  }

//
// --- Allocate temporary array that will keep indexes of used ports
//
  arr = (int * ) malloc ( sizeof(loc_port)*(*port_max - *port_min) ) ;
  if ( &arr == NULL ){
       return -2 ;
  }

  bzero ( arr, sizeof(arr) );  

//
//--- Read the tcp status file, and parse the local address port
//
  while(  fgets ( buf, sizeof(buf)-1, TCP_STAT_FILE ) != NULL ) { 
//@       printf ( "find_free_port: net: %s", buf ) ; /*  %%%%%%%% */
       if ( strstr( buf, "local_address" ) != NULL ) continue ;
//
//---- Check local port
//
       sscanf ( &buf[15], "%4x", &loc_port  ) ;
//@       printf ( "find_free_port: Found local  port in /proc/net/tcp  %d %.4s\n", loc_port, &buf[15]  ) ; /*  %%%%%%%% */
       if ( loc_port >= *port_min  && loc_port <= *port_max ){
//
// --------- Set the status of this local port "used"
//
//@            printf ( "find_free_port: Used local  port in /proc/net/tcp  port %d %.4s \n", loc_port, &buf[15] ) ; /*  %%%%%%%% */
	    arr[loc_port-*port_min] = 1 ;
       }
//
//---- Check remote port
//
       sscanf ( &buf[29], "%4x", &rem_port  ) ;
//@       printf ( "find_free_port: Found remote port in /proc/net/tcp  %d %.4s \n", rem_port, &buf[29]  ) ; /*  %%%%%%%% */
       if ( rem_port >= *port_min  && rem_port <= *port_max ){
//
// --------- Set the status of this remote port "used"
//
//@            printf ( "find_free_port: Used remote port in /proc/net/tcp  port %d %.4s \n", rem_port, &buf[29] ) ; /*  %%%%%%%% */
	    arr[rem_port-*port_min] = 1 ;
       }
  }
  
  for ( i = *port_min; i < *port_max; i++ ){
	if ( arr[i-*port_min] == 0 ){
//
// ---------- This port is free. The mission is accomplished.
//
	     free ( arr );
             return i ;
        }
  }
  free ( arr );
  return 0 ;
}
