#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int check_port ( int * port )

/*
# ************************************************************************
# *                                                                      *
# *   Routine check_port  scans the table of tcp state, available        *
# *   at /proc/net/tcp under Linux and checks whether the port is free.  *
# *   If the port is free, i.e. is not used either as a local or as      *
# *   a remote port, then check_port returns 0, otherwise it returns 1.  *
# *                                                                      *
# *   Caveat: This routine is supposed to work only under Linux.         *
# *                                                                      *
# *  ### 2009.05.22   check_port   v1.2 (c)  L. Petrov    2009.07.17 ### *
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
//--- Read the tcp status file, and parse the local address port
//
  while(  fgets ( buf, sizeof(buf)-1, TCP_STAT_FILE ) != NULL ) { 
       if ( strstr( buf, "local_address" ) != NULL ) continue ;
//
//---- Check local port
//
       sscanf ( &buf[15], "%4x", &loc_port  ) ;
       if ( loc_port == *port ){
	    return 1;
       }
//
//---- Check remote port
//
       sscanf ( &buf[29], "%4x", &rem_port  ) ;
       if ( rem_port == *port ){
	    return 1;
       }
  }
  
  return 0 ;
}
