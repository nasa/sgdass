#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <fcntl.h>
#include <netdb.h>
#include <errno.h>

int sock_write ( int *sock_fd, int *len_buf, char buf[], 
                 char *message[], unsigned int len_message )
{
  int    flags;
  struct timeval tim;
  long   bytes_sent, total_bytes_sent, rem_bytes;
  char   mes[128];
  char  *ret;
  
  total_bytes_sent = 0;
  rem_bytes = *len_buf; 
//
//--- Clear the message string
//
  clrch ( (char *)&(*message), len_message ) ;

//
//--- Set the channel in the blocking mode
//
  if ( (flags = fcntl( *sock_fd, F_GETFL, 0) ) < 0 ) {
       ret = strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_write -- error in getting socket flags",
                 len_message );
       if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
       if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
       return -1;
  }

  if ( fcntl ( *sock_fd, F_SETFL, flags & (~O_NONBLOCK)) < 0 ) {
       ret = strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_write -- error in setting socket in the blocking mode",
                 len_message );
       if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
       if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
       return -1;
  }

  while ( total_bytes_sent < *len_buf ) {
    bytes_sent = send ( *sock_fd, buf, rem_bytes, 0);
    if ( bytes_sent == 0 ){
         strncpy ( (char *)&(*message), "send: remote host dropped connection", 
                   len_message-1 ); 
         return -1;
    }
    if ( bytes_sent == -1 ){
         strncpy ( (char *)&(*message), "send: ", len_message-1   ); 
         ret = strerror_r ( errno, mes, sizeof(mes)-1 );
         if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
         if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
         return -1;
    }
    total_bytes_sent = total_bytes_sent + bytes_sent;
    rem_bytes = rem_bytes - bytes_sent;

  }
  return total_bytes_sent;
}
