#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netdb.h>
#include <errno.h>

int sock_read ( int *rem_fd, int *len_buf, char *buf, 
                int *len_exp, double *timeout, char *message[], 
                unsigned int len_message  )
{

  int num_fd;
  fd_set read_fd;
  struct timeval tim;
  char mes[128];
  int res ;
  char *ret;
  long received_bytes, rec_len;
  long len_remained;
  
  if ( *len_exp > *len_buf ){
       strncpy ( (char *)&(*message), "ERROR: sock_read -- the expected number of bytes is greater than the buffer length",
                 len_message );
       return -1;
  }
//
//--- Clear the message string
//
  clrch ( (char *)&(*message), len_message ) ;

  tim.tv_sec  = (time_t)  *timeout;
  tim.tv_usec = (suseconds_t )(*timeout - tim.tv_sec);
  
  num_fd = *rem_fd + 1;
  FD_ZERO ( &read_fd );
  FD_SET  ( *rem_fd, &read_fd ) ;

  received_bytes = 0 ;
  len_remained = *len_exp ;

  while ( ( *len_exp >  0 && len_remained    > 0 ) ||
          ( *len_exp <= 0 && received_bytes == 0 )   ){
    select_restart:
    res = select ( num_fd, &read_fd, NULL, NULL, &tim );
    if ( res == EINTR ) goto select_restart;
    if ( res < 0 ){
         strncpy ( (char *)&(*message), "select: ", len_message-1 ); 
         ret = strerror_r ( errno, mes, sizeof(mes)-1 );
         if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
         return -1;
    }
    if ( num_fd == 0 ){
         strncpy ( (char *)&(*message), "select: time out has expired", 
         len_message-1 ); 
         return -1;
    }
    
    if ( ! FD_ISSET(*rem_fd, &read_fd) ){
         strncpy ( (char *)&(*message), "select: Time out has expired", 
         len_message-1 ); 
         return -1;
    }

    rec_len = recv( *rem_fd, (char *)buf+received_bytes, *len_exp, 0);
    if ( rec_len == 0 ){
         strncpy ( (char *)&(*message), "recv: client closed connection", 
                   len_message-1 ); 
         return -1;
    }

    if ( rec_len < 0 ){
         printf ( "errno:   %d \n", errno ) ;
         printf ( "len_buf  %d \n", *len_buf ) ;
         printf ( "len_exp  %d \n", *len_exp ) ;
         printf ( "buf_adr  %d \n", &buf    ) ;
         printf ( "loc(len) %d \n", &len_buf    ) ;
         strncpy ( (char *)&(*message), "recv: ", len_message-1   ); 
         ret = strerror_r ( errno, mes, sizeof(mes)-1 );
         if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
         if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
         return -1;
    }
    
    received_bytes = received_bytes + rec_len;
    len_remained = len_remained - rec_len;
  }
  return received_bytes;
}
