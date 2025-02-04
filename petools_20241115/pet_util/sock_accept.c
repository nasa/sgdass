#include <stdio.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netdb.h>
#include <errno.h>

int sock_accept ( int *sock_fd, int *rem_fd, double *timeout, 
                  char *message[], unsigned int len_message  )
/*
# ************************************************************************
# *                                                                      *
# *   Routine sock_accept accepts incoming communication on socket       *
# *   sock_fd within timeout timeout. It creates file descriptor rem_fd  *
# *   for the remote connection descriptor rem_id with remote host.      *
# *   It retunrs the ip address of the remote host as unsigned integer   *
# *   or -1 in a case of error. In a case of error it returns also       *
# *   message.                                                           *
# *                                                                      *
# * ### 24-MAR-2009 sock_open_server  v1.1 (c) L. Petrov 07-JAN-2015 ### *
# *                                                                      *
# ************************************************************************
*/
{
  int num_fd; 
  fd_set read_fd;
  struct timeval tim;
  struct sockaddr_storage remote_adr;
  char   mes[128];
  int    res, adr_len;
  char  *ret;

  tim.tv_sec  = (time_t)  *timeout;
  tim.tv_usec = (suseconds_t )(*timeout - tim.tv_sec);
//
//--- Clear the message string
//
  clrch ( (char *)&(*message), len_message ) ;
  
  num_fd = *sock_fd + 1;
  FD_ZERO ( &read_fd );
  FD_SET  ( *sock_fd, &read_fd ) ;

  select_restart:
  res = select ( num_fd, &read_fd, NULL, NULL, &tim );
  if ( res == EINTR ){ 
       goto select_restart;
  }
  if ( res < 0 ){
       strncpy ( (char *)&(*message), "select: ", len_message-1 ); 
       ret = strerror_r ( errno, mes, sizeof(mes)-1 );
       if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
       if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
       return -1;
  }
  if ( num_fd == 0 ){
       strncpy ( (char *)&(*message), "select: num_fd == 0 time out has expired", 
       len_message-1 ); 
       return -1;
  }
    
  if ( ! FD_ISSET(*sock_fd, &read_fd) ){
       strncpy ( (char *)&(*message), "select: FD_ISSET Time out has expired", 
       len_message-1 ); 
       return -1;
  }

  adr_len = sizeof(remote_adr);

  *rem_fd = accept( *sock_fd, (struct sockaddr *)&remote_adr, &adr_len );
  if ( rem_fd < 0 ){
       strncpy ( (char *)&(*message), "accept: ", len_message-1 ); 
       ret = strerror_r ( errno, mes, sizeof(mes)-1 );
       if ( ret == NULL ) strncat ( (char *)message, mes, len_message-1 ) ;
       if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
       return -1;
  }
  struct sockaddr_in *sin = (struct sockaddr_in *)&remote_adr;
  unsigned int *ip = (unsigned int *)&sin->sin_addr.s_addr;
  return *ip;
}  
