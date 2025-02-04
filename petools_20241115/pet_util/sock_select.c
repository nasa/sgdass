#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netdb.h>
#include <errno.h>

int sock_select ( int *sock_fd, double *timout, char *message[], 
                  unsigned int len_message  )
{

  int num_fd, res;
  fd_set read_fd;
  struct timeval tim;
  char   buf[128];
  char  *ret;
   
  tim.tv_sec  = (long)   timout;
  tim.tv_usec = (long) (*timout - tim.tv_sec);
  
  num_fd = *sock_fd + 1;
  FD_ZERO ( &read_fd );
  FD_SET  ( *sock_fd, &read_fd ) ;

  res = select ( num_fd, &read_fd, NULL, NULL, &tim );
  if ( res < 0 ){
       strncpy ( (char *)&(*message), "select: ", sizeof("select: ")+1  ); 
       ret = (char *)strerror_r ( errno, buf, sizeof(buf)-1 );
       if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
       if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
       return res;
  }
  if ( num_fd == 0 ){
       strncpy ( (char *)&(*message), "select: time out has expired", 
                               sizeof("select: time out has expired")+1  ); 
       return ETIME;
  }
  
  if ( ! FD_ISSET(*sock_fd, &read_fd) ){
       strncpy ( (char *)&(*message), "select: Time out has expired", 
                               sizeof("select: Time out has expired")+1  ); 
       return ETIME;
  }

}
