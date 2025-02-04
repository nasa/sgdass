#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/time.h>
#include <poll.h>
#include <errno.h>


/* If your system supports ppoll ( kernel 2.6.16, glibc 2.4 or newer, 
   uncomment the next lin  */

/* #define PPOLL  */

/*
# ************************************************************************
# *                                                                      *
# *   Routine sock_read_poll  reads a buffer with data  buf  of length   *
# *   len_buf  from the file descriptor  rem_fd  associated with         *
# *   a connected socket. The operation may take no longer than          *
# *   timeout  seconds. If the timeout has expired, the error message    *
# *   is generated, and the socket and the associated file descriptor    *
# *   are still available for I/O operations.                            *
# *                                                                      *
# *   Usage:                                                             *
# *                                                                      *
# *     IS = SOCK_READ_POLL ( REM_FD, LEN(BUF), %VAL(LOC(BUF)), &        *
# *   &                       NUM_BYTES, TIMEOUT, MESSAGE )              *
# *     IF ( IS .NE. 0 ) THEN                                            *
# *          WRITE ( 6, * ) 'Error in SOCK_READ_POLL '//                 *
# *   &                      MESSAGE(1:I_LEN(MESSAGE))                   *
# *     END IF                                                           *
# *                                                                      *
# * _________________________ Input parameters: ________________________ *
# *                                                                      *
# *    REM_FD ( INTEGER*4 ) -- File descriptor of the channel connected  *
# *                            to a socket.                              *
# *   LEN_BUF ( INTEGER*4 ) -- Length of the I/O buffer                  *
# *       BUF ( INTEGER*1 ) -- Buffer to be read from the channel        *
# * NUM_BYTES ( INTEGER*4 ) -- The number of bytes to read. Operation    *
# *                            is not considered completed, unless all   *
# *                            bytes are received.                       *
# *  TIMEOUT ( REAL*8     ) -- Timeout in seconds.                       *
# *  MESSAGE ( CHARACTER  ) -- Error message. It is all blank, if        *
# *                            the operation has completed               *
# *                            successfully.                             *
# *                                                                      *
# * _________________________ Output parameters: _______________________ *
# *                                                                      *
# *                                                                      *
# * <SOCK_READ_POLL> ( INTEGER*4 ) -- The number of received bytes.      *
# *                                                                      *
# * Caveat: timeout is set for each I/O operation. The data may require  *
# *         more than one I/O operation. If between I/O operation        *
# *         a delay will happen, the total amount of time which          *
# *         sock_read_poll will require to complete may exceed timeout   *
# *         amount of time.                                              *
# *                                                                      *
# *  ### 08-OCT-2009 sock_read_poll v1.0 (c)  L. Petrov 08-OCT-2009 ###  *
# *                                                                      *
# ************************************************************************
*/

int sock_read_poll ( int *rem_fd, int *len_buf, char *buf, 
                     int *len_exp, double *timeout, 
                     char *message[], unsigned int len_message  )
{

  int    flags;
  fd_set read_fd;
  struct timespec tim_req, tim_rem;
  struct pollfd   poll_fd;
  char mes[128];
  int res, ret, i_wait;
  double tim_inc, tim_waited;
  long received_bytes, rec_len;
  long len_remained;
  
//
//--- Clear the message string
//
  clrch ( (char *)&(*message), len_message ) ;

//
//--- Check whether the buffer is long enough
//
  if ( *len_exp > *len_buf ){
       strncpy ( (char *)&(*message), "ERROR: sock_read_poll -- the expected number of bytes is greated than the buffer length",
                 len_message );
       return -1;
  }
//
//--- Set timeout
//
  tim_req.tv_sec  = *timeout ;
  tim_req.tv_nsec = (*timeout - tim_req.tv_sec)*1000000000 ;

//
//--- Set polling parameters
//
  poll_fd.fd = *rem_fd ;
  poll_fd.events = POLLIN ;

//
//--- Set the channel in the non-blocking mode
//
  if ( (flags = fcntl( *rem_fd, F_GETFL, 0) ) < 0)
  {
       ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_read_poll -- error in getting socket flags",
                 len_message );
       if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
       return -1;
  }

  if ( fcntl(*rem_fd, F_SETFL, flags | O_NONBLOCK) < 0 )
  {
       ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_read_poll -- error in setting socket in the non-blocking mode",
                 len_message );
       if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
       return -1;
  }

  received_bytes = 0 ;
  len_remained = *len_exp ;

//
//--- Reading the data in the cycle. Each operation to read may receive
//--- less bytes than we requested. If this happens, then the operation is
//--- repeated till all the data are read
//
 while ( ( *len_exp >  0 && len_remained    > 0 ) ||
         ( *len_exp <= 0 && received_bytes == 0 )   ){

//
//--- Poll: wait for the channel will be recieve data or timeout, whichever
//--- occur first
//
#ifdef PPOLL
    ret = ppoll ( &poll_fd, 1, &tim_req, NULL ) ;
#else
    ret = poll ( &poll_fd, 1, (int) (*timeout)*1000.0 ) ; 
#endif
    if ( ret == 0 ){
         strncpy ( (char *)&(*message), "Time out has expired", 
                   len_message-1 ); 
         return -1;
    } else if ( ret < 0 ){
         printf ( "errno: %d \n", errno ) ;
         strncpy ( (char *)&(*message), "poll: ", len_message-1   ); 
         ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
         if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
         return -1;
    }

//
//--- Read the data
//
    rec_len = recv( *rem_fd, (char *)buf+received_bytes, *len_exp, 0);
    if ( rec_len == 0 ){
         strncpy ( (char *)&(*message), "recv: client closed connection", 
                   len_message-1 ); 
         return -1;
    }

    if ( rec_len < 0 ){
         printf ( "errno: %d \n", errno ) ;
         strncpy ( (char *)&(*message), "recv: ", len_message-1   ); 
         ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
         if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
         return -1;
    }
    
//
//--- Augment the counter of received bytes and the remaining bytes to be read
//
    received_bytes = received_bytes + rec_len;
    len_remained = len_remained - rec_len;
  }

//
//--- Set the channel back in the blocking mode
//
  if ( (flags = fcntl( *rem_fd, F_GETFL, 0) ) < 0 ){
       ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_read_poll -- error in getting socket flags",
                 len_message );
       if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
       return -1;
  }

  if ( fcntl ( *rem_fd, F_SETFL, flags & (~O_NONBLOCK)) < 0 ){
       ret = (int) strerror_r ( errno, mes, sizeof(mes)-1 );
       strncpy ( (char *)&(*message), "ERROR: sock_read_poll -- error in setting socket in the blocking mode",
                 len_message );
       if ( ret == 0 ) strncat ( (char *)message, mes, len_message-1 ) ;
       return -1;
  }

  return received_bytes;
}
