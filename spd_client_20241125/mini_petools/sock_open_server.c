#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <errno.h>

int sock_open_server ( int *port, int *sock_fd, char *message[], 
                       unsigned int len_message  )
{
/*
# ************************************************************************
# *                                                                      *
# *   Routine sock_open_server  opens communication at a specified port  *
# *   through sockets as a server. It returns zero in the case of        *
# *   success and non-zero return code and the error message in a case   *
# *   of errors.                                                         *
# *                                                                      *
# * ### 23-MAR-2009 sock_open_server  v1.2 (c) L. Petrov 10-JAN-2015 ### *
# *                                                                      *
# ************************************************************************
*/

int res;
struct addrinfo hints, *adr_res;
char   buf[128];
char   port_str[16];
int    backlog = 16;
int    optval;
long   snd_window_size  = 1024*1024; /* size of the send buffer */
long   recv_window_size = 1024*1024; /* size of the recv buffer */
char  *ret;

*message = 0;
sprintf( port_str, "%d", *port );

memset (&hints, 0, sizeof (hints));
hints.ai_family   = AF_UNSPEC;    /* use IPv4 or IPv6, whichever  */
hints.ai_socktype = SOCK_STREAM;
hints.ai_flags    = AI_PASSIVE ;

res = getaddrinfo( NULL, port_str, &hints, &adr_res) ;

if ( res != 0 ){
     strncpy ( (char *)&(*message), "getaddrinfo: ", sizeof("getaddrinfo: ")+1  ); 
     ret = strerror_r ( res, (char *)&(*buf), sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     return res;
}

// make a socket:

*sock_fd = socket ( adr_res->ai_family, adr_res->ai_socktype, adr_res->ai_protocol);
if ( *sock_fd < 0 ){
     strncpy ( (char *)&(*message), "socket: ", len_message-1 ); 
     ret = strerror_r ( *sock_fd, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return *sock_fd;
}

/*
optval = 1;
res = setsockopt ( *sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-1: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return -1;
}
*/

optval = 1;
res = setsockopt( *sock_fd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-2: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return -1;
}

optval = snd_window_size;
res = setsockopt( *sock_fd, SOL_SOCKET, SO_SNDBUF, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-3: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return -1;
}

optval = recv_window_size;
res = setsockopt( *sock_fd, SOL_SOCKET, SO_RCVBUF, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-3: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return -1;
}

// bind it to the port we passed in to getaddrinfo():

res = bind( *sock_fd, adr_res->ai_addr, adr_res->ai_addrlen);
if ( res < 0 ){
     strncpy ( (char *)&(*message), "bind: ", len_message-1 ); 
     ret = strerror_r ( errno, (char *)&buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return res;
}

res = listen( *sock_fd, backlog );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "listen: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( adr_res );
     return res;
}

freeaddrinfo ( adr_res );
return 0;
}
