#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/tcp.h>
#include <netdb.h>
#include <errno.h>

int sock_open_client ( char *server_name, int *port, int *sock_fd, 
                       char *message[], unsigned int len_name,
                       unsigned int len_message )
{
/*
# ************************************************************************
# *                                                                      *
# *   Routine sock_open_client  opens communication wiht a specified     *
# *   server at a specified port. It returns zero in the case of         *
# *   success and non-zero return code and the error message in a case   *
# *   of errors. In a case of success, the IP address of the host is     *
# *   returned in strung message.                                        *
# *                                                                      *
# * ### 23-MAR-2009 sock_open_server  v1.1 (c) L. Petrov 10-JAN-2015 ### *
# *                                                                      *
# ************************************************************************
*/

struct addrinfo hints, *server_info;
int    res;
int    conn_fd;
char   buf[128];
char   port_str[16];
char  *ret;
int    optval;
long   snd_window_size  = 256*1024;
long   recv_window_size = 256*1024;
struct sockaddr_in *addr;
char ipstr[INET_ADDRSTRLEN];

sprintf( port_str, "%d", *port );

memset (&hints, 0, sizeof (hints));
//hints.ai_family   = AF_UNSPEC;    /* use IPv4 or IPv6, whichever  */
hints.ai_family   = AF_INET;        
hints.ai_socktype = SOCK_STREAM;
hints.ai_flags    = AI_PASSIVE;     /* fill in my IP for me  */

res = getaddrinfo( (char *)server_name, (char *)&(*port_str), &hints, &server_info) ;

if ( res != 0 ){
     strncpy ( (char *)&(*message), "getaddrinfo: ", len_message-1 ); 
     ret = strerror_r ( res, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     return -1;
}

// make a socket:

*sock_fd = socket ( server_info->ai_family, server_info->ai_socktype, server_info->ai_protocol);
if ( *sock_fd < 0 ){
     strncpy ( (char *)&(*message), "socket: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

optval = 1;
res = setsockopt ( *sock_fd, SOL_SOCKET, SO_REUSEADDR, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-1: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

optval = 1;
res = setsockopt( *sock_fd, IPPROTO_TCP, TCP_NODELAY, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-2: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

optval = snd_window_size;
res = setsockopt( *sock_fd, SOL_SOCKET, SO_SNDBUF, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-3: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

optval = recv_window_size;
res = setsockopt( *sock_fd, SOL_SOCKET, SO_RCVBUF, &optval, sizeof(optval) );
if ( res < 0 ){
     strncpy ( (char *)&(*message), "setsockopt-3: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

conn_fd = connect ( *sock_fd, server_info->ai_addr, server_info->ai_addrlen );
if ( conn_fd < 0 ){
     strncpy ( (char *)&(*message), "connect: ", len_message-1 ); 
     ret = strerror_r ( errno, buf, sizeof(buf)-1 );
     if ( ret == NULL ) strncat ( (char *)message, buf, len_message-1 ) ;
     if ( ret != NULL ) strncat ( (char *)message, ret, len_message-1 ) ;
     freeaddrinfo ( (struct addrinfo *)server_info );
     return -1;
}

inet_ntop(server_info->ai_family, &((struct sockaddr_in *) server_info->ai_addr)->sin_addr, ipstr, sizeof(ipstr)-1 );
strcpy ( (char *)message, "Connected to "  ) ;
strcat ( (char *)message, (char *)ipstr ) ;

freeaddrinfo ( (struct addrinfo *)server_info );

return 0;
}
