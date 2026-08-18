#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netdb.h>
#include <errno.h>

int sock_shutdown ( int *sock_fd )
{
 shutdown( *sock_fd, SHUT_RDWR );
 return 0;
}
