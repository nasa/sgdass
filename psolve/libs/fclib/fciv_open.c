#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef _NEEDED
int fciv_open_(path,oflag,mode)
#else
int fciv_open(path,oflag,mode)
#endif
/*  variant of fc_open which does not abend with cryptic error message
    of the form error (2; no such file)
    created kdb 11/2/95       
    pet 2004.08.27  Addeed more verbose error message pringing
*/
 
    char **path;
    int *oflag, *mode;
{
	int opret;
	opret = open ( *path, *oflag, *mode );
	if ( opret == -1 ) {
            perror ( "fciv_open" );
            printf ( "fciv_open: OK oflag: %d, mode: %d \n", *oflag, *mode );
            printf ( "fciv_open: OK error in openning %s \n", *path );
#ifdef _NEEDED
            civerr_( " " );
#else
            civerr( " " );
#endif
	}
	return(opret);
}
