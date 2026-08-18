/*
	Ftseopenx is identical to ftseopen except that the first argument
	is passed in as a pointer to a character string, rather than as
	a string itself.  (MWH, 920507)

	Ftseopen provides a means for Fortran programs to call the open(2)
	system call.  Notice that type and mode are shorts! (hence the >s<)
        Will only work on existing files (hence the 'e') (i.e., will not
        create non-existing files.)

	fdesc = ftseopen(fpath,type,mode)

	where:
		fdesc  - return value; if >0, it is the file descriptor
			 if <0 an error has occurred.
                         (specifically, 
                            -1 for general error on second, real open of file,
                               once its existence has been verified
                            -2 if file does not exist
                            -3 for general error on initial open of file
                               to check its existence
                            -4 for general error on close to set up for real
                               open)
		fpath  - UNIX file pathname
		type   - 0 for read; 1 for write; 2 for both
		mode   - mode bits for file creation; 0 means default

	Written by KDB, based on ftopen and ftsopen by EJH and LEF.
        Exactly like ftsopen if the file exists.  If the file doesn't
         exist, ftseopen will return with an error, unlike ftsopen,
         which will create the file.
*/
#include <fcntl.h>
#include <errno.h>
#ifdef _NEEDED
int ftseopenx_(fpath,type,mode)
#else
int ftseopenx(fpath,type,mode)
#endif
    char **fpath;	/* mode is mode bits for file creation */
    short *type, *mode; /* type=0,1,2 for read, write, and read+write resp. */
{
    void perror();
    int oflag, fd, icl; /* open(), close(), removed by L. Petrov 2003.11.17 */
/*  Check to see if file exists. If not, return with an error. */
    fd = open(*fpath,O_RDONLY);
    if (fd < 0)
      { if (errno==ENOENT) return(-2);
        perror("ftseopen");
        return(-3); }
    icl = close(fd);
    if (icl < 0) 
      { perror("ftseopen");
        return(-4); }
/*  The file does exist, so proceed with normal processing */
    if (*type==0) oflag = O_RDONLY;
    else if( *type==1) oflag = O_WRONLY;
    else oflag = O_RDWR;
    oflag |= O_CREAT;
    if (*mode==0) fd = open(*fpath,oflag);
    else fd = open(*fpath,oflag,(int) *mode);
    if( fd < 0 ) perror("ftseopen");
    return(fd);
}
