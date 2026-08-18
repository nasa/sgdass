/*
  inkey - return a single character from file descriptor 0 if its tty
          ikey returns 1: one character read in *ikey
                       0: no characters available or not tty
                      -1: error
  NOTE: calling this routine will destroy type-ahead
        so before doing another read you better:

                     short inkey,ikey;
                     while(inkey(&ikey)>0) ;
        or:
                     INTEGER*2 INKEY,IKEY
                     DO WHILE(INKEY(IKEY).GT.0)
                     ENDDO
*/
 
#include <fcntl.h>
#ifdef DARWIN
  short inkey(ikey)
  short *ikey;
  { return 0; }
#else
  #include <sys/ioctl.h>
  #include <termios.h>

#ifdef DARWIN
   #define MY_GETA TIOCGETA
   #define MY_SETA TIOCSETA
#else 
   #define MY_GETA TCGETA
   #define MY_SETA TCSETA
#endif

#ifdef _NEEDED
short inkey_(ikey)
#else
short inkey(ikey)
#endif
    short *ikey;
{
    void perror();
    static struct termios tbuf, tbufsave;
    int iret;
    char c='\0';
    static int tty_fd=0,first=1,istty;

    *ikey=0;
    if(first) {
      first=0;
      istty=isatty(tty_fd);
      if(!istty) return (0);

    if(ioctl(tty_fd,MY_GETA,&tbuf) == -1) {
      perror("ioctl");
      return(-1);
    }

    tbufsave = tbuf;		/* save old terminal parameters  */
    tbuf.c_lflag &= ~ICANON;    /* turn off record processing */
    tbuf.c_cc[4] = 0;		/* minimum size */
    tbuf.c_cc[5] = 0;		/* minimum time (tenths of a second) */

    }
    if(!istty) return (0);
    if(ioctl(tty_fd,MY_SETA,&tbuf) == -1) {
	perror("ioctl 2");
	return(-1);
    }

    if((iret=read(tty_fd,&c,1))==-1) {
      perror("read");
      return(-1);
    }
			
    if(ioctl(tty_fd,MY_SETA,&tbufsave) == -1) {
	perror("ioctl 3");
	return(-1);
    }

    *ikey=c;
    return(iret);
}
#endif
