/*
	These routines replace the sencr/senkr routines on the A900.  These:
           1) Waits for a key to be entered
	   2) Sends the ASCII BELL character
           3) Sends the sequence that instructs the terminal to reply with
	      its absolute cursor position.
	   4) Reads the returned absolute cursor position
           5) Sends the sequence that instructs the terminal to reply with
	      its relative cursor position.
           6) Reads the returned relative cursor position
	   7) Returns the absolute position in "ix" and "iy", the relative
	      position packed in two integer*2 values packed into the integer*4
	      function value, and the key in the lower/upper byte of "ikey".
Latest version: (2/9/89 lef)
           1) Puts a blank in the unused portion of the returned ikey
	   2) senkr calls sencr (more efficient)
	   3) connects to "/dev/tty" rather than default file descriptors
	      0,1.
	   4) Uses more "#define" statements that SKED seems to require.
Still later version (3/7/89 lef)
	   1) Have sencr return zero in the other character position; while
	      senkr returns a blank in the that position.
*/

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <fcntl.h>
#ifdef LINUX
   #include <sys/ioctl.h>
#endif
#include <termios.h>

#ifdef DARWIN
   #define MY_GETA TIOCGETA
   #define MY_SETA TIOCSETA
#else 
   #define MY_GETA TCGETA
   #define MY_SETA TCSETA
#endif


#define ABS_REQ "\033a"		/* HP terminal dependence  */
#define REL_REQ "\033`"
#define NUM_REPLY 12
static int tty_is_open=0, tty_fd;

#ifdef _NEEDED
float sencr_(ix,iy,ikey) 	/* It is a float for historical reasons */
#else
float sencr(ix,iy,ikey) 	/* It is a float for historical reasons */
#endif
    short *ikey;
    short *ix, *iy;
{
    static char beep[] = "\007";
    char key[2];
    void perror();
    int toupper();
    struct termios tbuf, tbufsave;
    int iret;
    union {
	struct {
	    short x, y;
	} i2regs;
	float r4reg;
    } xy;
				/* open /dev/tty so that sencr will connect
				   to command terminal and not to some pipe */
    if(!tty_is_open) {
	if((tty_fd = open("/dev/tty",O_RDWR)) < 0) {
	    perror("sencr:open");
	    return(-1);
	}
        else tty_is_open = 1;
    }

    if(ioctl(tty_fd,MY_GETA,&tbuf) == -1) {
	perror("ioctl");
	return(-1);
    }

    tbufsave = tbuf;		/* save old terminal parameters  */
    tbuf.c_lflag &= ~(ICANON | ECHO);  /* HP changed the name of this (IECHO) */
    tbuf.c_cc[4] = 1;		/* minimum size */
    tbuf.c_cc[5] = 2;		/* minimum time (tenths of a second) */

    if(ioctl(tty_fd,MY_SETA,&tbuf) == -1) {
	perror("ioctl 2");
	return(-1);
    }

    while((iret = read(tty_fd,key,1)) < 1 ) {
	if(iret == -1) {
	    perror("read");
	    return(-1);
	}
    }
			
    *ikey = toupper((short) key[0]); /* set output, zeroing out other char */

/* We now have a character; reset input and send cursor request */

    write(tty_fd,beep,1);		/* send bell character to confirm */
    tbuf.c_cc[4] = 12;
    tbuf.c_cc[5] = 20;
    if(ioctl(tty_fd,MY_SETA,&tbuf) == -1) {
	perror("ioctl 3");
	return(-1);
    }

    snd_rpl_xy(ABS_REQ,ix,iy);
    snd_rpl_xy(REL_REQ,&(xy.i2regs.x),&(xy.i2regs.y));

    if(ioctl(tty_fd,MY_SETA,&tbufsave) == -1) {
	perror("ioctl 4");
	return(-1);
    }
    return(xy.r4reg);
}

#ifdef _NEEDED
snd_rpl_xy_(string,x,y)
#else
snd_rpl_xy(string,x,y)
#endif
    char *string;
    short *x, *y;
{
    int iret; char ret[NUM_REPLY+1]; char *p_ret;
    unsigned num;
    write(tty_fd,string,2);
    p_ret = ret;
    num = NUM_REPLY;
    while(((iret=read(tty_fd,p_ret,num)) >= 0) && num != 0) {
	num -= iret;
	p_ret += iret;
    }
    ret[6] = '\0';
    ret[10] = '\0';
    *x = atoi(&ret[3]);
    *y = atoi(&ret[7]);
}
#ifdef _NEEDED
float senkr_(ix,iy,ikey) 	/* It is a float for historical reasons */
#else
float senkr(ix,iy,ikey) 	/* It is a float for historical reasons */
#endif
    short *ikey;
    short *ix, *iy;
{
    float ret;
    ret = sencr(ix,iy,ikey);
    *ikey = (*ikey << 8) + ' '; /* Move the returned character to upper byte */
    return(ret);
}
