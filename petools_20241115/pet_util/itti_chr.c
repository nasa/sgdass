#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef DARWIN
   #include <sys/termios.h>
   #define MY_GETA TIOCGETA
   #define MY_SETA TIOCSETA
#else 
   #include <termio.h>
   #define MY_GETA TCGETA
   #define MY_SETA TCSETA
#endif

int itti_chr ( putchr, getchr, putlen, getlen )
/*
# ************************************************************************
# *                                                                      *
# *   Function  itti_chr  prints a line putchr at the screen (without    *
# *   carriage return) and then reads from the terminal the character    *
# *   line getstr in NOECHO mode. The input is terminated when the last  *
# *   symbol is entered. Symbol <Return> is not considered as            *
# *   a terminator.                                                      *
# *                                                                      *
# *   CALL ITTI_CHR ( PUTCHR, GETCHR )                                   *
# *                                                                      *
# *  ###  03-DEC-1999   ITTI_CHR    v1.1 (c)  L. Petrov 01-APR-2014 ###  *
# *                                                                      *
# ************************************************************************
*/
char *putchr;
char *getchr;
int  putlen;
int  getlen;

{
  int i;
#ifdef DARWIN
  struct termios save, term;
#else
  struct termio  save, term;
#endif
  fflush(0);
  if ( ioctl (0, MY_GETA, &term) == -1 ) {
     fprintf (stderr, "ITTI_CHR: standard input is not a tty\n");
     exit(1);
  }
  save = term;
  term.c_lflag &= ~ICANON;
  term.c_lflag &= ~ECHO;
  term.c_lflag &= ~ECHONL;

  term.c_cc[VMIN]  = 1;
  term.c_cc[VTIME] = 0;
 
  ioctl ( 0, MY_SETA, &term );

  write ( 0, putchr, putlen );

  fflush(0);

  i = 0 ;
  while ( i < getlen ) {
         read  ( 0, &getchr[i], 1 );
         i++;
    }

  ioctl ( 0, MY_SETA, &save ); 

  return(strlen(getchr));
}
