#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef DARWIN
   #include <sys/termios.h>
#else
   #include <termio.h>
#endif

#ifdef _NEEDED
int itti_ ()
#else
int itti ()
#endif
{
#ifdef DARWIN
  struct termios save, term;
#else
  struct termio  save, term;
#endif
  char  in[32], outbuf[32];
  int   nchar;
#ifdef DARWIN
  if ( ioctl (0, TIOCGETA, &term) == -1 ) {
     fprintf (stderr, "standard input is not a tty\n");
     exit(1);
  }
#else
  if ( ioctl (0, TCGETA, &term) == -1 ) {
     fprintf (stderr, "standard input is not a tty\n");
     exit(1);
  }
#endif
/*  fflush(1); */
  save = term;
  term.c_lflag &= ~ICANON;
  term.c_lflag &= ~ECHO;
  term.c_lflag &= ~ECHONL;

  term.c_cc[VMIN]  = 1;
  term.c_cc[VTIME] = 0;
 
#ifdef DARWIN
  ioctl (0, TIOCSETA, &term);
#else
  ioctl (0, TCSETA, &term);
#endif

  read (0, &in, 1);

#ifdef DARWIN
  ioctl (0, TIOCSETA, &save);
#else
  ioctl (0, TCSETA, &save);
#endif

  return(in[0]);
}
