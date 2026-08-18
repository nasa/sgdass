#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/termios.h>

// #define MY_GETA TIOCGETA
// #define MY_SETA TIOCSETA

#ifdef _NEEDED
int itti_ ()
#else
int itti ()
#endif
{
  struct termios save, term;
  char  in[32], outbuf[32];
//  if ( ioctl (0, MY_GETA, &term) == -1 ) {
//       fprintf (stderr, "standard input is not a tty\n");
//       exit(1);
//  }
///*  fflush(1); */
//  save = term;
//  term.c_lflag &= ~ICANON;
//  term.c_lflag &= ~ECHO;
//  term.c_lflag &= ~ECHONL;
//
//  term.c_cc[VMIN]  = 1;
//  term.c_cc[VTIME] = 0;
// 
//  ioctl (0, MY_SETA, &term);
//
//  read  (0, &in, 1);
//
//  ioctl (0, MY_SETA, &save);

    tcgetattr(fileno(stdin), &term);

    term.c_lflag &= ~ICANON;
    term.c_lflag &= ~ECHO;
    tcsetattr(fileno(stdin), 0, &term);

    read  (0, &in, 1);

    term.c_lflag |= ECHO;
    term.c_lflag |= ICANON;
    tcsetattr(fileno(stdin), 0, &term);

    return(in[0]);
}
