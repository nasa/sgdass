#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <termios.h>
#include <sys/ioctl.h>

int save_term(term)

#ifndef DARWIN
struct termio *term;
#else
struct termios *term;
#endif
{
#ifndef DARWIN
   ioctl(0,TCGETA,term);
#else
   ioctl(0,TIOCGETA,term);
#endif
   return(0);
}
long curlib_set_term(path)

char **path;
{
   FILE *fp;
#ifndef DARWIN
   struct termio term;
#else
   struct termios term;
#endif
   int isat;

   fp = fopen(*path,"r");
   if ( fp == 0 ) 
        { 
          perror ( "curlib_set_term" );
          printf ( "curlib_set_term: Error in opening file %s \n", path) ;
          hit_cont (0, 0 );
          return(1);
        }
#ifndef DARWIN
   fread((char *) &term, sizeof (struct termio), 1, fp);
   ioctl(0,TCSETA,&term);
#else
   fread((char *) &term, sizeof (struct termios), 1, fp);
   ioctl(0,TIOCSETA,&term);
#endif
   fclose(fp);
   return(0);
}
int w_trm(path,term)

char **path;
struct termio *term;
{
   FILE *fp;

//   printf ( "save_term: siz= %d \n", sizeof(term) ) ; /* %%%%%%%%%%%% */
//   printf ( "save_term: path %s \n", *path ) ; /* %%%%%%%%%%%%% */
   fp = fopen(*path,"w");
   if ( fp == 0 )
        { 
          printf ( "save_term: Cannot open output file %s \n", *path) ;
          exit (1);
        }
#ifndef DARWIN
   fwrite((char *) term, sizeof (struct termio), 1, fp);
#else
   fwrite((char *) term, sizeof (struct termios), 1, fp);
#endif
   fclose(fp);
   return(0);
}
