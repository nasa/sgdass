#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <termios.h>
#include <sys/ioctl.h>

int save_term(term)

struct termios *term;

#ifdef DARWIN
   #define MY_GETA TIOCGETA
   #define MY_SETA TIOCSETA
#else 
   #define MY_GETA TCGETA
   #define MY_SETA TCSETA
#endif

{
   ioctl(0,MY_GETA,term);
   return(0);
}
long curlib_set_term(path)

char **path;
{
   FILE *fp;
   struct termios term;
   int isat;

   fp = fopen(*path,"r");
   if ( fp == 0 ) 
        { 
          perror ( "curlib_set_term" );
          printf ( "curlib_set_term: Error in opening file %s \n", path) ;
          hit_cont (0, 0 );
          return(1);
        }

   fread((char *) &term, sizeof (struct termios), 1, fp);
   ioctl(0,MY_SETA,&term);

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
   fwrite((char *) term, sizeof (struct termios), 1, fp);
   fclose(fp);
   return(0);
}
