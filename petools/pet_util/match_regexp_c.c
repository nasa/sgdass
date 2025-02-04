#ifdef LINUX
   #include <sys/types.h>
#endif
#include <regex.h>
#include <string.h>
/****************************************************************************
*                                                                           *
*  Routine match_regexp_c  is an intermediary interface routine between     *
*  system subroutines regcomp, regexec, regerror and high level of          *
*  interface.                                                               *
*                                                                           *
*  ###  12-JUL-1999   match_regexp_c  v1.2  (c) L. Petrov 21-JUL-2002  ###  *
*                                                                           *
****************************************************************************/

#ifdef _NEEDED
       long int match_regexp_c_ ( char *string, char *pattern, char *message )
#else
       long int match_regexp_c  ( char *string, char *pattern, char *message )
#endif

{

  long int i;
  regex_t re;
  char buf[256];
  i = regcomp ( &re, pattern, REG_NOSUB );
 
  if ( i != 0 ) {
/*
* --------------- Error during compilation of regular expression
*/
                  (void)regerror ( i, &re, buf, sizeof buf);
		  strcpy ( message, buf );
                  return(0);
                }
  i = regexec ( &re, string, (size_t) 0, NULL, 0 );
  regfree ( &re );
  if ( i != 0 ) {
/*
* --------------- Error during matching regular expression
*/
                  (void)regerror ( i, &re, buf, sizeof buf);
		  strcpy ( message, buf );
		  if ( i == REG_NOMATCH ) i = -1;
                  return(i);
                }
  return(0);
}
