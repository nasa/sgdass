/* C ************************************************************************/
/* C *                                                                      */
/* C *   Routine  get_termsize  gets the actual number of lines and columns */
/* C *   of the screen.                                                     */
/* C *                                                                      */
/* C * ________________________ OUTPUT PARAMETERS _________________________ */
/* C *                                                                      */
/* C *   ILIN (INTEGER*4 ) -- Number of lines of the screen.                */
/* C *   ICOL (INTEGER*4 ) -- Number of columns of the screen.              */
/* C *                                                                      */
/* C *  ###  14-MAR-97    TERM_SIZE   v1.1  (c)  L. Petrov  02-MAR-2003 ### */
/* C *                                                                      */
/* C ************************************************************************/
#ifdef HPUX
   #include <sys/termio.h>
#endif
#ifdef SUN
   #include <sys/termio.h>
#endif
#ifdef LINUX
   #include <asm/termios.h>
#endif
#ifdef DARWIN
   #include <sys/ttycom.h>
#endif


#ifdef _NEEDED
int get_termsize_( lines, columns )
#else
int get_termsize ( lines, columns )
#endif
int *lines, *columns ;

{
     struct winsize ws;
     int *winlen;
     int i4_col, i4_row ;

         ioctl ( 0, TIOCGWINSZ, &ws ) ;

         i4_row = ws.ws_row ;
         i4_col = ws.ws_col ;

         *lines   = i4_row ;
         *columns = i4_col ;

     return 0 ;
}
