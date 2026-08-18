#include <curses.h>
#include <ctype.h>
#include <stdio.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <signal.h>

/*  Alan Fey  2001.03.21  -- fixed the big: the previous version used
                             toupper and didn't make checks whether the
                             argument is in the range [-1, 255] what caused
                             lack of support of arrows on the systems with 
                             certain patches. The new version checks whether
                             the result of getch in in the range and doesn't
                             call toupper if not in range
*/

#define upcase(a) ((a)>-2&&(a)<256 ? toupper(a):(a))
#define F95__TRUE  FORTRAN_TRUE
#define F95__FALSE FORTRAN_TRUE

extern long  is_curlib_on();

start_mn()
{
    static int first = TRUE;

    signal(SIGWINCH, SIG_IGN);

    if (first) {
      initscr();
      nonl();
      cbreak();
      noecho();
      intrflush(stdscr,FALSE);
      keypad(stdscr,TRUE);
      scrollok(stdscr,TRUE);
      idlok(stdscr,TRUE);
      first = FALSE;
    }
    else {
      clear();
      refresh();
    }
/*                              
# --- Set flag of using curlib  
*/
      set_curlib_on();

      return;
}
end_mn()
{
      if ( is_curlib_on() == F95__TRUE ) endwin();
/*                              
# --- Clear flag of using curlib  
*/
      set_curlib_off();
}
nl_mn()
{
      if ( is_curlib_on() == F95__TRUE )
           addch ( '\n' );
        else
	   printf ( "\n" );
	   printf ( "\r" );
}
addstr_mn(str)
char *str;
{
     if ( is_curlib_on() == F95__TRUE )
	   addstr(str);
        else
	   printf ( "%s", str);
     
}
getxy_mn(x,y)
int *y,*x;
{
      if ( is_curlib_on() == F95__TRUE ) getyx(stdscr, *y, *x);
}
int getstr_mn(str)
char *str;
{

     if ( is_curlib_on() == F95__TRUE )
	{
/*                              
# -------- We are in curlib mode
*/
	   nodelay(stdscr,FALSE);
           nocbreak();
           echo();
           nl();
           clrtoeol();
           refresh();
           getstr(str);
           cbreak();
           noecho();
           nonl();
           return(strlen(str));
	}
        else
	{
/*                              
# -------- non-curlib mode
*/
	   str="?";
           return(strlen(str));
        }
}
int getstr_bp(str)
char *str;
{
  int ic,ind;

  if ( is_curlib_on() == F95__TRUE ) {
/*                              
----- We are in curlib mode
*/
      nodelay(stdscr,TRUE);
      echo();
      nl();
      clrtoeol();
      refresh();
      ind = 0;
      ic = getch();
      while (ic != 10)
      {
	if (ic == -1) {
	  beep();
	  beep();
	  sleep(1);
	  ic = getch();
	}
	else {
	  str[ind++] = ic;
	  addch(ic);
	  refresh();
	  ic = getch();
	}
      }
      str[ind] = 0;
      noecho();
      nonl();
      return(strlen(str));
  }
  else
/*                              
#---- non-curlib mode
*/
      str="?";
      return(strlen(str));
}
setcr_mn(x,y)
int *x, *y;
{
  if ( is_curlib_on() == F95__TRUE ) move(*y,*x);
}
senxy_mn(x,y)
int *x, *y;
{
      int c;

      if ( is_curlib_on() == F95__TRUE ) {
/*                              
#---- We do anything only in curlib mode
*/
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
         c=getch();
         switch (c)
         {
            case KEY_LEFT:
            case 'h':
                *x=*x-1;
                break;
            case KEY_RIGHT:
            case 'l':
                *x=*x+1;
                break;
            case KEY_UP:
            case 'k':
                *y=*y-1;
                break;
            case KEY_DOWN:
            case 'j':
                *y=*y+1;
                break;
            case ' ':
                return;
         }
         move(*y,*x);
         refresh();
      }
     }
}
senkr_nb(x,y,ikey)
int *x, *y, *ikey;
{
      if ( is_curlib_on() == F95__TRUE ) {
/*                              
#---- We do anything only in curlib mode
*/
      nodelay(stdscr,FALSE);
#ifndef DARWIN
      ioctl(0,TCFLSH,0);
#else
      ioctl(0,CFLUSH,0);
#endif
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
         *ikey=getch();
         *ikey=upcase(*ikey);
         switch (*ikey)
         {
            case KEY_LEFT:
            case KEY_F(1):
                *x=*x-1;
                break;
            case KEY_RIGHT:
            case KEY_F(4):
                *x=*x+1;
                break;
            case KEY_UP:
            case KEY_F(3):
                *y=*y-1;
                break;
            case KEY_DOWN:
            case KEY_F(2):
                *y=*y+1;
                break;
            default:
                return;
         }
         move(*y,*x);
	 refresh();
      }
    }
}
senkr_bp(x,y,ikey)
int *x, *y, *ikey;
{
      if ( is_curlib_on() == F95__TRUE ) {
/*                              
#---- We do anything only in curlib mode
*/
      nodelay(stdscr,TRUE);
#ifndef DARWIN
      ioctl(0,TCFLSH,0);
#else
      ioctl(0,CFLUSH,0);
#endif
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
         *ikey=getch();
         *ikey=upcase(*ikey);
         switch (*ikey)
         {
	    case -1:
		beep();
		beep();
		sleep(1);
		break;
            case KEY_LEFT:
                *x=*x-1;
                break;
            case KEY_RIGHT:
                *x=*x+1;
                break;
            case KEY_UP:
                *y=*y-1;
                break;
            case KEY_DOWN:
                *y=*y+1;
                break;
            default:
                return;
         }
         move(*y,*x);
	 refresh();
      }
    }
}
reverse_on_mn()
{
      if ( is_curlib_on() == F95__TRUE )     attron(A_REVERSE);
}
reverse_off_mn()
{
      if ( is_curlib_on() == F95__TRUE )     attroff(A_REVERSE);
}
blink_on_mn()
{
      if ( is_curlib_on() == F95__TRUE )     attron(A_BLINK);
}
blink_off_mn()
{
      if ( is_curlib_on() == F95__TRUE )     attroff(A_BLINK);
}
beep_mn()
{
      if ( is_curlib_on() == F95__TRUE )     beep();
}
clear_mn()
{
      if ( is_curlib_on() == F95__TRUE )     clear();
}
deleteln_mn()
{
      if ( is_curlib_on() == F95__TRUE )     deleteln();
}
clrtobot_mn()
{
      if ( is_curlib_on() == F95__TRUE )     clrtobot();
}
clrtoeol_mn()
{
      if ( is_curlib_on() == F95__TRUE )     clrtoeol();
}
refresh_mn()
{
      if ( is_curlib_on() == F95__TRUE ) 
           refresh();
        else
	   term_flush_fort();
}
setscrreg_mn(top,bot)
int *top,*bot;
{
      if ( is_curlib_on() == F95__TRUE ) setscrreg(*top,*bot);
}
scroll_mn()
{
          if ( is_curlib_on() == F95__TRUE ) scroll(stdscr);
}
senkrs_nb(x,y,ikey)
int *x, *y, *ikey;
{
  if ( is_curlib_on() == F95__TRUE ) {
/*                              
#---- We do anything only in curlib mode
*/
      nodelay(stdscr,FALSE);
#ifndef DARWIN
      ioctl(0,TCFLSH,0);
#else
      ioctl(0,CFLUSH,0);
#endif
      getyx(stdscr, *y, *x);
      refresh();
         *ikey=getch();
         *ikey=upcase(*ikey);
         switch (*ikey)
         {
            case KEY_LEFT:
		*ikey='<';
                break;
            case KEY_RIGHT:
		*ikey='>';
                break;
            case KEY_UP:
		*ikey='-';
                break;
            case KEY_DOWN:
		*ikey='+';
                break;
            default:
		;
         }
         return;
  }
}
senkrs_bp(x,y,ikey)
int *x, *y, *ikey;
{
  if ( is_curlib_on() == F95__TRUE ) {
/*                              
#---- We do anything only in curlib mode
*/
      nodelay(stdscr,TRUE);
#ifndef DARWIN
      ioctl(0,TCFLSH,0);
#else
      ioctl(0,CFLUSH,0);
#endif
      getyx(stdscr, *y, *x);
      refresh();
      while (TRUE)
      {
         *ikey=getch();
         *ikey=upcase(*ikey);
         switch (*ikey)
         {
	    case -1:
		beep();
		beep();
		sleep(1);
		break;
            case KEY_LEFT:
		*ikey='<';
                return;
            case KEY_RIGHT:
		*ikey='>';
                return;
            case KEY_UP:
		*ikey='-';
                return;
            case KEY_DOWN:
		*ikey='+';
                break;
            default:
		return;
         }
      }
         return;
  }
}
