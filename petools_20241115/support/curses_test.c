#include <curses.h>
#include <ctype.h>
#include <stdio.h>
#include <termio.h>
#include <signal.h>

/* For testing whetehr curses is supported or not */

void main (int argc, char *const *argv)
{
      int char_read ;

      initscr();
      nonl();
      cbreak();
      noecho();
      intrflush(stdscr,FALSE);
      char_read=getch();
      switch (char_read)
         {
            case KEY_LEFT:
	      exit(0);
            case KEY_RIGHT:
	      exit(0);
         }
      exit(0);
}
