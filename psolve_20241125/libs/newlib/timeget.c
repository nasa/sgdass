/*
Here follows the original Fortran comments (minus those silly c's):

      subroutine timeget(itime,iyear)
  This simulates/replaces the exec(11 ...) call in HP RTE-A.

  Returned GMT values:
	ITIME - is a five element I*2 array as follows:
	     ITIME(1) - tens of milliseconds (always returns 0)
	     ITIME(2) - seconds
	     ITIME(3) - minutes
	     ITIME(4) - hours
	     ITIME(5) - day (numbered from the beginning of the year)
	IYEAR - is the four digit year (I*2)
	integer*2 itime(5),iyear
*/

#include <time.h>

#ifdef _NEEDED
int timeget_(itime,iyear)
#else
int timeget(itime,iyear)
#endif

    short itime[5], *iyear;
{
    void perror();
    long my_clock;
    struct tm *ptr_times;
    int ierr;

    if(ierr = time(&my_clock) ==  -1)
	perror("time");

    else {
	ptr_times = gmtime(&my_clock);
	*iyear = 1900 + ptr_times -> tm_year;
	itime[0] = 0;
	itime[1] = ptr_times -> tm_sec;
	itime[2] = ptr_times -> tm_min;
	itime[3] = ptr_times -> tm_hour;
	itime[4] = 1 + ptr_times -> tm_yday;   /* unix starts day # at 0 */
	ierr = 0;
    }
    return(ierr);
}
