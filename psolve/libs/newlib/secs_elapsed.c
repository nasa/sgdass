/*
  secs_elapsed: calculates the seconds elapsed since a start time. 
                written 5/26/94 by kdb
                input: init - 1 to initialize the reference start time
                            - 0 to calculate the current difference
                output: if init 1,
                           returns 0 for success
                        if init 0,
                           returns number of seconds elapsed
                    */
#include <time.h>
double secs_elapsed (init)
int *init;
{
  static time_t start_time;
  time_t cur_time;
  double dzero;
/* */
  if (*init) {    /* initialization case)  */
    time(&start_time);  /* get initial time */
    dzero = 0;
    return (dzero);
  }
  else {  /* subsequent case - calculating the time elapsed */
    time(&cur_time);
    return (difftime(cur_time,start_time));
  }
}
