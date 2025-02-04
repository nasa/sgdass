/*  Functions that handle compatibility with HP RTE-A IFBRK()
    written by Linton Floyd 6/10/88			      */

#include <signal.h>

int brk_val = 0;

/* setup_brk() is called to set up the ifbrk system.
   handle_brk() is the local routine setup as the interrupt routine
   ifbrk() is the query to see if a break has happened        */

#ifdef LINUX
/* 
 *     A stub for Linux

 */
setup_brk_() 
{
  return 0;
}
handle_brk_() 
{
  return 0;
}

#else

#ifdef _NEEDED
setup_brk_() {
#else
setup_brk() {
#endif
	int handle_brk();

	if(signal(SIGINT, SIG_IGN) != SIG_IGN) signal(SIGINT,handle_brk);
}

#ifdef _NEEDED
handle_brk_() {
#else
handle_brk() {
#endif
	signal(SIGINT, handle_brk);  /* reset for next interrupt */
	brk_val = -1;		     /* set switch		 */
}
#ifdef _NEEDED
int ifbrk_() {
#else
int ifbrk() {
#endif
	int ret_val;
	ret_val = brk_val;
	brk_val = 0;
	return(ret_val);
}
#endif
