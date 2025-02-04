/*  Functions that handle compatibility with HP RTE-A IFBRK()
    written by Linton Floyd 6/10/88
    _vec version weh 890616 sigvector interface so that system
    calls can be restarted */

#include <signal.h>

static int brk_val_vec = 0;

/* setup_brk_vec() is called to set up the ifbrk system.
   handle_brk_vec() is the local routine setup as the interrupt routine
   ifbrk_vec() is the query to see if a break has happened        */

#if defined(LINUX) || defined(DARWIN)
/* 
 *     A stub for Linux
 */
setup_brk_vec() 
{
  return 0;
}
handle_brk_vec() 
{
  return 0;
}

int ifbrk_vec() 
{
  return 0;
}

#else

#ifdef _NEEDED
setup_brk_vec_() {
#else
setup_brk_vec() {
#endif
  /*        void sigvector(); */
        struct sigvec vec;
	int handle_brk_vec();

        vec.sv_handler = handle_brk_vec;
        vec.sv_mask = 0;
        vec.sv_flags= 0;
 
        sigvector(SIGINT,&vec,0);
}

#ifdef _NEEDED
handle_brk_vec_(sig,code,scp)
#else
handle_brk_vec(sig,code,scp)
#endif
int sig,code;
struct sigcontext *scp;
{
        scp->sc_syscall_action = SIG_RESTART;  /* set to restart */
	brk_val_vec = -1;		     /* set switch		 */
}
#ifdef _NEEDED
int ifbrk_vec_() {
#else
int ifbrk_vec() {
#endif
	int ret_val;
	ret_val = brk_val_vec;
	brk_val_vec = 0;
	return(ret_val);
}
#endif
