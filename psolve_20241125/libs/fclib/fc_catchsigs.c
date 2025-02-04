#include <stdio.h>
#include <signal.h>

#define BADSIG (int (*)())-1

/* static void setsig(sig,fcn) */
static void setsig(sig,fcn)
int sig;
void (*fcn)();
{
    int signl;

    signl = signal(sig,SIG_IGN);
    if ( signal(sig,fcn) == BADSIG)
#ifdef _NEEDED
	syserr_("signal");
#else
	syserr("signal");
#endif
}


#ifdef _NEEDED
void fc_catchsigs_()
#else
void fc_catchsigs()
#endif
{
    void cleanup();

    setsig(SIGHUP,cleanup);
    setsig(SIGINT,cleanup);
    setsig(SIGSEGV,cleanup);
    setsig(SIGQUIT,cleanup);
}

void cleanup(sig)
int sig;
{
    int errtyp;

    errtyp=0;
    if ( signal(sig,SIG_IGN) == BADSIG)
#ifdef _NEEDED
	syserr_("signal");
#else
	syserr("signal");
#endif
    switch (sig) {
    case SIGHUP:
	errtyp=1;	
	break;
    case SIGINT:
	errtyp=2;	
	break;
    case SIGQUIT:
	errtyp=4;	
	break;
    case SIGSEGV:
	errtyp=5;	
    }
#ifdef _NEEDED
    ferrx_(&errtyp);
#else
    ferrx(&errtyp);
#endif
    exit(1);
}
