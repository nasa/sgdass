void sigmes ( signal )
int *signal;
/* ************************************************************************/
/* *                                                                      */
/* *   Routine signes prints to stdout the string with description of the */
/* *   signal.                                                            */
/* *                                                                      */
/* *  ### 13-NOV-2003     sigmes    v1.0 (c)  L. Petrov  13-NOV-2003 ###  */
/* *                                                                      */
/* ************************************************************************/
{

/* @@@@@@@@@@@@@@@ Section for HP-UX @@@@@@@@@@@@@@   */
#ifdef HPUX
    switch ( *signal )
      {
#ifdef SIGHUP	
        case SIGHUP	:
             printf ( "Signal %d: Hangup \n", *signal );
             break;
#endif
#ifdef SIGINT	
        case SIGINT	:
             printf ( "Signal %d: Interrupt \n", *signal );
             break;
#endif
#ifdef SIGQUIT	
        case SIGQUIT	:
             printf ( "Signal %d: quit \n", *signal );
             break;
#endif
#ifdef SIGILL	
        case SIGILL	:
             printf ( "Signal %d: Illegal instruction \n", *signal );
             break;
#endif
#ifdef SIGTRAP	
        case SIGTRAP	:
             printf ( "Signal %d: trace trap \n", *signal );
             break;
#endif
#ifdef SIGIOT	
        case SIGIOT:
             printf ( "Signal %d: IOT instruction \n", *signal );
             break;
#endif
#ifdef SIGEMT	
        case SIGEMT:
             printf ( "Signal %d: EMT instruction \n", *signal );
             break;
#endif
#ifdef SIGFPE	
        case SIGFPE	:
             printf ( "Signal %d: Floating point exception \n", *signal );
             break;
#endif
#ifdef SIGKILL	
        case SIGKILL	:
             printf ( "Signal %d: kill \n", *signal );
             break;
#endif
#ifdef SIGBUS	
        case SIGBUS	:
             printf ( "Signal %d: bus error \n", *signal );
             break;
#endif
#ifdef SIGSEGV	
        case SIGSEGV	:
             printf ( "Signal %d: Segmentation violation \n", *signal );
             break;
#endif
#ifdef SIGSYS	
        case SIGSYS	:
             printf ( "Signal %d: bad argument to system call \n", *signal );
             break;
#endif
#ifdef SIGPIPE	
        case SIGPIPE	:
             printf ( "Signal %d: write on a pipe with no one to read it \n", *signal );
             break;
#endif
#ifdef SIGALRM	
        case SIGALRM	:
             printf ( "Signal %d: alarm clock \n", *signal );
             break;
#endif
#ifdef SIGTERM	
        case SIGTERM	:
             printf ( "Signal %d: Software termination signal from kill \n", *signal );
             break;
#endif
#ifdef SIGUSR1	
        case SIGUSR1	:
             printf ( "Signal %d: user defined signal 1 \n", *signal );
             break;
#endif
#ifdef SIGUSR2	
        case SIGUSR2	:
             printf ( "Signal %d: User defined signal 2 \n", *signal );
             break;
#endif
#ifdef SIGCHLD	
        case SIGCHLD	:
             printf ( "Signal %d: child process terminated or stopped \n", *signal );
             break;
#endif
#ifdef SIGPWR	
        case SIGPWR	:
             printf ( "Signal %d: Power state indication \n", *signal );
             break;
#endif
#ifdef SIGVTALRM	
        case SIGVTALRM	:
             printf ( "Signal %d: Virtual timer alarm \n", *signal );
             break;
#endif
#ifdef SIGPROF	
        case SIGPROF	:
             printf ( "Signal %d: Profiling timer alarm \n", *signal );
             break;
#endif
#ifdef SIGIO	
        case SIGIO	:
             printf ( "Signal %d: Asynchronous I/O \n", *signal );
             break;
#endif
#ifdef SIGWINCH	
        case SIGWINCH	:
             printf ( "Signal %d: Window size change signal \n", *signal );
             break;
#endif
#ifdef SIGSTOP	
        case SIGSTOP	:
             printf ( "Signal %d: Stop signal (cannot be caught or ignored) \n", *signal );
             break;
#endif
#ifdef SIGTSTP	
        case SIGTSTP	:
             printf ( "Signal %d: Interactive stop signal \n", *signal );
             break;
#endif
#ifdef SIGCONT	
        case SIGCONT	:
             printf ( "Signal %d: Continue if stopped \n", *signal );
             break;
#endif
#ifdef SIGTTIN	
        case SIGTTIN	:
             printf ( "Signal %d: Read from control terminal attempted by a 			  	   member of a background process group \n", *signal );
             break;
#endif
#ifdef SIGTTOU	
        case SIGTTOU	:
             printf ( "Signal %d: Write to control terminal attempted by a member of a background process group \n", *signal );
             break;
#endif
#ifdef SIGURG	
        case SIGURG	:
             printf ( "Signal %d: urgent condition on IO channel \n", *signal );
             break;
#endif
#ifdef SIGLOST	
        case SIGLOST	:
             printf ( "Signal %d: remote lock lost  (NFS)        \n", *signal );
             break;
#endif
#ifdef SIGRESERVE	
        case SIGRESERVE	:
             printf ( "Signal %d: Save for future use \n", *signal );
             break;
#endif
#ifdef SIGDIL 	
        case SIGDIL 	:
             printf ( "Signal %d: DIL signal \n", *signal );
             break;
#endif
#ifdef SIGXCPU	
        case SIGXCPU	:
             printf ( "Signal %d: CPU time limit exceeded (setrlimit)  \n", *signal );
             break;
#endif
#ifdef SIGXFSZ	
        case SIGXFSZ	:
             printf ( "Signal %d: CPU file size limit exceeded (setrlimit)  \n", *signal );
             break;
#endif
#ifdef SIGCANCEL    
        case SIGCANCEL    :
             printf ( "Signal %d: Used for pthread cancellation. \n", *signal );
             break;
#endif
#ifdef SIGGFAULT    
        case SIGGFAULT    :
             printf ( "Signal %d: Graphics framebuffer fault \n", *signal );
             break;
#endif
#ifdef _SIGRTMIN     
        case _SIGRTMIN:
             printf ( "Signal %d: First (highest priority) realtime signal \n", *signal );
             break;
#endif
#ifdef _SIGRTMAX     
        case _SIGRTMAX:
             printf ( "Signal %d: Last (lowest priority) realtime signal \n", *signal );
             break;
#endif
        default:
             printf ( "Signal %d: \n", *signal );
      }

#endif

/* @@@@@@@@@@@@@@@ Section for Linux @@@@@@@@@@@@@@   */

#if defined(LINUX) || defined(DARWIN) 
    switch ( *signal )
      {

#ifdef SIGHUP		
       case SIGHUP:
            printf ( "Signal %d: Hangup \n", *signal );
            break;
#endif
#ifdef SIGINT
       case SIGINT:
            printf ( "Signal %d: Interrupt \n", *signal );
            break;
#endif

#ifdef  SIGQUIT	   
        case SIGQUIT:
             printf ( "Signal %d: Quit \n", *signal );
             break;
#endif

#ifdef  SIGILL	   
        case SIGILL:
             printf ( "Signal %d: Illegal instruction \n", *signal );
             break;
#endif

#ifdef  SIGTRAP	   
        case SIGTRAP:
             printf ( "Signal %d: Trace trap \n", *signal );
             break;
#endif

#ifdef  SIGABRT	   
        case SIGABRT:
             printf ( "Signal %d: Abort \n", *signal );
             break;
#endif

#ifdef  SIGBUS	   
        case SIGBUS:
             printf ( "Signal %d: BUS error BSD).  \n", *signal );
             break;
#endif

#ifdef  SIGFPE	   
        case SIGFPE:
             printf ( "Signal %d: Floating-point exception \n", *signal );
             break;
#endif

#ifdef  SIGKILL	   
        case SIGKILL:
             printf ( "Signal %d: Kill, unblockable \n", *signal );
             break;
#endif

#ifdef  SIGUSR1	   
        case SIGUSR1:
             printf ( "Signal %d: User-defined signal 1 \n", *signal );
             break;
#endif

#ifdef  SIGSEGV	   
        case SIGSEGV:
             printf ( "Signal %d: Segmentation violation \n", *signal );
             break;
#endif

#ifdef  SIGUSR2	   
        case SIGUSR2:
             printf ( "Signal %d: User-defined signal 2 \n", *signal );
             break;
#endif

#ifdef  SIGPIPE	   
        case SIGPIPE:
             printf ( "Signal %d: Broken pipe \n", *signal );
             break;
#endif

#ifdef  SIGALRM	   
        case SIGALRM:
             printf ( "Signal %d: Alarm clock... \n", *signal );
             break;
#endif

#ifdef  SIGTERM	   
        case SIGTERM:
             printf ( "Signal %d: Termination \n", *signal );
             break;
#endif

#ifdef  SIGSTKFLT  
        case SIGSTKFLT:
             printf ( "Signal %d: Stack fault \n", *signal );
             break;
#endif

#ifdef  SIGCHLD	   
        case SIGCHLD:
             printf ( "Signal %d: Child status has changed \n", *signal );
             break;
#endif

#ifdef  SIGCONT	   
        case SIGCONT:
             printf ( "Signal %d: Continue \n", *signal );
             break;
#endif

#ifdef  SIGSTOP	   
        case SIGSTOP:
             printf ( "Signal %d: Stop, unblockable \n", *signal );
             break;
#endif

#ifdef  SIGTSTP	   
        case SIGTSTP:
             printf ( "Signal %d: Keyboard stop \n", *signal );
             break;
#endif

#ifdef  SIGTTIN	   
        case SIGTTIN:
             printf ( "Signal %d: Background read from tty \n", *signal );
             break;
#endif

#ifdef  SIGTTOU	   
        case SIGTTOU:
             printf ( "Signal %d: Background write to tty \n", *signal );
             break;
#endif

#ifdef  SIGURG	   
        case SIGURG:
             printf ( "Signal %d: Urgent condition on socket \n", *signal );
             break;
#endif

#ifdef  SIGXCPU	   
        case SIGXCPU:
             printf ( "Signal %d: CPU limit exceeded \n", *signal );
             break;
#endif

#ifdef  SIGXFSZ	   
        case SIGXFSZ:
             printf ( "Signal %d: File size limit exceeded \n", *signal );
             break;
#endif

#ifdef  SIGVTALRM  
        case SIGVTALRM:
             printf ( "Signal %d: Virtual alarm clock \n", *signal );
             break;
#endif

#ifdef  SIGPROF	   
        case SIGPROF:
             printf ( "Signal %d: Profiling alarm clock \n", *signal );
             break;
#endif

#ifdef  SIGWINCH   
        case SIGWINCH:
             printf ( "Signal %d: Window size change \n", *signal );
             break;
#endif

#ifdef  SIGIO	   
        case SIGIO:
             printf ( "Signal %d: I/O now possible \n", *signal );
             break;
#endif

#ifdef  SIGPWR	   
        case SIGPWR:
             printf ( "Signal %d: Power failure restart \n", *signal );
             break;
#endif

#ifdef  SIGSYS	   
        case SIGSYS:
             printf ( "Signal %d: Bad system call \n", *signal );
             break;
#endif

        default:
             printf ( "Signal %d: \n", *signal );
      }
#endif
}

