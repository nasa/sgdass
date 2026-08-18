/*
	This is a replacement for break.c which uses messages instead of
	signals.  To send a message to the process using this, one may use
	the companion program "br".
*/

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <errno.h>

#define STR_NUM 5
#define MSG_KEY 1000000

struct my_msgbuf {
    long mtype;
    char mtext[6];
};

char break_str[] = "break";	/* This is the message to be read.	*/
int mqid, pid;		/* message queue ID; process ID of this process */

#ifdef _NEEDED
int setup_brk_() { /* This must be called first by the program using ifbrk() */
#else
int setup_brk() { /* This must be called first by the program using ifbrk() */
#endif
    void perror();
    int getpid();
    pid = getpid();
    if((mqid=msgget((key_t)MSG_KEY,IPC_CREAT | 0666)) == -1) perror("msgget");
    return(mqid);	/* If negative, there was an error encountered. */
}

#ifdef _NEEDED
int ifbrk_() {		/* Called the same way as on the HP1000	*/
#else
int ifbrk() {		/* Called the same way as on the HP1000	*/
#endif
    int nbytes, strncmp();
    char in_str[STR_NUM+1], *strncpy();
    extern int errno;
    struct my_msgbuf buf;
    if((nbytes=msgrcv(mqid,&buf,STR_NUM,(long) pid,IPC_NOWAIT)) == -1) {
        if(errno == ENOMSG) return(0);		/* no message; return */
    /*    else perror("msgrcv");*/	/* some other error; perhaps this should go */
    }
    else {
	strncpy(in_str,buf.mtext,nbytes);
	if(strncmp(break_str,in_str,STR_NUM) == 0) return(-1);
    }
    return(0);
}
