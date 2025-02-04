#include<signal.h>
#include<stdio.h>

void petools_void_handler(int sig);

struct sigaction *petools_set_ararm_handler ( struct sigaction * new_sa )
/*
# ************************************************************************
# *                                                                      *
# *   If the first argumetn NULL it set the alaram handler that passes   *
# *   the signal to the sistem call and cause it to fail with EINTR      *
# *   error code. If not NULL than it expects it contains the sigaction  *
# *   signal handler that it sets.                                       *
# *                                                                      *
# *   Return: the old sigaction handler.                                 *
# *                                                                      *
# * ### 17-OCT-2019 petools_set_ararm_handler v1.1 (c) L. Petrov  ###    *
# *                                                                      *
# ************************************************************************
*/
{
    static struct sigaction petools_sa, petools_old_sa;
     
    if ( new_sa == NULL ){
         petools_sa.sa_handler = petools_void_handler;
         petools_sa.sa_flags = 0;
         sigemptyset(&petools_sa.sa_mask);

         sigaction(SIGALRM, &petools_sa, &petools_old_sa);
         return &petools_old_sa;
    } else {
         sigaction(SIGALRM, (struct sigaction *) new_sa, &petools_old_sa);
         return &petools_old_sa;
    }
}
void petools_void_handler(int sig){
}
