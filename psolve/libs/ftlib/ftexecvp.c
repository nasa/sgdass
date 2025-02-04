/*
    FTEXECVP is a Fortran callable subroutine that will make a call to
    the system call execvp(2).  It's needed because the calling sequences
    of the system call use call by value.

    Input:  command - character string containing the command line that
		is to be executed.  The calling program should not expect
		to regain control.  Upon failure, the function value 
		will be negative.
*/
#ifdef _NEEDED
long ftexecvp_(command,command_len)
#else
long ftexecvp(command,command_len)
#endif
    char *command;
    int command_len;
{
    char *args[11];
    int i, j;
    long ierr;

/*
     Zeros are placed at the end of the substrings in what follows.
     There should be no '\0' characters in the input string.
*/
    if(!(command[command_len-1] == ' ') || (command[command_len-1] == '\0'))
	return(-2);
    i = 0;			    /* pointer to character in command string */
    for ( j=0 ; j<=10 ; j++) {      /* j is the nth argument to execvp        */
	for (; *(command+i) == ' '; i++) {	       /* skip leading blanks */
	    if( i >= (command_len-1) ) goto done;
	}
	args[j] = command+i;			  /* set pointer to substring */
	for (; *(command+i) != ' '; i++ ) {
	    if( i >= (command_len-1) ) goto done;
	}
	*(command+(i++)) = '\0';
    }
done: if(j==0) return(-3);
    else args[j] = 0;

    if((ierr = execvp(args[0],args)) != 0)  /* the actual execvp call; execvp */
	perror("execvp");		    /* is used because it looks at    */
					   /* the PATH environment variable   */
/*
   If successful, it should never get here; however, unset all zeros back to
   blanks for unsuccessful cases.
*/
    for (i=0; i<command_len; i++ ) {	/* blank out all 0s	*/
	if(*(command+i) == '\0')
	    *(command+i) = ' ';
    }
    return(ierr);
}
