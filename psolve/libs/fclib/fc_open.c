#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

#ifdef _NEEDED
int fc_open_(path,oflag,mode)
#else
int fc_open(path,oflag,mode)
#endif
    char **path;
    int *oflag, *mode;
{
	int opret;
	opret =open(*path,*oflag,*mode);
	if (opret == -1) {
#ifdef _NEEDED
	  syserr_(" ");
#else
	  syserr(" ");
#endif
	}
	return(opret);
}
