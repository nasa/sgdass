/*
	Fortran callable version of popen(3s) and pclose(3s)
	These use stream I/O and stdio.
*/
#include <stdio.h>

#ifdef _NEEDED
int ftpopen_(command,type)
#else
int ftpopen(command,type)
#endif
    char *command, *type;
{
    FILE *popen();
    union {
	int ret;
	FILE *file;
    } ptr;
    ptr.file = popen(command,type);
    return(ptr.ret);
}

#ifdef _NEEDED
int ftpclose_(pt_stream)
#else
int ftpclose(pt_stream)
#endif
    FILE **pt_stream;
{
    int pclose();
    return(pclose(*pt_stream));
}

#ifdef _NEEDED
int ftfprintf_(pt_stream,outstring) /* This prints outstring on *pt_stream. */
#else
int ftfprintf(pt_stream,outstring) /* This prints outstring on *pt_stream. */
#endif
    FILE **pt_stream;		   /* As such, it is not exactly analogous */
    char *outstring;		   /* to fprintf.                          */  
{
/* int fprintf(); -- removed by L. Petrov on 2003.11.17 */
    return(fprintf(*pt_stream,outstring));
}

#ifdef _NEEDED
int ftfscanf_(pt_stream,instring)   /* This reads in instring from *pt_stream */
#else
int ftfscanf(pt_stream,instring)   /* This reads in instring from *pt_stream */
#endif
    FILE **pt_stream;		   /* As such, it is not exactly analogous */
    char *instring;		   /* to fscanf.                           */  
{
/* int fscanf();     -- removed by L. Petrov on 2003.11.17 */
    return(fscanf(*pt_stream,instring));
}

#ifdef _NEEDED
int ftfgetc_(pt_stream)
#else
int ftfgetc(pt_stream)
#endif
    FILE **pt_stream;
{
    int fgetc();
    return(fgetc(*pt_stream));
}

#ifdef _NEEDED
int ftfputc_(pt_c,pt_stream)
#else
int ftfputc(pt_c,pt_stream)
#endif
    FILE **pt_stream;
    int *pt_c;
{
    int fputc();
    return(fputc(*pt_c,*pt_stream));
}
