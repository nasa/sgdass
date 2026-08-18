/*
   ftperror - allows fortran to call perror.
   ftperror also terminates the input string before calling perror(3s)
*/
#ifdef _NEEDED
void ftperror_(string,length)
#else
void ftperror(string,length)
#endif
    char *string;
    int length;
{
    void perror();
    char *zt();
    perror(zt(string,length));
}

char out_str[200];

#ifdef _NEEDED
char *zt_(str,len)
#else
char *zt(str,len)
#endif
    char *str;
    int len;
{
    int i, j;
    for (i=len-1 ; i>=0 ; i--) {
	if(*(str+i) != ' ') break;
    }
    *(out_str+i+1) = '\0';
    for (j=i ; j>=0 ; j--) {
	*(out_str+j) = *(str+j);
    }
    return(out_str);
}
