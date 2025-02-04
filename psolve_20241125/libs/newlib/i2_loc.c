/*  
    This function simulates the VMS %LOC function for getting the address
    of an integer*2 (short) integer.  Analogous routines will need to
    be written for other types.
*/
#ifdef _NEEDED
int i2_loc_(var)
#else
int i2_loc(var)
#endif
    short *var;
{
    union {
        int shadr;
	short *shvar;
    } temp;
    temp.shvar = var;
    return(temp.shadr);
}
/*
#ifdef _NEEDED
void i2n_loc_(var,varadr)
#else
void i2n_loc(var,varadr)
#endif
    short *var;
    int *varadr;
{
    union {
        int shadr;
	short *shvar;
    } temp;
    temp.shvar = var;
    *varadr = temp.shadr;
}
*/
