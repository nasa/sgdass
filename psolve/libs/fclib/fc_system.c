#ifdef _NEEDED
int fc_system_(string)
#else
int fc_system(string)
#endif
char **string;
{
    return(system(*string));
}
