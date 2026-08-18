
#ifdef _NEEDED
void fc_perror_(s)
#else
void fc_perror(s)
#endif
char **s;
{
    void perror();

    perror(*s);
}
