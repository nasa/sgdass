#ifdef _NEEDED
int fc_wait_(status)
#else
int fc_wait(status)
#endif
int **status;
{
  return(wait(*status));
}
