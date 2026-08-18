
#ifdef _NEEDED
void fc_exit_(status)
#else
void fc_exit(status)
#endif
int *status;
{
  void exit();
  exit(*status);
}
