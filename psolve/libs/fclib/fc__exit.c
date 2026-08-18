#ifdef _NEEDED
void fc__exit_(status)
#else
void fc__exit(status)
#endif
int *status;
{
  void _exit();
  _exit(*status);
}
