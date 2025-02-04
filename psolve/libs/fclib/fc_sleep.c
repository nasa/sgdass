#ifdef _NEEDED
int fc_sleep_(seconds)
#else
int fc_sleep(seconds)
#endif
int *seconds;
{
  unsigned long sleep();
  return((int) sleep((unsigned long) *seconds));
}
