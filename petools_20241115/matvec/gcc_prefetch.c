void gcc_prefetch_rd_0 (long *adr )
{
  __builtin_prefetch (&adr, 0, 0);
  return;
}

void gcc_prefetch_rw_0 (long *adr )
{
  __builtin_prefetch (&adr, 1, 0);
  return;
}

void gcc_prefetch_rd_1 (long *adr )
{
  __builtin_prefetch (&adr, 0, 1);
  return;
}

void gcc_prefetch_rw_1 (long *adr )
{
  __builtin_prefetch (&adr, 1, 1);
  return;
}

void gcc_prefetch_rd_2 (long *adr )
{
  __builtin_prefetch (&adr, 0, 2);
  return;
}

void gcc_prefetch_rw_2 (long *adr )
{
  __builtin_prefetch (&adr, 1, 2);
  return;
}

void gcc_prefetch_rd_3 (long *adr )
{
  __builtin_prefetch (&adr, 0, 3);
  return;
}

void gcc_prefetch_rw_3 (long *adr )
{
  __builtin_prefetch (&adr, 1, 3);
  return;
}
