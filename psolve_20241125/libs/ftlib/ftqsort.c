/*   
       ftqsort sorts the given simple variable or array.  ftqsort will treat the
         num_el*size_el bytes in memory, starting with the address pointed to
         by base_el, as a C character string.  (The item to be sorted does 
         not have to be a character variable.) It will then divide the
         string into num_el portions, each size_el bytes long, and sort
         these portions.  The sorted portions will go back into the original
         variable.  kdb 10/11/89
                                                             */
      void ftqsort(base_el,num_el,size_el)
      char *base_el;
      short *num_el;
      short *size_el;
{
      int strcmp();
      qsort(base_el,*num_el,*size_el,strcmp);
      return;
  }
