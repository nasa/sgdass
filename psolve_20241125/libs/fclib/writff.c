#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>

/* call this function from fortran like this:

       WRITFF(file_name, buf, wordlen, skip_recs, skip_wsum)  */

/* update to binary file */
/* update buffer must be exact size of record to update !! */
/* args -> file name (zero-termed, please)
           buffer to write (character string or integer array equivalenced)
           word length of buffer (word = 2 bytes)
           number of records to skip
           total number of words in skipped records    */
#ifdef LAHEY
#ifdef _NEEDED
writff_(file_name, len1, buf, len2, wordlen, skip_recs, skip_wsum)
#else
writff(file_name, len1, buf, len2, wordlen, skip_recs, skip_wsum)
#endif
#else 
#ifdef _NEEDED
writff_(file_name, buf, wordlen, skip_recs, skip_wsum, len1, len2 )
#else
writff(file_name, buf, wordlen, skip_recs, skip_wsum, len1, len2 )
#endif
#endif

char *file_name;
int len1;             /*  to receive length of file_name from fortran, invisible from fortran */
char *buf;
int len2;             /*  to receive length of buf from fortran, invisible from fortran */
short *wordlen; 
short *skip_recs;
short *skip_wsum;
{
   FILE *fptemp;
   long skipper;

   /* must open file for direct access */

   if((fptemp=fopen(file_name,"rb+")) == (FILE *) NULL) {
      printf("\nErr: fopen failure. \n");
      exit(1);
   }
   
   /* compute position of record to update */
      
   skipper=(*skip_recs)*18+(*skip_wsum)*2+14;

   /* and go there */

   if (fseek(fptemp,skipper,SEEK_SET) != 0) {
      printf("\nFseek error. Exiting.\n");
      exit(1);
   }

   /* update the record */

#ifdef LAHEY
   if (fwrite(buf,1,len2,fptemp) == 0) {
#else
   if (fwrite(buf,len2,1,fptemp) == 0) {
#endif
      printf("\nFwrite error. Exiting.\n");
      exit(1);
   }

   fclose(fptemp);

   return;
}
