#include <stdio.h>

#ifdef _NEEDED
int c_mnt_(astr,userlen)
#else
int c_mnt(astr,userlen)
#endif
char **astr;
short *userlen;

/* modifications:
   kdb 970721 This routine checks the input directory against field 2 of the
              /etc/mnttab.  However, a while ago, /etc/mnttab was updated to
              list this value as /MOUNTS/machine/directory for remote 
              directories.  Ordinarily this routine would have been
              unaffected, but an error in an earlier routine always passed /
              (the root directory) to this routine, and / always appears
              mounted.  Now that the error in the earlier routine is fixed,
              update this routine as well (to strip off the /MOUNTS/machine
              for comparison to the input directory).
   kdb 040813 The Linux mount table file has a different name.
   kdb 041020 Correct the compiler directive used for the 040813 update.
   pet 2004.10.20  Replaced TRUE and FALSE with FORTRAN_TRUE and FORTRAN_FALSE
*/

{
   FILE *fp;
   char str[80];
   char hold_astr[141];
   int is_mounted=FORTRAN_FALSE;
   int i;
   char pre_str[80], intermed_str[80], *ipt_search, *ipt_bd;

   strcpy(hold_astr,*astr);
   for (i=100;i>=0;i--) {
      if (hold_astr[i]==' ') {
         continue;
      }
      else {
         hold_astr[i+1]='\0';
         break;
      }
   }

   if (hold_astr[0] != '/')
   {
      return(-2);
   }
#ifdef LINUX
   if ((fp=fopen("/etc/mtab","r")) == NULL)
#else
   if ((fp=fopen("/etc/mnttab","r")) == NULL)
#endif
   {
      fclose(fp);
      return(-3);
   }
   while (fscanf(fp,"%*s %s %*[^\n]",pre_str) != EOF)
   { 
/*    In the UNIX mount table file, 
      block devices on other machines have the form
      /MOUNTS/machine/block_device. Strip off the /MOUNTS/machine
                                          */
      if (strncmp(pre_str,"/MOUNTS",7) == 0) {
        ipt_search = &pre_str[8];
        strcpy(intermed_str,ipt_search);
        ipt_bd = strchr(intermed_str,'/');
        strcpy(str,ipt_bd);
      }
      else {
        strcpy(str,pre_str);
      }
      if (strcmp(str,hold_astr) == 0)
      {
         is_mounted=FORTRAN_TRUE;
         break;
      }
   }
   if (is_mounted)
   {
      fclose(fp);
      return(0);
   }
   else
   {
      fclose(fp);
      return(-1);
   }
}
