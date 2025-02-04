#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#ifndef DARWIN
#include <termio.h>
#endif
#define TEK  1
#define LJ   2
#define HPGL 3
#define XWIN 4
#define PLOTUTIL "/usr/local/bin/pc8"

#ifndef DARWIN
struct termio save, term;
char *termval;
#endif

#ifdef _NEEDED
int c_fplot_(argv)
#else
int c_fplot(argv)
#endif
#ifdef DARWIN
{ return 0; }
#else

char **argv;
/* argv contains the control file and
   the device type in that order */
/* fgg -- added lpopt to specify printer 6-5-95 */
/* if lpopt is empty, will print to default printer */
/* if lpopt is set, must be of form "-dprintername" */
{
   char commstr[300];
   char cntlfil[210], devtype[210], lpopt[20];
   char *tok1, *tok2, *tok3;
   static char pgcomm[]=PLOTUTIL;
   int i, devnum=0;
   void init2393();
   void clos2393();
   int alen;

   tok1=strtok(*argv," ");
   tok2=strtok(NULL," ");
   tok3=strtok(NULL," ");
   strcpy(cntlfil,tok1);
   strcpy(devtype,tok2);
   strcpy(lpopt,tok3);
   alen=strlen(cntlfil)-1;
   for (i=alen;i>=0;i--) {
      if (cntlfil[i]==' ') {
         continue;
      }
      else {
         cntlfil[i+1]='\0';
         break;
      }
   }
   alen=strlen(devtype)-1;
   for (i=alen;i>=0;i--) {
      if (devtype[i]==' ') {
         continue;
      }
      else {
         devtype[i+1]='\0';
         break;
      }
   }
   alen=strlen(lpopt)-1;
   for (i=alen;i>=0;i--) {
      if (lpopt[i]==' ') {
         continue;
      }
      else {
         lpopt[i+1]='\0';
         break;
      }
   }
   for (i=0;i<strlen(devtype);i++)
      devtype[i]=toupper(devtype[i]);
   if (strncmp(devtype,"/T",2) == 0)
      devnum=TEK;
   if (strncmp(devtype,"/L",2) == 0)
      devnum=LJ;
   if (strncmp(devtype,"/H",2) == 0)
      devnum=HPGL;
   if (strncmp(devtype,"/X",2) == 0)
      devnum=XWIN;
   switch (devnum)
   {
      case TEK:
         if ((termval=strstr(getenv("TERM"),"2393")) == (char *) NULL)
         {
            printf("\nError: Your TERM should be set to 2393.\n");
            return(-1);
         }
         sprintf(commstr,"%s %s %s",pgcomm,cntlfil,devtype);
         break;
      case LJ:
         sprintf(commstr,"echo \"Processing...\";%s %s %s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb -oraw %s $tmpgfil;/bin/rm $tmpgfil", pgcomm,cntlfil,devtype,lpopt);
         break;
      case HPGL:
         sprintf(commstr,"echo \"Processing...\";%s %s %s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb %s $tmpgfil;/bin/rm $tmpgfil", pgcomm,cntlfil,devtype,lpopt);
         break;
      case XWIN:
         sprintf(commstr,"%s %s %s",pgcomm,cntlfil,devtype);
         break;
      default:
         printf("\nError: Unknown device type.\n");
         printf("Valid devices types are: \n");
         printf("/TEK4010    /TEKLAND \n");
         printf("/LJLOWRES   /LJLOWLAND \n");
         printf("/LJHIGHRES  /LJHIGHLAND \n");
         printf("/HPGLP      /HPGLL \n");
         return(-1);
   }
   if (((termval=strstr(getenv("TERM"),"2393")) != (char *) NULL) &&
      (devnum == TEK))
      init2393();
   system(commstr);
   if (((termval=strstr(getenv("TERM"),"2393")) != (char *) NULL) &&
      (devnum == TEK))
      clos2393();
   return(0);
}

#ifdef _NEEDED
void init2393_()
#else
void init2393()
#endif
{
   if (ioctl(0,TCGETA,&term) == -1)
   {
      fprintf(stderr,"standard input not a tty\n");
      exit(-1);
   }

   save=term;

   term.c_lflag &= ~ICANON;
   term.c_lflag &= ~ECHO;

   term.c_cc[VMIN]=1;
   term.c_cc[VTIME]=0;
   ioctl(0,TCSETA,&term);
   write(1,"\033&a0y0C\033J",11);   /* clear hp2393a screen */
   write(1,"\033*t1D",6);           /* switch to 4014 mode */
   write(1,"\033&s1p0Q",8);         /* switch to scaled submode */
   return;
}

#ifdef _NEEDED
void clos2393_()
#else
void clos2393()
#endif
{
   write(1,"\033&s0p0Q",8);       /*  end 4014 mode */
   ioctl(0,TCSETA,&save);
   system("/usr/bin/pg < /dev/null");
   return;
}
#endif
