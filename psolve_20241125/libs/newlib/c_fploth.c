#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <stdio.h>
#define TEK  1
#define LJ   2
#define HPGL 3
#define HISTUTIL "/users/fgg/oldhat/pghisto"
#define SCREENFIX "/users/fgg/.empty"


#ifdef LINUX
#include <termio.h>
struct termio save, term;
char *termval;
#endif



#ifdef _NEEDED
int c_fploth_(argv)
#else
int c_fploth(argv)
#endif
#ifdef DARWIN
{ return 0; }
#else
char **argv;
/* argv contains data_fil, num_lines, minx,
    maxx, num_bins, dev_type, in that order */
{
   char commstr[300];
   char datafil[250];
   char devtype[50];
   char numlines[50];
   char min_x[50];
   char max_x[50];
   char numbins[50];
   char *tok1,*tok2,*tok3,*tok4,*tok5,*tok6;
   static char pgcomm[]=HISTUTIL;
   int i, devnum=0;
   void init2393();
   void clos2393();
   int alen;

   tok1=strtok(*argv," "); 
   tok2=strtok(NULL," ");
   tok3=strtok(NULL," ");
   tok4=strtok(NULL," ");
   tok5=strtok(NULL," ");
   tok6=strtok(NULL," ");
   strcpy(datafil,tok1);
   strcpy(numlines,tok2);
   strcpy(min_x,tok3);
   strcpy(max_x,tok4);
   strcpy(numbins,tok5);
   strcpy(devtype,tok6);

   alen=strlen(datafil)-1;
   for (i=alen;i>=0;i--) {
      if (datafil[i]==' ') {
         continue;
      }
      else {
         datafil[i+1]='\0';
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
   alen=strlen(numlines)-1;
   for (i=alen;i>=0;i--) {
      if (numlines[i]==' ') {
         continue;
      }
      else {
         numlines[i+1]='\0';
         break;
      }
   }
   alen=strlen(min_x)-1;
   for (i=alen;i>=0;i--) {
      if (min_x[i]==' ') {
         continue;
      }
      else {
         min_x[i+1]='\0';
         break;
      }
   }
   alen=strlen(max_x)-1;
   for (i=alen;i>=0;i--) {
      if (max_x[i]==' ') {
         continue;
      }
      else {
         max_x[i+1]='\0';
         break;
      }
   }
   alen=strlen(numbins)-1;
   for (i=alen;i>=0;i--) {
      if (numbins[i]==' ') {
         continue;
      }
      else {
         numbins[i+1]='\0';
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
   switch (devnum)
   {
      case TEK:
         if ((termval=strstr(getenv("TERM"),"2393")) == (char *) NULL)
         {
            printf("\nError: Your TERM should be set to 2393.\n");
            return(-1);
         }
         sprintf(commstr,"%s %s %s %s %s %s %s",pgcomm,datafil,numlines,min_x,max_x,numbins,devtype);
         break;
      case LJ:
         sprintf(commstr,"echo \"Processing...\";%s %s %s %s %s %s %s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb -oraw $tmpgfil;/bin/rm $tmpgfil", pgcomm,datafil,numlines,min_x,max_x,numbins,devtype);
         break;
      case HPGL:
         sprintf(commstr,"echo \"Processing...\";%s %s %s %s %s %s %s 1>$HOME/.holdpgid;tmpgfil=`/usr/bin/awk 'NR==2 {print substr($0,3,length($0))}' $HOME/.holdpgid`;export tmpgfil;echo \"Created plotfile $tmpgfil\";/usr/bin/lp -onb $tmpgfil;/bin/rm $tmpgfil", pgcomm,datafil,numlines,min_x,max_x,numbins,devtype);
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
   system("/usr/bin/pg SCREENFIX");
   return;
}
#endif
