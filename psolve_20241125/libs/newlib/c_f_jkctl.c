#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <pwd.h>
#define LOCKDIR "/jshar/jukelock/"
#define LOCKQDIR "/jshar/jukequeue/"
#define MAXTRIES 1
#define NAPTIME 1
#define TRUE 1
#define FALSE 0
#define VALJUKES 32
#define REQLEN 30
#define VERBOSE if (strncmp(vtype,"V",1) == 0)

char vtype[2];

static char *
#ifdef _NEEDED
lockpath_(name)
#else
lockpath(name)
#endif
char *name;
{
   static char path[20];
   char *strcat();
   strcpy(path,LOCKDIR);
   return(strcat(path,name));
}
static char *
#ifdef _NEEDED
lockqpath_(name)
#else
lockqpath(name)
#endif
char *name;
{
   static char path[20];
   char *strcat();
   strcpy(path,LOCKQDIR);
   return(strcat(path,name));
}


#ifdef _NEEDED
int c_f_jkctl_(argv)
#else
int c_f_jkctl(argv)
#endif
char **argv;
/* argv contains ctl, platter, mood, in that order */
{
   char platt[REQLEN];
   char locktype[2];
   FILE *in, *popen();
   int numlocks;
   char buf[100];
   int push_lockqueue();
   int pop_lockqueue();
   void locks_status();
   void remove_latest_qentry();
   int retval;
   char *tok1,*tok2,*tok3;
 
   tok1=strtok(*argv," ");
   tok2=strtok(NULL," ");
   tok3=strtok(NULL," ");
   strcpy(locktype,tok1);
   strcpy(platt,tok2);
   strcpy(vtype,tok3);
   locktype[1]='\0';
   if (platt[2] != ' ')
      platt[3]='\0';
   else
      platt[2]='\0';
   vtype[1]='\0';
   VERBOSE locks_status();
   locktype[0]=toupper(locktype[0]);
   if (locktype[0] != 'L' && locktype[0] != 'U') {
      printf("\nINVALID locktype. Exiting.\n");
      retval=-1;
      return(retval);
   }
   VERBOSE printf("\nYou requested locktype %c\n", locktype[0]);
   if (locktype[0] == 'L') {
      sprintf(buf,"/bin/ls %s | /bin/wc -l", LOCKDIR);
      in=popen(buf,"r");
      if (in == (FILE *)NULL) {
         fprintf(stderr, "popen failed\n");
         return(-2);
      }
      fscanf(in,"%d",&numlocks);
      pclose(in);
   } 
   VERBOSE printf("\nYou requested platter %s\n", platt);
   if (! good_request(platt)) {
      printf("\nThere is no platter by that name.  Exiting.\n");
      return(-3);
   }
   if ((numlocks >= 2) && (locktype[0] == 'L') && not_locked(platt)) {
      VERBOSE printf("\nThere are already two other different platters being used.  Try again later.\n");
      return(-4);
   }
   if (locktype[0] == 'L') {
      if (otherside_locked(platt)) {
         VERBOSE printf("\nSomeone has locked the other side of that platter.  Try again later.\n");
         return(-5);
      } 
      if (! lock(platt)) {
         VERBOSE printf("\nSomeone is already locking platter %s.\n", platt);
         VERBOSE printf("You may use the platter also, but you cannot remove the lock unless you own it.\n");
         if (push_lockqueue(platt) == -2) return(-2);
      }
      else {
         VERBOSE printf("\nYou now own the lock on platter %s.\n", platt);
         VERBOSE locks_status();
      }
   }
   else {
      if (not_locked(platt)) {
         VERBOSE printf("\n(There is no lock on that platter.  Why are you trying to unlock it ?)\n");
         remove_latest_qentry(platt);
         return(-7);
      }
      else {
         if (! owner_lock(platt)) {
            VERBOSE printf("\nCannot remove the lock since you don't own it, but your latest entry will");
            VERBOSE printf("\nbe removed from the lock queue for %s .\n", platt);
            remove_latest_qentry(platt);
            return(-6);
         }
      }
      VERBOSE printf("\nSo ! You own the lock.\n");
      if (lockqueue_empty(platt)) {
         if ( ! unlock(platt))
            VERBOSE printf("\nCould not unlock platter %s.\n", platt);
         else {
            VERBOSE printf("\nPlatter %s now unlocked.\n", platt);
            VERBOSE locks_status();
         }
      }
      else {
         if (pop_lockqueue(platt) == -2 ) return(-2);
      }
   }
   return(0);
}
#ifdef _NEEDED
lock_(name)
#else
lock(name)
#endif
char *name;
{
   char *path, *lockpath();
   int fd, tries;
   extern int errno;
   path=lockpath(name);
   tries=0;
   while ((fd=creat(path,0)) == -1 && errno == EACCES) {
      if (++tries >= MAXTRIES)
         return(FALSE);
      sleep(NAPTIME);
   }
   if (fd == -1 || close(fd) == -1)
      perror("lock");
   return(TRUE);
}
#ifdef _NEEDED
unlock_(name)
#else
unlock(name)
#endif
char *name;
{
   char *lockpath();
   if (unlink(lockpath(name)) == -1) {
      perror("unlock");
      return(FALSE);
   }
   else
      return(TRUE);
}

#ifdef _NEEDED
good_request_(req)
#else
good_request(req)
#endif
char *req;
{
   int i;
   int goodreq=0;
   static char *vala[VALJUKES]={"1a","2a","3a","4a","5a","6a","7a","8a","9a",
                                "10a","11a","12a","13a","14a","15a","16a",
                                "17a","18a","19a","20a","21a","22a","23a","24a",
                                "25a","26a","27a","28a","29a","30a","31a","32a"};
   static char *valb[VALJUKES]={"1b","2b","3b","4b","5b","6b","7b","8b","9b",
                                "10b","11b","12b","13b","14b","15b","16b",
                                "17b","18b","19b","20b","21b","22b","23b","24b",
                                "25b","26b","27b","28b","29b","30b","31b","32b"};

   for (i=0;i<VALJUKES;i++) {            
      if ( (strcmp(req,vala[i]) == 0) || (strcmp(req,valb[i]) == 0) ) {
         goodreq=1;
         break;
      }
   }
   return(goodreq);
}
#ifdef _NEEDED
otherside_locked_(req)
#else
otherside_locked(req)
#endif
char *req;
{
   char otherside[REQLEN];
   int i,len;
   struct stat status;
   char *lockpath();

   len=strlen(req);
   for (i=0;i<len;i++)
      otherside[i]=req[i];
   otherside[len]='\0';
   if (req[len-1] == 'a')
      otherside[len-1]='b';
   else
      otherside[len-1]='a';
   if (stat(lockpath(otherside),&status) == -1)
      return(FALSE);
   else
      return(TRUE);
}
void
#ifdef _NEEDED
locks_status_()
#else
locks_status()
#endif
{
   char buf[100];
   strcpy(buf,'\0');
   printf("\nCurrent locks on jukebox platters:\n");
   sprintf(buf,"/bin/ls -la %s*[ab] 2>/dev/null",LOCKDIR);
   system(buf);
   strcpy(buf,'\0');
}
int
#ifdef _NEEDED
push_lockqueue_(name)
#else
push_lockqueue(name)
#endif
char *name;
{
   char buf[100];
   void perror( );
   FILE *fp;
   if ((fp=fopen(lockqpath(name),"a+")) == (FILE *) NULL) {
      printf("\nErr: Couldn't open lockqueue file for platter %s.\n", name);
      perror( " " );
      return(-2);
   }
   cuserid(buf);
   fprintf(fp,"%s\n",buf);
   fclose(fp);
   VERBOSE printf("\nAdded your name to the lock queue for platter %s.\n", name);
}
int
#ifdef _NEEDED
pop_lockqueue_(name)
#else
pop_lockqueue(name)
#endif
char *name;
{
  FILE *fp;
  char buf[200];
  void perror( );
  struct passwd *pwent, *getpwnam();
  struct stat status;
  char *lockpath(), *lockqpath();

  if ((fp=fopen(lockqpath(name),"r")) == (FILE *) NULL) {
     printf("\npop_lockqueue: err opening lockqueue for platter %s .\n", name);
     perror( " " );
     return(-2);
  }
  fscanf(fp,"%s\n",buf); 
  fclose(fp);
  if ((pwent=getpwnam(buf)) == (struct passwd *) NULL) {
     printf("\nErr: pop_lockqueue, no such user %s .\n", buf);
     return(-2);
  }
  if (stat(lockpath(name),&status) == -1 ) {
     printf("\nErr: pop_lockqueue, no such lockfile %s .\n", lockpath(name));
     return(-2);
  }
  if (chown(lockpath(name),pwent->pw_uid,status.st_gid) == -1) {
     printf("\nErr: pop_lockqueue, couldn't chown %s .\n", lockpath(name));
     return(-2);
  }
  VERBOSE printf("\nOwnership of lock on platter %s has been transferred to %s \n", name, buf);
  sprintf(buf,"/bin/sed 1d %s%s > /tmp/jjj$$;/bin/chmod a+w /tmp/jjj$$;/bin/mv /tmp/jjj$$ %s%s", LOCKQDIR, name, LOCKQDIR, name);
  system(buf);
}
#ifdef _NEEDED
lockqueue_empty_(name)  /* test for empty platter queue, located in LOCKQDIR */
#else
lockqueue_empty(name)  /* test for empty platter queue, located in LOCKQDIR */
#endif
char *name;
{
   struct stat status;
   if (stat(lockqpath(name),&status) == -1) {
      printf("\nErr: Couldn't stat lockqueue for platter %s\n", name);
      return(1);
   }
   if (status.st_size == (off_t) 0)   /* empty queue , noone's waiting to lock the platter */
      return(1);
   else {
      VERBOSE printf("\n%d people on queue.\n", status.st_size/4);
      return(0);
   }
}
#ifdef _NEEDED
not_locked_(name)
#else
not_locked(name)
#endif
char *name;
{
   char *lockpath();
   struct stat status;
   if (stat(lockpath(name),&status) == -1)
      return(1);
   else
      return(0);
}
#ifdef _NEEDED
owner_lock_(name)   /* returns true if this process owns the lock on the platter */
#else
owner_lock(name)   /* returns true if this process owns the lock on the platter */
#endif
char *name;
{
   char *lockpath();
   struct stat status;
   stat(lockpath(name),&status);
   return(getuid() == status.st_uid);
} 
void
#ifdef _NEEDED
remove_latest_qentry_(name)
#else
remove_latest_qentry(name)
#endif
char *name;
{
  char user[10];
  char buf[300];
  cuserid(user);
  sprintf(buf,"/usr/bin/awk '{line[i++]=$0} $0 == \"%s\" {last=NR-1;found=1} END {for (i=0;i<=(NR-1);i++) if (! found) print line[i]; else if (i != last) print line[i]; }' %s%s  > /tmp/jjj$$;/bin/chmod a+w /tmp/jjj$$;/bin/mv /tmp/jjj$$ %s%s", user, LOCKQDIR, name, LOCKQDIR, name);
  system(buf);
}
