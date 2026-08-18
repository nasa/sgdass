#ifdef LINUX
#define _GNU_SOURCE
#else
#define O_DIRECT      040000 /* Direct disk access.  */
#endif
#include <errno.h>
#include <stdio.h>
#include <signal.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <dirent.h>
#include <pwd.h>
#include <sys/utsname.h>
#include <sys/file.h>
#include <sys/types.h>
#include <netdb.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifdef SUN
  #define _UTSNAME_DOMAIN_LENGTH _SYS_NMLN
#endif
#include <fcntl.h>

/* # ************************************************************************/ 
/* # *                                                                      */ 
/* # *    routine get_system_constant returns values of some UNIX system    */
/* # *    constants defined in system headers.                              */
/* # *                                                                      */ 
/* # * ------------------- Input parameter: ------------------------------- */
/* # *                                                                      */ 
/* # *  name ( CHARACTER ) -- constant name as it used in Solve.            */ 
/* # *                                                                      */ 
/* # * ------------------- Output parameter: ------------------------------ */
/* # *                                                                      */ 
/* # *  arg  ( INTEGER*? ) -- argument. Meaning depends on contenxt.        */ 
/* # *  len  ( INTEGER*4 ) -- Lenght of the argument in bytes.              */ 
/* # *                                                                      */ 
/* # * ## 03-DEC-2003 get_system_constant v1.12 (c) L. Petrov 18-OCT-2019 # */
/* # *                                                                      */ 
/* # ************************************************************************/
void get_system_constant ( char *name, int *arg, int *len, int name_len )
{
struct dirent  dir_struct;
struct passwd  passwd_struct;
struct utsname utsname_struct;
struct sockaddr_storage sockaddr_struct;
struct addrinfo         addrinfo_struct;
int       var_int;
long      var_long;
long long var_long_long;
size_t    var_size_t;
int a1, a2;
  if ( strncmp ( name, "int", name_len ) == 0 ) 
     {
          *arg = sizeof(var_int);
          *len = 1;
     }
    else if ( strncmp ( name, "long", name_len ) == 0 ) 
     {
          *arg = sizeof(var_long);
          *len = 1;
     }
    else if ( strncmp ( name, "long_long", name_len ) == 0 ) 
     {
          *arg = sizeof(var_long_long);
          *len = 1;
     }
    else if ( strncmp ( name, "size_t", name_len ) == 0 ) 
     {
          *arg = sizeof(var_size_t);
          *len = 1;
     }
    /* ------------------------------------------------ */
    else if ( strncmp ( name, "O_WRONLY", name_len ) == 0 ) 
     {
          *arg = O_WRONLY;
          *len = 1;
     }
    else if ( strncmp ( name, "O_CREAT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_CREAT;
          *len = 1;
     }
    else if ( strncmp ( name, "O_RDONLY", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_RDONLY;
          *len = 1;
     }
    else if ( strncmp ( name, "O_RDWR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_RDWR;
          *len = 1;
     }
    else if ( strncmp ( name, "O_EXCL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_EXCL;
          *len = 1;
     }
    else if ( strncmp ( name, "O_APPEND", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_APPEND;
          *len = 1;
     }
    else if ( strncmp ( name, "S_IFDIR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IFDIR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IFIFO", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IFIFO;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IFLNK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IFLNK;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IFSOCK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IFSOCK;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IRUSR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IRUSR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWUSR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWUSR;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IRGRP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IRGRP;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWGRP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWGRP;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IROTH", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IROTH;
          *len = 4;
     }
    else if ( strncmp ( name, "S_IWOTH", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_IWOTH;
          *len = 4;
     }
    else if ( strncmp ( name, "S_ISUID", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_ISUID;
          *len = 4;
     }
    else if ( strncmp ( name, "S_ISGID", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = S_ISGID;
          *len = 4;
     }
    else if ( strncmp ( name, "O_DIRECT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = O_DIRECT;
          *len = 4;
     }
    else if ( strncmp ( name, "F_GETFD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = F_GETFD;
          *len = 4;
     }
    else if ( strncmp ( name, "F_SETFD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = F_SETFD;
          *len = 4;
     }
    else if ( strncmp ( name, "F_GETFL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = F_GETFL;
          *len = 4;
     }
    else if ( strncmp ( name, "F_SETFL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = F_SETFL;
          *len = 4;
     }
    else if ( strncmp ( name, "d_name", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &dir_struct.d_name;
          a2 = (long) &dir_struct;
          *arg = a1 - a2;
          *len = 1;
     }
    else if ( strncmp ( name, "MAXNAMLEN", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          *arg = MAXNAMLEN;
          *len = 1;
     }
    else if ( strncmp ( name, "pw_name", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_name;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "pw_gecos", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_gecos;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "pw_shell", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_shell;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "pw_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &passwd_struct.pw_shell;
          a2 = (long) &passwd_struct;
          *arg = a1 - a2 + sizeof(&passwd_struct.pw_shell);
          *len = 4;
     }
    else if ( strncmp ( name, "sysname", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.sysname;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "sysname_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
#       ifdef DARWIN
          *arg = _SYS_NAMELEN;
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "release", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.release;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "release_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
#       ifdef DARWIN
          *arg = _SYS_NAMELEN;
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "nodename", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.nodename;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "nodename_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_NODENAME_LENGTH; 
#       endif
#       ifdef DARWIN
          *arg = _SYS_NAMELEN;
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "machine", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
          a1 = (long) &utsname_struct.machine;
          a2 = (long) &utsname_struct;
          *arg = a1 - a2;
          *len = 4;
     }
    else if ( strncmp ( name, "machine_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SYS_NMLN;
#       endif
#       ifdef LINUX
          *arg = _UTSNAME_LENGTH; 
#       endif
#       ifdef DARWIN
          *arg = _SYS_NAMELEN;
#       endif
          *len = 1;
     }
    else if ( strncmp ( name, "domainname", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
#       if defined LINUX 
#          ifdef __USE_GNU
              a1 = (long) &utsname_struct.domainname;
#       else
              a1 = (long) &utsname_struct.__domainname;
#       endif
           a2 = (long) &utsname_struct;
           *arg = a1 - a2;
           *len = 4;
#endif
     }
    else if ( strncmp ( name, "domainname_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
#       if defined LINUX 
          *arg = _UTSNAME_DOMAIN_LENGTH;
          *len = 4;
#endif
     }
    else if ( strncmp ( name, "idnumber", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
           a1 = (long) &utsname_struct.__idnumber;
           a2 = (long) &utsname_struct;
           *arg = a1 - a2;
           *len = 4;
#endif
#       if defined LINUX || defined DARWIN
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
     }
    else if ( strncmp ( name, "idnumber_len", name_len ) == 0 ) 
    /* ------------------------------------------------ */
     {
#       ifdef HPUX
          *arg = _SNLEN;
          *len = 1;
#endif
#       if defined LINUX || defined DARWIN
	  printf ( "get_system_constant: argument %s is not defined \n", name );
          *len = 0;
	  exit ( 1 );
#endif
     }
    else if ( strncmp ( name, "SEEK_SET", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SEEK_SET;
          *len = 1;
      }
    else if ( strncmp ( name, "SEEK_CUR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SEEK_CUR;
          *len = 1;
      }
    else if ( strncmp ( name, "SEEK_END", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SEEK_END;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGCHLD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGCHLD;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGHUP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGHUP;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGINT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGINT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGQUIT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGQUIT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGTERM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGTERM;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGUSR1", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGUSR1;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGUSR2", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGUSR2;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGABRT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGABRT;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGTSTP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGTSTP;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGSTOP", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGSTOP;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGKILL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGKILL;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGALRM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = SIGALRM;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_IGN", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_IGN;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_DFL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_DFL;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_BLOCK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_BLOCK;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_UNBLOCK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_UNBLOCK;
          *len = 1;
      }
    else if ( strncmp ( name, "SIG_SETMASK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIG_SETMASK;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGCHLD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SIGCHLD;
          *len = 1;
      }
    else if ( strncmp ( name, "SIGCLD", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
#ifdef DARWIN
          *arg = (long) SIGCHLD;
#else
          *arg = (long) SIGCLD;
#endif
          *len = 1;
      }
    else if ( strncmp ( name, "WNOHANG", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) WNOHANG;
          *len = 1;
      }
    else if ( strncmp ( name, "WUNTRACED", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) WUNTRACED;
          *len = 1;
      }
    else if ( strncmp ( name, "PF_LOCAL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) PF_LOCAL;
          *len = 1;
      }
    else if ( strncmp ( name, "PF_UNIX", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) PF_UNIX;
          *len = 1;
      }
    else if ( strncmp ( name, "PF_INET", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) PF_INET;
          *len = 1;
      }
    else if ( strncmp ( name, "PF_INET6", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) PF_INET6;
          *len = 1;
      }
//    else if ( strncmp ( name, "PF_NETLINK", name_len ) == 0 ) 
//    /* ------------------------------------------------ */
//      {
//          *arg = (long) PF_NETLINK;
//          *len = 1;
//      }
    else if ( strncmp ( name, "SOCK_STREAM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SOCK_STREAM;
          *len = 1;
      }
    else if ( strncmp ( name, "SOCK_DGRAM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SOCK_DGRAM;
          *len = 1;
      }
    else if ( strncmp ( name, "SOCK_SEQPACKET", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SOCK_SEQPACKET;
          *len = 1;
      }
    else if ( strncmp ( name, "SOCK_RAW", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) SOCK_RAW;
          *len = 1;
      }
    else if ( strncmp ( name, "AI_PASSIVE", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) AI_PASSIVE;
          *len = 1;
      }
    else if ( strncmp ( name, "AF_UNSPEC", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) AF_UNSPEC;
          *len = 1;
      }
    else if ( strncmp ( name, "LOCK_SH", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) LOCK_SH;
          *len = 2;
      }
    else if ( strncmp ( name, "LOCK_EX", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) LOCK_EX;
          *len = 2;
      }
    else if ( strncmp ( name, "LOCK_UN", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) LOCK_UN;
          *len = 2;
      }
    else if ( strncmp ( name, "LOCK_NB", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = (long) LOCK_NB;
          *len = 2;
      }
    else if ( strncmp ( name, "addrinfo_size", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  *arg = (long) sizeof ( addrinfo_struct ) ;
          *len = 1;
      }
    else if ( strncmp ( name, "sockaddr_storage_size", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  *arg = (long) sizeof ( sockaddr_struct ) ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_family_offset", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  a1 = (long) &addrinfo_struct ;
	  a2 = (long) &addrinfo_struct.ai_family ;
	  *arg = a2 - a1 ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_family_size", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  *arg = sizeof ( addrinfo_struct.ai_family ) ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_socktype_offset", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  a1 = (long) &addrinfo_struct ;
	  a2 = (long) &addrinfo_struct.ai_socktype ;
	  *arg = a2 - a1 ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_socktype_size", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  *arg = sizeof ( addrinfo_struct.ai_socktype ) ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_flags_offset", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  a1 = (long) &addrinfo_struct ;
	  a2 = (long) &addrinfo_struct.ai_flags ;
	  *arg = a2 - a1 ;
          *len = 1;
      }
    else if ( strncmp ( name, "ai_flags_size", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
	  *arg = sizeof ( addrinfo_struct.ai_flags ) ;
          *len = 1;
      }
    else if ( strncmp ( name, "ITIMER_REAL", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ITIMER_REAL;
          *len = 2;
      }
    else if ( strncmp ( name, "EFAULT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EFAULT;
          *len = 2;
      }
    else if ( strncmp ( name, "EACCES", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EACCES;
          *len = 2;
      }
    else if ( strncmp ( name, "EDQUOT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EDQUOT;
          *len = 2;
      }
    else if ( strncmp ( name, "EEXIST", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EEXIST;
          *len = 2;
      }
    else if ( strncmp ( name, "EFAULT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EFAULT;
          *len = 2;
      }
    else if ( strncmp ( name, "EINTR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EINTR;
          *len = 2;
      }
    else if ( strncmp ( name, "EISDIR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EISDIR;
          *len = 2;
      }
    else if ( strncmp ( name, "EMFILE", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EMFILE;
          *len = 2;
      }
    else if ( strncmp ( name, "ENFILE", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENFILE;
          *len = 2;
      }
    else if ( strncmp ( name, "EISDIR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EISDIR;
          *len = 2;
      }
    else if ( strncmp ( name, "ENODEV", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENODEV;
          *len = 2;
      }
    else if ( strncmp ( name, "ENOENT", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENOENT;
          *len = 2;
      }
    else if ( strncmp ( name, "ENOMEM", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENOMEM;
          *len = 2;
      }
    else if ( strncmp ( name, "ENOSPC", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENOSPC;
          *len = 2;
      }
    else if ( strncmp ( name, "ENOTDIR", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENOTDIR;
          *len = 2;
      }
    else if ( strncmp ( name, "ENXIO", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = ENXIO;
          *len = 2;
      }
    else if ( strncmp ( name, "EOVERFLOW", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EOVERFLOW;
          *len = 2;
      }
    else if ( strncmp ( name, "EROFS", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EROFS;
          *len = 2;
      }
    else if ( strncmp ( name, "EWOULDBLOCK", name_len ) == 0 ) 
    /* ------------------------------------------------ */
      {
          *arg = EWOULDBLOCK;
          *len = 2;
      }
    else
    /* --*/
      {
	  printf ( "get_system_constant: unknown argument: %s \n", name );
          *len = 0;
	  exit ( 1 );
      }  
   return ;
}
