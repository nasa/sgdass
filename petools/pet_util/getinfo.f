      SUBROUTINE GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
! ************************************************************************
! *                                                                      *
! *   Routine  GETINFO_USER  asks UNIX system service and returns        *
! *   USER_NAME      -- user name as user registed in operating system   *
! *   USER_REALNAME  -- user name as in real life.                       *
! *   USER_E_ADDRESS -- user electronic address.                         *
! *                                                                      *
! * ###  04-MAY-1998  GETINFO_USER  v3.0  (c) L. Petrov 11-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_PWD
      PARAMETER  ( M_PWD = 32 )
      CHARACTER  USER_NAME*(*), USER_REALNAME*(*), USER_E_ADDRESS*(*)
      INTEGER*4  PASSWD_REC(M_PWD), PW_NAME, PW_GECOS, PW_LEN, ARG_LEN, &
     &           IP, UID
      ADDRESS__TYPE  IADR_PWD, IADR_PW_NAME, IADR_PW_GECOS
      CHARACTER  STR*64
      ADDRESS__TYPE, EXTERNAL :: GETPWUID
      INTEGER*4, EXTERNAL :: GETUID, ILEN, I_LEN
!
! --- Get offsets with resepctto the address of PASSWD_REC of the field of
! --- our interests
!
      CALL GET_SYSTEM_CONSTANT ( 'pw_name',  PW_NAME,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'pw_gecos', PW_GECOS, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'pw_len',   PW_LEN,   ARG_LEN )
!
! --- Get user identificator
!
      UID = GETUID()
!
! --- Get information from passwd file about user
!
      IADR_PWD = GETPWUID ( %VAL(UID) )
      IF ( IADR_PWD .EQ. 0 ) THEN
           CALL CLRCH ( USER_NAME      )
           CALL CLRCH ( USER_REALNAME  )
           CALL CLRCH ( USER_E_ADDRESS )
      END IF
!
! --- Extract this information from data structure returned by GETPWNAM
!
      CALL LIB$MOVC3 ( PW_LEN, %VAL(IADR_PWD), PASSWD_REC )
!
! --- Get addresses of the field of interests
!
      CALL LIB$MOVC3 ( SIZEOF(IADR_PW_NAME),  %VAL(LOC(PASSWD_REC(1))+PW_NAME), &
     &                                        IADR_PW_NAME  )
      CALL LIB$MOVC3 ( SIZEOF(IADR_PW_GECOS), %VAL(LOC(PASSWD_REC(1))+PW_GECOS), &
     &                                        IADR_PW_GECOS )
!
! --- Extract the strings of interest
!
      CALL GET_STRING0 ( %VAL(IADR_PW_NAME),  USER_NAME )
      CALL GET_STRING0 ( %VAL(IADR_PW_GECOS), USER_REALNAME )
!
      IP = INDEX ( USER_REALNAME, ',' )
      IF ( IP .GT. 0 ) CALL CLRCH ( USER_REALNAME(IP:) )
!
! --- Check: do we have a valid real user name?
!
      CALL CLRCH ( STR )
      STR = USER_REALNAME
      CALL CHASHL ( STR )
      IF ( ILEN(STR) .EQ. 0  .OR. STR(1:1) .EQ. ',' ) THEN
!
! -------- Alas, not. Then we form it as "user <user_name>"
!
           USER_REALNAME = 'user '//USER_NAME
      END IF
!
! --- Creation of user e-address
!
      CALL CLRCH        ( USER_E_ADDRESS )
      CALL GETINFO_HOST ( USER_E_ADDRESS )
      USER_E_ADDRESS = USER_NAME(1:I_LEN(USER_NAME))//'@'//USER_E_ADDRESS
!
      RETURN
      END  SUBROUTINE  GETINFO_USER  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GETINFO_HOST ( INTERNET_HOSTNAME )
! ************************************************************************
! *                                                                      *
! *   Routine  GETINO_HOST  returns Internet-name of the current         *
! *   processor as ASCII line.                                           *
! *                                                                      *
! * ###  04-MAY-1998   GETINFO_HOST   v2.1 (c) L. Petrov 17-APR-2009 ### *
! *                                                                      *
! ************************************************************************
      CHARACTER  INTERNET_HOSTNAME*(*), HOSTNAME*64
      INTEGER*4  M_HOS
      PARAMETER  ( M_HOS = 5 )
      INTEGER*4    IS
      ADDRESS__TYPE :: IAD_HOS, HOSTENT_REC(5)
      INTEGER*4,     EXTERNAL :: GETHOSTNAME
      ADDRESS__TYPE, EXTERNAL :: GETHOSTBYNAME
!
! --- Initialization
!
      CALL CLRCH ( INTERNET_HOSTNAME )
!
! --- Get hostname of the current processor
!
      CALL CLRCH ( HOSTNAME )
      IS = GETHOSTNAME ( HOSTNAME, INT2(LEN(HOSTNAME)) )
!
! --- Get datastructure which keeps intenet-name of the current host
!
#ifdef SUN
      INTERNET_HOSTNAME = HOSTNAME 
#else
      IAD_HOS = GETHOSTBYNAME ( HOSTNAME(1:I_LEN(HOSTNAME))//CHAR(0) )
      IF ( IAD_HOS .NE. 0 ) THEN
!
! -------- Extract from the datastructure
!
           CALL LIB$MOVC3   ( M_HOS*SIZEOF(HOSTENT_REC(1)), %VAL(IAD_HOS), &
     &                        HOSTENT_REC     )
           CALL GET_STRING0 ( %VAL(HOSTENT_REC(1)), INTERNET_HOSTNAME )
      END IF
#endif
!
      RETURN
      END  SUBROUTINE  GETINFO_HOST  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
! ************************************************************************
! *                                                                      *
! *   Routine GETIFO_SYSTEM  returens the following information about    *
! *   machine which runs the program:                                    *
! *   SYSNAME -- name of the operating system, its version and revision  *
! *   NODENAME -- name of the node;                                      *
! *   HARDWARE -- name of the hardware for processor.                    *
! *                                                                      *
! *  ###  04-MAY-98  GETINFO_SYSTEM v3.1 (c)  L. Petrov 29-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  SYSNAME*(*), NODENAME*(*), HARDWARE*(*)
      CHARACTER  RELEASE*255
      INTEGER*4  M_UTS_I4
      PARAMETER  ( M_UTS_I4 = 16384 )
      INTEGER*4  IP, ARG_LEN
      INTEGER*4, ALLOCATABLE :: UTSNAME(:)
      INTEGER*4      ARG_SYSNAME,  ARG_SYSNAME_LEN
      ADDRESS__TYPE  IADR_SYSNAME
      INTEGER*4      ARG_RELEASE,  ARG_RELEASE_LEN
      ADDRESS__TYPE  IADR_RELEASE
      INTEGER*4      ARG_MACHINE,  ARG_MACHINE_LEN
      ADDRESS__TYPE  IADR_MACHINE
      INTEGER*4      ARG_NODENAME, ARG_NODENAME_LEN
      ADDRESS__TYPE  IADR_NODENAME
      INTEGER*4,     EXTERNAL ::  UNAME, I_LEN, LOC__SUN$$_STR
!
! --- Initialization
!
      CALL CLRCH ( SYSNAME  )
      CALL CLRCH ( NODENAME )
      CALL CLRCH ( HARDWARE )
!
! --- Learn offsets and lengths of the fields in the data structure UTSNAME
!
      CALL GET_SYSTEM_CONSTANT ( 'sysname',      ARG_SYSNAME,      ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'sysname_len',  ARG_SYSNAME_LEN,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'release',      ARG_RELEASE,      ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'release_len',  ARG_RELEASE_LEN,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'machine',      ARG_MACHINE,      ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'machine_len',  ARG_MACHINE_LEN,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'nodename',     ARG_NODENAME,     ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'nodename_len', ARG_NODENAME_LEN, ARG_LEN )
!
! --- Get system service
!
      ALLOCATE ( UTSNAME(M_UTS_I4) )
      IP = UNAME ( UTSNAME )
!
! --- Extract information from datastructres created by UNAME
!
      IF ( IP .EQ. 0 ) THEN
!
! -------- Extract system name
!
           IADR_SYSNAME = LOC(UTSNAME) + ARG_SYSNAME
           CALL CLRCH ( SYSNAME )
#ifdef SUN
           CALL LIB$MOVC3 ( MIN(ARG_SYSNAME_LEN,LEN(SYSNAME)), &
     &                     %VAL(IADR_SYSNAME), %VAL(LOC__SUN$$_STR(SYSNAME)) )
#else
           CALL LIB$MOVC3 ( MIN(ARG_SYSNAME_LEN,LEN(SYSNAME)), &
     &                     %VAL(IADR_SYSNAME), %REF(SYSNAME) )
#endif
!
! -------- Extract system release
!
           CALL CLRCH ( RELEASE )
           IADR_RELEASE = LOC(UTSNAME) + ARG_RELEASE
#ifdef SUN
           CALL LIB$MOVC3 ( MIN(ARG_RELEASE_LEN,LEN(RELEASE)), &
     &                     %VAL(IADR_RELEASE), %VAL(LOC__SUN$$_STR(RELEASE)) )
#else
           CALL GET_STRING0 ( %VAL(IADR_RELEASE), RELEASE )
#endif
           SYSNAME  = SYSNAME(1:I_LEN(SYSNAME))//' '//RELEASE
!
! -------- Extract machine name
!
           IADR_MACHINE = LOC(UTSNAME) + ARG_MACHINE
           CALL CLRCH ( HARDWARE )
#ifdef SUN
           CALL LIB$MOVC3 ( MIN(ARG_MACHINE_LEN,LEN(HARDWARE)), &
     &                     %VAL(IADR_MACHINE), %VAL(LOC__SUN$$_STR(HARDWARE)) )
#else
           CALL GET_STRING0 ( %VAL(IADR_MACHINE), HARDWARE )
#endif
!
! -------- Extract nodename
!
           CALL CLRCH ( NODENAME )
           IADR_NODENAME = LOC(UTSNAME) + ARG_NODENAME
#ifdef SUN
           CALL LIB$MOVC3 ( MIN(ARG_NODENAME_LEN,LEN(NODENAME)), &
     &                     %VAL(IADR_NODENAME), %VAL(LOC__SUN$$_STR(NODENAME)) )
#else
           CALL GET_STRING0 ( %VAL(IADR_NODENAME), NODENAME )
#endif
      END IF
      DEALLOCATE ( UTSNAME )
!
      RETURN
      END  SUBROUTINE  GETINFO_SYSTEM  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   GET_PROC_INFO ( PAR )
! ************************************************************************
! *                                                                      *
! *   Linu-sopecific subroutine GET_PROC_INFO returns value of parameter *
! *   PAR from the /proc/$$/status file.                                 *
! *                                                                      *
! * ### 31-JUL-2019  GET_PROC_INFO  v1.0 (c)  L. Petrov 31-JUL-2019 ###  *
! *                                                                      *
! ************************************************************************
      INTEGER*4  GET_PROC_INFO 
      CHARACTER  PAR*(*)
      INTEGER*4  GET_PID
      INTEGER*4  MP
      PARAMETER  ( MP = 128 )
      CHARACTER  STR*128, FINAM*64, BUF(MP)*128
      INTEGER*4  J1, J2, NP, IUER
      LOGICAL*1  LEX
      INTEGER*4, EXTERNAL :: GETPID
!
      GET_PROC_INFO = -1
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(I8)') GETPID()
      CALL CHASHL ( STR )
      FINAM = '/proc/'//TRIM(STR)//'/status'
      INQUIRE ( FILE=FINAM, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           RETURN
      END IF
!
      IUER = 0
      CALL RD_TEXT ( FINAM, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) RETURN 
!
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:LEN(PAR)+1) == PAR//':' ) THEN
              CALL CLRCH ( STR )
              STR = BUF(J1)(LEN(PAR)+3:)
              CALL CHASHL ( STR )
              DO 420 J2=2,LEN(STR)
                 IF ( STR(J2:J2) == ' ' .OR. STR(J2:J2) == CHAR(9) ) THEN
                      CALL CHIN  ( STR(1:J2-1), GET_PROC_INFO )
                      RETURN 
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
      RETURN 
      END  FUNCTION   GET_PROC_INFO  !#!#
