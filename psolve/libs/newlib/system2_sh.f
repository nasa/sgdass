      FUNCTION   SYSTEM2_SH ( COMMAND_STRING )
! ************************************************************************
! *                                                                      *
! *   Function  SYSTEM2_SH  emulates function SYSTEM(3S) for the case    *
! *   when +U77 option was in use and SYSTEM(U77) is used.               *
! *                                                                      *
! *   The true is that HP-UX has TWO DIFFERENT functions with the same   *
! *   name SYSTEM: SYSTEM(3S) and SYSTEM(U77). When compiling and        *
! *   linking option +U77 is in use then SYSTEM(U77) is called otherwise *
! *   SYSTEM(3S) is called. Differences: a) SYSTEM(U77) returns value    *
! *   of INTEGER*4, but SYSTEM(3S) of INTEGER*2; b) SYSTEM(U77) executes *
! *   a command line under the shell to be specified by SHELL            *
! *   environment while SYSTEM(3S) executes a command line by /bin/sh    *
! *   shell.                                                             *
! *                                                                      *
! *   SYSTEM2_SH executes a command line by /bin/sh shell and returns    *
! *   compilation code of INTEGER*2.                                     *
! *                                                                      *
! *  ###  09-APR-99    SYSTEM_SH   v1.0  (c)  L. Petrov  09-APR-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*2  SYSTEM2_SH
      CHARACTER  COMMAND_STRING*(*)
      CHARACTER  SHELL_STR*255, SHELL_STR_OLD*255
      SAVE       SHELL_STR ! It is important!!!
      INTEGER*2  TRIMLEN
      INTEGER*4  J1, J2, J3, IS4, ILS
      INTEGER*4  SYSTEM
!
! --- Learn value of SHELL environment variable
!
      DO 410 J1=1,LEN(SHELL_STR_OLD)
         SHELL_STR_OLD(J1:J1) = ' '
 410  CONTINUE
      CALL GETENVAR ( 'SHELL', SHELL_STR_OLD )
      ILS = TRIMLEN(SHELL_STR_OLD)
      IF ( ILS .GT. 0 ) THEN
!
! -------- The name was specified. form a line in the format in which
! -------- environment variables are kept. Otherwise if SHELL environment
! -------- variable was not specified then SYSTEM(U77) will use /bin/sh
! -------- and therefore we have not to do anything
!
           ILS=ILS+1
           SHELL_STR_OLD(ILS:ILS) = CHAR(0)
           ILS=ILS+6
           SHELL_STR_OLD = 'SHELL='//SHELL_STR_OLD
!
! -------- Set temporarily SHELL envirnoment variable as /bin/sh
!
           DO 420 J2=1,LEN(SHELL_STR)
              SHELL_STR(J2:J2) = ' '
 420       CONTINUE
!
           SHELL_STR = 'SHELL=/bin/sh'//CHAR(0)
           CALL PUTENV ( SHELL_STR(1:14) )
      END IF
!
! --- Execute a command line
!
      IS4 = SYSTEM ( COMMAND_STRING )
      SYSTEM2_SH = INT2(IS4)
!
      IF ( ILS .GT. 0 ) THEN
!
! -------- Restore prevuis value of SHELL environment variable
!
           DO 430 J3=1,LEN(SHELL_STR)
              SHELL_STR(J3:J3) = ' '
 430       CONTINUE
           SHELL_STR = SHELL_STR_OLD
           CALL PUTENV ( SHELL_STR(1:ILS) )
      END IF
      RETURN
      END  !#!  SYSTEM2_SH  #!#
