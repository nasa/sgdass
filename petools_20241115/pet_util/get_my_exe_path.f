      SUBROUTINE GET_MY_EXE_PATH ( EXE_PATH, IUER )
! ************************************************************************
! *                                                                      *
! *   Rouitine GET_MY_EXE_PATH returns the full path to the executable.  *
! *   NB: it will work only under Linux, since it relies on /proc        *
! *   pseudo file system.                                                *
! *                                                                      *
! * ### 06-OCT-2012 GET_MY_EXE_PATH v3.0 (c)  L. Petrov  23-APR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  EXE_PATH*(*)
      INTEGER*4  IUER
!
! --- It sounds insane, but macos wuill return "cannot allocate memory" if STR is less than 2048
!
      CHARACTER  PID_STR*7, PROC_LINE*32, STR*4096 
      INTEGER*4  PID, IS
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, READLINK 
#if DARWIN
      INTEGER*4, EXTERNAL ::  PROC_PIDPATH
#endif
!
      CALL CLRCH ( EXE_PATH )
      PID = GETPID()
#if LINUX
      CALL INCH   ( PID, PID_STR )
      CALL CHASHL ( PID_STR )
      PROC_LINE = '/proc/'//PID_STR(1:I_LEN(PID_STR))//'/exe'
      CALL CLRCH ( EXE_PATH )
      IS = READLINK ( %REF(PROC_LINE(1:I_LEN(PROC_LINE))//CHAR(0)), &
     &                %REF(EXE_PATH), %VAL(LEN(EXE_PATH)) )
      IF ( IS < 0 ) THEN
           CALL CLRCH ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1001, IUER, 'GET_MY_EXE_PATH', 'Trap of internal '// &
     &         'control in an attempt to learn the full path to the current '// &
     &         'executable: we tried to read symbolic link '// &
     &          PROC_LINE(1:I_LEN(PROC_LINE))//' but system replied '//STR )
           RETURN 
      END IF
#endif
#if DARWIN
      IS = PROC_PIDPATH ( %VAL(PID), STR )
      IF ( IS .LE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 1002, IUER, 'GET_MY_EXE_PATH', 'Trap of internal '// &
     &         'control in an attempt to learn the full path to the current '// &
     &         'executable: system replied '//STR )
           RETURN 
      END IF
      EXE_PATH = STR
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_MY_EXE_PATH  !#!  
