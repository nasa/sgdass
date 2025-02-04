      FUNCTION   SPD_SIGTERM_PROC ( )
! ************************************************************************
! *                                                                      *
! *   Signal hadler routine for child termination
! *                                                                      *
! * ### 12-DEV-2014  SPD_SIGTERM_PROC v1.0 (c) L. Petrov 23-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  SPD_SIGTERM_PROC
      INCLUDE   'spd.i'
      INCLUDE   'spd_common.i'
#ifdef GNU
      INTRINSIC FLUSH
#endif
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      WRITE ( LUN_SER, '(A)' ) GET_CDATE_MS()//' received terminate signal'
      CALL FLUSH ( LUN_SER )
      CALL CLOSE ( LUN_SER )
      CALL UNLINK ( FILE_PID(1:I_LEN(FILE_PID))//CHAR(0) )
      CALL EXIT  ( 0 ) 
!
      SPD_SIGTERM_PROC = 0
      RETURN
      END  FUNCTION  SPD_SIGTERM_PROC  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_SIGCLD_PROC ( )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_SIGCLD_PROC 
! *                                                                      *
! * ### 23-JAN-2015  SPD_SIGCLD_PROC  v1.0 (c) L. Petrov 23-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_common.i'
      INTEGER*4  SPD_SIGCLD_PROC
      INTEGER*8  SIG
      INTEGER*4  PID, IS, STATUS, EXIT_CODE, OLD_SPD_NUM_PROC, &
     &           OLD_SPD_PIDS(SPD__M_REQ), OLD_REM_FDS(SPD__M_REQ), J1, J2
      INTEGER*8  BLOCK_MASK
      CHARACTER  STR*128, STR1*128
      INTEGER*4  PID_CHLD, WNOHANG, SIGCLD, SIGALRM, SIG_BLOCK, SIG_UNBLOCK, ARG_LEN
      INTEGER*4, EXTERNAL :: GETPID, WAITPID, SIGEMPTYSET, SIGADDSET, SIGPROCMASK, &
     &                       ILEN, I_LEN
!
! --- Get constants
!
      CALL GET_SYSTEM_CONSTANT ( 'WNOHANG', WNOHANG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIG_BLOCK',   SIG_BLOCK,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIG_UNBLOCK', SIG_UNBLOCK, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGCLD',  SIGCLD,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGALRM', SIGALRM, ARG_LEN )
!
! --- Block signals CLD and ALRM
!
      IS = SIGEMPTYSET ( BLOCK_MASK )
      IS = SIGADDSET   ( BLOCK_MASK, %VAL(SIGCLD) )
      IS = SIGADDSET   ( BLOCK_MASK, %VAL(SIGALRM) )
      IS = SIGPROCMASK ( %VAL(SIG_BLOCK), BLOCK_MASK, %VAL(0) )
!
      IS = WAITPID ( %VAL(-1), STATUS, %VAL(WNOHANG) )
      IF ( IS > 0 ) THEN
           EXIT_CODE = STATUS/256
           CALL CLRCH ( STR )
           CALL INCH  ( IS, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( EXIT_CODE, STR1 )
           CALL SPD_LOG ( 'SIGCLD', 0, 'I', ' ', 'Process '//STR(1:I_LEN(STR))// &
     &         ' is terminated with exit code '//STR1 )
           DO 410 J1=1,SPD_NUM_PROC
              IF ( IS == SPD_PIDS(J1) ) THEN
                   CALL CLOSE ( %VAL(REM_FDS(J1)) )
              END IF
 410       CONTINUE 
!
! -------- Update process list
!
           OLD_SPD_PIDS = SPD_PIDS
           OLD_REM_FDS  = REM_FDS
           OLD_SPD_NUM_PROC = SPD_NUM_PROC 
           SPD_NUM_PROC = 0
           DO 420 J2=1,OLD_SPD_NUM_PROC
              IF ( OLD_SPD_PIDS(J2) .NE. IS ) THEN
                   SPD_NUM_PROC = SPD_NUM_PROC + 1
                   SPD_PIDS(SPD_NUM_PROC) = OLD_SPD_PIDS(J2)
                   REM_FDS(SPD_NUM_PROC)  = OLD_REM_FDS(J2)  
              END IF
 420       CONTINUE 
      END IF
!
! --- Restore signal processing
!
      IS = SIGPROCMASK ( %VAL(SIG_UNBLOCK), BLOCK_MASK, %VAL(0) )
      SPD_SIGCLD_PROC = 0
      RETURN
      END  FUNCTION   SPD_SIGCLD_PROC  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_ALARM_PROC ( )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_ALARM_PROC
! *                                                                      *
! * ### 24-JAN-2015  SPD_ALARM_PROC  v1.0 (c) L. Petrov 24-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_common.i'
      INTEGER*4  SPD_ALARM_PROC 
      INTEGER*4, EXTERNAL :: SIGNAL, ALARM
      TYPE     ( SPD_COM__TYPE  ) :: SEND_COM
      INTEGER*4  MY_PID, IS, SIGALRM, ARG_LEN
      CHARACTER  MESSAGE*128
      INTEGER*4, EXTERNAL :: ALRAM, GETPID, SOCK_WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SIGALRM', SIGALRM, ARG_LEN )
      MY_PID = GETPID()
      SEND_COM%VERB = 'wait_1s '
      SEND_COM%LEN  = 0
      IS = SOCK_WRITE ( SUB_REM_FD, SIZEOF(SEND_COM), SEND_COM, MESSAGE )
      IS = ALARM ( %VAL(SPD__ALARM_INT) )
      SPD_ALARM_PROC = 0
      RETURN 
      END  FUNCTION   SPD_ALARM_PROC  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_UPDATE_PROC ( PID, REM_FD )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_UPDATE_PROC 
! *                                                                      *
! * ### 23-JAN-2015  SPD_UPDATE_PROC  v1.0 (c) L. Petrov 23-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_common.i'
      INTEGER*4  SPD_UPDATE_PROC
      INTEGER*4  PID, REM_FD
      INTEGER*8  SIG
      INTEGER*4  IS, STATUS, EXIT_CODE, OLD_SPD_NUM_PROC, &
     &           OLD_SPD_PIDS(SPD__M_REQ), J1
      INTEGER*8  BLOCK_MASK
      CHARACTER  STR*128, STR1*128
      INTEGER*4  PID_CHLD, WNOHANG, SIGCLD, SIGALRM, SIG_BLOCK, SIG_UNBLOCK, ARG_LEN
      INTEGER*4, EXTERNAL :: GETPID, WAITPID, SIGEMPTYSET, SIGADDSET, SIGPROCMASK, &
     &                       ILEN, I_LEN
!
! --- Get constants
!
      CALL GET_SYSTEM_CONSTANT ( 'WNOHANG', WNOHANG, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIG_BLOCK',   SIG_BLOCK,   ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIG_UNBLOCK', SIG_UNBLOCK, ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGCLD',  SIGCLD,  ARG_LEN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGALRM', SIGALRM, ARG_LEN )
!
! --- Block signals CLD and ALRM
!
      IS = SIGEMPTYSET ( BLOCK_MASK )
      IS = SIGADDSET   ( BLOCK_MASK, %VAL(SIGCLD) )
      IS = SIGADDSET   ( BLOCK_MASK, %VAL(SIGALRM) )
      IS = SIGPROCMASK ( %VAL(SIG_BLOCK), BLOCK_MASK, %VAL(0) )
!
      SPD_NUM_PROC = SPD_NUM_PROC + 1
      SPD_PIDS(SPD_NUM_PROC) = PID
      REM_FDS(SPD_NUM_PROC)  = REM_FD
!
! --- Restore signal processing
!
      IS = SIGPROCMASK ( %VAL(SIG_UNBLOCK), BLOCK_MASK, %VAL(0) )
      CALL CLRCH ( STR )
      CALL INCH  ( PID, STR )
      CALL SPD_LOG ( 'MAIN  ', 0, 'I', 'SPD_UPDATE_PROC', 'Launched '// &
     &    'subprocess '//STR )
      SPD_UPDATE_PROC = 0
      RETURN
      END  FUNCTION   SPD_UPDATE_PROC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_LOG ( REQ_ID, NERR, SEV, ROUTINE, MESSAGE )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_LOG 
! *                                                                      *
! *  ### 22-JAN-2015     SPD_LOG   v1.0 (c)  L. Petrov  22-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_common.i'
      INTEGER*4  NERR
      CHARACTER  REQ_ID*(*), SEV*(*), ROUTINE*(*), MESSAGE*(*)
      TYPE     ( SPD_COM__TYPE  ) ::  SEND_COM
      CHARACTER  STR*128
      INTEGER*4  IS 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, SOCK_WRITE 
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23
!
      IF ( ILEN(ROUTINE) == 0 ) THEN
           WRITE ( UNIT=LUN_SER, FMT='(A,1X,A,1X,A,1X,I4,1X,A)') GET_CDATE_MS(), &
     &             REQ_ID, SEV, NERR, MESSAGE(1:I_LEN(MESSAGE))
!!           WRITE ( UNIT=6, FMT='(A,1X,A,1X,A,1X,I4,1X,A,1X,A)') GET_CDATE_MS(), &
!!     &             REQ_ID, SEV, NERR, MESSAGE(1:I_LEN(MESSAGE))
         ELSE 
           WRITE ( UNIT=LUN_SER, FMT='(A,1X,A,1X,A,1X,I4,1X,A,1X,A)') GET_CDATE_MS(), &
     &             REQ_ID, SEV, NERR, ROUTINE(1:I_LEN(ROUTINE)), MESSAGE(1:I_LEN(MESSAGE))
!!           WRITE ( UNIT=6, FMT='(A,1X,A,1X,A,1X,I4,1X,A,1X,A)') GET_CDATE_MS(), &
!!     &             REQ_ID, SEV, NERR, ROUTINE(1:I_LEN(ROUTINE)), MESSAGE(1:I_LEN(MESSAGE))
      END IF
      CALL FLUSH ( LUN_SER )
      IF ( SEV .NE. 'I'            .AND. &
     &     REQ_ID(1:4) .NE. 'MAIN' .AND. &
     &     NERR > 0                      ) THEN
!
           SEND_COM%VERB = 'error   '
           SEND_COM%LEN  = ILEN(MESSAGE)
           IS = SOCK_WRITE ( SUB_REM_FD, SIZEOF(SEND_COM), SEND_COM, STR )
           IS = SOCK_WRITE ( SUB_REM_FD, ILEN(MESSAGE),    MESSAGE,  STR )
           SEND_COM%VERB = 'bye   '
           SEND_COM%LEN  =  0
           IS = SOCK_WRITE ( SUB_REM_FD, SIZEOF(SEND_COM), SEND_COM, STR )
      END IF
!
      RETURN
      END  SUBROUTINE  SPD_LOG  !#!#
