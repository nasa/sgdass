      SUBROUTINE SET_SIGNAL_CTRLC ( IPAR_PETOOLS )
! ************************************************************************
! *                                                                      *
! *   Routine SET_SIGNAL_CTRLC sets the signal handler on Ctrl/C         *
! *                                                                      *
! * ### 21-JUN-2007 SET_SIGNAL_CTRLC v1.1 (c) L. Petrov  17-OCT-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IPAR_PETOOLS, IS, LN, SIGTERM, SIGHUP, SIGINT
      ADDRESS__TYPE :: SAVED_ALARM_SIGNAL
      INTEGER*4  IPAR
      COMMON   / PETOOLS_SIGNAL_HANDLER / SAVED_ALARM_SIGNAL, IPAR
      INTEGER*4, EXTERNAL :: SIGNAL, PETOOLS_SIGNAL_HANDLER_ROUTINE 
!
      IPAR = IPAR_PETOOLS
      CALL GET_SYSTEM_CONSTANT ( 'SIGTERM', SIGTERM, LN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGHUP',  SIGHUP,  LN )
      CALL GET_SYSTEM_CONSTANT ( 'SIGINT',  SIGINT,  LN )
      IS = SIGNAL ( %VAL(SIGTERM), PETOOLS_SIGNAL_HANDLER_ROUTINE )
      IS = SIGNAL ( %VAL(SIGHUP),  PETOOLS_SIGNAL_HANDLER_ROUTINE )
      IS = SIGNAL ( %VAL(SIGINT),  PETOOLS_SIGNAL_HANDLER_ROUTINE )
      RETURN
      END  SUBROUTINE  SET_SIGNAL_CTRLC  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   PETOOLS_SIGNAL_HANDLER_ROUTINE ( )
! ************************************************************************
! *                                                                      *
! *   Signal hadler routine for child termination
! *                                                                      *
! * # 06-SEP-2006 SIGNAL_HANDLER_ROUTINE v1.1 (c) L. Petrov 17-OCT-2019 #*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  PETOOLS_SIGNAL_HANDLER_ROUTINE 
      ADDRESS__TYPE :: SAVED_ALARM_SIGNAL
      INTEGER*4  IPAR
      COMMON   / PETOOLS_SIGNAL_HANDLER / SAVED_ALARM_SIGNAL, IPAR
#ifdef GNU
      INTRINSIC FLUSH
#endif
!
      CALL FLUSH ( 6 )
      IF ( IPAR == 0 ) THEN
         ELSE IF ( IPAR == 1 ) THEN
           CALL LIB$WAIT ( 0.5D0 )
           CALL FLUSH    ( 6 )
           WRITE ( 6, '(A)' ) ' '
         ELSE IF ( IPAR == 2 ) THEN
           CALL LIB$WAIT ( 0.5D0 )
           CALL FLUSH    ( 6 )
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'The process is stopped by Ctrl/C'
         ELSE IF ( IPAR == 3 ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'The process is stopped by Ctrl/C'
      END IF
      CALL EXIT ( 1 ) 
!
      PETOOLS_SIGNAL_HANDLER_ROUTINE = 1
      RETURN
      END  FUNCTION   PETOOLS_SIGNAL_HANDLER_ROUTINE   !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   PETOOLS_VOID_SIGNAL_FUNCTION()
      INTEGER*4  PETOOLS_VOID_SIGNAL_FUNCTION
      PETOOLS_VOID_SIGNAL_FUNCTION = 0
      RETURN
      END  FUNCTION   PETOOLS_VOID_SIGNAL_FUNCTION  !#!  
