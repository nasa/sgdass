      SUBROUTINE PROC_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  PROC.                               *
! *   PROC_DEFAULT sets default values in glbc4 for PROC -- program for  *
! *   computation of normal system.                                      *
! *                                                                      *
! *  ###  10-JUL-98  PROC_DEFAULT  v1.0  (c)  L. Petrov  10-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4    MVALS1, IL
      PARAMETER  ( MVALS1     = SNGCHK_ACT__LSIN )
      CHARACTER  VALS1(MVALS1)*16, STR*32
      DATA       VALS1 / 'NONE            ', &
     &                   'WARNING         ', &
     &                   'REPARAMETERIZE  ', &
     &                   'STOP            '   /
      INTEGER*4  IUER, IER, ILEN, LTM_DIF, I_LEN
!
! --- First of all: system-wide defaults
!
      SNGCHK_ACTION = SNGCHK_ACTION__DEF
      SNGCHK_SOUMIN = SNGCHK_SOUMIN__DEF
      SNGCHK_STAMIN = SNGCHK_STAMIN__DEF
      SNGCHK_BASMIN = SNGCHK_BASMIN__DEF
!
! --- Examining SNGCHK_ACTION
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SNGCHK_ACTION', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IL = LTM_DIF ( 0, MVALS1, VALS1, STR )
           IF ( IL .LE. 0 ) THEN
                CALL ERR_LOG ( 6891, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_ACTION has wrong value: '//STR )
                RETURN
           END IF
           SNGCHK_ACTION = IL
      END IF
!
! --- Examining SNGCHK_SOUMIN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SNGCHK_SOUMIN', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) STR='0'
           CALL IFOR_MEN ( STR, SNGCHK_SOUMIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6892, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_SOUMIN has wrong value: '//STR )
                RETURN
           END IF
           IF ( ELIM_VRB .LT. 0  ) THEN
                CALL ERR_LOG ( 6893, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_SOUMIN has wrong value: '// &
     &               STR(1:I_LEN(STR))//' -- non-negative values was expected' )
                RETURN
           END IF
      END IF
!
! --- Examining SNGCHK_STAMIN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SNGCHK_STAMIN', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) STR='0'
           CALL IFOR_MEN ( STR, SNGCHK_STAMIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6894, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_STAMIN has wrong value: '//STR )
                RETURN
           END IF
           IF ( SNGCHK_STAMIN .LT. 0  ) THEN
                CALL ERR_LOG ( 6895, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_STAMIN has wrong value: '// &
     &               STR(1:I_LEN(STR))//' -- non-negative values was expected' )
                RETURN
           END IF
      END IF
!
! --- Examining SNGCHK_BASMIN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SNGCHK_BASMIN', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) STR='0'
           CALL IFOR_MEN ( STR, SNGCHK_BASMIN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6896, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_BASMIN has wrong value: '//STR )
                RETURN
           END IF
           IF ( SNGCHK_BASMIN .LT. 0  ) THEN
                CALL ERR_LOG ( 6897, IUER, 'PROC_DEFAULT', 'Environment '// &
     &              'variable SNGCHK_BASMIN has wrong value: '// &
     &               STR(1:I_LEN(STR))//' -- non-negative values was expected' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PROC_DEFAULT  #!#
