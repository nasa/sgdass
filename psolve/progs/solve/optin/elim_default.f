      SUBROUTINE ELIM_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  ELIM.                               *
! *   ELIM_DEFAULT sets default values in glbc4 for ELIM -- program for  *
! *   outliers elimination/rejection in automatic mode.                  *
! *                                                                      *
! *  ###  17-SEP-97  ELIM_DEFAULT  v1.2  (c)  L. Petrov  30-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, ILEN, I_LEN
      REAL*8     VAL
      CHARACTER  STR*20
!
! --- First of all: system-wide defaults
!
      ELIM_MOD = ELIM_MOD__DEF
      ELIM_CNF = ELIM_CNF__DEF
      ELIM_THR = ELIM_THR__DEF
      ELIM_CUT = ELIM_CUT__DEF
      ELIM_VRB = ELIM_VRB__DEF
      ELIM_TYP = ELIM_TYP__DEF
      ELIM_UPD = ELIM_UPD__DEF
      ELIM_MSR = ELIM_MSR__DEF
!
! --- Examining ELIM_MOD
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_MOD', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                ELIM_MOD = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                ELIM_MOD = .FALSE.
             ELSE
                CALL ERR_LOG ( 6871, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_MOD has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining ELIM_CNF
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_CNF', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                ELIM_CNF = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                ELIM_CNF = .FALSE.
             ELSE
                CALL ERR_LOG ( 6872, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_CNF has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining ELIM_THR
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_THR', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, ELIM_THR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6873, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_THR has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining ELIM_CUT
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_CUT', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, ELIM_CUT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6874, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_CUT has wrong value: '//STR )
                RETURN
           END IF
           IF ( ELIM_CUT .LT. 0.0D0 ) ELIM_CUT = 0.0D0
      END IF
!
! --- Examining ELIM_VRB
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_VRB', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, ELIM_VRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6875, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_VRB has wrong value: '//STR )
                RETURN
           END IF
           IF ( ELIM_VRB .LT. 0  .OR.  ELIM_VRB .GT. 4 ) THEN
                CALL ERR_LOG ( 6876, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_VRB has wrong value: '//STR(1:I_LEN(STR))// &
     &              ' what is out of range [0, 4]' )
                RETURN
           END IF
      END IF
!
! --- Examining ELIM_TYP
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_TYP', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .NE. 'BA'  .AND.  STR(1:2) .NE. 'GL' ) THEN
                CALL ERR_LOG ( 6879, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_TYP has wrong value: "'//STR(1:I_LEN(STR))// &
     &              '" Acceptable values are only "BA" or "GL"' )
                RETURN
           END IF
           ELIM_TYP = STR
      END IF
!
! --- Examining ELIM_UPD
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_UPD', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, ELIM_UPD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6880, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_UPD has wrong value: '//STR )
                RETURN
           END IF
           IF ( ELIM_UPD .LT. 1  ) ELIM_UPD = 1
      END IF
!
! --- Examining ELIM_MSR
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ELIM_MSR', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, ELIM_MSR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6881, IUER, 'ELIM_DEFAULT', 'Environment '// &
     &              'variable ELIM_MSR has wrong value: '//STR )
                RETURN
           END IF
           IF ( ELIM_MSR .LT. 0.0D0 ) ELIM_MSR = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  ELIM_DEFAULT  #!#
