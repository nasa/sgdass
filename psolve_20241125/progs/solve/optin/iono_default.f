      SUBROUTINE IONO_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  IONO_DEFAULT.                       *
! *   IONO_DEFAULT sets default values in glbc4 for IONO -- program of   *
! *   calculation IONOSPHERE calibration.                                *
! *                                                                      *
! *  ###  10-AUG-97  IONO_DEFAULT  v1.0  (c)  L. Petrov  10-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, IVAL, ILEN, I_LEN
      REAL*8     VAL
      CHARACTER  STR*20
!
! --- First of all: sustem-wide defaults
!
      IONO_VERBOSE       = IONO_VERBOSE__DEF
      IONO_SEEK          = IONO_SEEK__DEF
!
! --- Examining IONO_VERBOSE
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'IONO_VERBOSE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'   ) THEN
                IONO_VERBOSE = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &          STR(1:3) .EQ. 'OFF' ) THEN
                IONO_VERBOSE = .FALSE.
             ELSE
                CALL ERR_LOG ( 6851, IUER, 'IONO_DEFAULT', 'Environment '// &
     &              'variable IONO_VERBOSE has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining IONO_SEEK
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'IONO_SEEK', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6852, IUER, 'IONO_DEFAULT', 'Environment '// &
     &              'variable IONO_SEEK has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 1 ) THEN
                CALL ERR_LOG ( 6853, IUER, 'IONO_DEFAULT', 'Environment '// &
     &              'variable IONO_SEEK '//STR//' is less than 1 ' )
                RETURN
           END IF
           IONO_SEEK = IVAL
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  IONO_DEFAULT  #!#
