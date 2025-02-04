      SUBROUTINE REWAY_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  REWAY.                              *
! *   REWAY_DEFAULT sets default values in glbc4 for REWAY -- program of *
! *   calcultaions constans which added quadratically to weights will    *
! *   reduce the ratio of sum of sqares of post-fit residuals to its     *
! *   mathematical expactation (or by another words: chi squares per     *
! *   degree of freeedom) to unity.                                      *
! *                                                                      *
! *  ###  07-AUG-97  REWAY_DEFAULT  v2.0 (c)  L. Petrov  21-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      CHARACTER  STR*80
!
      INTEGER*4  IUER, IER, IVAL, ILEN, I_LEN
      REAL*8     VAL
!
! --- Setting system-wise defaults
!
      REWAY_FLODEL   = REWAY_FLODEL__DEF
      REWAY_FLORATE  = REWAY_FLORATE__DEF
      REWAY_CEIDEL   = REWAY_CEIDEL__DEF
      REWAY_CEIRATE  = REWAY_CEIRATE__DEF
      REWAY_CHITOL   = REWAY_CHITOL__DEF
      REWAY_MAXIT    = REWAY_MAXIT__DEF
      REWAY_VERBOSE  = REWAY_VERBOSE__DEF
      REWAY_TYPE     = REWAY_TYPE__DEF
      REWAY_NEWUPD   = REWAY_NEWUPD__DEF
      REWAY_FALLBACK = REWAY_FALLBACK__DEF
!
! --- Examining REWAY_FLODEL
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'REWAY_FLODEL', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6821, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_FLODEL has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-10  .OR.  VAL .GT. 1000.0 ) THEN
                CALL ERR_LOG ( 6822, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_FLODEL '//STR//' is out of the range '// &
     &              'range [0, 1000.0]' )
                RETURN
           END IF
           REWAY_FLODEL = VAL
      END IF
!
! --- Examining REWAY_FLORATE
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'REWAY_FLORATE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6823, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_FLORATE has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-10  .OR.  VAL .GT. 10000.0 ) THEN
                CALL ERR_LOG ( 6824, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_FLORATE '//STR//' is out of the range '// &
     &              'range [0, 10000.0]' )
                RETURN
           END IF
           REWAY_FLORATE = VAL
      END IF
!
! --- Examining REWAY_CEIDEL
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'REWAY_CEIDEL', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6825, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CEIDEL has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-10  .OR.  VAL .GT. 1000.0 ) THEN
                CALL ERR_LOG ( 6826, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CEIDEL '//STR//' is out of the range '// &
     &              'range [0, 1000.0]' )
                RETURN
           END IF
           REWAY_CEIDEL = VAL
      END IF
!
! --- Examining REWAY_CEIRATE
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'REWAY_CEIRATE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6827, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CEIRATE has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-10  .OR.  VAL .GT. 10000.0 ) THEN
                CALL ERR_LOG ( 6828, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CEIRATE '//STR//' is out of the range '// &
     &              'range [0, 10000.0]' )
                RETURN
           END IF
           REWAY_CEIRATE = VAL
      END IF
!
! --- Examining REWAY_CHITOL
!
      CALL GETENVAR ( 'REWAY_CHITOL', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6829, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CHITOL has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. 0.0001  .OR.  VAL .GT. 1.0 ) THEN
                CALL ERR_LOG ( 6830, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_CHITOL '//STR//' is out of the range '// &
     &              'range [0.0001, 1.0]' )
                RETURN
           END IF
           REWAY_CHITOL = VAL
      END IF
!
! --- Examining REWAY_MAXIT
!
      CALL GETENVAR ( 'REWAY_MAXIT', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6831, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_MAXIT has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 1  .OR.  IVAL .GT. 100 ) THEN
                CALL ERR_LOG ( 6832, IUER, 'REWAY_DEFAULT', 'Environment '// &
     &              'variable REWAY_MAXIT '//STR//' is out of the range '// &
     &              'range [1, 100]' )
                RETURN
           END IF
           REWAY_MAXIT = IVAL
      END IF
!
! --- Examining REWAY_VERBOSE
!
      CALL GETENVAR ( 'REWAY_VERBOSE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN   ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'OFF'  .OR.  STR(1:5) .EQ. 'FALSE' ) THEN
                REWAY_VERBOSE = .FALSE.
             ELSE IF ( STR(1:2) .EQ. 'ON'  .OR.  STR(1:4) .EQ. 'TRUE' ) THEN
                REWAY_VERBOSE = .TRUE.
             ELSE
                CALL ERR_LOG ( 6833, IUER, 'REWAY_DEFAULT', 'Wrong value '// &
     &              'of environment variable REWAY_VERBOSE: "'// &
     &               STR(1:I_LEN(STR))//'"   ' )
                RETURN
           END IF
      END IF
!
! --- Examining REWAY_TYPE
!
      CALL GETENVAR ( 'REWAY_TYPE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN   ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) THEN
                REWAY_TYPE = 'NO'
             ELSE IF ( STR(1:2) .EQ. 'GL' ) THEN
                REWAY_TYPE = 'GL'
             ELSE IF ( STR(1:2) .EQ. 'ST' ) THEN
                REWAY_TYPE = 'ST'
             ELSE IF ( STR(1:2) .EQ. 'BA' ) THEN
                REWAY_TYPE = 'BA'
             ELSE
                CALL ERR_LOG ( 6834, IUER, 'REWAY_DEFAULT', 'Wrong value '// &
     &              'of environment variable REWAY_TYPE: "'// &
     &               STR(1:I_LEN(STR))//'"   Acceptable values would be: '// &
     &              '"NO", "GL", "ST", "BA"' )
                RETURN
           END IF
      END IF
!
! --- Examining REWAY_NEWUPD
!
      CALL GETENVAR ( 'REWAY_NEWUPD', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN   ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'OFF'  .OR.  STR(1:5) .EQ. 'FALSE' ) THEN
                REWAY_NEWUPD = .FALSE.
             ELSE IF ( STR(1:2) .EQ. 'ON'  .OR.  STR(1:4) .EQ. 'TRUE' ) THEN
                REWAY_NEWUPD = .TRUE.
             ELSE
                CALL ERR_LOG ( 6835, IUER, 'REWAY_DEFAULT', 'Wrong value '// &
     &              'of environment variable REWAY_NEWUPD: "'// &
     &               STR(1:I_LEN(STR))//'"   ' )
                RETURN
           END IF
      END IF
!
! --- Examining REWAY_NEWUPD
!
      CALL GETENVAR ( 'REWAY_FALLBACK', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN   ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'OFF'  .OR.  STR(1:5) .EQ. 'FALSE' ) THEN
                REWAY_FALLBACK = .FALSE.
             ELSE IF ( STR(1:2) .EQ. 'ON'  .OR.  STR(1:4) .EQ. 'TRUE' ) THEN
                REWAY_FALLBACK = .TRUE.
             ELSE
                CALL ERR_LOG ( 6836, IUER, 'REWAY_DEFAULT', 'Wrong value '// &
     &              'of environment variable REWAY_FALLBACK: "'// &
     &               STR(1:I_LEN(STR))//'"   ' )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  REWAY_DEFAULT  #!#
