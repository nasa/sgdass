      SUBROUTINE SETFL_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  SETFL_DEFAULT.                      *
! *   SETFL_DEFAULT sets default values in glbc4 for SETFL.              *
! *                                                                      *
! *  ###  10-AUG-97  SETFL_DEFAULT  v1.1  (c)  L. Petrov  22-DEC-98  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, IVAL, ILEN, I_LEN
      REAL*8     VAL, MIN_VAL
      PARAMETER  ( MIN_VAL = 300.0D0 )
      CHARACTER  STR*20, STR1*20
!
! --- First of all: system-wide defaults
!
      SETFL_MDEG  = SETFL_MDEG__DEF
      DEF_CLOSPAN = DEF_CLOSPAN__DEF
      DEF_ATMSPAN = DEF_ATMSPAN__DEF
!
! --- Examining SETFL_MDEG
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'SETFL_MDEG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6861, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable SETFL_MDEG has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 1  .OR. IVAL .GT. 2 ) THEN
                CALL ERR_LOG ( 6862, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable SETFL_MDEG '//STR//' is out of range [1, 2]' )
                RETURN
           END IF
           SETFL_MDEG = IVAL
      END IF
!
! --- Examining DEF_CLOSPAN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'DEF_CLOSPAN', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6863, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable DEF_CLOSPAN has wrong value: '//STR )
                RETURN
           END IF
           IF ( VAL .LT. MIN_VAL ) THEN
                CALL CLRCH ( STR1 )
                WRITE ( UNIT=STR1, FMT='(F10.2)' ) MIN_VAL
                CALL CHASHL ( STR1 )
                CALL ERR_LOG ( 6864, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable DEF_CLOSPAN has wrong value: '//STR// &
     &              ' that is lower the minimal value: '//STR1 )
                RETURN
           END IF
           DEF_CLOSPAN = VAL
      END IF
!
! --- Examining DEF_ATMSPAN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'DEF_ATMSPAN', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6865, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable DEF_ATMSPAN has wrong value: '//STR )
                RETURN
           END IF
           IF ( VAL .LT. MIN_VAL ) THEN
                CALL CLRCH ( STR1 )
                WRITE ( UNIT=STR1, FMT='(F10.2)' ) MIN_VAL
                CALL CHASHL ( STR1 )
                CALL ERR_LOG ( 6866, IUER, 'SETFL_DEFAULT', 'Environment '// &
     &              'variable DEF_ATMSPAN has wrong value: '//STR// &
     &              ' that is lower the minimal value: '//STR1 )
                RETURN
           END IF
           DEF_ATMSPAN = VAL
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  SETFL_DEFAULT  #!#
