      SUBROUTINE GAMB_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  GAMB_DEFAULT.                       *
! *   GAMB_DEFAULT sets default values in glbc4 for GAMB -- program of   *
! *   automatic group delay ambiguity resolution, but not saves them.    *
! *                                                                      *
! *  ###  07-AUG-97  GAMB_DEFAULT  v1.0  (c)  L. Petrov  18-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, IVAL, ILEN, I_LEN
      REAL*8     VAL
      CHARACTER  STR*20
!
      GAMB_F_X_BAND      = GAMB_F_X_BAND__DEF
      GAMB_F_S_BAND      = GAMB_F_S_BAND__DEF
      GAMB_F_ION         = GAMB_F_ION__DEF
      GAMB_F_PREUSE      = GAMB_F_PREUSE__DEF
      GAMB_F_SAVE        = GAMB_F_SAVE__DEF
      GAMB_CUTOFF        = GAMB_CUTOFF__DEF
      GAMB_SPACING_CONST = GAMB_SPACING_CONST__DEF
      GAMB_MINOBS        = GAMB_MINOBS__DEF
      GAMB_IT            = GAMB_IT__DEF
      GAMB_BATCH         = GAMB_BATCH__DEF
!
! --- Examining GAMB_F_X_BAND
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_F_X_BAND', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE' ) THEN
                GAMB_F_X_BAND = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' ) THEN
                GAMB_F_X_BAND = .FALSE.
             ELSE
                CALL ERR_LOG ( 6831, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_F_X_BAND has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining GAMB_F_S_BAND
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_F_S_BAND', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE' ) THEN
                GAMB_F_S_BAND = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' ) THEN
                GAMB_F_S_BAND = .FALSE.
             ELSE
                CALL ERR_LOG ( 6832, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_F_S_BAND has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining GAMB_F_ION
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_F_ION', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE' ) THEN
                GAMB_F_ION = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' ) THEN
                GAMB_F_ION = .FALSE.
             ELSE
                CALL ERR_LOG ( 6833, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_F_ION has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining GAMB_F_PREUSE
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_F_PREUSE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:4) .EQ. 'GOOD' ) THEN
                GAMB_F_PREUSE = .TRUE.
             ELSE IF ( STR(1:3) .EQ. 'ALL' ) THEN
                GAMB_F_PREUSE = .FALSE.
             ELSE
                CALL ERR_LOG ( 6834, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_F_PREUSE has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining GAMB_F_SAVE
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_F_SAVE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE' ) THEN
                GAMB_F_SAVE = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' ) THEN
                GAMB_F_SAVE = .FALSE.
             ELSE
                CALL ERR_LOG ( 6835, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_F_SAVE has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining GAMB_CUTOFF
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_CUTOFF', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6836, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_CUTOFF has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. 1.D-10  .OR.  VAL .GT. 1.0D-6 ) THEN
                CALL ERR_LOG ( 6837, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_CUTOFF '//STR//' is out of the range '// &
     &              'range [1.D-10, 1.D-6]' )
                RETURN
           END IF
           GAMB_CUTOFF = VAL
      END IF
!
! --- Examining GAMB_SPACING_CONST
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_SPACING_CONST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6838, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_SPACING_CONST  has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( DABS(VAL).GT. 1.D-13  .AND. &
     &          ( VAL .LT. 1.D-9  .OR.  VAL .GT. 1.D-6 ) ) THEN
                CALL ERR_LOG ( 6839, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_SPACING_CONST '//STR//' is out of the '// &
     &              'range [1.D-9, 1.D-6] or 0' )
                RETURN
           END IF
           GAMB_SPACING_CONST = VAL
      END IF
!
! --- Examining GAMB_MINOBS
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_MINOBS', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6840, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_MINOBS has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 4  .OR.  IVAL .GT. 1000 ) THEN
                CALL ERR_LOG ( 6841, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_MINOBS '//STR//' is out of the '// &
     &              'range [4, 1000]' )
                RETURN
           END IF
           GAMB_MINOBS = IVAL
      END IF
!
! --- Examining GAMB_IT
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_IT', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6842, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_IT has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 0  .OR.  IVAL .GT. 5 ) THEN
                CALL ERR_LOG ( 6843, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_IT '//STR//' is out of the '// &
     &              'range [0, 5]' )
                RETURN
           END IF
           GAMB_IT = IVAL
      END IF
!
! --- Examining GAMB_BATCH
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'GAMB_BATCH', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE' ) THEN
                GAMB_BATCH = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' ) THEN
                GAMB_BATCH = .FALSE.
             ELSE
                CALL ERR_LOG ( 6844, IUER, 'GAMB_DEFAULT', 'Environment '// &
     &              'variable GAMB_BATCH has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  GAMB_DEFAULT  #!#
