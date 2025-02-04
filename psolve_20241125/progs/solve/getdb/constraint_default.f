      SUBROUTINE CONSTRAINT_DEFAULT ( ATM_CNST, CLO_CNST, POL_CNST, &
     &                                UT1_CNST, GRR_CNST, GRO_CNST, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CONSTRAINT_DEFAULT  sets defaults values of the           *
! *   constraints. It firstly copieed constants defined in glbc4 and     *
! *   then tries to read environment variables.                          *
! *                                                                      *
! * ###  05-NOV-97  CONSTRAINT_DEFAULT  v1.0 (c) L. Petrov 05-NOV-97 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, IVAL
      REAL*8     ATM_CNST, CLO_CNST, POL_CNST, UT1_CNST, GRR_CNST, GRO_CNST
      REAL*8     VAL
      CHARACTER  STR*20
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ATM_CNST = ATM_CNST__DEF
      CLO_CNST = CLO_CNST__DEF
      POL_CNST = POL_CNST__DEF
      UT1_CNST = UT1_CNST__DEF
      GRR_CNST = GRR_CNST__DEF
      GRO_CNST = GRO_CNST__DEF
!
! --- Examining ATM_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ATM_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6701, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable ATM_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6702, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable ATM_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           ATM_CNST = VAL
      END IF
!
! --- Examining CLO_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'CLO_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6703, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable CLO_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6704, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable CLO_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           CLO_CNST = VAL
      END IF
!
! --- Examining POL_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'POL_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6705, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable POL_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6706, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable POL_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           POL_CNST = VAL
      END IF
!
! --- Examining UT1_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'UT1_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6707, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable UT1_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6708, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable UT1_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           UT1_CNST = VAL
      END IF
!
! --- Examining GRR_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'GRR_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6709, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable GRR_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6710, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable GRR_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           GRR_CNST = VAL
      END IF
!
! --- Examining GRO_CNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'GRO_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6711, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable GRO_CNST has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( VAL .LT. -1.D-18  .OR.  VAL .GT. 1.D9 ) THEN
                CALL ERR_LOG ( 6712, IUER, 'CONSTRAINT_DEFAULT', &
     &              'Environment variable GRO_CNST '//STR//' is out of the '// &
     &              'range [-1.D-18, 1.D9]' )
                RETURN
           END IF
           GRO_CNST = VAL
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  CONSTRAINT_DEFAULT  #!#
