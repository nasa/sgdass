      SUBROUTINE PIMA_READ_TIME_FLAG ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_READ_TIME_FLAG 
! *                                                                      *
! * ## 10-JUL-2008 PIMA_READ_TIME_FLAG v1.2 (c) L. Petrov 19-NOV-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER, ALLOCATABLE :: BUF(:)*40
      INTEGER*4  J1, J2, J3, NP, NBUF, IND_OBS, IND_AP, IOS, &
     &           MAX_NUM_UV, IER
      REAL*8     WEI
      CHARACTER  STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, MAX_I4
!
! --- First learn how many records are in the file
!
      NP = PIM__MOBS
      DO 410 J1=1,PIM%NOBS
         MAX_NUM_UV = MAX_I4 ( PIM__MUVS, PIM%OBS(J1)%NUM_EPC )
         NP = NP + MAX_NUM_UV
 410  CONTINUE 
!
      ALLOCATE ( BUF(NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( NP*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 7431, IUER, 'PIMA_READ_TIME_FLAG', 'Failure in '// &
     &         'an attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN 
      END IF
!
! --- Read the external flag file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%TIME_FLAG_FILE, NP, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7432, IUER, 'PIMA_READ_TIME_FLAG', 'Failure in '// &
     &         'an attempt to read the time flag file '// &
     &         PIM%CONF%TIME_FLAG_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      IF ( BUF(1) == '#  PIMA_FLAG  Format of 2008.07.09' ) THEN
           BUF(1) = '# PIMA_FLAG  Format of 2008.07.09 '
      END IF
!
! --- Check the first and the last line of the weight file
!
      IF ( BUF(1)(1:LEN(PIMA__FLAG_LABEL)) .NE. PIMA__FLAG_LABEL ) THEN
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 7433, IUER, 'PIMA_READ_TIME_FLAG', 'Failure in '// &
     &         'parsing the first line of the time flag file '// &
     &         PIM%CONF%TIME_FLAG_FILE(1:I_LEN(PIM%CONF%TIME_FLAG_FILE))// &
     &         ' -- the first line is '//STR(1:LEN(BUF(1)))// &
     &         ' while the label '//PIMA__FLAG_LABEL//' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
!@      IF ( BUF(NPUB)(1:LEN(PIMA__FLAG_LABEL)) .NE. PIMA__FLAG_LABEL ) THEN
!@           STR = BUF(1)
!@           CALL TRAN ( 13, STR, STR )
!@           CALL ERR_LOG ( 7434, IUER, 'PIMA_READ_TIME_FLAG', 'Failure in '// &
!@     &         'parsing the last line of the time flag file '// &
!@     &         PIM%CONF%TIME_FLAG_FILE(1:I_LEN(PIM%CONF%TIME_FLAG_FILE))// &
!@     &         ' -- the first line is '//STR(1:LEN(BUF(1)))// &
!@     &         ' while the label '//PIMA__FLAG_LABEL//' was expected' )
!@           DEALLOCATE ( BUF )
!@           RETURN 
!@      END IF
!
! --- Parse the file
!
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
         READ ( UNIT=BUF(J2), FMT=110, IOSTAT=IOS ) IND_OBS, IND_AP, WEI
 110     FORMAT ( I6, 2X, I4, 2X, F8.5 )
!
! ------ Allocate memory if necessary
!
         IF ( IND_OBS < 1 .OR. IND_OBS > PIM%NOBS ) GOTO 420
         IF ( .NOT. ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) ) THEN
              MAX_NUM_UV = MAX_I4 ( PIM__MUVS, PIM%OBS(IND_OBS)%NUM_EPC )
              ALLOCATE ( PIM%OBS(IND_OBS)%USER_FLAG(MAX_NUM_UV), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7435, IUER, 'PIMA_READ_TIME_FLAG', &
     &                 'Failure to allocate memory for FLAG array for the '// &
     &                 BUF(1:6)//'-th observation' )
                   DEALLOCATE ( BUF )
                   RETURN 
              END IF
              PIM%OBS(IND_OBS)%USER_FLAG = 1.0
         END IF
!
! ------ Put the flag (weight) in the appropriate slot
!
         IF ( IND_OBS .GE. 1  .AND.  IND_OBS .LE. PIM%NOBS ) THEN
              IF ( PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) > 0 ) THEN
                   IF ( IND_AP .GE. 1 .AND. IND_AP .LE. PIM%OBS(IND_OBS)%NUM_EPC(PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)) ) THEN
                        PIM%OBS(IND_OBS)%USER_FLAG(IND_AP) = WEI
                   END IF
              END IF
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_READ_TIME_FLAG  !#!#
