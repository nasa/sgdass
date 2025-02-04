      SUBROUTINE PIMA_GET_KEY_R8 ( PIM, K_FIL, TABLE_NAME, KEY_NAME, &
     &                             VAL_R8, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_GET_KEY_R8 inquires the table of keys      *
! *   of FITS file gets the value of key KEY_NAME from the table with    *
! *   NAME TABLE_NAME.                                                   *
! *                                                                      *
! *   Special case: if TABLE_NAME == ' ', then the keys from the first   *
! *   header table are examined.                                         *
! *                                                                      *
! * ## 06-JAN-2006   PIMA_GET_KEY_R8  v1.0 (c) L. Petrov 06-JAN-2006  ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  TABLE_NAME*(*), KEY_NAME*(*)
      REAL*8     VAL_R8
      INTEGER*4  IUER
      INTEGER*4    MIND
      PARAMETER  ( MIND =   32 )
      INTEGER*4    K_FIL, J1, J2, J3, IK, IT, IND_TAB, LIND, IND(2,MIND), IER
      CHARACTER    REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//"'" )
      LOGICAL*4  FL_FOUND
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IK = I_LEN(KEY_NAME)
      IT = I_LEN(TABLE_NAME)
      IND_TAB = 0
      IF ( TABLE_NAME(1:IT) == ' ' ) IND_TAB = 1
      FL_FOUND = .FALSE.
      DO 410 J1=1,PIM%FILE(K_FIL)%L_HDR
         DO 420 J2=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( PIM%FILE(K_FIL)%KEY(J2,J1)(1:8) == 'EXTNAME' ) THEN
                 IF ( INDEX ( PIM%FILE(K_FIL)%KEY(J2,J1), &
     &                        TABLE_NAME(1:IT) ) > 0 ) THEN
                      IND_TAB = J1
                 END IF
            END IF
 420     CONTINUE 
!
         DO 430 J3=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( J1 == IND_TAB                                .AND. &
     &           PIM%FILE(K_FIL)%KEY(J3,J1)(1:IK) == KEY_NAME       ) THEN
!
                 CALL EXWORD ( PIM%FILE(K_FIL)%KEY(J3,J1)(10:), MIND, LIND, &
     &                         IND, REG, -3 )
                 READ ( UNIT=PIM%FILE(K_FIL)%KEY(J3,J1)(IND(1,1)+9:IND(2,1)+9), &
     &                  FMT='(F30.12)', IOSTAT=IER ) VAL_R8
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7051, IUER, 'PIMA_GET_KEY_R8', &
     &                    'Error in decoding keyword '//KEY_NAME(1:IK)// &
     &                    ' -- string '// &
     &                    PIM%FILE(K_FIL)%KEY(J3,J1)(IND(1,1)+9:IND(2,1)+9) )
                      RETURN 
                 END IF
                 FL_FOUND = .TRUE.
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      IF ( IND_TAB == 0 ) THEN
           CALL ERR_LOG ( 7052, IUER, 'PIMA_GET_KEY_R8', 'Table '// &
     &          TABLE_NAME(1:IT)//' was not found in FITS UV file '// &
     &          PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      IF ( .NOT. FL_FOUND ) THEN
           CALL ERR_LOG ( 7053, IUER, 'PIMA_GET_KEY_R8', 'Value of the '// &
     &         'keyword '//KEY_NAME(1:IK)//' in table '//TABLE_NAME(1:IT)// &
     &         ' was not found in FITS UV file '//PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_KEY_R8  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_GET_KEY_I4 ( PIM, K_FIL, TABLE_NAME, KEY_NAME, &
     &                             VAL_I4, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_GET_KEY_I4 inquires the table of keys      *
! *   of FITS file gets the value of key KEY_NAME from the table with    *
! *   NAME TABLE_NAME.                                                   *
! *                                                                      *
! * ## 06-JAN-2006   PIMA_GET_KEY_I4  v1.0 (c) L. Petrov 06-JAN-2006  ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  TABLE_NAME*(*), KEY_NAME*(*)
      INTEGER*4  VAL_I4, IUER
      INTEGER*4    MIND
      PARAMETER  ( MIND =   32 )
      INTEGER*4  K_FIL, J1, J2, J3, IK, IT, IND_TAB, LIND, IND(2,MIND), IER
      CHARACTER    REG*4
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//"'" )
      LOGICAL*4  FL_FOUND
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IK = I_LEN(KEY_NAME)
      IT = I_LEN(TABLE_NAME)
      IND_TAB = 0
      IF ( TABLE_NAME(1:IT) == ' ' ) IND_TAB = 1
      FL_FOUND = .FALSE.
      DO 410 J1=1,PIM%FILE(K_FIL)%L_HDR
         DO 420 J2=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( PIM%FILE(K_FIL)%KEY(J2,J1)(1:8) == 'EXTNAME' ) THEN
                 IF ( INDEX ( PIM%FILE(K_FIL)%KEY(J2,J1), &
     &                        TABLE_NAME(1:IT) ) > 0 ) THEN
                      IND_TAB = J1
                 END IF
            END IF
 420     CONTINUE 
         DO 430 J3=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( J1 == IND_TAB  .AND. &
     &           PIM%FILE(K_FIL)%KEY(J3,J1)(1:IK) == KEY_NAME ) THEN
                 CALL EXWORD ( PIM%FILE(K_FIL)%KEY(J3,J1)(10:), MIND, LIND, &
     &                         IND, REG, -3 )
                 READ ( UNIT=PIM%FILE(K_FIL)%KEY(J3,J1)(IND(1,1)+9:IND(2,1)+9), &
     &                  FMT='(I11)', IOSTAT=IER ) VAL_I4
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7061, IUER, 'PIMA_GET_KEY_I4', &
     &                    'Error in decoding keyword '//KEY_NAME(1:IK)// &
     &                    ' -- string '// &
     &                    PIM%FILE(K_FIL)%KEY(J3,J1)(IND(1,1)+9:IND(2,1)+9) )
                      RETURN 
                 END IF
                 FL_FOUND = .TRUE.
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      IF ( IND_TAB == 0 ) THEN
           CALL ERR_LOG ( 7062, IUER, 'PIMA_GET_KEY_I4', 'Table '// &
     &          TABLE_NAME(1:IT)//' was not found in FITS UV file '// &
     &          PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      IF ( .NOT. FL_FOUND ) THEN
           CALL ERR_LOG ( 7063, IUER, 'PIMA_GET_KEY_I4', 'Value of the '// &
     &         'keyword '//KEY_NAME(1:IK)//' in table '//TABLE_NAME(1:IT)// &
     &         ' was not found in FITS UV file '//PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_KEY_I4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_GET_KEY_CH ( PIM, K_FIL, TABLE_NAME, KEY_NAME, &
     &                             VAL_CH, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine PIMA_GET_KEY_CH inquires the table of keys      *
! *   of FITS file gets the value of key KEY_NAME from the table with    *
! *   NAME TABLE_NAME.                                                   *
! *                                                                      *
! * ## 06-JAN-2006   PIMA_GET_KEY_CH  v1.0 (c) L. Petrov 06-JAN-2006  ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  TABLE_NAME*(*), KEY_NAME*(*)
      INTEGER*4    IUER
      CHARACTER    VAL_CH*(*)
      INTEGER*4  K_FIL, J1, J2, J3, IK, IT, IND_TAB, IB, IE, IER
      LOGICAL*4  FL_FOUND
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IK = I_LEN(KEY_NAME)
      IT = I_LEN(TABLE_NAME)
      IND_TAB = 0
      IF ( TABLE_NAME(1:IT) == ' ' ) IND_TAB = 1
      FL_FOUND = .FALSE.
      DO 410 J1=1,PIM%FILE(K_FIL)%L_HDR
         DO 420 J2=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( PIM%FILE(K_FIL)%KEY(J2,J1)(1:8) == 'EXTNAME' ) THEN
                 IF ( INDEX ( PIM%FILE(K_FIL)%KEY(J2,J1), &
     &                        TABLE_NAME(1:IT) ) > 0 ) THEN
                      IND_TAB = J1
                 END IF
            END IF
 420     CONTINUE 
         DO 430 J3=1,PIM%FILE(K_FIL)%L_KWD(J1)
            IF ( J1 == IND_TAB  .AND. &
     &           PIM%FILE(K_FIL)%KEY(J3,J1)(1:IK) == KEY_NAME ) THEN
                 IB = INDEX ( PIM%FILE(K_FIL)%KEY(J3,J1)(10:), "'" ) + 9
                 IE = INDEX ( PIM%FILE(K_FIL)%KEY(J3,J1)(IB+1:), "'" ) + IB
                 IF ( IB < 10  .OR.  IE-1 < IB+1 ) THEN
                      CALL ERR_LOG ( 7071, IUER, 'PIMA_GET_KEY_CH', &
     &                    'Error in decoding keyword '//KEY_NAME(1:IK)// &
     &                    ' -- a pair of apostrophes was not found in the '// &
     &                    'string '//PIM%FILE(K_FIL)%KEY(J3,J1)(10:) )
                      RETURN 
                 END IF
                 VAL_CH = PIM%FILE(K_FIL)%KEY(J3,J1)(IB+1:IE-1)
                 FL_FOUND = .TRUE.
            END IF
 430     CONTINUE 
 410  CONTINUE 
!
      IF ( IND_TAB == 0 ) THEN
           CALL ERR_LOG ( 7072, IUER, 'PIMA_GET_KEY_CH', 'Table '// &
     &          TABLE_NAME(1:IT)//' was not found in FITS UV file '// &
     &          PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      IF ( .NOT. FL_FOUND ) THEN
           CALL ERR_LOG ( 7073, IUER, 'PIMA_GET_KEY_CH', 'Value of the '// &
     &         'keyword '//KEY_NAME(1:IK)//' in table '//TABLE_NAME(1:IT)// &
     &         ' was not found in FITS UV file '//PIM%FILE(K_FIL)%NAME )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_KEY_CH  !#!#
