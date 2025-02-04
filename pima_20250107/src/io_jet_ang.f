      SUBROUTINE IO_JET_ANG ( OPCODE, FIL, ANG, IMA_WIN, QUAL_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  IO_JET_ANG
! *                                                                      *
! *  ### 02-AUG-2007  IO_JET_ANG  v1.0 (c)  L. Petrov  02-AUG-2007 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      CHARACTER  FIL*(*)
      REAL*8     ANG, IMA_WIN
      INTEGER*4  OPCODE, QUAL_CODE, IUER
      INTEGER*4  M_SOU
      PARAMETER  ( M_SOU = 8192 )
      CHARACTER  BUF(M_SOU)*128
      CHARACTER  STR*128, FINAM*128, SOU_NAME*10, MAP_BAND*1, MAP_DATE*10, &
     &           MAP_ANAL*3
      CHARACTER  JET__LABEL*42
      PARAMETER  ( JET__LABEL = '# Jet Angle Format.  Version of 2007.08.02' )
      LOGICAL*4  LEX, FL_FOUND
      INTEGER*4  J1, J2, J3, J4, N_BUF, IB, IER
      REAL*8     IMA_WIN_MAS, ANG_DEG
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, LINDEX
!
      IF ( OPCODE == 1 ) THEN
!
! -------- Set default values
!
           IMA_WIN = 10.0D0
           ANG     =  0.0D0
         ELSE IF ( OPCODE == 2 ) THEN
           CONTINUE 
         ELSE 
           WRITE ( 6, * ) 'OPCODE = ', OPCODE
           CALL ERR_LOG ( 7411, IUER, 'IO_JET_ANG', 'Wrong OPCODE vairable' )
           RETURN 
      END IF
!
      CALL GETENVAR ( 'JET_ANG_FIL', FINAM )
      IF ( ILEN(FINAM) == 0 ) THEN
           CALL ERR_LOG ( 7412, IUER, 'IO_JET_ANG', 'Environment variable '// &
     &         'JET_ANG_FIL is not set up' )
           RETURN 
      END IF
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 7413, IUER, 'IO_JET_ANG', 'The file with jet '// &
     &         'angles specified in the environment variable JET_ANG_FIL: '// &
     &          FINAM(1:I_LEN(FINAM))//' does not exist' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FINAM, M_SOU, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7414, IUER, 'IO_JET_ANG', 'Error in reading '// &
     &         'the file with jet angles specified in the environment '// &
     &         'variable JET_ANG_FIL: '//FINAM )
           RETURN 
      END IF
      IF ( BUF(1)(1:LEN(JET__LABEL)) == JET__LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 7415, IUER, 'IO_JET_ANG', 'Wrong format of '// &
     &         'the file with jet angles specified in the environment '// &
     &         'variable JET_ANG_FIL: '//FINAM(1:I_LEN(FINAM))// &
     &         ' -- the first line if '//BUF(1)(1:I_LEN(BUF(1)))// &
     &         ' while the label '//JET__LABEL//' was expected' )
           RETURN 
      END IF
!
      IB = LINDEX ( FIL, '/' ) + 1
      SOU_NAME = FIL(IB:IB+9)
      MAP_BAND = FIL(IB+11:IB+11)
      MAP_DATE = FIL(IB+13:IB+22)
      MAP_ANAL = FIL(IB+24:IB+26)
!  write ( 6, * ) ' ' ! %%%
!  write ( 6, * ) ' SOU_NAME >>'//SOU_NAME(1:I_LEN(SOU_NAME))//'<<  ' ! %%%
!  write ( 6, * ) ' MAP_DATE >>'//MAP_DATE(1:I_LEN(MAP_DATE))//'<<  ' ! %%%
!  write ( 6, * ) ' MAP_ANAL >>'//MAP_ANAL(1:I_LEN(MAP_ANAL))//'<<  ' ! %%%
!  write ( 6, * ) ' MAP_BAND >>'//MAP_BAND(1:I_LEN(MAP_BAND))//'<<  ' ! %%%
!
      FL_FOUND = .FALSE.
      DO 410 J1=1,N_BUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         IF ( BUF(J1)(1:10)  == SOU_NAME  .AND. &
     &        BUF(J1)(12:21) == MAP_DATE  .AND. &
     &        BUF(J1)(23:25) == MAP_ANAL        ) THEN
              IF ( MAP_BAND == 'S' ) THEN
                   IF ( OPCODE == 1 ) THEN
                        READ  ( UNIT=BUF(J1)(29:33), FMT='(F5.1)' ) IMA_WIN_MAS
                        READ  ( UNIT=BUF(J1)(37:41), FMT='(F5.0)' ) ANG_DEG
                        READ  ( UNIT=BUF(J1)(61:61), FMT='(I1)' ) QUAL_CODE
                      ELSE IF ( OPCODE == 2 ) THEN
                        IMA_WIN_MAS = IMA_WIN*RAD__TO__MAS
                        ANG_DEG     = ANG/DEG__TO__RAD
                        WRITE ( UNIT=BUF(J1)(29:33), FMT='(F5.1)' ) IMA_WIN_MAS
                        WRITE ( UNIT=BUF(J1)(37:41), FMT='(F5.0)' ) ANG_DEG
                        WRITE ( UNIT=BUF(J1)(61:61), FMT='(I1)'   ) QUAL_CODE
                        BUF(J1)(63:81) = GET_CDATE()
                   END IF
                 ELSE IF ( MAP_BAND == BUF(J1)(43:43) ) THEN
                   IF ( OPCODE == 1 ) THEN
                        READ  ( UNIT=BUF(J1)(47:51), FMT='(F5.1)' ) IMA_WIN_MAS
                        READ  ( UNIT=BUF(J1)(55:59), FMT='(F5.0)' ) ANG_DEG
                        READ  ( UNIT=BUF(J1)(61:61), FMT='(I1)' ) QUAL_CODE
                      ELSE IF ( OPCODE == 2 ) THEN
                        IMA_WIN_MAS = IMA_WIN*RAD__TO__MAS
                        ANG_DEG     = ANG/DEG__TO__RAD
                        WRITE ( UNIT=BUF(J1)(47:51), FMT='(F5.1)' ) IMA_WIN_MAS
                        WRITE ( UNIT=BUF(J1)(55:59), FMT='(F5.0)' ) ANG_DEG
                        WRITE ( UNIT=BUF(J1)(61:61), FMT='(I1)'   ) QUAL_CODE
                        BUF(J1)(63:81) = GET_CDATE()
                   END IF
              END IF
              FL_FOUND = .TRUE.
         END IF
 410  CONTINUE 
      IF ( .NOT. FL_FOUND ) THEN
           CALL ERR_LOG ( 7416, IUER, 'IO_JET_ANG', 'Did not find the '// &
     &         'jet angle for the map '//FIL(1:I_LEN(FIL))//' in the file '// &
     &         'with jet angles specified in the environment '// &
     &         'variable JET_ANG_FIL: '//FINAM )
           RETURN 
      END IF
!
      IF ( OPCODE == 1 ) THEN
           IMA_WIN = IMA_WIN_MAS*MAS__TO__RAD
           ANG     = ANG_DEG*DEG__TO__RAD
        ELSE IF ( OPCODE == 2 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( N_BUF, BUF, FINAM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7417, IUER, 'IO_JET_ANG', 'Error in an '// &
     &              'attempt to write into thye file with jet angles '// &
     &              'specified in the environment variable JET_ANG_FIL: '// &
     &               FINAM )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  IO_JET_ANG  !#!#
