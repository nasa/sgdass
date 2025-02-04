      SUBROUTINE PARSE_PCAL_MASK_GEN_CNT ( PIM, FIL_GEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_PCAL_MASK_GEN_CNT 
! *                                                                      *
! * # 10-MAY-2015 PARSE_PCAL_MASK_GEN_CNT v1.0 (c) Petrov 10-MAY-2015 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FIL_GEN*(*)
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 64*1024 )
      PARAMETER  ( MIND = 64      )
      CHARACTER  BUF(MBUF)*128
      CHARACTER  STR*32, STR1*32, WORD_BEG*32, WORD_END*32, REG*3
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*1  MASK_VALUE 
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IP, NBUF, LIND, IND(2,MIND), &
     &           IND_TONE_BEG, IND_TONE_END, &
     &           IND_FRQ_BEG, IND_FRQ_END, IND_STA, IND_ABS_CHN_BEG, &
     &           IND_ABS_CHN_END, IND_ABS_CHN, IER
      LOGICAL*4  FL_EXIST
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
!
      INQUIRE ( FILE=FIL_GEN, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 4652, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Cannot find '// &
     &         'control file for pcal mask generation: '//FIL_GEN )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( FIL_GEN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4653, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure to read '// &
     &         'the control file for pcal mask generation: '//FIL_GEN )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__PCAL_MASK_GEN )) .NE. PIMA__PCAL_MASK_GEN .AND. &
     &     BUF(1)(1:LEN(PIMA__PCAL_RPT_LABEL )) .NE. PIMA__PCAL_RPT_LABEL  ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4654, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Cannot '// &
     &         'recognize format of the input file '// &
     &          FIL_GEN(1:I_LEN(FIL_GEN))//' -- the first line is '// &
     &          STR(1:I_LEN(STR))//' while the format label '// &
     &          PIMA__BPASS_MASK_GEN//' or '// PIMA__PCAL_RPT_LABEL// ' was expected' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%PCAL_MASK(PIM%NPCT,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NPCT*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 4654, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'memory for array PCAL_MASK' ) 
           RETURN 
      END IF
      PIM%PCAL_MASK_STATUS = PIMA__ALLOCATED
!
! --- Initialization
!
      PIM%PCAL_MASK = 1
!
      DO 410 J1=1,NBUF
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IF ( BUF(J1)(1:1) == '!' ) GOTO 410
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
!
         CALL TRAN ( 11, BUF(J1), BUF(J1) )
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, REG, -2 )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PCAL' ) THEN
              CONTINUE    
           ELSE 
              CALL ERR_LOG ( 4655, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &            'in parsing control file for pcal mask generation: '// &
     &             FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &             STR(1:I_LEN(STR))//' -- unsupported first word '// &
     &             BUF(J1)(IND(1,1):IND(2,1))//' while PCAL was expected' )
              RETURN 
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'ALL:' ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'ON'  .OR. &
     &             BUF(J1)(IND(1,3):IND(2,3)) == 'OFF'      ) THEN
!
                   IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'ON' ) THEN
                        MASK_VALUE = 1
                     ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'OFF' ) THEN
                        MASK_VALUE = 0
                   END IF
                   PIM%PCAL_MASK(1:PIM%NPCT,1:PIM%NFRQ,1:PIM%NSTA) = MASK_VALUE
                 ELSE 
                   CALL ERR_LOG ( 4656, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for pcal mask generation: '// &
     &                  FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                  STR(1:I_LEN(STR))//' -- unsupported third word '// &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' while ON or OFF '// &
     &                 'were expected' )
                  RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'STA:' ) THEN
              IF ( LIND < 6 ) THEN
                   CALL ERR_LOG ( 4657, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for pcal mask generation: '// &
     &                  FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                  STR(1:I_LEN(STR))//' -- too few words, less than 6' )
                   RETURN 
              END IF
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'ALL' ) THEN
                   IND_STA = -1
                ELSE
                   IND_STA = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, &
     &                                 BUF(J1)(IND(1,3):IND(2,3)) )
                   IF ( IND_STA .LE. 0 ) THEN
                        CALL ERR_LOG ( 4658, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                       STR(1:I_LEN(STR))//' -- station '// &
     &                       BUF(J1)(IND(1,3):IND(2,3))//' did not participate '// &
     &                      'in this experiment' )
                        RETURN 
                  END IF
              END IF
!
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'IND_ABS_CHN:' ) THEN
                   CALL CLRCH ( WORD_END ) 
                   WORD_BEG = BUF(J1)(IND(1,5):IND(2,5))
                   IP = INDEX ( WORD_BEG, '-' )
                   IF ( IP > 1 .AND. IP < ILEN(WORD_BEG) ) THEN
                        WORD_END = WORD_BEG(IP+1:)
                        WORD_BEG = WORD_BEG(1:IP-1)
                   END IF
!
                   READ ( UNIT=WORD_BEG, FMT='(I6)', IOSTAT=IER ) IND_ABS_CHN_BEG
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4659, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                      'Failure to parse the first part of the fifth '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_ABS_CHN_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4660, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for pcal mask '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_ABS_CHN_END = IND_ABS_CHN_BEG
                   END IF
!
                   IF ( IND_ABS_CHN_END < IND_ABS_CHN_BEG ) THEN
                        CALL ERR_LOG ( 4661, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_ABS_CHN_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4662, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_ABS_CHN_END > PIM%NPCT*PIM%NFRQ ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NPCT*PIM%NFRQ, STR1 )
                        CALL ERR_LOG ( 4663, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is greater than the product '// &
     &                     'PIM%NPCT*PIM%NFRQ: '//STR1 )
                        RETURN 
                   END IF
!
                   IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'ON' ) THEN
                        MASK_VALUE = 1
                      ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'OFF' ) THEN
                        MASK_VALUE = 0
                      ELSE 
                        CALL ERR_LOG ( 4664, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                       STR(1:I_LEN(STR))//' -- unsupported sixth word '// &
     &                       BUF(J1)(IND(1,6):IND(2,6))//' while ON or OFF '// &
     &                      'were expected' )
                       RETURN 
                   END IF
!
                   IND_ABS_CHN = 0
                   DO 420 J2=1,PIM%NSTA
                      IF ( J2 == IND_STA .OR. IND_STA == -1 ) THEN
                           DO 430 J3=1,PIM%NFRQ
                              DO 440 J4=1,PIM%NPCT
                                 IND_ABS_CHN = IND_ABS_CHN + 1
                                 IF ( IND_ABS_CHN .GE. IND_ABS_CHN_BEG  .AND. &
     &                                IND_ABS_CHN .LE. IND_ABS_CHN_END        ) THEN
!
                                      PIM%PCAL_MASK(J4,J3,J2) = MASK_VALUE 
                                 END IF
 440                          CONTINUE 
 430                       CONTINUE 
                      END IF
 420               CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'IND_FRQ:' ) THEN
                   IF ( LIND < 7 ) THEN
                        CALL ERR_LOG ( 4665, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                      'Failure in parsing control file for pcal mask '// &
     &                      'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                      ' at line '//STR(1:I_LEN(STR))// &
     &                      ' -- too few words, less than 7' )
                   END IF
                   IF ( BUF(J1)(IND(1,6):IND(2,6)) .NE. 'IND_TONE:' ) THEN
                        CALL ERR_LOG ( 4666, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                      'Failure in parsing control file for pcal mask '// &
     &                      'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                      ' at line '//STR(1:I_LEN(STR))// &
     &                      ' -- the sixth word is '// &
     &                      BUF(J1)(IND(1,6):IND(2,6))//' while IND_TONE: '// &
     &                      'was expected' )
                        RETURN 
                   END IF
!
                   CALL CLRCH ( WORD_END ) 
                   WORD_BEG = BUF(J1)(IND(1,5):IND(2,5))
                   IP = INDEX ( WORD_BEG, '-' )
                   IF ( IP > 1 .AND. IP < ILEN(WORD_BEG) ) THEN
                        WORD_END = WORD_BEG(IP+1:)
                        WORD_BEG = WORD_BEG(1:IP-1)
                   END IF
!
                   READ ( UNIT=WORD_BEG, FMT='(I6)', IOSTAT=IER ) IND_FRQ_BEG
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4667, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                      'Failure to parse the first part of the fifth '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_FRQ_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4668, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for pcal mask '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_FRQ_END = IND_FRQ_BEG
                   END IF
!
                   IF ( IND_FRQ_END < IND_FRQ_BEG ) THEN
                        CALL ERR_LOG ( 4669, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_FRQ_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4670, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_FRQ_END > PIM%NFRQ ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NFRQ, STR1 )
                        CALL ERR_LOG ( 4671, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is greater than '// &
     &                     'PIM%NFRQ: '//STR1 )
                        RETURN 
                   END IF
!
                   CALL CLRCH ( WORD_END ) 
                   WORD_BEG = BUF(J1)(IND(1,7):IND(2,7))
                   IP = INDEX ( WORD_BEG, '-' )
                   IF ( IP > 1 .AND. IP < ILEN(WORD_BEG) ) THEN
                        WORD_END = WORD_BEG(IP+1:)
                        WORD_BEG = WORD_BEG(1:IP-1)
                   END IF
!
                   READ ( UNIT=WORD_BEG, FMT='(I6)', IOSTAT=IER ) IND_TONE_BEG
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4672, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                      'Failure to parse the first part of the seventh '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_TONE_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4673, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for pcal mask '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_TONE_END = IND_TONE_BEG
                   END IF
!
                   IF ( IND_TONE_END < IND_TONE_BEG ) THEN
                        CALL ERR_LOG ( 4674, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_TONE_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4675, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_TONE_END > PIM%NPCT ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NPCT, STR1 )
                        CALL ERR_LOG ( 4676, IUER, 'PARSE_PCAL_MASK_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for pcal mask generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is greater than '// &
     &                     'PIM%NPCT: '//STR1 )
                        RETURN 
                   END IF
!
                   IF ( BUF(J1)(IND(1,8):IND(2,8)) == 'ON' ) THEN
                        MASK_VALUE = 1
                      ELSE IF ( BUF(J1)(IND(1,8):IND(2,8)) == 'OFF' ) THEN
                        MASK_VALUE = 0
                      ELSE 
                        CALL ERR_LOG ( 4677, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for pcal mask generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                       STR(1:I_LEN(STR))//' -- unsupported eighth word '// &
     &                       BUF(J1)(IND(1,8):IND(2,8))//' while ON or OFF '// &
     &                      'were expected' )
                       RETURN 
                   END IF
!
                   DO 450 J5=1,PIM%NSTA
                      IF ( J5 == IND_STA .OR. IND_STA == -1 ) THEN
                           DO 460 J6=IND_FRQ_BEG,IND_FRQ_END
                              DO 470 J7=IND_TONE_BEG,IND_TONE_END
                                 IND_ABS_CHN = IND_ABS_CHN + 1
                                 PIM%PCAL_MASK(J7,J6,J5) = MASK_VALUE 
 470                          CONTINUE 
 460                       CONTINUE 
                      END IF
 450               CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 4678, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for pcal mask generation: '// &
     &                  FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                  STR(1:I_LEN(STR))//' -- unsupported fourth word '// &
     &                  BUF(J1)(IND(1,4):IND(2,4))//' while IND_ABS_CHN or '// &
     &                 'IND_FRQ: were expected' )
                   RETURN 
              END IF
            ELSE 
              CALL ERR_LOG ( 4679, IUER, 'PARSE_PCAL_MASK_GEN_CNT', 'Failure '// &
     &            'in parsing control file for pcal mask generation: '// &
     &             FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR(1:I_LEN(STR))// &
     &            ' -- unsupported second word '//BUF(J1)(IND(1,2):IND(2,2))// &
     &            ' while ALL: or STA: were expected' )
              RETURN 
         END IF
 410  CONTINUE 
      PIM%PCAL_MASK_STATUS = PIMA__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_PCAL_MASK_GEN_CNT  !#!#
