      SUBROUTINE PARSE_BPS_GEN_CNT ( PIM, FIL_GEN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PARSE_BPS_GEN_CNT                                         
! *                                                                      *
! *   The routine supports legacy codes BOTH ( = AUTC and FRNG ) and     *
! *   COARSE ( = AUTC ) codes.                                           *
! *                                                                      *
! * ### 05-FEB-2009 PARSE_BPS_GEN_CNT v1.4 (c) L. Petrov 10-MAR-2017 ### *
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
     &           IND_CHN_BEG, IND_CHN_END, &
     &           IND_FRQ_BEG, IND_FRQ_END, IND_STA, IND_ABS_CHN_BEG, &
     &           IND_ABS_CHN_END, IND_ABS_CHN, BPS_MODE, IER
      LOGICAL*4  FL_EXIST
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
!
      INQUIRE ( FILE=FIL_GEN, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 4611, IUER, 'PARSE_BPS_GEN_CNT', 'Cannot find '// &
     &         'control file for bandpass generation: '//FIL_GEN )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT  ( FIL_GEN, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4612, IUER, 'PARSE_BPS_GEN_CNT', 'Failure to read '// &
     &         'the control file for bandpass generation: '//FIL_GEN )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(PIMA__BPASS_MASK_GEN )) .NE. PIMA__BPASS_MASK_GEN ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR )
           CALL ERR_LOG ( 4613, IUER, 'PARSE_BPS_GEN_CNT', 'Cannot '// &
     &         'recognize format of the input file '// &
     &          FIL_GEN(1:I_LEN(FIL_GEN))//' -- the first line is '// &
     &          STR(1:I_LEN(STR))//' while the format label '// &
     &          PIMA__BPASS_MASK_GEN//' was expected' )
           RETURN 
      END IF
!
      ALLOCATE ( PIM%BANDPASS_MASK(PIM%NCHN,PIM%NFRQ,PIM%NSTA,PIMA__MASKS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NCHN*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 4614, IUER, 'PARSE_BPS_GEN_CNT', 'Failure in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'memory for array BANDPASS_MASK' ) 
           RETURN 
      END IF
!
! --- Initialization
!
      PIM%BANDPASS_MASK = 1
      BPS_MODE = PIMA__BOTH
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
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ALL' ) THEN
              BPS_MODE = PIMA__ALL
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'AUTC' ) THEN
              BPS_MODE = PIMA__MASK_AUTC
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPAS' ) THEN
              BPS_MODE = PIMA__MASK_BPAS
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FRNG' ) THEN
              BPS_MODE = PIMA__MASK_FRNG
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'FINE' ) THEN
              BPS_MODE = PIMA__FINE
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SPLT' ) THEN
              BPS_MODE = PIMA__MASK_SPLT
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BFS'  ) THEN
              BPS_MODE = PIMA__BFS
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'CROS' ) THEN
              BPS_MODE = PIMA__BFS
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BOTH' ) THEN
              BPS_MODE = PIMA__BOTH   ! Legacy
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COARSE' ) THEN
              BPS_MODE = PIMA__COARSE ! Legacy
            ELSE 
              CALL ERR_LOG ( 4615, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &            'in parsing control file for bandpass generation: '// &
     &             FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &             STR(1:I_LEN(STR))//' -- unsupported first word '// &
     &             BUF(J1)(IND(1,1):IND(2,1))//' while ALL, AUTC, BPAS, '// &
     &            'FRNG, SPLT were expected' )
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
                   IF ( BPS_MODE == PIMA__ALL ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_AUTC) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_BPAS) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_FRNG) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_SPLT) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__BFS ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_BPAS) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_FRNG) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_SPLT) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__MASK_AUTC ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,1:PIMA__MASK_AUTC) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__MASK_BPAS ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,1:PIMA__MASK_BPAS) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__MASK_FRNG ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,1:PIMA__MASK_FRNG) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__MASK_SPLT ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,1:PIMA__MASK_SPLT) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__COARSE ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_AUTC) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_BPAS) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_FRNG) = 1
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_SPLT) = 1
                      ELSE IF ( BPS_MODE == PIMA__FINE ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_AUTC) = 1
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_BPAS) = 1
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_FRNG) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_SPLT) = MASK_VALUE
                      ELSE IF ( BPS_MODE == PIMA__BOTH ) THEN
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_AUTC) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_BPAS) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_FRNG) = MASK_VALUE
                        PIM%BANDPASS_MASK(1:PIM%NCHN,1:PIM%NFRQ,1:PIM%NSTA,PIMA__MASK_SPLT) = 1
                   END IF
                 ELSE 
                   CALL ERR_LOG ( 4616, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for bandpass generation: '// &
     &                  FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                  STR(1:I_LEN(STR))//' -- unsupported third word '// &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' while ON or OFF '// &
     &                 'were expected' )
                  RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'STA:' ) THEN
              IF ( LIND < 6 ) THEN
                   CALL ERR_LOG ( 4617, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for bandpass generation: '// &
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
                        CALL ERR_LOG ( 4618, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for bandpass generation: '// &
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
                        CALL ERR_LOG ( 4619, IUER, 'PARSE_BPS_GEN_CNT', &
     &                      'Failure to parse the first part of the fifth '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for bandpass generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_ABS_CHN_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4620, IUER, 'PARSE_BPS_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for bandpass '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_ABS_CHN_END = IND_ABS_CHN_BEG
                   END IF
!
                   IF ( IND_ABS_CHN_END < IND_ABS_CHN_BEG ) THEN
                        CALL ERR_LOG ( 4621, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_ABS_CHN_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4622, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_ABS_CHN_END > PIM%NCHN*PIM%NFRQ ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NCHN*PIM%NFRQ, STR1 )
                        CALL ERR_LOG ( 4623, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is greater than the product '// &
     &                     'PIM%NCHN*PIM%NFRQ: '//STR1 )
                        RETURN 
                   END IF
!
                   IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'ON' ) THEN
                        MASK_VALUE = 1
                      ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'OFF' ) THEN
                        MASK_VALUE = 0
                      ELSE 
                        CALL ERR_LOG ( 4624, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for bandpass generation: '// &
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
                              DO 440 J4=1,PIM%NCHN
                                 IND_ABS_CHN = IND_ABS_CHN + 1
                                 IF ( IND_ABS_CHN .GE. IND_ABS_CHN_BEG  .AND. &
     &                                IND_ABS_CHN .LE. IND_ABS_CHN_END        ) THEN
!
                                      IF ( BPS_MODE == PIMA__MASK_AUTC ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_AUTC) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__MASK_BPAS ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_BPAS) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__MASK_FRNG ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_FRNG) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__MASK_SPLT ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_SPLT) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__COARSE ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_AUTC) = MASK_VALUE 
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_BPAS) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__FINE   ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_FRNG) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__BFS    ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_BPAS) = MASK_VALUE 
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_FRNG) = MASK_VALUE 
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_SPLT) = MASK_VALUE 
                                        ELSE IF ( BPS_MODE == PIMA__BOTH   ) THEN
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_AUTC) = MASK_VALUE 
                                           PIM%BANDPASS_MASK(J4,J3,J2,PIMA__MASK_FRNG) = MASK_VALUE 
                                      END IF 
                                 END IF
 440                          CONTINUE 
 430                       CONTINUE 
                      END IF
 420               CONTINUE 
                 ELSE IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'IND_FRQ:' ) THEN
                   IF ( LIND < 7 ) THEN
                        CALL ERR_LOG ( 4625, IUER, 'PARSE_BPS_GEN_CNT', &
     &                      'Failure in parsing control file for bandpass '// &
     &                      'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                      ' at line '//STR(1:I_LEN(STR))// &
     &                      ' -- too few words, less than 7' )
                   END IF
                   IF ( BUF(J1)(IND(1,6):IND(2,6)) .NE. 'IND_CHN:' ) THEN
                        CALL ERR_LOG ( 4626, IUER, 'PARSE_BPS_GEN_CNT', &
     &                      'Failure in parsing control file for bandpass '// &
     &                      'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                      ' at line '//STR(1:I_LEN(STR))// &
     &                      ' -- the sixth word is '// &
     &                      BUF(J1)(IND(1,6):IND(2,6))//' while IND_CHN: '// &
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
                        CALL ERR_LOG ( 4627, IUER, 'PARSE_BPS_GEN_CNT', &
     &                      'Failure to parse the first part of the fifth '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for bandpass generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_FRQ_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4628, IUER, 'PARSE_BPS_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for bandpass '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_FRQ_END = IND_FRQ_BEG
                   END IF
!
                   IF ( IND_FRQ_END < IND_FRQ_BEG ) THEN
                        CALL ERR_LOG ( 4629, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_FRQ_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4630, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_FRQ_END > PIM%NFRQ ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NFRQ, STR1 )
                        CALL ERR_LOG ( 4631, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the fifth word '// &
     &                      BUF(J1)(IND(1,5):IND(2,5))//' in the control '// &
     &                     'file for bandpass generation: '// &
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
                   READ ( UNIT=WORD_BEG, FMT='(I6)', IOSTAT=IER ) IND_CHN_BEG
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4632, IUER, 'PARSE_BPS_GEN_CNT', &
     &                      'Failure to parse the first part of the seventh '// &
     &                      'word '//WORD_BEG(1:I_LEN(WORD_BEG))//' in the '// &
     &                      'control file for bandpass generation: '// &
     &                       FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR )
                        RETURN 
                   END IF
                   IF ( ILEN(WORD_END) > 0 ) THEN
                        READ ( UNIT=WORD_END, FMT='(I6)', IOSTAT=IER ) &
     &                         IND_CHN_END
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 4633, IUER, 'PARSE_BPS_GEN_CNT', &
     &                           'Failure to parse the second part of the '// &
     &                           'fourth word '//WORD_END(1:I_LEN(WORD_END))// &
     &                           ' in the control file for bandpass '// &
     &                           'generation: '//FIL_GEN(1:I_LEN(FIL_GEN))// &
     &                           ' at line '//STR )
                             RETURN 
                        END IF
                      ELSE 
                        IND_CHN_END = IND_CHN_BEG
                   END IF
!
                   IF ( IND_CHN_END < IND_CHN_BEG ) THEN
                        CALL ERR_LOG ( 4634, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is less than the first '// &
     &                     'part ' )
                        RETURN 
                   END IF
!
                   IF ( IND_CHN_BEG < 1 ) THEN
                        CALL ERR_LOG ( 4635, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the first '// &
     &                     'part of the word is less than 0' ) 
                        RETURN 
                   END IF
!
                   IF ( IND_CHN_END > PIM%NCHN ) THEN
                        CALL CLRCH ( STR1 ) 
                        CALL INCH  ( PIM%NCHN, STR1 )
                        CALL ERR_LOG ( 4636, IUER, 'PARSE_BPS_GEN_CNT', &
     &                     'Failure to parse the seventh word '// &
     &                      BUF(J1)(IND(1,7):IND(2,7))//' in the control '// &
     &                     'file for bandpass generation: '// &
     &                      FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                      STR(1:I_LEN(STR))//' -- the second '// &
     &                     'part of the word is greater than '// &
     &                     'PIM%NCHN: '//STR1 )
                        RETURN 
                   END IF
!
                   IF ( BUF(J1)(IND(1,8):IND(2,8)) == 'ON' ) THEN
                        MASK_VALUE = 1
                      ELSE IF ( BUF(J1)(IND(1,8):IND(2,8)) == 'OFF' ) THEN
                        MASK_VALUE = 0
                      ELSE 
                        CALL ERR_LOG ( 4637, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                      'in parsing control file for bandpass generation: '// &
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
                              DO 470 J7=IND_CHN_BEG,IND_CHN_END
                                 IND_ABS_CHN = IND_ABS_CHN + 1
                                 IF ( BPS_MODE == PIMA__MASK_AUTC ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_AUTC) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__MASK_BPAS ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_BPAS) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__MASK_FRNG ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_FRNG) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__MASK_SPLT ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_SPLT) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__ALL ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_AUTC) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_BPAS) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_FRNG) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_SPLT) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__COARSE ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_AUTC) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_BPAS) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__FINE   ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_FRNG) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__BFS    ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_BPAS) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_FRNG) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_SPLT) = MASK_VALUE 
                                   ELSE IF ( BPS_MODE == PIMA__BOTH   ) THEN
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_AUTC) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_BPAS) = MASK_VALUE 
                                      PIM%BANDPASS_MASK(J7,J6,J5,PIMA__MASK_FRNG) = MASK_VALUE 
                                 END IF 
 470                          CONTINUE 
 460                       CONTINUE 
                      END IF
 450               CONTINUE 
                 ELSE 
                   CALL ERR_LOG ( 4638, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &                 'in parsing control file for bandpass generation: '// &
     &                  FIL_GEN(1:I_LEN(FIL_GEN))//' at line '// &
     &                  STR(1:I_LEN(STR))//' -- unsupported fourth word '// &
     &                  BUF(J1)(IND(1,4):IND(2,4))//' while IND_ABS_CHN or '// &
     &                 'IND_FRQ: were expected' )
                   RETURN 
              END IF
            ELSE 
              CALL ERR_LOG ( 4639, IUER, 'PARSE_BPS_GEN_CNT', 'Failure '// &
     &            'in parsing control file for bandpass generation: '// &
     &             FIL_GEN(1:I_LEN(FIL_GEN))//' at line '//STR(1:I_LEN(STR))// &
     &            ' -- unsupported second word '//BUF(J1)(IND(1,2):IND(2,2))// &
     &            ' while ALL: or STA: were expected' )
              RETURN 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_BPS_GEN_CNT  !#!#
