      SUBROUTINE MONICA_TO_ANTAB ( PIM, STA_NAM, FILIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MONICA_TO_ANTAB 
! *                                                                      *
! * ### 08-DEC-2017 MONICA_TO_ANTAB  v1.0 (c) L. Petrov  08-DEC-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  STA_NAM*(*), FILIN*(*)
      INTEGER*4  IUER
      INTEGER*4  MP, MIND
      PARAMETER  ( MP = 64*1024 )
      PARAMETER  ( MIND = 32 )
      CHARACTER  MON__FMT*26, BUF(MP)*128, STR*512, STR1*128, OUT(MP)*512, &
     &           FILOUT*128
      PARAMETER  ( MON__FMT = '! MONICA  TSYS Format  2.0' )
      INTEGER*4  ISTA, J1, J2, J3, J4, J5, J6, J7, J8, J9, IP, NP, NT, DOY, &
     &           IND_IF, IND_RF, IF_TO_RF(PIM__MFRQ), LIND, IND(2,MIND), &
     &           MJD_NY, MJD_VAL, ID, IB, IE, ISC, IMN, IHR, NOUT, IER
      REAL*8     TAI_NY, TIM_ARR(MP), TSYS_ARR(MP,PIM__MFRQ), TIM_VAL, &
     &           MIN_DEC, TSYS_VAL, TIM_SCA_BEG, TIM_SCA_END, TAI_VAL, &
     &           TSYS_OUT(PIM__MFRQ), TSYS_MIN, TSYS_MAX
      PARAMETER  ( TSYS_MIN = 10.0D0 )
      PARAMETER  ( TSYS_MAX = 5000.0D0 )
      LOGICAL*1  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      ISTA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
      IF ( ISTA < 1 ) THEN
           CALL ERR_LOG ( 4311, IUER, 'MONICA_TO_ANTAB', 'Wrong '// &
     &         'value of the 1st keyword: station '//TRIM(STA_NAM)// &
     &         ' did not observe in experiment '//PIM%CONF%SESS_CODE )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILIN, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4312, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &         'reading input file with Tsys '//FILIN )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(MON__FMT)) == MON__FMT ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 4313, IUER, 'MONICA_TO_ANTAB', 'Unsupported '// &
     &         'format of input file '//TRIM(FILIN)//' -- format label '// &
     &         'was not found' )
           RETURN 
      END IF
!
      IF_TO_RF = 0
      TIM_ARR  = 0.0D0
      TSYS_ARR = 0.0D0
!
      STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, IER )
      STR = STR(1:4)//'.01.01_00:00:00.0'
      CALL DATE_TO_TIME ( STR, MJD_NY, TAI_NY, IER )
      NT = 0
      DO 410 J1=2,NP
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
         IF ( BUF(J1)(1:5) == '! FRQ' ) THEN
              IF ( LIND < 5 ) THEN
                   CALL ERR_LOG ( 4314, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- less than 5 words' )
                   RETURN 
              END IF
              CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)), IND_IF )
              CALL CHIN ( BUF(J1)(IND(1,5):IND(2,5)), IND_RF )
              IF ( IND_IF < 1 ) THEN
                   CALL ERR_LOG ( 4315, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- failure to read the 3rd word '// &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' -- a positive integer '// &
     &                  'number was exected' )
                   RETURN 
              END IF
              IF ( IND_IF > PIM%NFRQ ) THEN
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( PIM%NFRQ, STR1 )
                   CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- the IF index '// &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' exceeded the total number '// &
     &                  'of IFs: '//STR1 )
                   RETURN 
              END IF
              IF ( IND_RF < 1 ) THEN
                   CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- failure to read the 5th word '// &
     &                  BUF(J1)(IND(1,5):IND(2,5))//' -- a positive integer '// &
     &                  'number was exected' )
                   RETURN 
              END IF
              IF_TO_RF(IND_IF) = IND_RF
           ELSE IF ( BUF(J1)(1:4) == 'TSYS' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J1)(1:4) == 'GAIN' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J1)(1:6) == 'INDEX=' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J1)(1:1) == '/' ) THEN
              CONTINUE 
           ELSE IF ( BUF(J1)(1:1) == '!' ) THEN
              CONTINUE 
           ELSE
              CALL CHIN ( BUF(J1)(IND(1,1):IND(2,1)), DOY )
              IF ( DOY < 1 .OR. DOY > 366) THEN
                   CALL CLRCH ( STR1 ) 
                   CALL INCH  ( PIM%NFRQ, STR1 )
                   CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- the first word should be a positive '// &
     &                 'integer not exceeding 366' )
                   RETURN 
              END IF
              IF ( BUF(J1)(IND(1,2)+1:IND(1,2)+1) .NE. ':' .AND. &
     &             BUF(J1)(IND(1,2)+2:IND(1,2)+2) .NE. ':'       ) THEN
                   CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                 'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                  TRIM(FILIN)//' -- the second word should have colomn '// &
     &                  'in the 2nd or 3rd position' )
                   RETURN 
              END IF
              IF ( BUF(J1)(IND(1,2)+1:IND(1,2)+1) == ':' ) THEN
                   IP = 1
              END IF
              IF ( BUF(J1)(IND(1,2)+2:IND(1,2)+2) == ':' ) THEN
                   IP = 2
              END IF
!
              CALL CHIN ( BUF(J1)(IND(1,2):IND(1,2)+IP-1), IHR )
              IF ( BUF(J1)(IND(1,2)+2:IND(1,2)+2) == ':' .AND. &
     &             BUF(J1)(IND(1,2)+5:IND(1,2)+5) == ':'       ) THEN
!
! ---------------- Time format: hours, minutes and seconds
!
                   READ ( UNIT=BUF(J1)(IND(1,2)+3:IND(1,2)+4), FMT=* ) IMN
                   READ ( UNIT=BUF(J1)(IND(1,2)+6:IND(1,2)+7), FMT=* ) ISC
                   TAI_VAL = 3600.0*IHR + IMN*60.0D0 + ISC - PIM%UTC_MTAI
                 ELSE               
!
! ---------------- Time format: hours, minute and its fraction
!
                   READ ( UNIT=BUF(J1)(IND(1,2)+IP+1:IND(2,2)), FMT=* ) MIN_DEC
                   TAI_VAL = 3600.0*IHR + MIN_DEC*60.0D0 - PIM%UTC_MTAI
              END IF
              NT = NT + 1
              MJD_VAL = MJD_NY + DOY - 1
              TIM_ARR(NT) = (TAI_VAL - PIM%TAI_0) + (MJD_VAL - PIM%MJD_0)*86400.0D0
              DO 420 J2=3,LIND
                 READ ( UNIT=BUF(J1)(IND(1,J2):IND(2,J2)), FMT=*, IOSTAT=IER ) TSYS_VAL
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( J2, STR1 )
                      CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &                    'parsing line '//TRIM(STR)//' of the Tsys file '// &
     &                     TRIM(FILIN)//' -- cannot parse the '//TRIM(STR1)// &
     &                    'th word '//BUF(J1)(IND(1,J2):IND(2,J2))// &
     &                    ' -- a real number of was expected' )
                      RETURN 
                 END IF
                 DO 430 J3=1,PIM%NFRQ
                    IF ( IF_TO_RF(J3) == J2-2 ) THEN
                         TSYS_ARR(NT,J3) = TSYS_VAL
                    END IF
 430             CONTINUE 
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL CLRCH ( STR  )
      CALL CLRCH ( STR1 )
      CALL TRAN  ( 12, PIM%CONF%SESS_CODE,      STR  )
      CALL TRAN  ( 12, PIM%STA(ISTA)%ORIG_NAME, STR1 )
      ID = LINDEX ( PIM%CONF_FILE, '/' )
      IF ( ID < 1 ) ID = 2
      FILOUT = PIM%CONF_FILE(1:ID-1)//'/'//TRIM(STR)//'_'//STR1(1:2)//'.ant'
      INQUIRE ( FILE=FILOUT, EXIST=LEX )
!!      write ( 6, * ) 'filout= ', trim(filout), ' lex= ', lex ! %%%%
      IF ( .NOT. LEX ) THEN
           NOUT = 0
           NOUT = NOUT + 1 ; OUT(NOUT) = '# LOG-ANTAB Format  Version of 2009.08.07'
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           NOUT = NOUT + 1 ; OUT(NOUT) = '# Generator: PIMA (monica_to_antab) '//PIMA__LABEL
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           ID = LINDEX ( FILIN, '/' ) + 1
           NOUT = NOUT + 1 ; OUT(NOUT) = '# Generated from monica file '//TRIM(FILIN(ID:))//' on '//GET_CDATE()
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
           NOUT = NOUT + 1 ; OUT(NOUT) = 'STATION:  '//PIM%STA(ISTA)%IVS_NAME
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILOUT, MP, OUT, NP, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &              'reading existing antab file '//FILOUT )
                RETURN 
           END IF
           NOUT = NP
           DO 440 J4=1,NP
              IF ( OUT(J4)(1:9) == 'NUMB_FRQ:' ) THEN
                   NOUT = J4 - 2
              END IF
 440       CONTINUE 
           IF ( OUT(NOUT)(1:13) == '# Overwritten' ) NOUT = NOUT - 1
      END IF
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '#'
      IF ( LEX ) THEN
           NOUT = NOUT + 1 ; OUT(NOUT) = '# Overwritten using monica file '//TRIM(FILIN(ID:))//' on '//GET_CDATE()
           NOUT = NOUT + 1 ; OUT(NOUT) = '#'
      END IF
      CALL CLRCH  ( STR )
      CALL INCH   ( PIM%NFRQ, STR(1:2) )
      CALL CHASHR ( STR(1:2) )
      NOUT = NOUT + 1 ; OUT(NOUT) = 'NUMB_FRQ:    '//STR(1:2)
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      NOUT = NOUT + 1 ; OUT(NOUT) = '#       Chan     IF_Freq     LO_Freq    Sky_freq   Pol'
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      DO 450 J5=1,PIM%NFRQ
         NOUT = NOUT + 1
         WRITE ( UNIT=OUT(NOUT), FMT=110 ) J5, 0.0D0, 0.0D0, &
     &                                     1.0D-6*PIM%FREQ_ARR(1,J5,PIM%CONF%FRQ_GRP), &
     &                                     'R'
 110     FORMAT ( 'FRQ:  ', 2X, I4, 2X, F10.2, 2X, F10.2, 2X, F10.2, 5X, A1 )
         CALL CHASHR ( OUT(NOUT)(9:12) )
 450  CONTINUE 
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      STR  = '#       Scan  Scan_name   Sou_name    TAI_Time_tag             Ts #01  Ts #02  Ts #03  Ts #04  Ts #05  Ts #06  Ts #07  Ts #08  Ts #09  Ts #10  Ts #11  Ts #12  Ts #13  Ts #14  Ts #15  Ts #16  Ts #17  Ts #18  Ts #19  Ts #20  Ts #21  Ts #22  Ts #23  Ts #24  Ts #25  Ts #26  Ts #27  Ts #28  Ts #29  Ts #30  Ts #31  Ts #32'
      NOUT = NOUT + 1 ; OUT(NOUT) = STR(1:61+8*PIM%NFRQ)
      NOUT = NOUT + 1 ; OUT(NOUT) = '# ' 
      CALL CLRCH  ( STR )
      CALL INCH   ( PIM%NSCA, STR )
      NOUT = NOUT + 1 ; OUT(NOUT) = '# NUMB_TSYS:   '//STR(1:I_LEN(STR))
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
!
      DO 460 J6=1,PIM%NSCA
         TIM_SCA_BEG = PIM%TIM_R8(PIM%SCA(J6)%TIM_IND)
         TIM_SCA_END = PIM%TIM_R8(PIM%SCA(J6)%TIM_IND + PIM%SCA(J6)%NUM_EPC)
         IB = 0
         IE = 0
         DO 470 J7=1,NT
            IF ( TIM_ARR(J7) .GE. TIM_SCA_BEG .AND. TIM_ARR(J7) .LE. TIM_SCA_END ) THEN
                 IF ( IB == 0 ) IB = J7
                 IE = J7
            END IF
 470     CONTINUE 
!!         write ( 6, * ) 'j6= ', int2(j6), ' scan= ', PIM%SCA(J6)%SCAN_NAME, ' ib= ', int2(ib), ' ie= ', int2(ie)
         CALL CLRCH ( STR1 ) 
         STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                           PIM%TIM_R8(PIM%SCA(J6)%TIM_IND + PIM%SCA(J6)%NUM_EPC/2), IER )
              NOUT = NOUT +  1
         IF ( IB < 1 ) THEN
              TSYS_OUT = -1.0
            ELSE
              DO 480 J8=1,PIM%NFRQ
                 TSYS_OUT(J8) = 0.0D0
                 IP = 0
                 DO 490 J9=IB,IE
                    IF ( TSYS_ARR(J9,J8) > TSYS_MIN .AND. TSYS_ARR(J9,J8) < TSYS_MAX ) THEN
                         TSYS_OUT(J8) = TSYS_OUT(J8) + TSYS_ARR(J9,J8) 
                         IP = IP + 1
                    END IF
 490             CONTINUE 
                 IF ( IP == 0 ) THEN
                      TSYS_OUT(J8) = -1.0D0
                    ELSE
                      TSYS_OUT(J8) = TSYS_OUT(J8)/IP
                 END IF
 480          CONTINUE 
         END IF              
         WRITE ( OUT(NOUT), 120 ) J6, &
     &                            PIM%SCA(J6)%SCAN_NAME, &
     &                            PIM%C_SOU(PIM%SCA(J6)%SOU_IND), &
     &                            STR1(1:22), &
     &                            TSYS_OUT(1:PIM%NFRQ)
 120     FORMAT ( 'TSYS:   ',I4, 2X, A, 2X, A, 4X , A, 1X, 32(1X,F7.1) )
 460  CONTINUE 
!
      NOUT = NOUT + 1 ; OUT(NOUT) = '# '
      NOUT = NOUT + 1 ; OUT(NOUT) = '# LOG-ANTAB Format  Version of 2009.08.07'
!      
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT ( NOUT, OUT, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4316, IUER, 'MONICA_TO_ANTAB', 'Error in '// &
     &         'writing into the output file '//FILOUT )
           RETURN 
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Output antab file is written: '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MONICA_TO_ANTAB   !#!#
