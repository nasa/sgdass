      SUBROUTINE PARSE_LOG_PRE911 ( MODE, NBUF, BUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, &
     &                       MJD_TSYS, UTC_TSYS, N_ONS, MJD_ONS, UTC_ONS, &
     &                       SOURCE_ONS, IF_FRQ, LO_FRQ, SCAN_NAME, &
     &                       SOURCE_NAME, TSYS, N_CAB, MJD_CAB, UTC_CAB, &
     &                       CAB, N_ATM, MJD_ATM, UTC_ATM, &
     &                       PRES, TEMP, HUMID, STA_NAM, YEAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_LOG_PRE911
! *                                                                      *
! * ### 10-FEB-2008 PARSE_LOG_PRE911 v2.9 (c) L. Petrov  05-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MODE, NBUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, N_ONS, &
     &           N_CAB, N_ATM, YEAR, IVRB, IUER
      CHARACTER  BUF(NBUF)*(*), SCAN_NAME(M_TIM)*(*), &
     &           SOURCE_NAME(M_TIM)*(*), SOURCE_ONS(M_TIM)*(*), &
     &           STA_NAM*(*)
      INTEGER*4  MJD_TSYS(M_TIM), MJD_ONS(2,M_TIM), MJD_CAB(M_TIM), &
     &           MJD_ATM(M_TIM)
      REAL*8     UTC_TSYS(M_TIM), UTC_ONS(2,M_TIM), UTC_CAB(M_TIM), &
     &           UTC_ATM(M_TIM),  IF_FRQ(M_FRQ), LO_FRQ(M_FRQ), &
     &           TSYS(M_FRQ,M_TIM), CAB(M_TIM), PRES(M_TIM), TEMP(M_TIM), &
     &           HUMID(M_TIM)
      INTEGER*4  MIND, M_LO, M_SET, M_SET5
      PARAMETER  ( MIND   = 64 )
      PARAMETER  ( M_LO   = 20 )
      PARAMETER  ( M_SET  = 30 )
      PARAMETER  ( M_SET5 =  1 )
      CHARACTER  DELIM*12, SETUP_CHR(M_SET)*9, SETUP_CHR5(M_SET5)*10
      REAL*8     TSYS__MIN, TSYS__MAX
      PARAMETER ( DELIM  = ' '// &       !  1
     &                     CHAR(0)// &   !  2
     &                     CHAR(13)// &  !  3
     &                     ','// &       !  4
     &                     ';'// &       !  5
     &                     ':'// &       !  6
     &                     '&'// &       !  7
     &                     '@'// &       !  8
     &                     '='// &       !  9
     &                     '#'// &       ! 10
     &                     '|'// &       ! 11
     &                     '/'        )  ! 12
!
      DATA        SETUP_CHR / &
     &                        '&bbc018 ', & !  1
     &                        '&bbc01? ', & !  2
     &                        '&bbcsx4 ', & !  3
     &                        '&bbcsx8 ', & !  4
     &                        '&bbcsx? ', & !  5
     &                        '&bbc01d ', & !  6
     &                        '&dbbc01d', & !  7
     &                        '&dbbcsx4', & !  8
     &                        '&dbbcsx8', & !  9
     &                        '&setup01', & ! 10
     &                        '&setupsx', & ! 11
     &                        '&sx8v12a', & ! 12
     &                        '&sx8v14a', & ! 13
     &                        '&vc018  ', & ! 14
     &                        '&vc801  ', & ! 15
     &                        '&vc1601 ', & ! 16
     &                        '&vc01d  ', & ! 17
     &                        '&vcsx4  ', & ! 18
     &                        '&vcsx8  ', & ! 19
     &                        '&&ifdsx ', & ! 20
     &                        'bbcsx4  ', & ! 21
     &                        'dbbcsx4 ', & ! 22
     &                        'dbbcsx8 ', & ! 23
     &                        '&dbbc8f8', & ! 24
     &                        '&ifd8f  ', & ! 25
     &                        '&setup8f', & ! 26
     &                        '&vc8f8  ', & ! 27
     &                        '&bbc8f8 ', & ! 28
     &                        '&bbcsxd ', & ! 29
     &                        '&dbbcsxd'  & ! 30
     &                      /
      DATA        SETUP_CHR5 / &
     &                        '/dbbc/dbbc' & !  1
     &                       /
      PARAMETER  ( TSYS__MIN = 8.0 )
      PARAMETER  ( TSYS__MAX = 30000.0 )
      CHARACTER  CDATE*22, VC_LABEL*8, LO_MARK(M_FRQ)*2, IF_LABEL*8, &
     &           LO_LABEL(M_LO)*1, TSYS_LAB(M_FRQ)*4, LO_STR*1, STR*32, &
     &           SOURCE_NAME_CURRENT*10, CURRENT_SCAN_NAME*64, TSYS_SUB_BAND*1, &
     &           FS_VERS*8, TSYS_SUB_USE*1, SUB_BAND*2
      REAL*8     UTC_TAG, LO_FRQ_TAB(M_LO), TSYS_ARR(M_FRQ), TSYS_VAL, FRQ_WID, &
     &           FRQ_ARR(M_FRQ), LO_ARR(M_FRQ)
      INTEGER*4  IND(2,MIND), LIND, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, IFRQ_WD, INDV, IL, IP, &
     &           MJD_TAG, DOY, IND_FRQ, N_LO, NV, LV, REF_CHN(M_FRQ,M_LO), &
     &           K_FRQ, K_CHN, MAX_FRQ, K_LO, IND_SETUP, IER, IER1, IER2, IER3
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      LOGICAL*4  FL_PREOB_SYSTEMP, FL_DISK, FL_NEW_SCAN, FL_FOUND_TSYS, &
     &           FL_TSYS_SX, FL_SETUP_LO
!
      N_TSYS = 0
      N_CAB = 0
      N_ATM = 0
      N_FRQ = 0
      N_ONS = 0
      N_LO  = 0
      NV    = 0
      MAX_FRQ = 0
      MJD_ONS = 0
      UTC_ONS = 0.0D0
      CALL NOUT_R8 ( M_FRQ,      IF_FRQ  )
      CALL NOUT_R8 ( M_FRQ,      LO_FRQ  )
      CALL NOUT_I4 ( M_FRQ*M_LO, REF_CHN )
      FL_PREOB_SYSTEMP = .FALSE.
      CALL CLRCH ( STA_NAM )
      TSYS = 0.0D0
      FL_TSYS_SX  = .FALSE.
      FL_SETUP_LO = .FALSE.
      TSYS_SUB_USE = '?'
      IVRB = 3
!
      SUB_BAND = '  '
      FRQ_WID  = 0.0D0
      CALL GETENVAR ( 'PIMAVAR_FRQ_WID', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F8.2)' ) FRQ_WID
      END IF
      CALL GETENVAR ( 'PIMAVAR_SUB_BAND', STR )
      IF ( ILEN(STR) > 0 ) THEN
           SUB_BAND = STR(1:2)
      END IF
!
      FL_DISK = .FALSE.
      SOURCE_NAME = '??????????'
      CURRENT_SCAN_NAME = '??????????'
      IF ( MODE == 11 ) THEN
           CURRENT_SCAN_NAME = 'No00001'
      END IF
      DO 410 J1=1,NBUF
         CALL TRAN ( 12, BUF(J1), BUF(J1) )
         IF ( MODE == 1  .OR. &
     &        MODE == 2  .OR. &
     &        MODE == 5  .OR. &
     &        MODE == 6  .OR. &
     &        MODE == 25      ) THEN
              IF ( BUF(J1)(12:12) == ':' ) BUF(J1)(12:12) = '.'
              IF ( BUF(J1)(15:15) == ':' ) BUF(J1)(15:15) = '.'
            ELSE IF ( MODE == 3 ) THEN
               CALL INCH ( YEAR, STR(1:4) )
               BUF(J1) = STR(1:4)//'.'//BUF(J1)(1:3)//'.'//BUF(J1)(4:5)//'.'// &
     &                   BUF(J1)(6:7)//'.'//BUF(J1)(8:9)//'.00'//BUF(J1)(10:)
            ELSE IF ( MODE == 11 ) THEN
               IF ( BUF(J1)(1:1) == '#' ) GOTO 410
               CALL INCH ( YEAR, STR(1:4) )
               BUF(J1) = STR(1:4)//'.'//BUF(J1)(1:3)//'.'//BUF(J1)(4:5)//'.'// &
     &                   BUF(J1)(6:7)//'.'//BUF(J1)(8:9)//'.00'//BUF(J1)(10:)
         END IF
         IF ( INDEX ( BUF(J1), 'source=stow' ) > 0 ) GOTO 410
         IND = 1
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -2 )
         IF ( IND(2,2) > LEN(BUF(J1)) - 8 ) GOTO 410
!
! ------ If the line contain less than 2 words -- bypass this line
!
         IF ( LIND .LT. 2 ) GOTO 410
!
! ------ If this line is a comment line -- bypass this line
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'log' .AND. LIND .GE. 9 ) THEN
              FS_VERS = BUF(J1)(IND(1,9):IND(2,9)) 
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'sched_end' ) GOTO 810
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'location' ) THEN
              STA_NAM = BUF(J1)(IND(1,3):IND(2,3))
              CALL TRAN ( 11, STA_NAM, STA_NAM )
              IF ( STA_NAM == 'DSS65   ' ) STA_NAM = 'DSS65A  '
              IF ( STA_NAM == 'MATERA20' ) STA_NAM = 'MATERA  '
              IF ( STA_NAM == 'NYALESUN' ) STA_NAM = 'NYALES20'
              IF ( STA_NAM == 'ONSALA  ' ) STA_NAM = 'ONSALA60'
              IF ( STA_NAM == 'HOBART  ' ) STA_NAM = 'HOBART26'
              IF ( STA_NAM == 'HART26M ' ) STA_NAM = 'HARTRAO '
              IF ( STA_NAM == 'KE12    ' ) STA_NAM = 'KATH12M '
              IF ( STA_NAM == 'WARKWORT' ) STA_NAM = 'WARK12M '
              IF ( STA_NAM == 'WARKWRTH' ) STA_NAM = 'WARK12M '
              IF ( STA_NAM == 'YARRA12 ' ) STA_NAM = 'YARRA12M'
              IF ( STA_NAM == 'YEBES13M' ) STA_NAM = 'RAEGYEB '
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'station' .AND. &
     &        MODE == 11 ) THEN
              STA_NAM = BUF(J1)(IND(1,4):IND(2,4))
              CALL TRAN ( 11, STA_NAM, STA_NAM )
              IF ( STA_NAM == 'YS' ) STA_NAM = 'KVNYS   '
              IF ( STA_NAM == 'US' ) STA_NAM = 'KVNUS   '
              IF ( STA_NAM == 'TN' ) STA_NAM = 'KVNTN   '
         END IF
!
! ------ If the first symbol is not digit -- bypass this line
!
         IF ( INDEX ( '0123456789', BUF(J1)(1:1) ) .LE. 0 ) GOTO 410
!
! ------ Get the word with date/time
!
         CALL CLRCH ( CDATE )
         IF ( BUF(J1)(5:5) == '.' ) THEN
              CDATE = BUF(J1)(1:4)//'.01.01_'//BUF(J1)(10:11)//':'// &
     &                BUF(J1)(13:14)//':'//BUF(J1)(16:22)
            ELSE
              CDATE = '19'//BUF(J1)(1:2)//'.01.01_'//BUF(J1)(6:7)//':'// &
     &                BUF(J1)(8:9)//':'//BUF(J1)(10:11)//'.'//BUF(J1)(12:13)
         END IF
         IER = -2
         CALL DATE_TO_TIME ( CDATE, MJD_TAG, UTC_TAG, IER )
         IF ( BUF(J1)(5:5) == '.' ) THEN
              CALL CHIN ( BUF(J1)(6:8), DOY )
            ELSE
              CALL CHIN ( BUF(J1)(3:5), DOY )
         END IF
         MJD_TAG = MJD_TAG + DOY - 1
!
         IF ( MODE == 5 ) THEN
              IND_SETUP = LTM_DIF ( 0, M_SET5, SETUP_CHR5, BUF(J1)(IND(1,2)-1:IND(1,2)+8)  )
              IF ( IND_SETUP > 0 ) THEN
                   VC_LABEL = 'dbbc'
                   IF ( IVRB .GE. 4 )  write ( 6, * ) 'V5 j1= ', j1, ' vc_label= ', vc_label, ' Word2: >>'//BUF(J1)(IND(1,2):IND(2,2))//'<< '  ! %%%
              END IF
            ELSE 
              IND_SETUP = LTM_DIF ( 0, M_SET, SETUP_CHR, BUF(J1)(IND(1,2)-1:IND(2,2))  )
              IF ( IND_SETUP > 0 .AND. LIND > 2 ) THEN
                   IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == 'bbc' ) THEN
                        VC_LABEL = BUF(J1)(IND(1,3):IND(2,3))
                     ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == 'vci' ) THEN
                        VC_LABEL = BUF(J1)(IND(1,3):IND(1,3)+2)
                        IF_LABEL = 'if'
                     ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+1) == 'vc' ) THEN
                        VC_LABEL = BUF(J1)(IND(1,3):IND(2,3))
                     ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == 'sy=' ) THEN
                        CONTINUE 
                     ELSE 
                        VC_LABEL = BUF(J1)(IND(1,2):IND(2,2))  
                   END IF
                   IF ( IVRB .GE. 3 )  write ( 6, * ) 'V1 vc_label = ', vc_label, ' if_label= ', if_label, ' j1= ', j1  ! %%%
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'systemp12' .AND. N_FRQ == 0 ) THEN
              FL_TSYS_SX = .TRUE.
              N_FRQ = 2
              LO_FRQ(1) = 2020.0D0
              IF_FRQ(1) =    0.0D0
              LO_FRQ(2) = 8080.0D0
              IF_FRQ(2) =    0.0D0
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'setupsx' .AND. LIND  == 8 ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,8)) == '"channel  sky freq  lo freq  video' ) THEN
                   FL_SETUP_LO = .TRUE.
                   TSYS_SUB_USE = 'u'
              END IF 
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'setupsx' .AND. LIND == 7 .AND. &
     &        FL_SETUP_LO ) THEN
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(I4)' ) IND_FRQ
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F8.2)' ) LO_FRQ(IND_FRQ)
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'setupsx' .AND. LIND .GE. 3 ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'bank_check' ) THEN
                   FL_SETUP_LO = .FALSE.
              END IF 
         END IF
!
         IF ( IND_SETUP > 0 .OR. &
     &        ( BUF(J1)(IND(1,2)-1:IND(1,2)+1) == '&if' .AND. &
     &          ( MODE == 1 .OR. MODE == 6 ) )                ) THEN
!
              IF ( BUF(J1)(IND(1,3):IND(1,3)+1) == 'if' ) THEN
                   IF_LABEL = BUF(J1)(IND(1,3):IND(2,3))
              END IF
              IF ( BUF(J1)(IND(1,3):IND(2,3))   == 'lo' .AND. &
     &             BUF(J1)(IND(1,4):IND(1,4)+1) == 'lo'       ) THEN
                   IF_LABEL = BUF(J1)(IND(1,4)+2:IND(1,4)+2)
                   IF ( N_LO > 0 ) THEN
                        IF ( LTM_DIF ( 0, N_LO, LO_LABEL, IF_LABEL ) > 0 ) THEN
                             GOTO 410
                        END IF
                   END IF
!
                   N_LO = N_LO + 1
                   IF ( N_LO > M_LO ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 4729, IUER, 'PARSE_LOG_PRE911', 'Error in parsing '// &
     &                      'line '//STR(1:I_LEN(STR))//' of the log file: '// &
     &                      'too many LO entries' )
                        RETURN
                   END IF
                   LO_LABEL(N_LO) = IF_LABEL 
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
                   IF ( IVRB .GE. 3 )  write ( 6, * ) 'K1 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO), ' frq= ', LO_FRQ_TAB(N_LO)
              END IF
         END IF
!
         IF ( MODE == 2 .OR. MODE == 3 ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == '"lo' .AND. &
     &             ( BUF(J1)(IND(1,4):IND(2,4)) == 'usb' .OR. &
     &               BUF(J1)(IND(1,4):IND(2,4)) == 'lsb'      ) ) THEN
                   DO 4100 J10=4,LIND
                      IF_LABEL = BUF(J1)(IND(1,2)+2:IND(1,2)+2)
                      IF ( N_LO > 0 ) THEN
                           IF ( LTM_DIF ( 0, N_LO, LO_LABEL, IF_LABEL ) > 0 ) THEN
                                GOTO 410
                           END IF
                      END IF
!
                      N_LO = N_LO + 1
                      READ ( UNIT=BUF(J1)(IND(1,J10):IND(2,J10)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
                      LO_LABEL(N_LO) = CHAR(ICHAR('a')+N_LO-1)
                      IF ( IVRB .GE. 3 )  write ( 6, * ) 'M1 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO)
 4100              CONTINUE
                      N_LO = N_LO + 1
                      READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
                      LO_LABEL(N_LO) = IF_LABEL 
                   IF ( IVRB .GE. 3 )  write ( 6, * ) 'M2 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO), ' LO_FRQ_TAB= ', LO_FRQ_TAB(N_LO)
                 ELSE
              END IF
           ELSE IF ( MODE == 5 .OR. MODE == 6 ) THEN
              IF ( BUF(J1)(IND(1,3)-1:IND(2,3)+1) == '/lo=' ) THEN
                   N_LO = N_LO + 1
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
                   LO_LABEL(N_LO) = BUF(J1)(IND(1,4)+2:IND(2,4)+2)
                   IF ( IVRB .GE. 3 )  write ( 6, * ) 'K5 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO), ' LO_FRQ_TAB= ', LO_FRQ_TAB(N_LO)
              END IF
           ELSE IF ( MODE == 11 ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'lo' ) THEN
                   IF_LABEL = BUF(J1)(IND(1,3):IND(2,3))
                   IF ( N_LO > 0 ) THEN
                        IF ( LTM_DIF ( 0, N_LO, LO_LABEL, IF_LABEL ) > 0 ) THEN
                             GOTO 410
                        END IF
                   END IF
                   N_LO = N_LO + 1
                   CALL CLRCH ( LO_LABEL(N_LO) )
                   CALL CLRCH ( LO_MARK(N_LO)  )
                   LO_LABEL(N_LO) = IF_LABEL
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
                   LO_MARK(N_LO) = LO_LABEL(N_LO)
                   N_FRQ = N_LO
                   IF ( IVRB .GE. 3 )  write ( 6, * ) 'K8 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO), ' LO_FRQ_TAB= ', LO_FRQ_TAB(N_LO)
              END IF
              VC_LABEL = 'bbc'
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == VC_LABEL  .AND. &
     &        VC_LABEL(1:6)== 'bbcsx4'                        ) THEN
              CALL CHIN ( BUF(J1)(IND(2,4)-1:IND(2,4)), IND_FRQ )
              IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '.' ) > 0 ) GOTO 410
              IF ( IND_FRQ < 1 ) GOTO 410
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
                   LO_MARK(IND_FRQ) = BUF(J1)(IND(1,6):IND(2,6))
              END IF
           ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == VC_LABEL  .AND. &
     &        VC_LABEL(1:3) == 'bbc'                        ) THEN
              IF ( MODE == 11 ) THEN
                   CALL CHIN ( BUF(J1)(IND(1,4):IND(2,4)), IND_FRQ )
                 ELSE 
                   CALL CHIN ( BUF(J1)(IND(1,3)+3:IND(1,3)+4), IND_FRQ )
              END IF
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   IF ( MODE == 11 ) THEN
                        STR = BUF(J1)(IND(1,5):IND(2,5))//'.0'
                        READ ( UNIT=STR, FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
                        LO_MARK(IND_FRQ) = BUF(J1)(IND(1,3):IND(2,3)) 
                      ELSE 
                        READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
                        LO_MARK(IND_FRQ) = BUF(J1)(IND(1,5):IND(2,5))
                   END IF
              END IF
           ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+2) == VC_LABEL  .AND. &
     &               VC_LABEL(1:3) == 'vci'                    .AND. &
     &               BUF(J1)(IND(1,5):IND(2,5)) == 'agc'             ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2)+3:IND(1,2)+4), IND_FRQ )
              IF ( IND_FRQ > N_FRQ ) N_FRQ = IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
              END IF
              LO_MARK(IND_FRQ) = BUF(J1)(IND(1,LIND):IND(2,LIND))
           ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) == VC_LABEL  .AND. &
     &               VC_LABEL(1:2) == 'vc'                   .AND. &
     &               LIND .GE. 5                                   ) THEN
              CALL CHIN ( BUF(J1)(IND(1,3)+2:IND(1,3)+3), IND_FRQ )
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) FRQ_WID
                   SUB_BAND = BUF(J1)(IND(1,6):IND(2,6))
              END IF
           ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == VC_LABEL(1:3)           .AND. &
     &               ( VC_LABEL(1:3) == 'vci' .OR. VC_LABEL(1:4) == 'dbbc' ) .AND. &
     &               MODE .NE. 5                                             .AND. &
     &               LIND .GE. 5                                                   ) THEN
              CALL CHIN ( BUF(J1)(IND(1,3)+3:IND(1,3)+4), IND_FRQ )
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)' ) IF_FRQ(IND_FRQ)
              END IF
           ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == VC_LABEL  .AND. &
     &               VC_LABEL(1:3) == 'bbc'                        ) THEN
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == '*' ) GOTO 410
              CALL CHIN ( BUF(J1)(IND(1,3)+3:IND(1,3)+4), IND_FRQ )
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 .AND. LIND .GE. 4 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)', IOSTAT=IER  ) IF_FRQ(IND_FRQ)
                   IF ( IER .NE. 0 ) IF_FRQ(IND_FRQ) = 0.0
                   LO_MARK(IND_FRQ) = BUF(J1)(IND(1,5):IND(2,5))
              END IF
           ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == VC_LABEL  .AND. &
     &               VC_LABEL(1:2) == 'vc'                        ) THEN
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == '*' ) GOTO 410
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'alarm' ) GOTO 410
              CALL CHIN ( BUF(J1)(IND(1,3)+2:IND(1,3)+3), IND_FRQ )
              IF ( IND_FRQ > N_FRQ ) N_FRQ =IND_FRQ
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)', IOSTAT=IER ) IF_FRQ(IND_FRQ)
                   IF ( IER .NE. 0 ) IF_FRQ(IND_FRQ) = 0.0
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) FRQ_WID
                   SUB_BAND = BUF(J1)(IND(1,6):IND(2,6))
              END IF
           ELSE IF ( MODE == 5 .AND. BUF(J1)(IND(1,2):IND(2,2)) == VC_LABEL ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2)+9:IND(1,2)+10), IND_FRQ )
              IF ( IND_FRQ > 0 ) THEN
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.2)', IOSTAT=IER ) IF_FRQ(2*(IND_FRQ-1)+1)
!
! ---------------- Get IF width
!
                   READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(I4)', IOSTAT=IER   ) IFRQ_WD
                   IF_FRQ(2*(IND_FRQ-1)+2) = IF_FRQ(2*(IND_FRQ-1)+1)            ! Upper subband
                   IF_FRQ(2*(IND_FRQ-1)+1) = IF_FRQ(2*(IND_FRQ-1)+1) - IFRQ_WD  ! Low   subband
                   LO_MARK(2*(IND_FRQ-1)+1) = BUF(J1)(IND(1,5):IND(2,5))
                   LO_MARK(2*(IND_FRQ-1)+2) = BUF(J1)(IND(1,5):IND(2,5))
                   IF ( IVRB .GE. 4 ) write ( 6, * ) 'F5 j1= ', j1, ' Ind_frq= ', 2*(IND_FRQ-1)+1, ' LO_MARK= ', LO_MARK(2*(IND_FRQ-1)+1), ' IF_Frq: ', IF_FRQ(2*(IND_FRQ-1)+1)
              END IF 
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == IF_LABEL  .AND.  &
     &        IF_LABEL(1:2) == 'if'                   .AND.  &
     &        BUF(J1)(IND(1,3):IND(1,3)+1) == 'lo'    .AND.  &
     &        LIND .GE. 5                                    ) THEN
!
              N_LO = N_LO + 1
              IF ( N_LO > M_LO ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4711, IUER, 'PARSE_LOG_PRE911', 'Error in parsing '// &
     &                 'line '//STR(1:I_LEN(STR))//' of the log file: '// &
     &                 'too many LO entries' )
                   RETURN
              END IF
              LO_LABEL(N_LO) = BUF(J1)(IND(1,4)+2:IND(1,4)+2)
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F8.2)' ) LO_FRQ_TAB(N_LO)
              IF ( IVRB .GE. 3 )  write ( 6, * ) 'M3 j1= ', j1, ' n_lo= ', int2(n_lo), ' lo_label= ', LO_LABEL(N_LO), ' LO_FRQ_TAB= ', LO_FRQ_TAB(N_LO)
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == IF_LABEL  .AND.  &
     &        IF_LABEL(1:2) == 'if'                   .AND.  &
     &        BUF(J1)(IND(1,3):IND(2,3)) == 'patch'   .AND.  &
     &        VC_LABEL(1:2) == 'vc'                   .AND.  &
     &        LIND .GE. 4                                    ) THEN
!
              LO_STR = BUF(J1)(IND(1,4)+2:IND(1,4)+2)
              DO 420 J2=5,LIND
                 STR = BUF(J1)(IND(1,J2):IND(2,J2))
                 IL = ILEN(STR)
                 STR(IL:IL) =  ' '
                 CALL CHIN ( STR, IND_FRQ )
                 LO_MARK(IND_FRQ) = LO_STR
 420          CONTINUE
         END IF
!
         IF ( LIND .GE. 4 ) THEN
              IF ( BUF(J1)(IND(1,2):IND(1,2)+2) == 'ifd'   .AND.  &
     &             BUF(J1)(IND(1,3):IND(2,3)) == 'patch'   .AND.  &
     &             BUF(J1)(IND(1,4):IND(1,4)+1) == 'lo '   .AND.  &
     &             VC_LABEL(1:2) == 'vc'                          ) THEN
!
                   LO_STR = BUF(J1)(IND(1,4)+2:IND(1,4)+2)
                   DO 424 J2=5,LIND
                      STR = BUF(J1)(IND(1,J2):IND(2,J2))
                      IL = ILEN(STR)
                      STR(IL:IL) =  ' '
                      CALL CHIN ( STR, IND_FRQ )
                      IF ( IND_FRQ > 0 ) THEN 
                           LO_MARK(IND_FRQ) = LO_STR
                      END IF
 424               CONTINUE
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'preob'   .AND. &
     &        BUF(J1)(IND(1,3):IND(2,3)) == 'systemp'       ) THEN
!
              FL_PREOB_SYSTEMP = .TRUE.
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'scan_name' .OR. BUF(J1)(IND(1,2):IND(2,2)) == 'MK6_SCAN' ) THEN
              IF ( NV > 0 ) THEN
                   DO 430 J3=1,NV
                      IL = I_LEN(TSYS_LAB(J3))
                      IF ( TSYS_LAB(J3)(IL:IL) == 'u' .OR. &
     &                     TSYS_LAB(J3)(IL:IL) == 'l' .OR. &
     &                     TSYS_LAB(J3)(IL:IL) == 'd'      ) THEN
                           TSYS_LAB(J3)(IL:IL) = ' '
                      END IF
                      CALL CHIN ( TSYS_LAB(J3), IND_FRQ )
                      IF ( TSYS_LAB(J3)(1:1) == 'a' ) IND_FRQ = 10
                      IF ( TSYS_LAB(J3)(1:1) == 'b' ) IND_FRQ = 11
                      IF ( TSYS_LAB(J3)(1:1) == 'c' ) IND_FRQ = 12
                      IF ( TSYS_LAB(J3)(1:1) == 'd' ) IND_FRQ = 13
                      IF ( TSYS_LAB(J3)(1:1) == 'e' ) IND_FRQ = 14
                      IF ( TSYS_LAB(J3)(1:1) == 'f' ) IND_FRQ = 15
                      IF ( TSYS_LAB(J3)(1:1) == 'g' ) IND_FRQ = 16
                      IF ( IND_FRQ > 0 ) THEN
                           TSYS(IND_FRQ,N_TSYS) = TSYS_ARR(J3)
                      END IF
 430               CONTINUE
                 ELSE
                   IF ( .NOT. FL_PREOB_SYSTEMP .AND. &
     &                  .NOT. FL_TSYS_SX             ) THEN
                        IF ( MODE == 1 .AND. N_TSYS > 0 ) N_TSYS = N_TSYS - 1
                   END IF
              END IF
              NV = 0
              IF ( N_TSYS > 0 ) THEN
                   IF ( SOURCE_NAME(N_TSYS) == '??????????' .OR. &
     &                  SCAN_NAME(N_TSYS)   == 'A?????????'      ) THEN
                        N_TSYS = N_TSYS - 1 
                   END IF
              END IF
              N_TSYS = N_TSYS + 1
              MJD_TSYS(N_TSYS) = MJD_TAG
              UTC_TSYS(N_TSYS) = UTC_TAG
!??              CALL NOUT_R8 ( M_FRQ, TSYS(1,N_TSYS) )
              SCAN_NAME(N_TSYS) = BUF(J1)(IND(1,3):IND(2,3))
              IF ( N_TSYS > 1 ) THEN
                   IF ( SCAN_NAME(N_TSYS) == SCAN_NAME(N_TSYS-1) ) THEN
                        N_TSYS = N_TSYS - 1
                   END IF
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)+1) == 'source=' .OR. &
     &        BUF(J1)(IND(1,2):IND(2,2)+1) == '"source='     ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'idle' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'stow' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'hold' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'azel' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'azeluncr' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'track' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == '*' ) THEN
                   CONTINUE
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'disable' ) THEN
                   CONTINUE
                 ELSE
                   SOURCE_NAME_CURRENT = BUF(J1)(IND(1,3):IND(2,3))
                   CALL TRAN ( 11, SOURCE_NAME_CURRENT, SOURCE_NAME_CURRENT )
              END IF
         END IF
         IF ( ( BUF(J1)(IND(1,2):IND(2,2)+1) == 'source='  .OR. &
     &          BUF(J1)(IND(1,2):IND(2,2)+1) == '"source='      ).AND. &
     &        N_TSYS > 0                                               ) THEN
!
              IF ( ILEN(SOURCE_NAME_CURRENT) == 0 ) THEN
                   WRITE ( 6, * ) 'Empty source name on line ',J1
                   CALL EXIT ( 1 )
              END IF
              SOURCE_NAME(N_TSYS) = SOURCE_NAME_CURRENT
         END IF
         IF ( MODE == 11 .AND. BUF(J1)(IND(1,2):IND(2,2)) == 'source' ) THEN
              CALL CLRCH ( SOURCE_NAME_CURRENT )
              CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,3)), SOURCE_NAME_CURRENT )
              IF ( J1 > 3  ) THEN
                   IF ( BUF(J1-1)(1:7) == '# auto1' ) THEN
                        IP = INDEX ( BUF(J1-2), ',' ) 
                        CURRENT_SCAN_NAME = BUF(J1-2)(2:IP-1)
                        CALL CHASHL ( CURRENT_SCAN_NAME )
                     ELSE IF ( BUF(J1-3)(1:1) == '#' ) THEN
                        IP = INDEX ( BUF(J1-3), ',' ) 
                        IF ( IP > 2 ) THEN
                             CURRENT_SCAN_NAME = BUF(J1-3)(2:IP-1)
                             CALL CHASHL ( CURRENT_SCAN_NAME )
                        END IF
                   END IF
              END IF
         END IF
!
         IF ( LIND .GE. 3 ) THEN
              IF ( ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/tsys/'  .OR. &
     &             BUF(J1)(IND(1,3)-1:IND(2,3)+1) == '#tsys/'  .OR. &
     &             BUF(J1)(IND(1,3)-2:IND(2,3)+1) == '"/tsys/'      )  .AND. &
     &           ( MODE == 1 .OR. MODE == 6                         )  .AND. &
     &           N_TSYS > 0                                                  ) THEN
!
                 IF ( ILEN(SOURCE_NAME(N_TSYS)) == 0  .OR. &
     &                SOURCE_NAME(N_TSYS)(1:1) == '?' ) THEN
                      WRITE ( 6, * ) 'T1 Empty source name when processing tsys at line ', j1
                      CALL EXIT ( 1 )
                 END IF
!
                 MJD_TSYS(N_TSYS) = MJD_TAG
                 UTC_TSYS(N_TSYS) = UTC_TAG
                 LV = (LIND - 2)/2
                 IF ( BUF(J1)(IND(1,2):IND(2,2)) == '"' ) LV = (LIND - 3)/2
                 DO 440 J4=1,LV
                    NV = NV + 1
                    IF ( NV > M_FRQ ) THEN
                         WRITE ( 6, * ) 'Line ', J1, ' LV= ', LV, ' M_FRQ= ', M_FRQ
                         WRITE ( 6, * ) 'Labels: ', TSYS_LAB(1:NV)
                         CALL ERR_LOG ( 4712, IUER, 'PARSE_LOG_PRE911', 'Trap of internal '// &
     &                       'control: too many labels' )
                         RETURN 
                    END IF
                    IF ( BUF(J1)(IND(1,2):IND(2,2)) == '"'        .OR. &
     &                   BUF(J1)(IND(1,2)-1:IND(1,2)+2) == '#tpi'      ) THEN
                         TSYS_LAB(NV) = BUF(J1)(IND(1,4+(J4-1)*2):IND(2,4+(J4-1)*2))
                       ELSE 
                         TSYS_LAB(NV) = BUF(J1)(IND(1,3+(J4-1)*2):IND(2,3+(J4-1)*2))
                    END IF
                    IF ( NV > 1 ) THEN
                         INDV = LTM_DIF ( 0, NV-1, TSYS_LAB, TSYS_LAB(NV) )
                         IF ( INDV .LE. 0 ) INDV = NV
                       ELSE
                         INDV = 1
                    END IF
!
                    IF ( TSYS_LAB(INDV)(I_LEN(TSYS_LAB(INDV)):I_LEN(TSYS_LAB(INDV))) == 'u' .OR. &
     &                   TSYS_LAB(INDV)(I_LEN(TSYS_LAB(INDV)):I_LEN(TSYS_LAB(INDV))) == 'd' .OR. &
     &                   TSYS_LAB(INDV)(I_LEN(TSYS_LAB(INDV)):I_LEN(TSYS_LAB(INDV))) == 'h' .OR. &
     &                   TSYS_LAB(INDV)(I_LEN(TSYS_LAB(INDV)):I_LEN(TSYS_LAB(INDV))) == 'l' ) THEN
                         IF ( BUF(J1)(IND(1,2):IND(2,2)) == '"'        .OR. &
     &                        BUF(J1)(IND(1,2)-1:IND(1,2)+2) == '#tpi'      ) THEN
                              READ ( UNIT=BUF(J1)(IND(1,4+(J4-1)*2+1):IND(2,4+(J4-1)*2+1)), &
     &                               FMT='(F8.1)', IOSTAT=IER ) TSYS_ARR(INDV)
                            ELSE 
                              READ ( UNIT=BUF(J1)(IND(1,3+(J4-1)*2+1):IND(2,3+(J4-1)*2+1)), &
     &                               FMT='(F8.1)', IOSTAT=IER ) TSYS_ARR(INDV)
                         END IF
                         IF ( IER .NE. 0 ) TSYS_ARR(INDV) = -1.0D0
                         IF ( TSYS_ARR(INDV) < TSYS__MIN ) TSYS_ARR(INDV) = -1.0D0
                         IF ( TSYS_ARR(INDV) > TSYS__MAX ) TSYS_ARR(INDV) = -1.0D0
                         IF ( INDV < NV ) NV = NV - 1
                       ELSE
                         NV = NV - 1
                         GOTO 440
                    END IF
 440             CONTINUE
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/tsys/'  .AND. &
     &        ( MODE   == 2  .OR.  MODE == 3 )            .AND. &
     &        N_TSYS > 0                                  .AND. &
     &        .NOT. FL_TSYS_SX                                  ) THEN

!
              IF ( ILEN(SOURCE_NAME(N_TSYS)) == 0  .OR. &
     &             SOURCE_NAME(N_TSYS)(1:1) == '?' ) THEN
                   WRITE ( 6, * ) 'T1 Empty source name when processing tsys at line ', j1
                   CALL EXIT ( 1 )
              END IF
!
              MJD_TSYS(N_TSYS) = MJD_TAG
              UTC_TSYS(N_TSYS) = UTC_TAG
              LV = (LIND - 2)/2
              DO 540 J4=1,LV
                 NV = NV + 1
                 TSYS_LAB(NV) = BUF(J1)(IND(1,3+(J4-1)*2):IND(2,3+(J4-1)*2))
                 IF ( TSYS_LAB(NV)(I_LEN(TSYS_LAB(NV)):I_LEN(TSYS_LAB(NV))) == 'u' .OR. &
     &                TSYS_LAB(NV)(I_LEN(TSYS_LAB(NV)):I_LEN(TSYS_LAB(NV))) == 'l' .OR. &
     &                TSYS_LAB(NV)(I_LEN(TSYS_LAB(NV)):I_LEN(TSYS_LAB(NV))) == 'd' .OR. &
     &                TSYS_LAB(NV)(I_LEN(TSYS_LAB(NV)):I_LEN(TSYS_LAB(NV))) == 'h' ) THEN
                      READ ( UNIT=BUF(J1)(IND(1,3+(J4-1)*2+1):IND(2,3+(J4-1)*2+1)), &
     &                       FMT='(F8.1)', IOSTAT=IER ) TSYS_ARR(NV)
                      IF ( IER .NE. 0 ) TSYS_ARR(NV) = -1.0D0
                      IF ( TSYS_ARR(NV) < TSYS__MIN ) TSYS_ARR(NV) = -1.0D0
                      IF ( TSYS_ARR(NV) > TSYS__MAX ) TSYS_ARR(NV) = -1.0D0
                    ELSE
                      NV = NV - 1
                      GOTO 540
                 END IF
 540          CONTINUE
         END IF
!
         IF ( ( MODE == 2  .OR.  MODE == 3 )             .AND. &
     &        ( BUF(J1)(IND(1,2)-1:IND(2,2)) == '&sxcts' .AND. &
     &          BUF(J1)(IND(1,3):IND(1,3)+3) == 'tsys'         ) &
     &        ) THEN
              CALL CHIN ( BUF(J1)(IND(1,3)+4:IND(1,3)+4), K_FRQ )
              IF ( K_FRQ > 0 ) THEN
                   MAX_FRQ = MAX ( MAX_FRQ, K_FRQ )
                   DO 550 J5=1,M_FRQ
                      CALL CHIN ( BUF(J1)(IND(1,3+J5):IND(2,3+J5)-1), K_CHN )
                      IF ( K_CHN < 0 ) GOTO 850
!@                    write ( 6, * ) ' j5= ',j5,' k_chn = ', k_chn, ' k_frq =',k_frq
                      REF_CHN(J5,K_FRQ) = K_CHN
 550               CONTINUE
 850               CONTINUE
              END IF
         END IF
         IF ( ( MODE == 2  .OR.  MODE == 3 )         .AND. &
     &        N_TSYS > 0                             .AND. &
     &        BUF(J1)(IND(1,2):IND(1,2)+3) == 'tsys' .AND. &
     &        .NOT. FL_TSYS_SX                             ) THEN
              CALL CHIN ( BUF(J1)(IND(1,2)+4:IND(1,2)+4), K_FRQ )
              IF ( K_FRQ > 0  .AND.  K_FRQ .LE. MAX_FRQ ) THEN
                   DO 560 J6=1,M_FRQ
                      IF ( REF_CHN(J6,K_FRQ) == 0 ) GOTO 860
                      READ ( UNIT=BUF(J1)(IND(1,2+J6):IND(2,2+J6)), &
     &                       FMT='(F6.1)', IOSTAT=IER ) TSYS(REF_CHN(J6,K_FRQ),N_TSYS)
                      IF ( IER .NE. 0 ) TSYS(REF_CHN(J6,K_FRQ),N_TSYS) = -1.0D0
 560               CONTINUE
 860               CONTINUE
              END IF
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'tsys'  .AND. MODE == 11 ) THEN
              FL_FOUND_TSYS = .FALSE.
              DO 570 J7=1,N_LO
                 IF ( BUF(J1)(IND(1,3):IND(2,3)) == '0'//LO_LABEL(J7) ) THEN
                      IF ( .NOT. FL_FOUND_TSYS ) THEN
                            FL_FOUND_TSYS = .TRUE.
                            IF ( BUF(J1-1)(IND(1,2):IND(2,2)) .NE. 'tsys' ) THEN
                                 N_TSYS = N_TSYS + 1
                            END IF
                      END IF
                      READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F12.6)' ) TSYS_VAL
                      DO 580 J8=1,N_FRQ
                         IF ( LO_MARK(J8) == LO_LABEL(J7) ) THEN
                              TSYS(J8,N_TSYS) = TSYS_VAL
                         END IF
 580                  CONTINUE 
                      SOURCE_NAME(N_TSYS) = SOURCE_NAME_CURRENT
                      MJD_TSYS(N_TSYS) = MJD_TAG
                      UTC_TSYS(N_TSYS) = UTC_TAG
                      SCAN_NAME(N_TSYS) = CURRENT_SCAN_NAME
                 END IF
 570          CONTINUE
         END IF
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'tsys'  .AND. MODE == 25 ) THEN
              IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'u5' ) THEN
                   N_FRQ = 1
                   IF ( INDEX ( BUF(J1)(IND(1,4):IND(2,4)), '$' ) > 0 ) THEN
                        TSYS(1,N_TSYS) = -1.0D0
                      ELSE
                        READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F12.6)' ) TSYS(1,N_TSYS)
                   END IF
                   IF ( TSYS(1,N_TSYS) < TSYS__MIN ) TSYS(1,N_TSYS) = -1.0D0
                   IF ( TSYS(1,N_TSYS) > TSYS__MAX ) TSYS(1,N_TSYS) = -1.0D0
              END IF
         END IF
         IF ( BUF(J1)(IND(1,3):IND(2,3)) == '"user_device'  .AND. MODE == 25 ) THEN
              IF ( BUF(J1)(IND(1,4):IND(2,4)) == 'u5' ) THEN
                   N_LO = 1
                   READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F12.6)' ) LO_FRQ(1)
                   LO_FRQ_TAB(N_LO) = LO_FRQ(1)
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '#tsys#' .AND. &
     &        BUF(J1)(IND(1,3):IND(2,3)-1)   == 'tsys'   .AND. &
     &        .NOT. FL_TSYS_SX                                 ) THEN
              IF ( ILEN(SOURCE_NAME(N_TSYS)) == 0  .OR. &
     &             SOURCE_NAME(N_TSYS)(1:1) == '?' ) THEN
                   WRITE ( 6, * ) 'Empty source name when processing tsys at line ', j1
                   CALL EXIT ( 1 )
              END IF
              IF ( BUF(J1)(IND(2,3):IND(2,3)) == 'x' .OR. &
     &             BUF(J1)(IND(2,3):IND(2,3)) == 's'      ) GOTO 410
!
              MJD_TSYS(N_TSYS) = MJD_TAG
              UTC_TSYS(N_TSYS) = UTC_TAG
              CALL CHIN ( BUF(J1)(IND(2,3):IND(2,3)), N_FRQ )
              IF ( N_FRQ < 1 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4712, IUER, 'PARSE_LOG_PRE911', 'Failure in reading '// &
     &                 'the IF index from line '//STR(1:I_LEN(STR))// &
     &                 ' of the log file IF_label: '//BUF(J1)(IND(2,3):IND(2,3)) )
                   RETURN
              END IF
              IF_FRQ(N_FRQ) = 1.0
              LO_FRQ(N_FRQ) = 1.0
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F8.1)', IOSTAT=IER ) &
     &               TSYS(N_FRQ,N_TSYS)
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '#tsys#' .AND. &
     &        BUF(J1)(IND(1,3):IND(2,3)) == 'tsyss'      .AND. &
     &        FL_TSYS_SX                                       ) THEN
!
              IF ( N_TSYS == 0 ) N_TSYS = 1
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F12.6)' ) TSYS(1,N_TSYS)
              SOURCE_NAME(N_TSYS) = SOURCE_NAME_CURRENT
              MJD_TSYS(N_TSYS) = MJD_TAG
              UTC_TSYS(N_TSYS) = UTC_TAG
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '#tsys#' .AND. &
     &        BUF(J1)(IND(1,3):IND(2,3)) == 'tsysx'      .AND. &
     &        FL_TSYS_SX                                       ) THEN
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F12.6)' ) TSYS(2,N_TSYS)
         END IF
         IF ( J1 > 1 .AND. IND(2,2)-2 > IND(1,2) ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)-2) == '"tsys' ) THEN
                   IF ( BUF(J1-1)(IND(1,2):IND(1,2)+9) == '"frequency' ) THEN
                        MJD_TSYS(N_TSYS)  = MJD_TAG
                        UTC_TSYS(N_TSYS)  = UTC_TAG
                        SOURCE_NAME(N_TSYS) = SOURCE_NAME_CURRENT
                        FL_TSYS_SX = .TRUE.
                   END IF 
                   DO 590 J9=4,LIND,2
                      READ ( UNIT=BUF(J1)(IND(2,J9)-1:IND(2,J9)),   FMT='(I2)' ) IND_FRQ
                      READ ( UNIT=BUF(J1)(IND(1,J9+1):IND(2,J9+1)), FMT='(F6.2)' ) TSYS(IND_FRQ,N_TSYS) 
 590               CONTINUE 
              END IF
         END IF
         IF ( MODE == 5 .AND. BUF(J1)(IND(1,2):IND(1,2)+4) == '"tsys' .AND. &
     &        BUF(J1)(IND(1,2)+7:IND(1,2)+10) == '[k]:' ) THEN
              IF ( IVRB .GE. 3 ) WRITE ( 6, * ) 'T5 j1= ', j1, ' N_TSYS= ', INT2(N_TSYS), ' N_FRQ= ', INT2(N_FRQ)
              IF ( .NOT. FL_FOUND_TSYS ) THEN
                   FL_FOUND_TSYS = .TRUE.
                   N_TSYS = N_TSYS + 1
                   MJD_TSYS(N_TSYS)  = MJD_TAG
                   UTC_TSYS(N_TSYS)  = UTC_TAG
                   SOURCE_NAME(N_TSYS) = SOURCE_NAME_CURRENT
              END IF
!               
              DO 5100 J10=4,LIND,2
                 IF ( BUF(J1)(IND(1,J10):IND(1,J10)+2) == 'bbc' ) THEN
                      IND(1,J10) = IND(1,J10) + 3
                 END IF
                 READ ( UNIT=BUF(J1)(IND(1,J10):IND(2,J10)),     FMT='(I2)'   ) IND_FRQ
                 IND_FRQ = 2*(IND_FRQ-1) + 1
                 READ ( UNIT=BUF(J1)(IND(1,J10+1):IND(2,J10+1)), FMT='(F6.2)' ) TSYS(IND_FRQ,N_TSYS) 
                 IND_FRQ = IND_FRQ + 1
                 READ ( UNIT=BUF(J1)(IND(1,J10+1):IND(2,J10+1)), FMT='(F6.2)' ) TSYS(IND_FRQ,N_TSYS) 
                 N_FRQ = MAX ( IND_FRQ, N_FRQ )
 5100         CONTINUE 
         END IF
!
         IF ( BUF(J1)(22:32) == 'disk_record' .OR. &
     &        BUF(J1)(22:29) == 'mk5b_rec'         ) THEN
              FL_DISK = .TRUE.
         END IF
         IF ( FL_DISK .AND. &
     &        ( BUF(J1)(22:35) == 'disk_record=on' .OR. &
     &          BUF(J1)(15:26) == '"data start"'   .OR. &
     &          BUF(J1)(22:33) == 'mk5b_rec/ on'        ) ) THEN
!
              N_ONS = N_ONS + 1
              MJD_ONS(1,N_ONS)  = MJD_TAG
              UTC_ONS(1,N_ONS)  = UTC_TAG
              SOURCE_ONS(N_ONS) = SOURCE_NAME_CURRENT
         END IF
         IF ( ( BUF(J1)(21:26) == ':preob' .OR. &
     &          BUF(J1)(14:19) == ':preob'      ) ) THEN
!
              FL_NEW_SCAN = .FALSE.
              IF ( FL_DISK .AND. N_ONS > 0 ) THEN
!
! ---------------- It may happen that in DISK mode command disk_record=on may be skipped
!
                   IF ( FS_VERS < '9.10.4' .AND. &
     &                  SOURCE_NAME_CURRENT .NE. SOURCE_ONS(N_ONS) ) THEN
!
                        FL_NEW_SCAN = .TRUE.
                   END IF
              END IF
              IF ( .NOT. FL_DISK ) FL_NEW_SCAN = .TRUE.
              IF ( FL_NEW_SCAN ) THEN
                   N_ONS = N_ONS + 1
                   MJD_ONS(1,N_ONS)  = MJD_TAG
                   UTC_ONS(1,N_ONS)  = UTC_TAG
                   SOURCE_ONS(N_ONS) = SOURCE_NAME_CURRENT
              END IF
         END IF
!
         IF ( N_ONS > 0 ) THEN
              IF ( MJD_ONS(2,N_ONS) == 0 ) THEN
!
! ---------------- It may happen that in DISK mode command disk_record=off may be skipped
!
                   IF ( BUF(J1)(22:36) == 'disk_record=off' .OR. &
     &                  BUF(J1)(15:25) == '"data stop"'     .OR. &
     &                  BUF(J1)(22:34) == 'mk5b_rec/ off'        ) THEN
!
                        MJD_ONS(2,N_ONS) = MJD_TAG
                        UTC_ONS(2,N_ONS) = UTC_TAG
                   END IF
!
                   IF ( BUF(J1)(21:27) == ':postob' .OR. &
    &                   BUF(J1)(14:20) == ':postob'      ) THEN
!
                        MJD_ONS(2,N_ONS) = MJD_TAG
                        UTC_ONS(2,N_ONS) = UTC_TAG
                  END IF
              END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '/cable/' ) THEN
!
! ----------- Extract cable delay
!
              N_CAB = N_CAB + 1
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(E20.10)', &
     &               IOSTAT=IER ) CAB(N_CAB)
              MJD_CAB(N_CAB) = MJD_TAG
              UTC_CAB(N_CAB) = UTC_TAG
              IF ( IER .EQ. 0 ) THEN
                   CAB(N_CAB) = CAB(N_CAB)*1.D-6
                 ELSE
                   N_CAB = N_CAB - 1
              END IF
         END IF
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '/wx/' .OR. &
     &        BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '/wx|' .OR. &
     &        BUF(J1)(IND(1,2):IND(2,2)+1)   ==  '"wx/' .OR. &
     &        BUF(J1)(IND(1,2):IND(2,2)+1)   ==  'wx/'       ) THEN
!
! ----------- Extract meteorological parameters
!
              N_ATM = N_ATM + 1
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F5.0)', &
     &               IOSTAT=IER1 ) TEMP(N_ATM)
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.0)', &
     &               IOSTAT=IER2 ) PRES(N_ATM)
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F5.0)', &
     &               IOSTAT=IER3 ) HUMID(N_ATM)
              IF ( STA_NAM == 'CRIMEA' ) THEN
!
! ---------------- Convert pressure from millimeter of mercurury
! ---------------- to millibars
!
                   IF ( PRES(N_ATM) > 650.0D0 .AND. &
     &                  PRES(N_ATM) < 850.0D0       ) THEN
                        PRES(N_ATM) = PRES(N_ATM)*1013.25/760.0D0
                   END IF
              END IF
              MJD_ATM(N_ATM) = MJD_TAG
              UTC_ATM(N_ATM) = UTC_TAG
              IF ( IER1 == 0  .AND.  IER2 == 0  .AND.  IER3 == 0 .AND. &
     &             PRES(N_ATM) > 0.0 ) THEN
!
                   TEMP(N_ATM) = TEMP(N_ATM) + 276.16D0
                   PRES(N_ATM) = PRES(N_ATM)*100.0D0
                 ELSE
                   N_ATM = N_ATM - 1
              END IF
           ELSE IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '#wx#' ) THEN
!
! ----------- Extract meteorological parameters
!
              N_ATM = N_ATM + 1
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F5.0)', &
     &               IOSTAT=IER1 ) TEMP(N_ATM)
              READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.0)', &
     &               IOSTAT=IER2 ) PRES(N_ATM)
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F5.0)', &
     &               IOSTAT=IER3 ) HUMID(N_ATM)
              MJD_ATM(N_ATM) = MJD_TAG
              UTC_ATM(N_ATM) = UTC_TAG
              IF ( IER1 == 0  .AND.  IER2 == 0  .AND.  IER3 == 0 .AND. &
     &             PRES(N_ATM) > 0.0 ) THEN
!
                   TEMP(N_ATM) = TEMP(N_ATM) + 276.16D0
                   PRES(N_ATM) = PRES(N_ATM)*100.0D0
                 ELSE
                   N_ATM = N_ATM - 1
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
      IF ( NV > 0 ) THEN
           DO 450 J5=1,NV
              IL = I_LEN(TSYS_LAB(J5))
!!            write ( 6 , * ) ' j5= ', int2(j5), ' tsys_lab: '//tsys_lab(j5) ! %%%%%%%%%%%%%%%%%%
              IF ( TSYS_LAB(J5)(IL:IL) == 'u' .OR. &
     &             TSYS_LAB(J5)(IL:IL) == 'l' .OR. &
     &             TSYS_LAB(J5)(IL:IL) == 'd'      ) THEN
                   TSYS_SUB_BAND = TSYS_LAB(J5)(IL:IL) 
                   TSYS_LAB(J5)(IL:IL) = ' '
              END IF
              IF ( TSYS_SUB_USE == 'u' ) TSYS_SUB_BAND = 'u'
              CALL CHIN ( TSYS_LAB(J5), IND_FRQ )
              IF ( TSYS_LAB(J5)(1:1) == 'a' ) IND_FRQ = 10
              IF ( TSYS_LAB(J5)(1:1) == 'b' ) IND_FRQ = 11
              IF ( TSYS_LAB(J5)(1:1) == 'c' ) IND_FRQ = 12
              IF ( TSYS_LAB(J5)(1:1) == 'd' ) IND_FRQ = 13
              IF ( TSYS_LAB(J5)(1:1) == 'e' ) IND_FRQ = 14
              IF ( TSYS_LAB(J5)(1:1) == 'f' ) IND_FRQ = 15
              IF ( TSYS_LAB(J5)(1:1) == 'g' ) IND_FRQ = 16
              IF ( IND_FRQ > 0 ) THEN
                   TSYS(IND_FRQ,N_TSYS) = TSYS_ARR(J5)
                   IF ( FS_VERS < "9.10.2" ) THEN
                        IF ( TSYS_SUB_BAND == 'l' ) IF_FRQ(IND_FRQ) = -IF_FRQ(IND_FRQ) 
                   END IF
              END IF
 450       CONTINUE
      END IF
      IF ( IVRB .GE. 3 ) WRITE ( 6, * ) 'N_FRQ= ', N_FRQ, ' N_LO= ', N_LO, ' N_TSYS= ', N_TSYS
      DO 460 J6=1,N_FRQ
         DO 470 J7=1,N_LO
            IF ( LO_MARK(J6) == LO_LABEL(J7) ) THEN
                 LO_FRQ(J6) = LO_FRQ_TAB(J7)
            END IF
 470     CONTINUE
 460  CONTINUE
!
      IF ( N_TSYS > 1 ) THEN
           IF ( SCAN_NAME(N_TSYS) == '??????????' ) N_TSYS = N_TSYS - 1
      END IF
!
      IF ( SUB_BAND == 'ul' ) THEN
           DO 480 J8=1,N_TSYS
              TSYS_ARR(1:N_FRQ) = TSYS(1:N_FRQ,J8)
              DO 490 J9=1,N_FRQ
                 TSYS(2*(J9-1)+1,J8)   = TSYS_ARR(J9)
                 TSYS(2*(J9-1)+2,J8) = TSYS_ARR(J9)
 490          CONTINUE
 480       CONTINUE
!
           FRQ_ARR(1:N_FRQ) = IF_FRQ(1:N_FRQ)
           LO_ARR(1:N_FRQ)  = LO_FRQ(1:N_FRQ)
           DO 4110 J11=1,N_FRQ
              IF_FRQ(2*(J11-1)+1) = FRQ_ARR(J11) - FRQ_WID
              IF_FRQ(2*(J11-1)+2) = FRQ_ARR(J11)
              LO_FRQ(2*(J11-1)+1) = LO_ARR(J11)
              LO_FRQ(2*(J11-1)+2) = LO_ARR(J11)
 4110      CONTINUE 
           N_FRQ = 2*N_FRQ
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PARSE_LOG_PRE911  !#!#
