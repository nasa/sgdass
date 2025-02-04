      SUBROUTINE PIMA_PARSE_LOG ( MODE, NBUF, BUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, &
     &                       MJD_TSYS, UTC_TSYS, N_ONS, MJD_ONS, UTC_ONS, &
     &                       SOURCE_ONS, IF_FRQ, LO_FRQ, POL_FRQ, SCAN_NAME, &
     &                       SOURCE_TSYS, TSYS, N_CAB, MJD_CAB, UTC_CAB, &
     &                       CAB, N_ATM, MJD_ATM, UTC_ATM, &
     &                       PRES, TEMP, HUMID, STA_NAM, YEAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PARSE_LOG
! *                                                                      *
! * ### 10-FEB-2008 PIMA_PARSE_LOG v3.5  (c)  L. Petrov  27-DEC-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  MODE, NBUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, N_ONS, &
     &           N_CAB, N_ATM, YEAR, IVRB, IUER
      CHARACTER  BUF(NBUF)*(*), SCAN_NAME(M_TIM)*(*), POL_FRQ(M_FRQ)*(*), &
     &           SOURCE_TSYS(M_TIM)*(*), SOURCE_ONS(M_TIM)*(*), &
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
      PARAMETER  ( M_SET  = 28 )
      PARAMETER  ( M_SET5 =  1 )
      CHARACTER  DELIM*12
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
      PARAMETER  ( TSYS__MIN = 8.0 )
      PARAMETER  ( TSYS__MAX = 30000.0 )
      CHARACTER  CDATE*22, FS_VERS*8, TSYS_SUB_USE*1, IF_SETUP*8, LO_SETUP*8, &
     &           WORD2*32, WORD3*32
      REAL*8     UTC_TAG, LO_FRQ_TAB(M_LO), TSYS_ARR(M_FRQ), TSYS_VAL, FRQ_WID, &
     &           FRQ_ARR(M_FRQ), LO_ARR(M_FRQ), UTC_SCA(M_TIM), TIM_DIF
      REAL*8     TEMP_C_MIN, TEMP_C_MAX, PRES_MBAR_MIN, PRES_MBAR_MAX, TIM_DIF_CAB_MIN
      PARAMETER  ( TEMP_C_MIN = -60.0D0 )
      PARAMETER  ( TEMP_C_MAX =  60.0D0 )
      PARAMETER  ( PRES_MBAR_MIN =  400.0D0 )
      PARAMETER  ( PRES_MBAR_MAX = 1100.0D0 )
      PARAMETER  ( TIM_DIF_CAB_MIN = 0.499999D0 ) ! Minumum difference in consecutive &
     &                                            ! time epoch for cable delay
      INTEGER*4  IND(2,MIND), LIND, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           DOY, IND_FRQ, N_LO, K_TSYS, IER, IER1, IER2, IER3, IND_1ST, &
     &           MAX_IND_BBC
      INTEGER*4  N_BBC, LO__IND(M_FRQ), SUB_LO(M_FRQ), LO_IND_BBC(M_FRQ), &
     &           IND_BBC, IND_SUB, N_SCA, LV, IND_LO, MJD_TAG, MJD_SCA(M_TIM), &
     &           BBC_MODE
      REAL*8     FRQ_BBC(M_FRQ), BW_BBC(M_FRQ), FRQ_LO(M_FRQ), TAU_TAG, &
     &           TSYS_TMP(M_FRQ,2,M_TIM)
      LOGICAL*1  SUB_BBC(M_FRQ,2)
      CHARACTER  STR*128, LO_LABEL(M_FRQ), LO_LABEL_BBC(M_FRQ), &
     &           POL_LO(M_FRQ), IF_LABEL(M_FRQ)*3, SOU_SCA(M_TIM)*16
      LOGICAL*1  FL_DISK, FL_NEW_SCAN, FL_RDTC
!
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      N_SCA  = 0
      N_TSYS = 0
      N_CAB = 0
      N_ATM = 0
      N_FRQ = 0
      N_ONS = 0
      N_LO  = 0
      MJD_ONS = 0
      UTC_ONS = 0.0D0
      IF_FRQ  = 0.0D0
      LO_FRQ  = 0.0D0
      CALL CLRCH ( STA_NAM )
      TSYS = 0.0D0
      FL_DISK = .FALSE.
      IF_SETUP = '????????'
      LO_SETUP = '????????'
      LO_IND_BBC = -1
      N_BBC    =  0
      TSYS_TMP =  0.0D0
      SUB_BBC  = .FALSE.
      MAX_IND_BBC = -1
      BBC_MODE = 0
      IVRB = 2 ! %%
!
      FL_RDTC = .FALSE.
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IF ( BUF(J1)(1:1) == '!' ) GOTO 410
         CALL TRAN ( 12, BUF(J1), BUF(J1) )
         IF ( BUF(J1)(12:12) == ':' ) BUF(J1)(12:12) = '.'
         IF ( BUF(J1)(15:15) == ':' ) BUF(J1)(15:15) = '.'
         IF ( INDEX ( BUF(J1), 'source=stow' ) > 0 ) GOTO 410
         IND = 0
         LIND = 0
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -2 )
         IF ( IND(2,2) > LEN(BUF(J1)) - 8 ) GOTO 410
!
! ------ If the line contain less than 2 words -- bypass this line
!
         IF ( LIND .LT. 2 ) GOTO 410
!
         IF ( BUF(J1)(IND(1,2)-1:IND(1,2)+3) == '#rdtc' ) THEN
              IF ( .NOT. FL_RDTC .AND. IVRB > 0 ) THEN
                   WRITE ( 6, * ) 'PIMA_GET_ANTAB: input file provides raw #rdtc '// &
     &                            'data. These data should be processed with '// &
     &                            'log2ant. No tsys will be extractd' 
              END IF
              FL_RDTC = .TRUE.
              GOTO 410
         END IF
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
              IF ( STA_NAM == 'MGO12M  ' ) STA_NAM = 'MACGO12M'
              IF ( STA_NAM == 'HOBART  ' ) STA_NAM = 'HOBART26'
              IF ( STA_NAM == 'HART26M ' ) STA_NAM = 'HARTRAO '
              IF ( STA_NAM == 'KE12    ' ) STA_NAM = 'KATH12M '
              IF ( STA_NAM == 'WARKWORT' ) STA_NAM = 'WARK12M '
              IF ( STA_NAM == 'WARKWRTH' ) STA_NAM = 'WARK12M '
              IF ( STA_NAM == 'YARRA12 ' ) STA_NAM = 'YARRA12M'
              IF ( STA_NAM == 'YEBES13M' ) STA_NAM = 'RAEGYEB '
              IF ( STA_NAM == 'IRBENE  ' ) STA_NAM = 'IRBENE32'
              IF ( STA_NAM == 'NYAL13NS' ) STA_NAM = 'NYALE13S'
              IF ( STA_NAM == 'RAEGSMA ' ) STA_NAM = 'RAEGSMAR'
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
         IF ( LIND .GE. 3 ) THEN
              IF ( INDEX ( BUF(J1)(IND(1,2):IND(1,2)+4), 'setup'    ) > 0 .OR. &
     &             INDEX ( BUF(J1)(IND(1,2):IND(1,2)+6), 'setmode'  ) > 0      ) THEN
!
                   IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'bbc_gain'  ) GOTO 410
                   IF ( BUF(J1)(IND(1,3):IND(1,3)+1) == 'vc'   .OR. &
     &                  BUF(J1)(IND(1,3):IND(1,3)+2) == 'vci'  .OR. &
     &                  BUF(J1)(IND(1,3):IND(1,3)+2) == 'bbc'  .OR. &
     &                  BUF(J1)(IND(1,3):IND(1,3)+2) == 'dbc'  .OR. &
     &                  BUF(J1)(IND(1,3):IND(1,3)+3) == 'dbbc'      ) THEN
                        IF_SETUP = BUF(J1)(IND(1,3):IND(2,3))
                        N_BBC = 0
                        IF ( IVRB .GE. 3 ) WRITE ( 6, * ) 'EE J1= ', J1, ' IF_SETUP= ', IF_SETUP
                      ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+1) == 'if' ) THEN
                        LO_SETUP = BUF(J1)(IND(1,3):IND(2,3))
                        N_LO = 0
                   END IF
             END IF
        END IF
        IF ( LIND > 3 ) THEN
             IF ( MODE == 7                               .AND. &
     &            BUF(J1)(IND(1,2):IND(1,2)+4) == 'setup' .AND. &
     &            BUF(J1)(IND(1,3):IND(2,3))   == 'vsi1'          ) THEN
                  MAX_IND_BBC = 0
                  DO 520 J2=4,LIND
                     IF_LABEL(J2-3) = BUF(J1)(IND(1,J2):IND(2,J2))
                     MAX_IND_BBC = MAX_IND_BBC + 1
 520              CONTINUE 
             END IF
        END IF        
        IF ( INDEX ( BUF(J1)(IND(1,2)-1:IND(1,2)+7), '&setmode_'  ) > 0      ) THEN
             IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'lo=' ) THEN
                  N_LO = 0
                  N_BBC = 0
             END IF
             IF ( BUF(J1)(IND(1,3):IND(1,3)+3) == 'lo=l' ) THEN
                  LO_SETUP = BUF(J1)(IND(1,2)+1:IND(2,2))
                  N_LO = N_LO + 1
                  LO_LABEL(N_LO) = BUF(J1)(IND(2,4):IND(2,4))
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,5):IND(2,5))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.2)' ) FRQ_LO(N_LO)
                  IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'usb' ) THEN
                       SUB_LO(N_LO) =  1
                     ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'lsb' ) THEN
                       SUB_LO(N_LO) = -1
                  END IF
                  IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'rcp' ) THEN
                       POL_LO(N_LO) = 'R'
                     ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'lcp' ) THEN
                       POL_LO(N_LO) = 'L'
                  END IF
               ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == 'bbc' ) THEN
                  N_BBC = N_BBC + 1
                  N_BBC = N_BBC + 1
                  CALL CHIN ( BUF(J1)(IND(1,3)+3:IND(2,3)), IND_BBC )
                  MAX_IND_BBC = MAX ( IND_BBC, MAX_IND_BBC )
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,4):IND(2,4))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.2)' ) FRQ_BBC(IND_BBC)
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,6):IND(2,6))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.3)' ) BW_BBC(IND_BBC)
                  SUB_BBC(IND_BBC,1) = .TRUE.
                  SUB_BBC(IND_BBC,2) = .TRUE.
                  LO_LABEL_BBC(IND_BBC) = BUF(J1)(IND(1,5):IND(2,5))
                  IF ( IVRB .GE. 3  ) write ( 6, * ) ' j1= ', j1, ' ind_bbc= ', ind_bbc, ' lo_label_bbc= ', lo_label_bbc(ind_bbc), sngl(frq_bbc(ind_bbc)), sngl(bw_bbc(ind_bbc)) !%%%
             END IF
        END IF
!
        IF ( BUF(J1)(IND(1,2):IND(2,2)) == IF_SETUP ) THEN
             IF ( BUF(J1)(IND(1,3):IND(1,3)+2) == 'bbc'      ) THEN
                  N_BBC = N_BBC + 1
                  CALL CHIN ( BUF(J1)(IND(1,3)+3:IND(2,3)), IND_BBC )
                  MAX_IND_BBC = MAX ( IND_BBC, MAX_IND_BBC )
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,4):IND(2,4))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.2)' ) FRQ_BBC(IND_BBC)
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,6):IND(2,6))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.3)' ) BW_BBC(IND_BBC)
                  SUB_BBC(IND_BBC,1) = .TRUE.
                  SUB_BBC(IND_BBC,2) = .TRUE.
                  LO_LABEL_BBC(IND_BBC) = BUF(J1)(IND(1,5):IND(2,5))
                  IF ( IVRB .GE. 3  ) write ( 6, * ) ' j1= ', j1, ' ind_bbc= ', ind_bbc, ' lo_label_bbc= ', lo_label_bbc(ind_bbc), sngl(frq_bbc(ind_bbc)), sngl(bw_bbc(ind_bbc)) !%%%
               ELSE IF ( BUF(J1)(IND(1,3):IND(1,3)+1) == 'vc' ) THEN
                  N_BBC = N_BBC + 1
                  CALL CHIN ( BUF(J1)(IND(1,3)+2:IND(2,3)), IND_BBC )
                  MAX_IND_BBC = MAX ( IND_BBC, MAX_IND_BBC )
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,4):IND(2,4))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.2)' ) FRQ_BBC(IND_BBC)
!
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,5):IND(2,5))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.3)' ) BW_BBC(IND_BBC)
                  IF ( INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'l'  ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'ul' ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'hd' ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'd'  ) > 0      ) THEN
                       SUB_BBC(IND_BBC,1) = .TRUE.
                  END IF
                  IF ( INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'u'  ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'ul' ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'h'  ) > 0 .OR. &
     &                 INDEX ( BUF(J1)(IND(1,6):IND(2,6)), 'hd' ) > 0      ) THEN
                       SUB_BBC(IND_BBC,2) = .TRUE.
                  END IF
                  LO_LABEL_BBC(IND_BBC) = '?'
                  IF ( IVRB .GE. 3  ) write ( 6, * ) ' j1= ', j1, ' ind_bbc= ', ind_bbc, ' lo_label_bbc= ', lo_label_bbc(ind_bbc) !%%%
!@               ELSE IF ( BUF(J1)(IND(1,2):IND(1,2)+2) == 'vci' ) THEN
!@                  N_BBC = N_BBC + 1
!@                  CALL CHIN ( BUF(J1)(IND(1,2)+3:IND(2,2)), IND_BBC )
!@                  MAX_IND_BBC = MAX ( IND_BBC, MAX_IND_BBC )
!@!
!@                  READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)),   FMT='(F10.2)' ) FRQ_BBC(IND_BBC)
!@                  READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)-1), FMT='(F10.3)' ) BW_BBC(IND_BBC)
!@                  IF ( INDEX ( BUF(J1)(IND(2,4):IND(2,4)), 'l'  ) > 0  ) THEN
!@                       SUB_BBC(IND_BBC,1) = .TRUE.
!@                  END IF
!@                  IF ( INDEX ( BUF(J1)(IND(2,4):IND(2,4)), 'u'  ) > 0 ) THEN
!@                       SUB_BBC(IND_BBC,2) = .TRUE.
!@                  END IF
!@                  LO_LABEL_BBC(IND_BBC) = '?'
!@                  IF ( IVRB .GE. 3  ) write ( 6, * ) ' j1= ', j1, ' ind_bbc= ', ind_bbc, ' lo_label_bbc= ', lo_label_bbc(ind_bbc) !%%%
             END IF
        END IF
!
        IF ( BUF(J1)(IND(1,2):IND(2,2)) == LO_SETUP ) THEN
             IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'lo' .AND. LIND > 3 ) THEN
                  N_LO = N_LO + 1
                  LO_LABEL(N_LO) = BUF(J1)(IND(2,4):IND(2,4))
                  CALL CLRCH ( STR ) 
                  STR = BUF(J1)(IND(1,5):IND(2,5))
                  IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                  READ ( UNIT=STR, FMT='(F10.2)' ) FRQ_LO(N_LO)
                  IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'usb' ) THEN
                       SUB_LO(N_LO) =  1
                     ELSE IF ( BUF(J1)(IND(1,6):IND(2,6)) == 'lsb' ) THEN
                       SUB_LO(N_LO) = -1
                  END IF
                  IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'rcp' ) THEN
                       POL_LO(N_LO) = 'R'
                     ELSE IF ( BUF(J1)(IND(1,7):IND(2,7)) == 'lcp' ) THEN
                       POL_LO(N_LO) = 'L'
                  END IF
                  DO 420 J2=1,MAX_IND_BBC
                     IF ( LO_LABEL_BBC(J2) == LO_LABEL(N_LO) ) THEN
                          LO_IND_BBC(J2) = N_LO
                     END IF
 420              CONTINUE 
                  IF ( IVRB .GE. 3  ) write ( 6, * ) ' j1= ', j1, ' n_lo= ', n_lo, ' lo_label(n_lo)= ', lo_label(n_lo), ' frq_lo= ', sngl(frq_lo(n_lo)), ' || ', BUF(J1)(IND(1,5):IND(2,5)) !%%%
             END IF
             IF ( BUF(J1)(IND(1,3):IND(2,3)) == 'patch' .AND. LIND > 3 ) THEN
                  IND_LO = LTM_DIF ( 0, N_LO, LO_LABEL, BUF(J1)(IND(2,4):IND(2,4)) )
                  IF ( IND_LO > 0 ) THEN
                       DO 430 J3=5,LIND
                          CALL ERR_PASS ( IUER, IER )
                          CALL GET_BBC_IND ( MODE, MAX_IND_BBC, IF_LABEL, &
     &                                       BUF(J1)(IND(1,J3):IND(2,J3)), IND_BBC, IND_SUB, IER )
                          IF ( IER .NE. 0 ) THEN
                               CALL INCH ( J1, STR )
                               CALL ERR_LOG ( 1411, IUER, 'PIMA_PARSE_LOG', 'Error in parsing '// &
     &                             'the '//TRIM(STR)//' string of the log file: '//BUF(J1) )
                               RETURN
                          END IF
                          LO_IND_BBC(IND_BBC) = IND_LO
 430                  CONTINUE 
                    ELSE 
                      CALL INCH ( J1, STR )
                      CALL ERR_LOG ( 1412, IUER, 'PIMA_PARSE_LOG', 'Error in parsing '// &
     &                    'the '//TRIM(STR)//' string of the log file: '//BUF(J1) )
                      RETURN
                  END IF
             END IF
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'scan_name' ) THEN
              N_SCA = N_SCA + 1
              SCAN_NAME(N_SCA) = BUF(J1)(IND(1,3):IND(2,3)) 
              MJD_SCA(N_SCA) = MJD_TAG
              UTC_SCA(N_SCA) = UTC_TAG
         END IF
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) == 'source' ) THEN
              IF ( N_SCA < 1 ) GOTO 410
              SOU_SCA(N_SCA) = BUF(J1)(IND(1,3):IND(2,3)) 
              CALL TRAN ( 11, SOU_SCA(N_SCA), SOU_SCA(N_SCA) )
         END IF
!
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/tsys/'  .OR. &
     &        BUF(J1)(IND(1,3)-1:IND(2,3)+1) == '#tsys/'  .OR. &
     &        BUF(J1)(IND(1,3)-2:IND(2,3)+1) == '"/tsys/'      ) THEN
!
              IF ( N_SCA < 1 ) GOTO 410
              MJD_TSYS(N_SCA) = MJD_TAG
              UTC_TSYS(N_SCA) = UTC_TAG
              IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/tsys/'  ) IND_1ST = 3
              IF ( BUF(J1)(IND(1,3)-1:IND(2,3)+1) == '#tsys/'  ) IND_1ST = 4
              IF ( BUF(J1)(IND(1,3)-2:IND(2,3)+1) == '"/tsys/' ) IND_1ST = 4
!              
              DO 440 J4=IND_1ST,LIND,2
                 CALL ERR_PASS ( IUER, IER )
                 IF ( BUF(J1)(IND(1,J4):IND(1,J4)) == 'i' ) GOTO 440
                 CALL GET_BBC_IND ( MODE, MAX_IND_BBC, IF_LABEL, &
     &                              BUF(J1)(IND(1,J4):IND(2,J4)), IND_BBC, IND_SUB, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL INCH ( J1, STR )
                      GOTO 410
!                      CALL ERR_LOG ( 1413, IUER, 'PIMA_PARSE_LOG', 'Error in parsing '// &
!     &                    'the '//TRIM(STR)//' string of the log file: '//BUF(J1) )
!                      RETURN 
!                      LO_IND_BBC(IND_BBC) = IND_LO
                 END IF
                 CALL CLRCH ( STR ) 
                 STR = BUF(J1)(IND(1,J4+1):IND(2,J4+1))
                 IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
                 READ ( UNIT=STR, FMT='(F8.1)', IOSTAT=IER ) TSYS_VAL
                 IF ( IER .NE. 0 ) TSYS_VAL = 0.0
                 TSYS_TMP(IND_BBC,IND_SUB,N_SCA) = TSYS_VAL
                 IF ( MODE == 7 ) SUB_BBC(IND_BBC,1) = .TRUE.
 440          CONTINUE
         END IF
!
         IF ( BUF(J1)(IND(1,3):IND(2,3)-1) == 'tsys' ) THEN
              CALL CHIN ( BUF(J1)(IND(2,3):IND(2,3)), IND_LO )
              CALL CLRCH ( STR ) 
              STR = BUF(J1)(IND(1,4):IND(2,4))
              IF ( INDEX ( STR, '.' ) < 1 ) STR = TRIM(STR)//'.0'
              READ ( UNIT=STR, FMT='(F8.1)', IOSTAT=IER ) TSYS_VAL
              DO 450 J5=1,MAX_IND_BBC
                 IF ( LO_IND_BBC(J5) == IND_LO ) THEN
                      IF ( SUB_BBC(J5,1) ) THEN
                           TSYS_TMP(J5,1,N_SCA) = TSYS_VAL
                      END IF
                      IF ( SUB_BBC(J5,2) ) THEN
                           TSYS_TMP(J5,2,N_SCA) = TSYS_VAL
                      END IF
                 END IF
 450          CONTINUE 
         END IF
!
         IF ( BUF(J1)(22:32) == 'disk_record' .OR. &
     &        BUF(J1)(22:29) == 'mk5b_rec'         ) THEN
              FL_DISK = .TRUE.
         END IF
         IF ( FL_DISK   .AND. &
     &        N_SCA > 0 .AND. &
     &        ( BUF(J1)(22:35) == 'disk_record=on' .OR. &
     &          BUF(J1)(15:26) == '"data start"'   .OR. &
     &          BUF(J1)(22:33) == 'mk5b_rec/ on'        ) ) THEN
!
              N_ONS = N_ONS + 1
              MJD_ONS(1,N_ONS)  = MJD_TAG
              UTC_ONS(1,N_ONS)  = UTC_TAG
              SOURCE_ONS(N_ONS) = SOU_SCA(N_SCA)
         END IF
         IF ( ( BUF(J1)(21:26) == ':preob' .OR. &
     &          BUF(J1)(14:19) == ':preob'      ) ) THEN
!
              FL_NEW_SCAN = .FALSE.
              IF ( .NOT. FL_DISK ) FL_NEW_SCAN = .TRUE.
              IF ( FL_NEW_SCAN .AND. N_SCA > 0 ) THEN
                   N_ONS = N_ONS + 1
                   MJD_ONS(1,N_ONS)  = MJD_TAG
                   UTC_ONS(1,N_ONS)  = UTC_TAG
                   SOURCE_ONS(N_ONS) = SOU_SCA(N_SCA)
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
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/cable/' .OR. &
     &        BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/CDMS/'  .OR. &
     &        BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/cdms/'       ) THEN
!
! ----------- Extract cable delay
!
              N_CAB = N_CAB + 1
              MJD_CAB(N_CAB) = MJD_TAG
              UTC_CAB(N_CAB) = UTC_TAG
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(E20.10)', &
     &               IOSTAT=IER ) CAB(N_CAB)
              IF ( IER .EQ. 0 ) THEN
                   IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) == '/cable/' ) THEN
                        CAB(N_CAB) = CAB(N_CAB)*1.D-6
                      ELSE
                        CAB(N_CAB) = CAB(N_CAB)*1.D-12
                   END IF
                   IF ( N_CAB > 1 ) THEN
!
! --------------------- Check whether the time tag is at least 
! --------------------- TIM_DIF_CAB_MIN  seconds after the previous cable epoch
!
                        TIM_DIF = (MJD_CAB(N_CAB) - MJD_CAB(N_CAB-1))*86400.0D0 + &
     &                            (UTC_CAB(N_CAB) - UTC_CAB(N_CAB-1))
                        IF ( TIM_DIF < TIM_DIF_CAB_MIN ) THEN
                             N_CAB = N_CAB - 1
                        END IF
                   END IF
                 ELSE
                   N_CAB = N_CAB - 1
              END IF
         END IF
!
         IF ( LIND .GE. 4 ) THEN
              IF ( BUF(J1)(IND(1,3)-1:IND(2,3)+1) ==  '/cdms/' ) THEN
!
! ---------------- Extract cable delay
!
                   N_CAB = N_CAB + 1
                   MJD_CAB(N_CAB) = MJD_TAG
                   UTC_CAB(N_CAB) = UTC_TAG
                   READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(E20.10)', &
     &                    IOSTAT=IER ) CAB(N_CAB)
                   IF ( IER .EQ. 0 ) THEN
                        CAB(N_CAB) = CAB(N_CAB)*1.D-12
                        IF ( N_CAB > 1 ) THEN
!
! -------------------------- Check whether the time tag is at least 
! -------------------------- TIM_DIF_CAB_MIN  seconds after the previous cable epoch
!
                             TIM_DIF = (MJD_CAB(N_CAB) - MJD_CAB(N_CAB-1))*86400.0D0 + &
     &                                 (UTC_CAB(N_CAB) - UTC_CAB(N_CAB-1))
                             IF ( TIM_DIF < TIM_DIF_CAB_MIN ) THEN
                                  N_CAB = N_CAB - 1
                             END IF
                        END IF
                      ELSE
                        N_CAB = N_CAB - 1
                   END IF
              END IF
         END IF
         IF ( BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '/wx/' .OR. &
     &        BUF(J1)(IND(1,2)-1:IND(2,2)+1) ==  '/wx|' .OR. &
     &        BUF(J1)(IND(1,2):IND(2,2)+1)   ==  '"wx/' .OR. &
     &        BUF(J1)(IND(1,2):IND(2,2)+1)   ==  'wx/'       ) THEN
              IF ( LIND < 5 ) goto 410
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
              IF ( IER1 == 0  .AND.  &
     &             IER2 == 0  .AND.  &
     &             IER3 == 0  .AND.  &
     &             TEMP(N_ATM) > TEMP_C_MIN    .AND. &
     &             TEMP(N_ATM) < TEMP_C_MAX    .AND. &
     &             PRES(N_ATM) > PRES_MBAR_MIN .AND. &
     &             PRES(N_ATM) < PRES_MBAR_MAX       ) THEN
!
                   TEMP(N_ATM) = TEMP(N_ATM) + 273.16D0
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
                   TEMP(N_ATM) = TEMP(N_ATM) + 273.16D0
                   PRES(N_ATM) = PRES(N_ATM)*100.0D0
                 ELSE
                   N_ATM = N_ATM - 1
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
      N_TSYS = N_SCA
      N_FRQ = 0
      DO 460 J6=1,MAX_IND_BBC
!!       write ( 6, * ) 'j6= ', int2(j6), ' sub_bbc= ',sub_bbc(j6,1:2) ! %%%
         IF ( SUB_BBC(J6,1) ) THEN
              N_FRQ = N_FRQ + 1
              IF ( IVRB .GE. 4 ) write ( 6, * ) 'Bl j6= ', int2(j6), ' n_frq= ', int2(n_frq), ' lo_ind_bbc= ', int2(lo_ind_bbc(j6)) ! %%
              IF ( LO_IND_BBC(J6) < 1 ) GOTO 460
              IF ( SUB_LO(LO_IND_BBC(J6)) == 1 ) THEN
                   IF_FRQ(N_FRQ)  =  FRQ_BBC(J6) - BW_BBC(J6)
                 ELSE IF ( SUB_LO(LO_IND_BBC(J6)) == -1 ) THEN
                   IF_FRQ(N_FRQ)  = -FRQ_BBC(J6) - BW_BBC(J6)
              END IF
              LO_FRQ(N_FRQ)  = FRQ_LO(LO_IND_BBC(J6))
              POL_FRQ(N_FRQ) = POL_LO(LO_IND_BBC(J6))
         END IF
         IF ( SUB_BBC(J6,2) ) THEN
              N_FRQ = N_FRQ + 1
              IF ( IVRB .GE. 4 ) write ( 6, * ) 'Bu j6= ', int2(j6), ' n_frq= ', int2(n_frq), ' lo_ind_bbc= ', int2(lo_ind_bbc(j6)) ! %%
              IF ( LO_IND_BBC(J6) < 1 ) GOTO 460
              IF ( SUB_LO(LO_IND_BBC(J6)) == 1 ) THEN
                   IF_FRQ(N_FRQ)  =  FRQ_BBC(J6)
                 ELSE 
                   IF_FRQ(N_FRQ)  = -FRQ_BBC(J6)
              END IF
              LO_FRQ(N_FRQ)  = FRQ_LO(LO_IND_BBC(J6))
              POL_FRQ(N_FRQ) = POL_LO(LO_IND_BBC(J6))
         END IF
 460  CONTINUE 
      IF ( IVRB .GE. 4 ) WRITE ( 6, * ) 'MAX_IND_BBC = ', MAX_IND_BBC
!
      N_TSYS = 0
      DO 470 J7=1,N_SCA
         IND_FRQ = 0
         K_TSYS = 0
         N_TSYS = N_TSYS + 1
         MJD_TSYS(N_TSYS) = MJD_SCA(J7)
         UTC_TSYS(N_TSYS) = UTC_SCA(J7)
         SOURCE_TSYS(N_TSYS) = SOU_SCA(J7)
         DO 480 J8=1,MAX_IND_BBC
            IF ( SUB_BBC(J8,1) ) THEN
                 IND_FRQ = IND_FRQ + 1
                 IF ( IVRB .GE. 4 ) write ( 6, * ) 'l-Sca: ',j7, ' ibbc= ', int2(j8), ' ind_frq= ', int2(ind_frq), ' tsys= ', sngl(tsys_tmp(j8,1,j7)), sngl(tsys_tmp(j8,2,j7)) ! %%%%%
                 IF ( TSYS_TMP(J8,1,J7) > TSYS__MIN .AND. &
     &                TSYS_TMP(J8,1,J7) < TSYS__MAX       ) THEN
                      TSYS(IND_FRQ,N_TSYS) = TSYS_TMP(J8,1,J7) 
                      K_TSYS = K_TSYS + 1
                    ELSE
                      TSYS(IND_FRQ,N_TSYS) = -1.0D0
                 END IF
            END IF
!
            IF ( SUB_BBC(J8,2) ) THEN
                 IND_FRQ = IND_FRQ + 1
                 IF ( IVRB .GE. 4 ) write ( 6, * ) 'u-Sca: ',j7, ' ibbc= ', int2(j8), ' ind_frq= ', int2(ind_frq), ' tsys= ', sngl(tsys_tmp(j8,1,j7)), sngl(tsys_tmp(j8,2,j7)) ! %%%%%
                 IF ( TSYS_TMP(J8,2,J7) > TSYS__MIN .AND. &
     &                TSYS_TMP(J8,2,J7) < TSYS__MAX       ) THEN
                      TSYS(IND_FRQ,N_TSYS) = TSYS_TMP(J8,2,J7) 
                      K_TSYS = K_TSYS + 1
                    ELSE
                      TSYS(IND_FRQ,N_TSYS) = -1.0D0
                 END IF
            END IF
            IF ( SUB_BBC(J8,1) .AND. SUB_BBC(J8,2) ) THEN
                 IF ( TSYS(IND_FRQ,N_TSYS)   .EQ. -1.0D0 .AND. &
     &                TSYS(IND_FRQ-1,N_TSYS) .NE. -1.0D0       ) THEN
                      TSYS(IND_FRQ,N_TSYS) = TSYS(IND_FRQ-1,N_TSYS) 
                   ELSE IF ( TSYS(IND_FRQ,N_TSYS)   .NE. -1.0D0 .AND. &
     &                TSYS(IND_FRQ-1,N_TSYS) .EQ. -1.0D0       ) THEN
                      TSYS(IND_FRQ,N_TSYS) = TSYS(IND_FRQ,N_TSYS) 
                 END IF
            END IF
 480     CONTINUE 
         IF ( K_TSYS == 0 ) THEN
              N_TSYS = N_TSYS - 1
         END IF
 470  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_PARSE_LOG  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_BBC_IND ( MODE, NIF, IF_LABEL, STR, IND_BBC, IND_SUB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_BBC_IND
! *                                                                      *
! *  ### 05-SEP-2016  GET_BBC_IND   v1.0 (c) L. Petrov  05-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, IND_BBC, IND_SUB, NIF, IUER 
      CHARACTER  STR*(*), IF_LABEL(*)*(*)
      INTEGER*4  IL, J1
      INTEGER*4, EXTERNAL :: ILEN
!
      IL = ILEN(STR)
      IF ( MODE == 7 ) THEN
           IND_BBC = 0
           DO 410 J1=1,NIF
              IF ( STR == IF_LABEL(J1) ) IND_BBC = J1
 410       CONTINUE 
           IND_SUB = 1
        ELSE
           IF (        STR(IL:IL) == 'd' .OR. STR(IL:IL) == 'l' ) THEN
                IND_SUB = 1
             ELSE IF ( STR(IL:IL) == 'u' .OR. STR(IL:IL) == 'h' ) THEN
                IND_SUB = 2
             ELSE
                CALL ERR_LOG ( 1431, IUER, 'GET_BBC_IND', 'Cannot parse '// &
          &         'BBC index: '//TRIM(STR) )
                RETURN 
           END IF
           IF ( STR(1:IL-1) == 'a' ) THEN
                IND_BBC = 10
             ELSE IF ( STR(1:IL-1) == 'b' ) THEN
                IND_BBC = 11
             ELSE IF ( STR(1:IL-1) == 'c' ) THEN
                IND_BBC = 12
             ELSE IF ( STR(1:IL-1) == 'd' ) THEN
                IND_BBC = 13
             ELSE IF ( STR(1:IL-1) == 'e' ) THEN
                IND_BBC = 14
             ELSE IF ( STR(1:IL-1) == 'f' ) THEN
                IND_BBC = 15
             ELSE
               CALL CHIN ( STR(1:IL-1), IND_BBC )
           END IF
           IF ( IND_BBC < 1 ) THEN
                CALL ERR_LOG ( 1432, IUER, 'GET_BBC_IND', 'Cannot parse '// &
          &         'BBC index '//TRIM(STR) )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE GET_BBC_IND  !#!#
