      SUBROUTINE PIMA_WRITE_OBS ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_WRITE_OBS
! *                                                                      *
! * ### 10-JAN-2006  PIMA_WRITE_OBS v 1.39 (c) L. Petrov 11-MAY-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER  FINAM*128, STR*128, STR_SCA*10, STR_EXC*1, STA1*8, STA2*8, &
     &           STR_ALP*15, STR_DEL*16, STR_COR_ALP*15, STR_COR_DEL*16, &
     &           INTERNET_HOSTNAME*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, J21, J22, J23, J24, J25, J26, &
     &           J27, J28, J29, IB, IE, LAST_SCA, LAST_OBS, IP, LUN, NFRG, &
     &           IND_OBS, IND_AUT, TIM_IND, SIZE_BYTES, NUM_WRONG_DATES, &
     &           NUM_A, NUM_B, NUM_C, NUM_D, NUM_F, NUM_E, NUM_S, NUM_T, NUM_Z, NUM_H, &
     &           KUM_A, KUM_B, KUM_C, KUM_D, KUM_F, KUM_E, KUM_S, KUM_T, KUM_Z, KUM_H, &
     &           NUM_GOOD, KUM_GOOD, NUM, KUM, KCHN, MAX_NUM_OBS, LEN_MDC, KPOL, &
     &           IER
      REAL*8     S_COR(3), DIST
      INTEGER*4  LBND, LCHN
      REAL*8     FREQ_BEG, FREQ_END, FREQ_AVR, FREQ_SQR, FREQ_EFF, AP_LEN
      REAL*8     BAND__MAX, FRQ__MIN
      PARAMETER  ( BAND__MAX = 1.0D9 )
      PARAMETER  ( FRQ__MIN  = 1.0D4 )
      LOGICAL*4  LEX, FL_OBS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT, MAX_I4
!
      IF ( PIM%NPOL == 1 ) THEN
           KPOL = 1
         ELSE
           KPOL = 2
      END IF
!
      IF ( PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE       ) THEN
!
! ======== Make UV-file (Do not do it if we in GEAN or OPAL mode)
!
! -------- Build the name of the output ascii file .uv
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.uv'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
           AP_LEN = PIM%TIM_R8(2) - PIM%TIM_R8(1)
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7611, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           LAST_SCA = 0
           NUM_WRONG_DATES = 0
!
! -------- First letter "T" stands for the total number of points
!
           NUM_A = 0
           NUM_B = 0
           NUM_C = 0
           NUM_D = 0
           NUM_E = 0
           NUM_F = 0
           NUM_S = 0
           NUM_T = 0
           NUM_Z = 0
           NUM_H = 0
           NUM   = 0
           NUM_GOOD = 0
!
! -------- First letter "K" stands for the number of cross-correlated
!
           KUM_A = 0
           KUM_B = 0
           KUM_C = 0
           KUM_D = 0
           KUM_E = 0
           KUM_F = 0
           KUM_S = 0
           KUM_T = 0
           KUM_H = 0
           KUM   = 0
           KUM_GOOD = 0
!
           DO 410 J1=1,PIM%NUV
              IF ( PIM%UV_IND(J1)%SCA_IND > 0 .AND. &
     &             PIM%UV_IND(J1)%SCA_IND .NE. LAST_SCA ) THEN
                   WRITE  ( UNIT=LUN, FMT=110, IOSTAT=IER ) '#SCA: ', &
     &                      PIM%UV_IND(J1)%SCA_IND
 110               FORMAT ( A, I6, 78X,' ' )
                   LAST_SCA = PIM%UV_IND(J1)%SCA_IND
              END IF
!
              IF ( PIM%UV_IND(J1)%TIM_IND > 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%UV_IND(J1)%TIM_IND), IER )
                   IF ( IER .NE. 0 ) THEN
                        IF ( PIM%UV_IND(J1)%POI_IND .NE. -1 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J1, STR )
                             WRITE ( 6, * ) 'J1= ', J1
                             WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                             WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                               PIM%TIM_R8(PIM%UV_IND(J1)%TIM_IND)
                             CALL ERR_LOG ( 7612, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                           'in an attempt to write the Julian date for '// &
     &                           'uv point '//STR )
                             NUM_WRONG_DATES = NUM_WRONG_DATES + 1
                        END IF
!
                        STR = 'Unknown date                 '
                   END IF
                   NUM = NUM + 1
                   IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                        KUM = KUM + 1
                   END IF
                   IF ( PIM%UV_IND(J1)%POI_IND == PIMA__EXCL ) THEN
                        STR_EXC = 'e'
                        NUM_E = NUM_E + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_E = KUM_E + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__AUTO_1 ) THEN
                        STR_EXC = 'a'
                        NUM_A = NUM_A + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_A = KUM_A + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__AUTO_2 ) THEN
                        STR_EXC = 'b'
                        NUM_B = NUM_B + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_B = KUM_B + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__NO_SCA ) THEN
                        STR_EXC = 'c'
                        NUM_C = NUM_C + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_C = KUM_C + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__SHORT  ) THEN
                        STR_EXC = 's'
                        NUM_S = NUM_S + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_S = KUM_S + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__NO_TIM ) THEN
                        STR_EXC = 't'
                        NUM_T = NUM_T + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_T = KUM_T + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__DFAP  ) THEN
                        STR_EXC = 'f'
                        NUM_F = NUM_F + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_F = KUM_F + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND == PIMA__CHAIN ) THEN
                        STR_EXC = 'h'
                        NUM_H = NUM_H + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_H = KUM_H + 1
                        END IF
                     ELSE IF ( PIM%UV_IND(J1)%POI_IND < -1 ) THEN
                        STR_EXC = 'd'
                        NUM_D = NUM_D + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_D = KUM_D + 1
                        END IF
                     ELSE
                        STR_EXC = ' '
                        NUM_GOOD = NUM_GOOD + 1
                        IF ( PIM%UV_IND(J1)%STA_IND(1) .NE. PIM%UV_IND(J1)%STA_IND(2) ) THEN
                             KUM_GOOD = KUM_GOOD + 1
                        END IF
                   END IF
                 ELSE
                   STR = '0000.00.00_00:00:00.0000000   '
                   STR_EXC = 'z'
                   NUM_Z = NUM_Z
              END IF
!
              IF ( PIM%UV_IND(J1)%SCA_IND > 0 ) THEN
                   STR_SCA = PIM%SCA(PIM%UV_IND(J1)%SCA_IND)%SCAN_NAME(1:10)
                 ELSE
                   STR_SCA = '        -1'
              END IF
!
              IF ( PIM%UV_IND(J1)%SOU_IND > 0    .AND. &
     &             PIM%UV_IND(J1)%STA_IND(1) > 0 .AND. &
     &             PIM%UV_IND(J1)%STA_IND(2) > 0 .AND. &
     &             PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE ) THEN
!
                   WRITE ( UNIT=LUN, FMT=120, IOSTAT=IER ) &
     &                     PIM%UV_IND(J1)%SCA_IND, STR_SCA, PIM%UV_IND(J1)%OBS_IND, &
     &                     STR(1:29), &
     &                     PIM%SOU(PIM%UV_IND(J1)%SOU_IND)%IVS_NAME, &
     &                     PIM%STA(PIM%UV_IND(J1)%STA_IND(1))%IVS_NAME, &
     &                     PIM%STA(PIM%UV_IND(J1)%STA_IND(2))%IVS_NAME, J1, &
     &                     STR_EXC, PIM%UV_IND(J1)%ORIG_IND, &
     &                     PIM%UV_IND(J1)%FIL_IND, PIM%UV_IND(J1)%FRG_IND, &
     &                     PIM%UV_IND(J1)%POI_AUT_IND, PIM%UV_IND(J1)%NEXT_UV_IND
 120               FORMAT ( I6, 1X, A, 1X, I6, 1X, A, 1X, A, 1X, A, 1X, A, 1X, &
     &                      I11, 1X, A, 2X, I11, 2X, I4, 2X, I2, 2X, I11, 1X, I11, &
     &                      1X, I11 )
                 ELSE
                   WRITE ( UNIT=LUN, FMT=120, IOSTAT=IER ) &
     &                     PIM%UV_IND(J1)%SCA_IND, STR_SCA, PIM%UV_IND(J1)%OBS_IND, &
     &                     STR(1:29), 'unknown ', 'unknown ', 'unknown ', &
     &                     J1, STR_EXC, 0, 0, 0
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 7613, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write in the output file '// &
     &                  FINAM(1:I_LEN(FINAM))//' error '//STR )
                   RETURN
              END IF
 410       CONTINUE
           CLOSE ( UNIT=LUN )
           IF ( NUM_WRONG_DATES > 0 .AND. PIM%CONF%CHECK_SEVERITY > 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( NUM_WRONG_DATES, STR )
                CALL ERR_LOG ( 7614, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &              'an attempt to write in the output file '// &
     &               FINAM(1:I_LEN(FINAM))//' -- '//STR(1:I_LEN(STR))// &
     &              ' observations with wrong dates were found. If you really '// &
     &              ' want to ignore this failure, please re-load data with '// &
     &              'CHECK_SEVERITY: 0' )
                RETURN
              ELSE IF ( NUM_WRONG_DATES > 0 ) THEN
                WRITE ( 6, * ) 'Found ', NUM_WRONG_DATES, ' observations '// &
     &                         'with wrong dates, but nevertheless, continue'
           END IF
      END IF
!
      IF ( PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE ) THEN
!
! ======== Make OBS-file (do not do it in OPAL mode)
!
! -------- Build the name of the output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.obs'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7615, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           LAST_SCA= 0
           DO 420 J2=1,PIM%NOBS
              IF ( PIM%OBS(J2)%SCA_IND > 0 .AND. &
     &             PIM%OBS(J2)%SCA_IND .NE. LAST_SCA ) THEN
                   WRITE  ( UNIT=LUN, FMT=110, IOSTAT=IER ) '#SCA: ', &
     &                      PIM%OBS(J2)%SCA_IND
                   LAST_SCA = PIM%OBS(J2)%SCA_IND
              END IF
              IF ( PIM%OBS(J2)%SCA_IND == 0 ) GOTO 420
!
              IF ( PIM%OBS(J2)%TIM_BEG_IND > 0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   STR(1:30)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                           PIM%OBS(J2)%TIM_BEG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J2, STR )
                        WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                        WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                          PIM%TIM_R8(PIM%OBS(J2)%TIM_BEG_IND)
                        CALL ERR_LOG ( 7616, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                      'in an attempt to write the Julian date for '// &
     &                       'observation '//STR )
                        RETURN
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   STR(31:60) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                           PIM%OBS(J2)%TIM_END, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J2, STR )
                        WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                        WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                          PIM%TIM_R8(PIM%OBS(J2)%TIM_BEG_IND + &
     &                                          PIM%OBS(J2)%NUM_EPC(PIM%CONF%FRQ_GRP) -1 ) + &
     &                                          PIM%OBS(J2)%AP_LEN
                        CALL ERR_LOG ( 7617, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                      'in an attempt to write the Julian date for '// &
     &                       'observation '//STR )
                        RETURN
                   END IF
                 ELSE
                   STR(1:30)  = '0000.00.00_00:00:00.0000000   '
                   STR(31:60) = '0000.00.00_00:00:00.0000000   '
              END IF
!
              IF ( PIM%OBS(J2)%SOU_IND    > 0 .AND. &
     &             PIM%OBS(J2)%STA_IND(1) > 0 .AND. &
     &             PIM%OBS(J2)%STA_IND(2) > 0       ) THEN
                   WRITE ( UNIT=LUN, FMT=130, IOSTAT=IER ) &
     &                     J2, &
     &                     PIM%OBS(J2)%SCA_IND, &
     &                     PIM%SCA(PIM%OBS(J2)%SCA_IND)%SCAN_NAME(1:10), &
     &                     STR(1:22), STR(31:52), &
     &                     PIM%OBS(J2)%TIM_BEG_IND, &
     &                     PIM%SOU(PIM%OBS(J2)%SOU_IND)%IVS_NAME, &
     &                     PIM%STA(PIM%OBS(J2)%STA_IND(1))%IVS_NAME, &
     &                     PIM%STA(PIM%OBS(J2)%STA_IND(2))%IVS_NAME, &
     &                     PIM%OBS(J2)%NUM_EPC, &
     &                     PIM%OBS(J2)%NUM_AP_SPAN, &
     &                     PIM%OBS(J2)%GLO_FRG_INDS
 130               FORMAT ( I6, 1X, I6, 1X, A, 1X, ' [ ', A, ', ', A, ' ] ', &
     &                      1X, I6, 1X, A, 1X, A, 1X, A, ' | ', 8(I5, 2X), &
     &                      ' | ', 8(I5, 2X), ' | ', 8(I1,1X) )
                ELSE
                   WRITE ( UNIT=LUN, FMT=130, IOSTAT=IER ) &
     &                     J2, &
     &                     PIM%OBS(J2)%SCA_IND, &
     &                     PIM%SCA(PIM%OBS(J2)%SCA_IND)%SCAN_NAME(1:10), &
     &                     STR(1:22), STR(31:52), PIM%OBS(J2)%NUM_EPC(PIM%CONF%FRQ_GRP), &
     &                     PIM%OBS(J2)%TIM_BEG_IND, &
     &                     'unknown ', 'unknown ', 'unknown '
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 7618, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write in the output file '// &
     &                  FINAM(1:I_LEN(FINAM))//' error '//STR )
                   RETURN
             END IF
 420       CONTINUE
           CLOSE ( UNIT=LUN )
!
! ======== Make SCA-file
!
! -------- Build the name of the output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.sca'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7619, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           DO 430 J3=1,PIM%NSCA
              STR(1:30)  = MJDSEC_TO_DATE  ( PIM%MJD_0, PIM%TAI_0 + &
     &                                       PIM%TIM_R8(PIM%SCA(J3)%TIM_IND), IER )
              STR(31:60) = MJDSEC_TO_DATE  ( PIM%MJD_0, PIM%TAI_0 + &
     &                                       PIM%TIM_R8(PIM%SCA(J3)%TIM_IND+PIM%SCA(J3)%NUM_EPC-1), IER )
              WRITE  ( UNIT=LUN, FMT=116, IOSTAT=IER ) '#SCA: ', J3, PIM%SCA(J3)%NUM_EPC, &
     &                                                 STR(1:22), STR(31:52)
 116          FORMAT ( A, I6, 2X, 'N_epc: ', I5, ' [ ', A, ', ', A, ' ]', 48X, ' ' )
              DO 440 J4=1,PIM%SCA(J3)%NBAS
                 IND_OBS = PIM%SCA(J3)%OBS_IND(J4)
                 IF ( IND_OBS .LE. 0 ) THEN
                      WRITE ( 6, * ) 'J3= ', J3, ' J4= ',J4, ' Nbas: ', PIM%SCA(J3)%NBAS
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_OBS, STR )
                      CALL ERR_LOG ( 7620, IUER, 'PIMA_WRITE_OBS', 'Trap of '// &
     &                    'internal control: wrong observation index: '// &
     &                     STR(1:I_LEN(STR))//' was found in an attempt '// &
     &                    'to open write into the output file '//FINAM )
                      RETURN
                 END IF
!
                 IF ( PIM%OBS(IND_OBS)%TIM_BEG_IND > 0 ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      STR(1:30)  = MJDSEC_TO_DATE  ( PIM%MJD_0, PIM%TAI_0 + &
     &                                               PIM%OBS(IND_OBS)%TIM_BEG, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J3, STR )
                           WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                           WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                     PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND)
                           CALL ERR_LOG ( 7621, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                         'in an attempt to write the Julian date for '// &
     &                         'scan '//STR )
                           RETURN
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      STR(31:60) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                              PIM%OBS(IND_OBS)%TIM_END, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J3, STR )
                           WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                           WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%OBS(IND_OBS)%TIM_BEG_IND   + &
     &                                               PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP) ) + &
     &                                               PIM%OBS(IND_OBS)%AP_LEN
                           CALL ERR_LOG ( 7622, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                         'in an attempt to write the Julian date for '// &
     &                         'scan '//STR )
                           RETURN
                      END IF
                   ELSE
                      STR(1:30)  = '0000.00.00_00:00:00.0000000   '
                      STR(31:60) = '0000.00.00_00:00:00.0000000   '
                 END IF
!
                 CALL INCH ( PIM%OBS(IND_OBS)%NUVS, STR(64:65) )
                 IF ( PIM%OBS(IND_OBS)%NUVS > 0 ) THEN
                      DO 450 J5=1,PIM%OBS(IND_OBS)%NUVS
                         IB = 71 + (J5-1)*3
                         IE = IB+2
                         CALL INCH ( PIM%OBS(IND_OBS)%GLO_FRG_INDS(J5), STR(IB:IE) )
 450                  CONTINUE
                    ELSE
                      STR(64:65) = ' 0'
                      STR(71:72) = '  '
                      IE = 71
                 END IF
                 IF ( PIM%SCA(J3)%SOU_IND > 0         .AND. &
     &                PIM%OBS(IND_OBS)%STA_IND(1) > 0 .AND. &
     &                PIM%OBS(IND_OBS)%STA_IND(2) > 0       ) THEN
!
                      WRITE ( UNIT=LUN, FMT=140, IOSTAT=IER ) &
     &                        J3, &
     &                        PIM%SCA(J3)%SCAN_NAME(1:10), &
     &                        IND_OBS, &
     &                        'o', &
     &                        STR(1:22), STR(31:52), PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP), &
     &                        PIM%OBS(IND_OBS)%TIM_BEG_IND, &
     &                        PIM%SOU(PIM%SCA(J3)%SOU_IND)%IVS_NAME, &
     &                        PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME, &
     &                        PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME, &
     &                        STR(64:65), STR(71:IE)
 140                 FORMAT ( I6, 1X, A, 1X, I6, 1X, A, ' [ ', A, ', ', A, ' ] ', &
     &                        I4, 1X, I6, 1X, A, 1X, A, 1X, A, 2X, A, 3X, A )
                   ELSE
                      WRITE ( UNIT=LUN, FMT=140, IOSTAT=IER ) &
     &                        J3, &
     &                        PIM%SCA(J3)%SCAN_NAME(1:10), &
     &                        IND_OBS, &
     &                        'o', &
     &                        STR(1:22), STR(31:52), PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP), &
     &                        PIM%OBS(IND_OBS)%TIM_BEG_IND, &
     &                        'unknown ', 'unknown ', 'unknown '
                 END IF
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 7623, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                   'an attempt to write in the output file '// &
     &                    FINAM(1:I_LEN(FINAM))//' error '//STR )
                     RETURN
                 END IF
 440          CONTINUE
!
              DO 460 J6=1,PIM%SCA(J3)%NSTA
                 IND_AUT = PIM%SCA(J3)%AUT_IND(J6)
!
                 IF ( IND_AUT > 0 ) THEN
                      TIM_IND = PIM%AUT(IND_AUT)%TIM_BEG_IND
                      IF ( PIM%AUT(IND_AUT)%STA_IND(1) > 0  .AND. &
     &                     PIM%AUT(IND_AUT)%STA_IND(2) > 0        ) THEN
                           STA1 = PIM%STA(PIM%AUT(IND_AUT)%STA_IND(1))%IVS_NAME
                           STA2 = PIM%STA(PIM%AUT(IND_AUT)%STA_IND(2))%IVS_NAME
                         ELSE
                           STA1 = 'unknown '
                           STA2 = 'unknown '
                      END IF
                      IF ( PIM%AUT(IND_AUT)%TIM_BEG_IND > 0 ) THEN
                           CALL ERR_PASS ( IUER, IER )
                           STR(1:30)  = MJDSEC_TO_DATE  ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%AUT(IND_AUT)%TIM_BEG_IND), IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL INCH  ( J3, STR )
                                WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                                WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                          PIM%TIM_R8(PIM%AUT(IND_AUT)%TIM_BEG_IND)
                                CALL ERR_LOG ( 7624, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                               'in an attempt to write the Julian date for '// &
     &                               'scan '//STR )
                                RETURN
                           END IF
!
                           CALL ERR_PASS ( IUER, IER )
                           STR(31:60) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%AUT(IND_AUT)%TIM_BEG_IND   + &
     &                                               PIM%AUT(IND_AUT)%NUM_EPC(PIM%CONF%FRQ_GRP) ) + &
     &                                               PIM%AUT(IND_AUT)%AP_LEN, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR )
                                CALL INCH  ( J3, STR )
                                WRITE ( 6, * ) 'IND_AUT= ', IND_AUT
                                WRITE ( 6, * ) 'MJD: ', PIM%MJD_0
                                WRITE ( 6, * ) 'TAI: ', PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%AUT(IND_AUT)%TIM_BEG_IND   + &
     &                                               PIM%AUT(IND_AUT)%NUM_EPC(PIM%CONF%FRQ_GRP) ) + &
     &                                               PIM%OBS(IND_AUT)%AP_LEN
                                WRITE ( 6, * ) 'TAI_0:   ', PIM%TAI_0
                                WRITE ( 6, * ) 'TIM_IND: ', PIM%AUT(IND_AUT)%TIM_BEG_IND
                                WRITE ( 6, * ) 'NUM_EPC: ', PIM%AUT(IND_AUT)%NUM_EPC(PIM%CONF%FRQ_GRP)
                                WRITE ( 6, * ) 'AP_LEN:  ', PIM%OBS(IND_AUT)%AP_LEN
                                CALL ERR_LOG ( 7625, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                               'in an attempt to write the Julian date for '// &
     &                               'scan '//STR(1:I_LEN(STR))//' Nevertheless, proceed' )
!!                           RETURN
                           END IF
                        ELSE
                           STR(1:30)  = '0000.00.00_00:00:00.0000000   '
                           STR(31:60) = '0000.00.00_00:00:00.0000000   '
                      END IF
                    ELSE
                      TIM_IND = -1
                      STA1 = '?'
                      STA2 = '?'
                      STR(1:30)  = '0000.00.00_00:00:00.0000000   '
                      STR(31:60) = '0000.00.00_00:00:00.0000000   '
                 END IF
!
                 IF ( PIM%SCA(J3)%SOU_IND > 0 ) THEN
                      IF ( PIM%AUT(IND_AUT)%NUVS > 0 ) THEN
                           DO 470 J7=1,PIM%AUT(IND_AUT)%NUVS
                              IB = 71 + (J7-1)*3
                              IE = IB+2
                              CALL INCH ( PIM%AUT(IND_AUT)%GLO_FRG_INDS(J7), &
     &                                    STR(IB:IE) )
 470                       CONTINUE
                         ELSE
                           STR(64:65) = ' 0'
                           STR(71:72) = '  '
                           IE = 71
                      END IF
!
                      WRITE ( UNIT=LUN, FMT=140, IOSTAT=IER ) &
     &                        J3, &
     &                        PIM%SCA(J3)%SCAN_NAME(1:10), &
     &                        IND_AUT, &
     &                        'a', &
     &                        STR(1:22), STR(31:52), PIM%AUT(IND_AUT)%NUM_EPC(PIM%CONF%FRQ_GRP), &
     &                        TIM_IND, &
     &                        PIM%SOU(PIM%SCA(J3)%SOU_IND)%IVS_NAME, &
     &                        STA1, STA2, STR(64:65), STR(71:IE)
                    ELSE
                      WRITE ( UNIT=LUN, FMT=140, IOSTAT=IER ) &
     &                        J3, &
     &                        PIM%SCA(J3)%SCAN_NAME(1:10), &
     &                        IND_AUT, &
     &                        'a', &
     &                        STR(1:22), STR(31:52), PIM%AUT(IND_AUT)%NUM_EPC(PIM%CONF%FRQ_GRP), &
     &                        TIM_IND, &
     &                        'unknown ', STA1, STA2
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IER, STR )
                      CALL ERR_LOG ( 7626, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write in the output file '// &
     &                     FINAM(1:I_LEN(FINAM))//' error '//STR )
                      RETURN
                 END IF
 460          CONTINUE
 430       CONTINUE
           CLOSE ( UNIT=LUN )
!
! ======== Make TIM-file
!
! -------- Build the name of the output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.tim'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7627, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           DO 480 J8=1,PIM%NEPC
              CALL ERR_PASS ( IUER, IER )
              STR(1:30) = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(J8), &
     &                                     IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J8, STR )
                   WRITE ( 8, * ) 'MJD: ', PIM%MJD_0
                   WRITE ( 8, * ) 'TAI: ', PIM%TAI_0 + PIM%TIM_R8(J8)
                   CALL ERR_LOG ( 7628, -2, 'PIMA_WRITE_OBS', 'Error '// &
     &                 'in an attempt to write the Julian date for '// &
     &                 'epoch '//STR )
                   RETURN
              END IF
              WRITE ( UNIT=LUN, FMT=150, IOSTAT=IER ) J8, PIM%TIM_R8(J8), STR(1:27)
 150          FORMAT ( 'ind_tim: ',I6,' tim: ', F22.10, ' dat: ', A )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 7629, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write in the output file '// &
     &                  FINAM(1:I_LEN(FINAM))//' error '//STR )
                   RETURN
              END IF
 480       CONTINUE
           CLOSE ( UNIT=LUN )
!
! ======== Make FRQ-file
!
! -------- Build the name of the output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.frq'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7630, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           DO 490 J9=1,PIM%NFRG
              DO 4100 J10=1,PIM%NFRQ
                 WRITE ( UNIT=LUN, FMT=160, IOSTAT=IER ) J9, J10, &
     &                   PIM%FRQ(J10,J9)%FREQ, &
     &                   PIM%FRQ(J10,J9)%BAND_WIDTH, &
     &                   PIM%FRQ(J10,J9)%CHAN_WIDTH, &
     &                   PIM%FRQ(J10,J9)%SIDE_BAND,  &
     &                   PIM%FRQ(J10,J9)%BB_SP_CHAN_IND
 160             FORMAT ( 'Ind_grp: ',I2, ' Ind_frq: ',I4,' freq = ',F15.2, &
     &                    ' Band_width: ', F12.2, &
     &                    ' Chan_width: ', F12.2, &
     &                    ' Side_band: ', I3,' Bb_sp_chan_id: ',I3 )
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IER, STR )
                      CALL ERR_LOG ( 7631, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write in the output file '// &
     &                     FINAM(1:I_LEN(FINAM))//' error '//STR )
                      RETURN
                 END IF
 4100         CONTINUE
              WRITE ( UNIT=LUN, FMT='(1X)', IOSTAT=IER )
 490       CONTINUE
           WRITE ( UNIT=LUN, FMT='(A)' ) ' '
           DO 4110 J11=1,PIM%NFRG
              KCHN = 0
              DO 4120 J12=1,PIM%NFRQ
                 DO 4130 J13=1,PIM%NCHN
                    KCHN = KCHN + 1
                    WRITE  ( UNIT=LUN, FMT=170, IOSTAT=IER ) J11, J12, J13, KCHN, &
     &                                                        PIM%FREQ_ARR(J13,J12,J11)
 170                FORMAT ( 'IND_GRP: ', I2, ' IND_FRQ: ',I3, ' IND_CHN: ', I5, &
     &                       ' K_CHN: ', I5, ' FREQ_ARR: ', F15.2 )
 4130            CONTINUE
 4120         CONTINUE
              WRITE ( UNIT=LUN, FMT='(1X)', IOSTAT=IER )
 4110      CONTINUE
           CLOSE ( UNIT=LUN )
!
! ======== Make STA-file
!
! -------- Build the name of the station output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.sta'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7632, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           DO 4140 J14=1,PIM%NSTA
              WRITE ( UNIT=LUN, FMT=180 ) J14, PIM%STA(J14)%IVS_NAME, &
     &                                         PIM%STA(J14)%ORIG_NAME, &
     &                                         PIM%STA(J14)%IND_ORIG, &
     &                                         PIM%NLEV(J14), &
     &                                         PIMA__POL(PIM%STA(J14)%POL_TYP(1)), &
     &                                         PIMA__POL(PIM%STA(J14)%POL_TYP(2)), &
     &                                         PIM%STA(J14)%COO_ORIG, &
     &                                         PIM%STA(J14)%VEL_ORIG*1.D3*YEAR__TO__SEC
 180          FORMAT ( I2,')  Station IVS_name: ', A, 2X, &
     &                    ' Orig_name: ', A, ' Ind_orig: ', I2, '  nlev: ', I2, &
     &                    ' Pol: ', 2A, ' Coo: ', 3(F14.3,1X), ' Vel: ', 3(F8.2,1X) )
 4140      CONTINUE
           CLOSE ( UNIT=LUN )
!
! ======== Make SOU-file
!
! -------- Build the name of the station output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.sou'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7633, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
!
           DO 4150 J15=1,PIM%NSOU
              CALL RH_TAT ( PIM%SOU(J15)%ALPHA, 5, STR_ALP, -2 )
              CALL RG_TAT ( PIM%SOU(J15)%DELTA, 6, STR_DEL, -2 )
              CALL CHASHL ( STR_ALP )
              IF ( STR_DEL(1:1) == ' ' ) STR_DEL(1:1) = '+'
!
              IF ( PIM%SOU(J15)%IND_SWAP == 0 ) THEN
                   CALL RH_TAT ( PIM%SOU(J15)%ALPHA_INP, 5, STR_COR_ALP, -2 )
                   CALL RG_TAT ( PIM%SOU(J15)%DELTA_INP, 6, STR_COR_DEL, -2 )
                   S_COR(1) = DCOS(PIM%SOU(J15)%DELTA_INP)*DCOS(PIM%SOU(J15)%ALPHA_INP)
                   S_COR(2) = DCOS(PIM%SOU(J15)%DELTA_INP)*DSIN(PIM%SOU(J15)%ALPHA_INP)
                   S_COR(3) = DSIN(PIM%SOU(J15)%DELTA_INP)
                 ELSE
                   CALL RH_TAT ( PIM%SOU(PIM%SOU(J15)%IND_SWAP)%ALPHA_INP, 5, STR_COR_ALP, -2 )
                   CALL RG_TAT ( PIM%SOU(PIM%SOU(J15)%IND_SWAP)%DELTA_INP, 6, STR_COR_DEL, -2 )
                   S_COR(1) = DCOS(PIM%SOU(PIM%SOU(J15)%IND_SWAP)%DELTA_INP)*DCOS(PIM%SOU(PIM%SOU(J15)%IND_SWAP)%ALPHA_INP)
                   S_COR(2) = DCOS(PIM%SOU(PIM%SOU(J15)%IND_SWAP)%DELTA_INP)*DSIN(PIM%SOU(PIM%SOU(J15)%IND_SWAP)%ALPHA_INP)
                   S_COR(3) = DSIN(PIM%SOU(PIM%SOU(J15)%IND_SWAP)%DELTA_INP)
              END IF
!
              CALL CHASHL ( STR_COR_ALP )
              IF ( STR_COR_DEL(1:1) == ' ' ) STR_COR_DEL(1:1) = '+'
!
! ----------- Check whether a given sources is associated with any observation
!
              FL_OBS = .FALSE.
              DO 4160 J16=1,PIM%NOBS
                 IF ( PIM%OBS(J16)%SOU_IND == J15 ) FL_OBS = .TRUE.
 4160         CONTINUE 
!
! ----------- Get the distance betwen the apriori positiosn used by PIMA and
! ----------- apriroi positions used by the correlator
!
              DIST = DSQRT ( (PIM%SOU(J15)%S_VEC(1)-S_COR(1))**2 + &
     &                       (PIM%SOU(J15)%S_VEC(2)-S_COR(2))**2 + &
     &                       (PIM%SOU(J15)%S_VEC(3)-S_COR(3))**2  &
     &                     )
!
              IF ( FL_OBS ) THEN
                   STR(1:1) = ' '
                 ELSE
                   STR(1:1) = '@'
              END IF 
              WRITE ( UNIT=LUN, FMT=190 ) J15, STR(1:1), PIM%SOU(J15)%IVS_NAME, &
     &                                    PIM%SOU(J15)%J2000_NAME, &
     &                                    PIM%SOU(J15)%DB_NAME, &
     &                                    STR_COR_ALP, STR_COR_DEL, &
     &                                    STR_ALP, STR_DEL, &
     &                                    DIST*RAD__TO__ARCSEC
 190          FORMAT ( I3,')', A, ' Sou: ', A, 2X, A, 2X, A, ' Coo_cor: ', A, 2X, A, &
     &                    ' Coo_apr: ', A, 2X, A, ' Dist: ', F10.4 ' arcsec  ' )
 4150      CONTINUE
           CLOSE ( UNIT=LUN )
      END IF
!
! === Make PIM-file
!
! --- Build the name of the output file
!
      FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.pim'
      IP = I_LEN(PIM%CONF%EXPER_DIR)
      IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
           FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
         ELSE
           FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
      END IF
!
! --- Open the old file exists, remove it
!
      INQUIRE ( FILE=FINAM, EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FINAM(1:I_LEN(FINAM))//CHAR(0) )
      END IF
!
! --- Open ouput file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 7634, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &         ' '//STR )
           RETURN
      END IF
!
! --- Write down the format label
!
      CALL ERR_PASS     ( IUER, IER )
      CALL WRBIN_STRING ( LUN, PIMA__FORMAT_LABEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7635, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to write the first record, format label into '// &
     &         'the output file '//FINAM )
           RETURN
      END IF
!
      SIZE_BYTES = LOC(PIM%STATUS) - LOC(PIM) + SIZEOF(PIM%STATUS)
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7636, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to write the first part of body of PIMA data '// &
     &         'structure into the output file '//FINAM )
           RETURN
      END IF
!
      SIZE_BYTES = LOC(PIM%CONF%LAST_FIELD) - LOC(PIM%CONF) + &
     &             SIZEOF(PIM%CONF%LAST_FIELD)
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%CONF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7637, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to write a part CONF of PIMA data '// &
     &         'structure into the output file '//FINAM )
           RETURN
      END IF
!
      DO 4170 J17=1,PIM%NSOU
         SIZE_BYTES = SIZEOF(PIM%SOU(J17))
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%SOU(J17), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7638, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a SOU part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
 4170 CONTINUE
!
      DO 4180 J18=1,PIM%NSTA
         SIZE_BYTES = LOC(PIM%STA(J18)%STATUS) - LOC(PIM%STA(J18)) + &
     &                SIZEOF(PIM%STA(J18)%STATUS)
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7639, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a STA part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
         IF ( PIM%FRG_USE == PIMA__SINGLE .OR. PIM%FRG_USE == PIMA__COMBINED ) THEN
              NFRG = PIM%NFRG
           ELSE 
              NFRG = PIM%VIRT_NFRG 
         END IF
         DO 4190 J19=1,NFRG
            SIZE_BYTES = LOC(PIM%STA(J18)%PCAL(J19)%PCAL_MASK_STATUS) - LOC(PIM%STA(J18)%PCAL(J19)) + &
     &                   SIZEOF(PIM%STA(J18)%PCAL(J19)%NPOL)
!
! --------- Write beginning of the pcal record
!
            CALL ERR_PASS ( IUER, IER )
            CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%PCAL(J19), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7640, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &               'attempt to write a PCAL part of PIMA data '// &
     &               'structure into the output file '//FINAM )
                 RETURN
            END IF
            IF ( PIM%STA(J18)%PCAL(J19)%PCAL_AVAIL ) THEN
!
! -------------- Write down information related to phase calibration
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%PHAS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7641, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%PHAS part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%AMPL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7642, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%AMPL part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%PHAS_RGR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7643, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%PHAS_RGR part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%AMPL_RGR, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7644, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%AMPL_RGR part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%PHAS_SCA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7645, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%PHAS_SCA part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%AMPL_SCA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7646, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%AMPL_SCA part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL(J19)%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%PRAT_SCA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7647, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%PRAT_SCA part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                      PIM%STA(J18)%PCAL(J19)%FREQ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7648, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%FREQ part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', &
     &                      PIM%STA(J18)%PCAL(J19)%NO_TONES*PIM%NFRQ*PIM%STA(J18)%PCAL(J19)%NPOI*PIM%STA(J18)%PCAL%NPOL, &
     &                      PIM%STA(J18)%PCAL(J19)%RATE, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7649, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL%RATE part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                              PIM%STA(J18)%PCAL(J19)%IPOI_SCA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7650, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%IPOI_SCA of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                              PIM%STA(J18)%PCAL(J19)%ISCA_POI, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7651, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%ISCA_POI of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                              PIM%STA(J18)%PCAL(J19)%SOU_IND, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7652, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%SOU_IND part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                              PIM%STA(J18)%PCAL(J19)%TIME_MID_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7653, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%TIME_MID_R8 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%PCAL(J19)%NSCA, &
     &                              PIM%STA(J18)%PCAL(J19)%TIME_SCA_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7654, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%TIME_SCA_R8 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%PCAL(J19)%NPOI, &
     &                              PIM%STA(J18)%PCAL(J19)%TIME_SPAN_R4, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7655, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a PCAL(J19)%TIME_SPAN_R4 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF
 4190    CONTINUE
         DO 4200 J20=1,PIM%NFRG
            SIZE_BYTES = LOC(PIM%STA(J18)%TSYS(J20)%NPOL) - LOC(PIM%STA(J18)%TSYS(J20)) + &
     &                   SIZEOF(PIM%STA(J18)%TSYS(J20)%NPOL)
            CALL ERR_PASS ( IUER, IER )
            CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%TSYS(J20), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7656, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &               'attempt to write a TSYS part of PIMA data '// &
     &               'structure into the output file '//FINAM )
                 RETURN
            END IF
!
            IF ( PIM%STA(J18)%TSYS(J20)%AVAIL ) THEN
!
! -------------- Write down information related to system temperature data
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%TSYS(J20)%NPOI*PIM%STA(J18)%TSYS(J20)%NPOL, &
     &                              PIM%STA(J18)%TSYS(J20)%TSYS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7657, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%TSYS part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%TSYS(J20)%NPOI, &
     &                              PIM%STA(J18)%TSYS(J20)%TIME_MID_R8, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7658, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%TIME_MID_R8 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%TSYS(J20)%NPOI, &
     &                              PIM%STA(J18)%TSYS(J20)%TIME_SPAN_R4, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7659, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%TIME_SPAN_R8 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%TSYS(J20)%NPOI, &
     &                              PIM%STA(J18)%TSYS(J20)%AZ_R4, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7660, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%AZ_R4 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%TSYS(J20)%NPOI, &
     &                              PIM%STA(J18)%TSYS(J20)%ELEV_R4, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7661, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%ELEV_R4 part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%TSYS(J20)%NPOI, &
     &                              PIM%STA(J18)%TSYS(J20)%SOU_IND, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7662, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a TSYS(J20)%SOU_IND part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF
!
            SIZE_BYTES = LOC(PIM%STA(J18)%STMO(J20)%STATUS) - LOC(PIM%STA(J18)%STMO(J20)) + &
     &                   SIZEOF(PIM%STA(J18)%STMO(J20)%STATUS)
            CALL ERR_PASS ( IUER, IER )
            CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%STMO(J20), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7663, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &               'attempt to write a STMO part of PIMA data '// &
     &               'structure into the output file '//FINAM )
                 RETURN
            END IF
!
            IF ( PIM%STA(J18)%STMO(J20)%N_OPA > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%STMO(J20)%N_OPA, &
     &                              PIM%STA(J18)%STMO(J20)%IND_SCA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7664, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STA(J18)%STMO(J20)%IND_STA '// &
     &                    'part of PIMA data structure into the output file '// &
     &                     FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%STMO(J20)%N_OPA, &
     &                              PIM%STA(J18)%STMO(J20)%TIM, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7665, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STA(J18)%STMO(J20)%TIM '// &
     &                    'part of PIMA data structure into the output file '// &
     &                     FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%STMO(J20)%N_OPA, &
     &                              PIM%STA(J18)%STMO(J20)%EL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7666, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STA(J18)%STMO(J20)%EL '// &
     &                    'part of PIMA data structure into the output file '// &
     &                     FINAM )
                      RETURN
                 END IF
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%STMO(J20)%N_OPA, &
     &                              PIM%STA(J18)%STMO(J20)%AZ, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7667, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STA(J18)%STMO(J20)%AZ '// &
     &                    'part of PIMA data structure into the output file '// &
     &                     FINAM )
                      RETURN
                 END IF
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_OPA, &
     &                              PIM%STA(J18)%STMO(J20)%OPA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7668, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%OPA part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            IF ( PIM%STA(J18)%STMO(J20)%N_TAT > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TAT, &
     &                              PIM%STA(J18)%STMO(J20)%TAT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7669, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TAT part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            IF ( PIM%STA(J18)%STMO(J20)%N_TREC > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TREC, &
     &                              PIM%STA(J18)%STMO(J20)%TREC, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7670, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TREC part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            IF ( PIM%STA(J18)%STMO(J20)%N_TSPI > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TSPI, &
     &                              PIM%STA(J18)%STMO(J20)%TSPI, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7671, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TSPI part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            IF ( PIM%STA(J18)%STMO(J20)%N_TSYS > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TSYS*KPOL, &
     &                              PIM%STA(J18)%STMO(J20)%TSYS_CLN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7672, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TSYS_CLN part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TSYS*KPOL, &
     &                              PIM%STA(J18)%STMO(J20)%TSYS_MOD, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7673, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TSYS_MOD part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            IF ( PIM%STA(J18)%STMO(J20)%N_TTOA > 0 ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*PIM%STA(J18)%STMO(J20)%N_TTOA, &
     &                              PIM%STA(J18)%STMO(J20)%TTOA, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7674, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TTOA part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
            IF ( PIM%STA(J18)%STMO(J20)%TSRAT_AVAIL ) THEN
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NFRQ*2, PIM%STA(J18)%STMO(J20)%TSRAT, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7675, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a STMO(J20)%TSRAT part of '// &
     &                    'PIMA data structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF 
!
            SIZE_BYTES = LOC(PIM%STA(J18)%GAIN(J20)%NTAB) - LOC(PIM%STA(J18)%GAIN(J20)) + &
     &                   SIZEOF(PIM%STA(J18)%GAIN(J20)%NTAB)
            CALL ERR_PASS ( IUER, IER )
            CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%GAIN(J20), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7676, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &               'attempt to write a GAIN part of PIMA data '// &
     &               'structure into the output file '//FINAM )
                 RETURN
            END IF
!
            IF ( PIM%STA(J18)%GAIN(J20)%AVAIL ) THEN
!
! -------------- Write down information related to gain
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                              PIM%STA(J18)%GAIN(J20)%TYP, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7677, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%TYP part of PIMA data '// &
     &                       'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                              PIM%STA(J18)%GAIN(J20)%NTERM, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7678, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%NTERM part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                              PIM%STA(J18)%GAIN(J20)%X_TYP, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7679, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%X_TYP part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                              PIM%STA(J18)%GAIN(J20)%Y_TYP, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7680, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%Y_TYP part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                                         PIM%STA(J18)%GAIN(J20)%X_VAL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7681, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%X_VAL part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                              (PIM%STA(J18)%GAIN(J20)%NTAB+1)* &
     &                               PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                               PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                               PIM%STA(J18)%GAIN(J20)%Y_VAL, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7682, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%Y_VAL part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', &
     &                              (PIM%STA(J18)%GAIN(J20)%NTAB+1)* &
     &                               PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                               PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                               PIM%STA(J18)%GAIN(J20)%GAIN, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7683, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                    'an attempt to write a GAIN(J20)%GAIN part of PIMA data '// &
     &                    'structure into the output file '//FINAM )
                      RETURN
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL WRBIN_ARRAY ( LUN, 'R4', PIM%STA(J18)%GAIN(J20)%NFRQ* &
     &                                         PIM%STA(J18)%GAIN(J20)%NPOL, &
     &                              PIM%STA(J18)%GAIN(J20)%SENS, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7684, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                       'an attempt to write a GAIN(J20)%SENS part of PIMA data '// &
     &                       'structure into the output file '//FINAM )
                      RETURN
                 END IF
            END IF
 4200    CONTINUE
!
         SIZE_BYTES = LOC(PIM%STA(J18)%CABLE%CABLE_SIGN) - LOC(PIM%STA(J18)%CABLE) + &
     &                    SIZEOF(PIM%STA(J18)%CABLE%CABLE_SIGN)
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%CABLE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7685, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a CABLE part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         IF ( PIM%STA(J18)%CABLE%CAB_AVAIL ) THEN
!
! ----------- Write down information related to cable calibration
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%CABLE%NPOI, &
     &                           PIM%STA(J18)%CABLE%TIM_CAB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7686, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a PIM%STA(J18)%CABLE%TIM_CAB '// &
     &                 'part of PIMA data structure into the output '// &
     &                 'file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%CABLE%NPOI, &
     &                           PIM%STA(J18)%CABLE%CAB_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7687, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a PIM%STA(J18)%CABLE%CAB_DEL '// &
     &                 'part of PIMA data structure into the output '// &
     &                 'file '//FINAM )
                   RETURN
              END IF
         END IF
!
         SIZE_BYTES = LOC(PIM%STA(J18)%WEATHER%NPOI) - LOC(PIM%STA(J18)%WEATHER) + &
     &                SIZEOF(PIM%STA(J18)%WEATHER%NPOI)
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%STA(J18)%WEATHER, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7688, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a WEATHER part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         IF ( PIM%STA(J18)%WEATHER%AVAIL ) THEN
!
! ----------- Write down information related to weather
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%WEATHER%NPOI, &
     &                           PIM%STA(J18)%WEATHER%TIME_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7689, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WEATHER%TIME_BEG part of '// &
     &                 'PIMA data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%WEATHER%NPOI, &
     &                           PIM%STA(J18)%WEATHER%TIME_END, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7690, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WEATHER%TIME_END part of '// &
     &                 'PIMA data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%WEATHER%NPOI, &
     &                           PIM%STA(J18)%WEATHER%PRES, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7691, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WEATHER%PRES part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%WEATHER%NPOI, &
     &                           PIM%STA(J18)%WEATHER%TEMP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7692, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WEATHER%TEMP part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%WEATHER%NPOI, &
     &                           PIM%STA(J18)%WEATHER%HUMID, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7693, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WEATHER%HUMID part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
         END IF
!
         IF ( PIM%STA(J18)%L_MOD > 0 ) THEN
!
! ----------- Write down information related to apriori model
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'B1', &
     &                           PIM%STA(J18)%L_MOD*SIZEOF(PIM%STA(J18)%MOD(1)), &
     &                           PIM%STA(J18)%MOD, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7694, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MOD part of PIMA data '// &
     &                 'structure into the output file '//FINAM )
                   RETURN
              END IF
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         LEN_MDC = LOC(PIM%STA(J18)%MDC%CLO_MODEL_STATUS) - &
     &             LOC(PIM%STA(J18)%MDC%CLO_OFFS) + &
     &             SIZEOF(PIM%STA(J18)%MDC%CLO_MODEL_STATUS)
         CALL WRBIN_ARRAY ( LUN, 'B1', LEN_MDC, PIM%STA(J18)%MDC%CLO_OFFS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7695, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &            'an attempt to write a MDC%CLO_OFFS part of PIMA '// &
     &            'data structure into the output file '//FINAM )
              RETURN
         END IF
         IF ( PIM%STA(J18)%L_MDC > 0 ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'I4', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%IND_SOU, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7696, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%IND_SOU part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%TIME_CEN, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7697, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%TIME_CEN part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%CLOCK_OFFSET, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7698, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%CLOCK_OFFSET part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%CLOCK_RATE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7699, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%CLOCK_RATE part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%ATMO_DELAY, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7700, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%ATMO_DELAY part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%ATMO_RATE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7701, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%ATMO_RATE part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%GDELAY, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7702, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%GDELAY part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_MDC, &
     &                           PIM%STA(J18)%MDC%GRATE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7703, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a MDC%GRATE part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
         END IF
         IF ( PIM%STA(J18)%L_WVR > 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_WVR, &
     &                           PIM%STA(J18)%WVR%TIM_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7704, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%TIM_ARR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_WVR, &
     &                           PIM%STA(J18)%WVR%DEL_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7705, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%DEL_ARR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_WVR, &
     &                           PIM%STA(J18)%WVR%DEL_ERR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7706, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%DEL_ARR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_WVR, &
     &                           PIM%STA(J18)%WVR%EL_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7707, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%EL_ARR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', PIM%STA(J18)%L_WVR, &
     &                           PIM%STA(J18)%WVR%AZ_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7708, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%AZ_ARR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL WRBIN_ARRAY ( LUN, 'R8', 1, &
     &                           PIM%STA(J18)%WVR%HEI_WVR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 7709, IUER, 'PIMA_WRITE_OBS', 'Error in '// &
     &                 'an attempt to write a WVR%HEI_WVR part of PIMA '// &
     &                 'data structure into the output file '//FINAM )
                   RETURN
              END IF
         END IF
 4180 CONTINUE
!
      SIZE_BYTES = SIZEOF(PIM%FRQ(1,1))
      DO 4210 J21=1,PIM%NFRG
         DO 4221 J22=1,PIM%NFRQ
            CALL ERR_PASS ( IUER, IER )
            CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FRQ(J22,J21), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 7710, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &               'attempt to write a PIM%FRQ part of PIMA data '// &
     &               'structure into the output file '//FINAM )
                 RETURN
            END IF
 4221    CONTINUE
 4210 CONTINUE
!
      DO 4230 J23=1,PIM%NSCA
         SIZE_BYTES = LOC(PIM%SCA(J23)%SCAN_NAME) - LOC(PIM%SCA(J23)) + &
     &                SIZEOF(PIM%SCA(J23)%SCAN_NAME)
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%SCA(J23), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7711, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a SCA part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', PIM%SCA(J23)%NSTA, PIM%SCA(J23)%AUT_IND, &
     &                      IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7712, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a SCA%AUT_IND part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', PIM%SCA(J23)%NBAS, PIM%SCA(J23)%OBS_IND, &
     &                      IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7713, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a SCA%OBS_IND part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
 4230 CONTINUE
!
      DO 4240 J24=1,PIM%L_FIL
         SIZE_BYTES = LOC(PIM%FILE(J24)%STATUS) - LOC(PIM%FILE(J24)) + &
     &                SIZEOF(PIM%FILE(J24)%STATUS)
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FILE(J24), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7714, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a FILE part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         SIZE_BYTES = PIM%FILE(J24)%L_HDR*PIM%FILE(J24)%M_KWD* &
     &                LEN(PIM%FILE(J24)%KEY(1,1))
!
! ------ Restore original file name
!
         PIM%FILE(J24)%NAME = PIM%FILE(J24)%ORIG_NAME 
!
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%FILE(J24)%KEY, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7715, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a PIM%FILE%KEY part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
 4240 CONTINUE
!
      SIZE_BYTES = PIM%NUV*SIZEOF(PIM%UV_IND(1))
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%UV_IND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7716, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to write a UV_IND part of PIMA data '// &
     &         'structure into the output file '//FINAM )
           RETURN
      END IF
!
      DO 4250 J25=1,PIM%NOBS
         SIZE_BYTES = LOC(PIM%OBS(J25)%REF_FRG_INDS(PIM__MUVS)) - LOC(PIM%OBS(J25)) + &
     &                SIZEOF(PIM%OBS(J25)%REF_FRG_INDS(PIM__MUVS))
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%OBS(J25), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7717, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a OBS part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%OBS(J25)%NUM_EPC)
         CALL ERR_PASS    ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%OBS(J25)%NUVS, &
     &                      PIM%OBS(J25)%UV_IND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7718, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a OBS%UV_IND part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         CALL ERR_PASS    ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%NFRG, &
     &                      PIM%OBS(J25)%CORR_FLAG, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7719, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a OBS%CORR_FLAG part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
 4250 CONTINUE
!
      DO 4260 J26=1,PIM%NAUT
         SIZE_BYTES = LOC(PIM%AUT(J26)%REF_FRG_INDS(PIM__MUVS)) - LOC(PIM%AUT(J26)) + &
     &                SIZEOF(PIM%AUT(J26)%REF_FRG_INDS(PIM__MUVS))
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'B1', SIZE_BYTES, PIM%AUT(J26), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7720, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a AUT part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
!
         MAX_NUM_OBS = MAX_I4 ( PIM__MUVS, PIM%AUT(J26)%NUM_EPC )
         CALL ERR_PASS    ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', MAX_NUM_OBS*PIM%AUT(J26)%NUVS, &
     &                      PIM%AUT(J26)%UV_IND, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7721, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &            'attempt to write a AUT%UV_IND part of PIMA data '// &
     &            'structure into the output file '//FINAM )
              RETURN
         END IF
 4260 CONTINUE
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'R8', PIM%NCHN*PIM%NFRQ*PIM%NFRG, PIM%FREQ_ARR, &
     &                   IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7722, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to write the frequency array into the output '// &
     &         'file '//FINAM )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7723, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &         'attempt to close output file '//FINAM )
           RETURN
      END IF
!
! === Make stat-file
!
      IF ( PIM%CONF%ACT_CODE .NE. PIMA__GEAN_CODE .AND. &
     &     PIM%CONF%ACT_CODE .NE. PIMA__OPAL_CODE ) THEN
!
! -------- Build the name of the output file
!
           FINAM = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.stt'
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
!
! -------- Open ouput file
!
           LUN = GET_UNIT()
           OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 7724, IUER, 'PIMA_WRITE_OBS', 'Error in an '// &
     &              'attempt to open output file '//FINAM(1:I_LEN(FINAM))// &
     &              ' '//STR )
                RETURN
           END IF
           CALL GETINFO_HOST ( INTERNET_HOSTNAME )
           WRITE ( UNIT=LUN, FMT='(A)' ) PIMA__STT_LABEL
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) '# Generated on '//GET_CDATE()
           WRITE ( UNIT=LUN, FMT='(A)' ) '# Generated    at '//INTERNET_HOSTNAME(1:I_LEN(INTERNET_HOSTNAME))
           WRITE ( UNIT=LUN, FMT='(A)' ) '#                 by '//PIMA__LABEL
           WRITE ( UNIT=LUN, FMT='(A)' ) '#'
           WRITE ( UNIT=LUN, FMT='(A)' ) 'Experiment name:    '//PIM%CONF%SESS_CODE
           WRITE ( UNIT=LUN, FMT='(A)' ) 'FITS generator:     '//PIM%GENERATOR
           WRITE ( UNIT=LUN, FMT='(A)' ) 'Correlator_name:    '//PIM%CORR_NAME
           WRITE ( UNIT=LUN, FMT='(A)' ) 'Correlator_version: '//PIM%CORR_VERS
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of FITS_IDI files:    ', PIM%L_FIL
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of stations:          ', PIM%NSTA
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of sources:           ', PIM%NSOU
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of frequencies:       ', PIM%NFRQ
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of frequency groups:  ', PIM%NFRG
           WRITE ( UNIT=LUN, FMT='(A,I6)' )      'Number of spectral channels: ', PIM%NCHN
           WRITE ( UNIT=LUN, FMT='(A,F10.6,A)' ) 'Spectral channel width:      ', 1.D-6*PIM%CHAN_BW, ' MHz'
           WRITE ( UNIT=LUN, FMT='(A,F10.6,A)' ) 'IF width:                    ', 1.D-6*PIM%NCHN*PIM%CHAN_BW, ' MHz'
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of bands:             ', PIM%NBND
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of polarizations:     ', PIM%NPOL
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of Stokes parameters: ', PIM%NSTK
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'The first Stokes parameter:  ', PIM%STK_1
           WRITE ( UNIT=LUN, FMT='(A,I8)' ) 'Number of time epochs:       ', PIM%NEPC
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of scans:             ', PIM%NSCA
           WRITE ( UNIT=LUN, FMT='(A,I6)' ) 'Number of observations:      ', PIM%NOBS
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Total number of UV points:             ', PIM%NUV
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Total number of used UV points:        ', NUM_GOOD
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl UV points:      ', KUM
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of auto-correl  UV points:      ', NUM - KUM
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of used cross-correl UV points: ', KUM_GOOD
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of used auto-correl  UV points: ', NUM_GOOD - KUM_GOOD
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Total number of deselected points:     ', &
     &                                      NUM_A + NUM_B + NUM_C + NUM_D + NUM_E + NUM_F + NUM_S + NUM_T + NUM_Z + NUM_H
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl NO_AUTO_1 deselected points:           ', KUM_A
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl NO_AUTO_2 deselected points:           ', KUM_B
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl NO_SCAN deselected points:             ', KUM_C
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl duplicate deselected points:           ', KUM_D
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl different AP deselected points:        ', KUM_F
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl scan assign failure deselected points: ', KUM_E
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl short scan deselected points:          ', KUM_S
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl points with a broken chain:            ', KUM_H
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of cross-correl points without time index:             ', KUM_T
           WRITE ( UNIT=LUN, FMT='(A,I9)' ) 'Number of unrecognized points:                                ', NUM_Z
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Number of phase-cal tones per IF:                                  ', PIM%NPCT
           WRITE ( UNIT=LUN, FMT='(A,I4)' ) 'Max polynomial degree of delay model: ', PIM%NMOD
           WRITE ( UNIT=LUN, FMT='(A,F12.9)' ) 'Accummulation period length_min:   ', PIM%AP_LEN_MIN
           WRITE ( UNIT=LUN, FMT='(A,F12.9)' ) 'Accummulation period length_max:   ', PIM%AP_LEN_MAX
           WRITE ( UNIT=LUN, FMT='(A,F8.3)'  ) 'UTC_minus_TAI: ', PIM%UTC_MTAI
           IF ( PIM%TIM_SCL == PIMA__UTC ) THEN
                WRITE ( LUN, * ) 'FITS time scale: UTC'
              ELSE IF ( PIM%TIM_SCL == PIMA__TAI ) THEN
                WRITE ( LUN, * ) 'FITS time scale: TAI'
              ELSE
                WRITE ( LUN, * ) 'FITS time scale: ', PIM%TIM_SCL 
           END IF 
           WRITE ( UNIT=LUN, FMT='(A,F12.9)' ) 'REF_PIXEL:     ', PIM%REF_PIXL
           WRITE ( UNIT=LUN, FMT='(A,F12.9)' ) 'VIS_SCAL:      ', PIM%VIS_SCAL
           WRITE ( UNIT=LUN, FMT='(A)'    ) 'Experiment nominal start: '// &
     &                                       MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, IER )
           WRITE ( UNIT=LUN, FMT='(A)'    ) 'Experiment nominal end:   '// &
     &                                       MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                       PIM%TAI_0 + PIM%TIM_R8(PIM%NEPC) + PIM%AP_LEN_MAX, IER )
           WRITE ( UNIT=LUN, FMT='(A,F16.9)' ) 'Experiment nominal duration: ', &
     &                                          PIM%TIM_R8(PIM%NEPC)
!
! -------- Compute band statistics
!
           LBND = 0
           DO 4270 J27=1,PIM%NFRG
              DO 4280 J28=1,PIM%NFRQ
                 DO 4290 J29=1,PIM%NCHN
                     IF ( LBND == 0 ) THEN
                          LBND = 1
                          LCHN = 0
                          FREQ_BEG = PIM%FREQ_ARR(J29,J28,J27)
                          FREQ_END = PIM%FREQ_ARR(J29,J28,J27)
                          FREQ_AVR = 0.0D0
                          FREQ_SQR = 0.0D0
                     END IF
!
! ------------------ If the frequency difference between this channel and the beginning
! ------------------ of the band is greater than BAND__MAX, we consider that we
! ------------------ have reached the end of the band and start next band
!
                     IF ( PIM%FREQ_ARR(J29,J28,J27) - FREQ_BEG > BAND__MAX  .OR. &
     &                    ( J29 == PIM%NCHN .AND. &
     &                      J28 == PIM%NFRQ .AND. &
     &                      J27 == PIM%NFRG       ) ) THEN
!
                          IF ( ( J29 == PIM%NCHN .AND. &
     &                           J28 == PIM%NFRQ .AND. &
     &                           J27 == PIM%NFRG       ) ) THEN
                               LCHN = LCHN + 1
                               FREQ_END = PIM%FREQ_ARR(J29,J28,J27)
                               FREQ_AVR = FREQ_AVR + FREQ_END
                               FREQ_SQR = FREQ_SQR + FREQ_END**2
                          END IF
!
                          WRITE ( UNIT=LUN, FMT=1100 ) LBND, FREQ_BEG*1.D-6, &
     &                                                 (FREQ_END + PIM%CHAN_BW)*1.D-6, &
     &                                                 (FREQ_END - FREQ_BEG + PIM%CHAN_BW)*1.D-6
                          IF ( LCHN > 0 ) THEN
                               WRITE ( UNIT=LUN, FMT=1110 ) LBND, 1.D9/ &
     &                                (1.D-12 + PI2*DSQRT( (FREQ_SQR/LCHN) - (FREQ_AVR/LCHN)**2 ))
                            ELSE 
                               WRITE ( UNIT=LUN, FMT=1110 ) LBND, 0.0D0
                          END IF
 1100                     FORMAT ( 'Band ',I2, ' Frequency range [ ', F10.3, &
     &                             ' , ', F10.3, ' ] MHz    Bandwidth: ' &
     &                             F10.3, ' MHz ' )
 1110                     FORMAT ( 'Band ',I2, ' Group delay error for SNR=1 ', &
     &                              2X, F9.6, ' ns ' )
                          LBND = LBND + 1
                          LCHN = 0
                          FREQ_BEG = PIM%FREQ_ARR(J29,J28,J27)
                          FREQ_END = PIM%FREQ_ARR(J29,J28,J27)
                          FREQ_AVR = 0.0D0
                          FREQ_SQR = 0.0D0
                     END IF
!
                     IF ( PIM%FREQ_ARR(J29,J28,J27) > FRQ__MIN ) THEN
                          LCHN = LCHN + 1
                          FREQ_END = PIM%FREQ_ARR(J29,J28,J27)
                          FREQ_AVR = FREQ_AVR + FREQ_END
                          FREQ_SQR = FREQ_SQR + FREQ_END**2
                     END IF
 4290            CONTINUE
 4280         CONTINUE
 4270      CONTINUE
      END IF
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_WRITE_OBS  !#!#
