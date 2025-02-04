      SUBROUTINE SUR_ASTRO_SEQ ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_SEQ
! *                                                                      *
! * ### 12-JAN-2007  SUR_ASTRO_SEQ  v1.25 (c) L. Petrov  26-SEP-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_STOP, IVRB, IUER
      REAL*8     TAI_STOP
      REAL*8     SCORE(SUR__M_SOU), SLEW_STA(SUR__M_STA), &
     &           SLEW_SRC(SUR__M_SOU), IND_SCORE(SUR__M_SOU), SLEW_TIME_MAX
      REAL*8     TAI_OBS, AZ, EL, HA, ALP, DEL, A, B, &
     &           SLEW_EL, SLEW_AZ, SLEW_HA, SLEW_DEL, SLEW_A, SLEW_B, &
     &           TIM_DIF, TIM_VIS, TOT_VIS, FUT_VIS, TIM1, TIM2, &
     &           TIM_LAST, HIST_FACT, FUT_FACT, ONS_FACT, DIST, DIST_MIN, &
     &           TIM_LAS2, A_LAST, B_LAST, DIF_A, DIF_B, DIF_EL, DIF_DEL, DIF_AZ, &
     &           TIM_DIF_BEST, TIM_DIF_NRML
      REAL*8     AZ_STA(SUR__M_STA), EL_STA(SUR__M_STA), HA_STA(SUR__M_STA), &
     &           EL_SCAN_MIN, DIF_HA, PAR1, PAR2, PAR3, PAR4, ARG, ARC_MAX, DUR_SES
      REAL*8     PRI, DUR, TIME_FROM_START, ARC, SLEW_TIME_STA(SUR__M_STA), &
     &           GAP_MIN, GAP_NOR, SUR_SOU_DUR, TEMP_VAR(2), TC, TW
      REAL*8     TAPE_CHANGE_MIN_TIME
      PARAMETER  ( TAPE_CHANGE_MIN_TIME = 120.001D0 ) ! Mimimum change tape time
      CHARACTER  STR*128, STR1*128, STR2*128, JSOU_NAME*10
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           J14, J15, J16, J17, J18, J19, K1, K2, K3, K4, L_SOU, MULT_TRY, &
     &           MJD_OBS, IND_SLEW, &
     &           IND_SRC, CUR_TYP, LAST_SRC, UTC_OBS_INT, IND_SCN, &
     &           NUM_OBS_SOU(32), K_STA, IND_SC2, NSCA_MIN, NSCA_MAX, &
     &           IND_SRC_CAL, M_TRY, SPL_STATUS, IND_SOU_SPC, IVAL, IER
      LOGICAL*4  FL_TAPE_CHANGE, FL_LONG, FL_STA(SUR__M_STA), FL_GEO_TEST, &
     &           FL_LAST_SCA_EXT
      INTEGER*4  TRY
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      REAL*8,    EXTERNAL :: SUR_SLEW_TIME, DP_VV_V, ARC_LEN_VEC 
      REAL*8,    EXTERNAL :: CPU_TIMER, WALL_TIMER
      INTEGER*4, EXTERNAL :: MAX_LIST_R8, ILEN, I_LEN
!
      FL_TAPE_CHANGE = .FALSE. ! No tape change should happen immediately
      FL_GEO_TEST = .FALSE.
      FL_LAST_SCA_EXT = .TRUE.
!
      CALL GETENVAR ( 'SUR_SKED_GEO_TEST', STR )
      IF ( STR == 'YES' ) FL_GEO_TEST = .TRUE.
      CALL GETENVAR ( 'SUR_PROHIBIT_LAST_SCAN_EXTENSION', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_LAST_SCA_EXT = .TRUE.
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'SUR_ASTROSEQ: prohibit last scan extension'
           END IF
      END IF
!
      DUR = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &      (SUR%TAI_STOP - SUR%TAI_START)
      FUT_FACT = 1.0
      ONS_FACT = 1.0
      M_TRY = 4
!
      DUR_SES = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &          (SUR%TAI_STOP - SUR%TAI_START)
!
      IND_SRC = 0
      DO 410 J1=2,SUR__M_SCN
         IF ( IVRB .GE. 2 ) THEN
              WRITE ( 6, 110 ) '  Astro_seq start  Scan ', J1, CHAR(13)
              CALL FLUSH  ( 6 )
 110          FORMAT ( A, I5,' ',A$ )
         END IF
         IF ( SUR%ALGORITHM == 'ASTROMET_03' .OR. &
     &        SUR%ALGORITHM == 'ASTROMET_07'      ) THEN
              DO 420 J2=1,SUR%L_SOU
                 SUR%NOBS_SRC(J2) = SUR%SOU(J2)%NOBS
 420          CONTINUE 
              DO 430 J3=1,SUR%L_SO2
                 SUR%NOBS_SO2(J3) = SUR%SO2(J3)%NOBS
 430          CONTINUE 
           ELSE IF ( SUR%ALGORITHM == 'ASTROMET_05' ) THEN
              IND_SRC_CAL = 0
              DO 440 J4=SUR%L_SCN,1,-1
                 IF ( SUR%SRC_TYP(J4) == SUR__TYP_CAL ) THEN
                      IND_SRC_CAL = SUR%IND_SRC(J4)
                      GOTO 840
                 END IF
 440          CONTINUE 
 840          CONTINUE 
              DO 450 J5=1,SUR%L_SOU
                 SUR%NOBS_SRC(J5) = SUR%SOU(J5)%NOBS
 450          CONTINUE 
         END IF
         TC = CPU_TIMER  ( %VAL(0) ) 
         TW = WALL_TIMER ( %VAL(0) ) 
!
         DO 470 J7=1,M_TRY
            IF ( J7 == 1 ) THEN
                 CUR_TYP = SUR__TYP_TAG
                 L_SOU = SUR%L_SOU
               ELSE
                 IF ( SUR%ALGORITHM == 'ASTROMET_05' ) THEN
!
! -------------------- Attempts 1, 2, 4, 6, 8, ... etc are made with the list
! --------------------                             of primary sources
! ------------------- Condition: not to schedule more than NOBS_MAX sources
! ------------------- is lifted after the 1st attempt
!
                      IF ( MOD(J7,2) == 0 ) THEN 
                           CUR_TYP = SUR__TYP_TAG
                           L_SOU = SUR%L_SOU
                         ELSE IF ( MOD(J7,2) == 1 ) THEN 
!
! ------------------------ Attempts 3,5,7, ... etc are made with the list
! ------------------------                     of secondary sources
!
                           CUR_TYP = SUR__TYP_SEC
                           L_SOU = SUR%L_SO2
                       END IF
                    ELSE
                      IF ( MOD(J7,2) == 1 ) THEN 
                           CUR_TYP = SUR__TYP_TAG
                           L_SOU = SUR%L_SOU
                         ELSE IF ( MOD(J7,2) == 0 ) THEN 
!
! ------------------------ Attempts 2,4,6, ... etc are made with the list
! ------------------------                     of secondary sources
!
                           CUR_TYP = SUR__TYP_SEC
                           L_SOU = SUR%L_SO2
                       END IF
                END IF
            END IF
            CALL NOUT_R8 ( L_SOU, SCORE )
            IF ( IVRB .GE. 5 ) THEN
                 WRITE ( 6, * ) 'Try ', INT2(J7), ' Current source type ', SUR__TYP_STR(CUR_TYP), ' Scan: ', INT2(SUR%L_SCN)
            END IF
!
            IF ( IND_SRC > 0 ) THEN
                 IF ( SUR%TROPO_BURST_INTERVAL > 0.0D0  .AND. &
     &                (SUR%MJD_CUR - SUR%MJD_TROPO_CUR)*86400.0D0 + &
     &                (SUR%TAI_CUR - SUR%TAI_TROPO_CUR) + MAX(0.0D0, SUR%PREOBS_LONG) + &
     &                 SUR%SOU(IND_SRC)%DUR > SUR%TROPO_BURST_INTERVAL      ) THEN
                      IF ( IVRB .GE. 8 ) THEN
                           STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,       SUR%TAI_CUR,       -2 )
                           STR2 = MJDSEC_TO_DATE ( SUR%MJD_TROPO_CUR, SUR%TAI_TROPO_CUR, -2 )
                           WRITE ( 6, * ) 'SUR_ASTRO_SEQ Started troposphere burst, because '// &
     &                                 'time_cur= ', STR1(1:19)//' time_tropo_cur= ', STR2(1:19)// &
     &                                 ' scan_len= ', SNGL(SUR%SOU(IND_SRC)%DUR), ' tbi= ', &
     &                                 SNGL(SUR%TROPO_BURST_INTERVAL), ' tci= ', &
     &                                 (MJD_OBS - SUR%MJD_TROPO_CUR)*86400.0D0 + &
     &                                 (TAI_OBS - SUR%TAI_TROPO_CUR) + SUR%PREOBS_LONG + &
     &                                 SUR%SCAN_LEN 
                      END IF
!
                      CALL ERR_PASS ( IUER, IER )
                      CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 1723, IUER, 'SUR_ASTRO_SEQ', 'Error in '// &
     &                         'scheduling the burst of troposphere calibrators' )
                           RETURN
                      END IF
!
                      IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.D0 + &
     &                     (SUR%TAI_OBS_END(SUR%L_SCN) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > 0.0D0 ) THEN
!
! ------------------------ End of the session
!
                           IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                                SUR%L_OBS_TAG = SUR%L_OBS_TAG - 1
                              ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                                SUR%L_OBS_SEC = SUR%L_OBS_SEC - 1
                              ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                                SUR%L_OBS_CAL = SUR%L_OBS_CAL - 1
                           END IF
                           SUR%L_SCN = SUR%L_SCN - 1
                           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
                           IF ( IVRB .GE. 6 ) WRITE ( 6, * ) 'JUMP-1 to end scheduling'
                           GOTO 810
                      END IF
                      IF ( J1 < SUR__M_SCN ) THEN
                           GOTO 410
                      END IF
                 END IF
            END IF
!
            MJD_OBS = SUR%MJD_CUR
            TAI_OBS = SUR%TAI_CUR
!
            IF ( TAI_OBS > 86400.0D0 ) THEN
                 MJD_OBS = MJD_OBS + 1
                 TAI_OBS = TAI_OBS - 86400.0D0
            END IF
!
            IF ( SUR%N_GAP > 0 ) THEN
                 DO 480 J8=1,SUR%N_GAP
                    IF ( ( (MJD_OBS - SUR%MJD_GAP(1,J8))*86400.0D0 +                         &
     &                     (TAI_OBS + SUR%AVR_SLEW_TIME - SUR%TAI_GAP(1,J8)) ) > 0.0D0 .AND. &
     &                   ( (MJD_OBS - SUR%MJD_GAP(2,J8))*86400.0D0 +                         &
     &                     (TAI_OBS - SUR%TAI_GAP(2,J8)) ) < 0.0D0                            ) THEN
                         IF ( IVRB .GE. 1 ) THEN
                              CALL CLRCH ( STR1 )
                              CALL CLRCH ( STR2 )
                              STR1 = MJDSEC_TO_DATE ( SUR%MJD_GAP(1,J8), SUR%TAI_GAP(1,J8), -2 )
                              STR2 = MJDSEC_TO_DATE ( SUR%MJD_GAP(2,J8), SUR%TAI_GAP(2,J8), -2 )
                              WRITE ( 6, * ) ' Serve gap ', INT2(J8), ' between ', &
     &                                        STR1(1:19), ' and ', STR2(1:19)
                         END IF
                         MJD_OBS = SUR%MJD_GAP(2,J8)
                         TAI_OBS = SUR%TAI_GAP(2,J8)
                         SUR%MJD_CUR = MJD_OBS 
                         SUR%TAI_CUR = TAI_OBS 
                    END IF
 480             CONTINUE 
            END IF
!
            TIME_FROM_START = (MJD_OBS - SUR%MJD_START)*86400.0D0 + &
     &                        (TAI_OBS - SUR%TAI_START)
!
! --------- Array NOB_OBS_SOU keeps the number of target sources 
! --------- which are set to have K scans per source
!
            NUM_OBS_SOU = 0
            IND_SOU_SPC = 0
            TIM_DIF     = 1.D30
            IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                 DO 490 J9=1,SUR%SCAN_PER_SOURCE_NORM 
                    DO 4100 J10=1,SUR%L_SOU
                       IF ( SUR%NOBS_SRC(J10) .GE. J9 ) THEN
                            NUM_OBS_SOU(J9) = NUM_OBS_SOU(J9) + 1
                       END IF
                       IF ( J9 == 1 .AND. SUR%ALGORITHM == 'SPACECRAFT_01' ) THEN
                            CALL CHIN ( SUR%SOU(J10)%J2000_NAME(4:10), IVAL )
                            IF ( DABS ( TIME_FROM_START - IVAL ) < TIM_DIF ) THEN
                                 TIM_DIF = DABS ( TIME_FROM_START - IVAL - SUR%UTC_M_TAI )
                                 IND_SOU_SPC = J10
                            END IF
                       END IF
 4100               CONTINUE 
 490             CONTINUE 
            END IF
!
! --------- The number of sources is different depending on mode:
! --------- target or secondary
!
            DO 4110 J11=1,L_SOU
!
! ------------ If the J11-th target source exceeded its quote, good bye
!
               IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                    NSCA_MIN = SUR%SOU(J11)%NSCA_MIN 
                    NSCA_MAX = SUR%SOU(J11)%NSCA_MAX
                    GAP_MIN  = SUR%SOU(J11)%GAP_MIN 
                    GAP_NOR  = SUR%SOU(J11)%GAP_NOR
                    IF ( J7 < 2 .AND. SUR%NOBS_SRC(J11) .GE. NSCA_MAX ) GOTO 4110
                    IF ( SUR%ALGORITHM == 'GNSS_01' .OR. &
                         SUR%ALGORITHM == 'GNSS_02'      ) THEN
                         IF ( DABS( (SUR%MJD_CUR - SUR%SOU(J11)%MJD_EPOCH)*86400.0D0 + &
     &                              ( SUR%TAI_CUR +           &
     &                                SUR%AVR_SLEW_TIME +     &
     &                                SUR%SOU(J11)%DUR/2.0D0 - &
     &                                SUR%SOU(J11)%TAI_EPOCH)  ) > SUR%SOU(J11)%RANGE ) THEN
                              GOTO 4110
                         END IF
                    END IF
                    JSOU_NAME = SUR%SOU(J11)%J2000_NAME
                 ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                    NSCA_MIN = SUR%SO2(J11)%NSCA_MIN 
                    NSCA_MAX = SUR%SO2(J11)%NSCA_MAX
                    GAP_MIN  = SUR%SO2(J11)%GAP_MIN 
                    GAP_NOR  = SUR%SO2(J11)%GAP_NOR
                    IF ( J7 < 3 .AND. SUR%NOBS_SO2(J11) .GE. NSCA_MAX ) GOTO 4110
                    IF ( SUR%ALGORITHM == 'GNSS_01' .OR. &
     &                   SUR%ALGORITHM == 'GNSS_02'      ) THEN
                         IF ( DABS( (SUR%MJD_CUR - SUR%SO2(J11)%MJD_EPOCH)*86400.0D0 + &
     &                              (SUR%TAI_CUR + SUR%AVR_SLEW_TIME - SUR%SOU(J11)%TAI_EPOCH) ) > &
     &                        SUR%SO2(J11)%RANGE ) THEN
                              GOTO 4110
                         END IF
                    END IF
                    JSOU_NAME = SUR%SOU(J11)%J2000_NAME
               END IF
!
! ------------ Compute time elapsed after observing that source last time
!
               IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                    IND_SCN = 0
                    IF ( SUR%NOBS_SRC(J11) > 0 ) THEN
                         IND_SCN  = SUR%IND_SCN_SRC(SUR%NOBS_SRC(J11),J11)
                         IF ( IND_SCN > 0 ) THEN
                              TIM_LAST = (MJD_OBS - SUR%MJD_OBS_BEG(IND_SCN))*86400.0D0 + &
     &                                   (TAI_OBS - SUR%TAI_OBS_BEG(IND_SCN))
                            ELSE 
                              TIM_LAST = 10.0D0*86400.0D0
                         END IF
                       ELSE
                         TIM_LAST = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &                              (SUR%TAI_STOP - (SUR%TAI_START - SUR%POSTSES_INTERVAL)) + 1.0D0
                    END IF
                  ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                    TIM_LAST = (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &                         (SUR%TAI_STOP - SUR%TAI_START)
                    IND_SCN = 0
                    DO 4120 J12=1,SUR%L_SOU
                       IF ( SUR%SOU(J12)%B1950_NAME == SUR%SO2(J11)%B1950_NAME ) THEN
                            IF ( SUR%NOBS_SRC(J12) > 0 ) THEN
                                 IND_SCN  = SUR%IND_SCN_SO2(SUR%NOBS_SRC(J12),J12)
                                 IF ( IND_SCN > 0 ) THEN
                                      TIM_LAST = MIN( TIM_LAST, &
     &                                          (MJD_OBS - SUR%MJD_OBS_BEG(IND_SCN))*86400.0D0 + &
     &                                          (TAI_OBS - SUR%TAI_OBS_BEG(IND_SCN)) )
                                 END IF
                            END IF
                            IF ( SUR%NOBS_SRC(J11) > 0 ) THEN
                                 IND_SCN  = SUR%IND_SCN_SRC(SUR%NOBS_SRC(J11),J11)
                                 IF ( IND_SCN > 0 ) THEN
                                      TIM_LAST = MIN( TIM_LAST, &
     &                                          (MJD_OBS - SUR%MJD_OBS_BEG(IND_SCN))*86400.0D0 + &
     &                                          (TAI_OBS - SUR%TAI_OBS_BEG(IND_SCN)) )
                                 END IF
                            END IF
                       END IF
 4120               CONTINUE 
               END IF
!
! ------------ If the source was observed recently, skip it
!
               IF ( CUR_TYP == SUR__TYP_TAG  .AND.  TIM_LAST < GAP_MIN ) THEN
                    IF ( SUR%SOU(J11)%NSCA_MIN < 999 ) THEN
                         IF ( GAP_MIN < DUR_SES ) THEN
                              GOTO 4110
                         END IF
                       ELSE 
                         IF ( SUR%TROPO_BURST_INTERVAL > 0.0D0 ) THEN
                              IF ( TIM_LAST < SUR%TROPO_BURST_INTERVAL/5.0D0 ) GOTO 4110
                         END IF
                    END IF
               END IF
               IF ( CUR_TYP == SUR__TYP_SEC  .AND.  TIM_LAST < GAP_MIN/J7 ) GOTO 4110
!
               IF ( SUR%L_SCN > 0 ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    SUR%L_SCN = SUR%L_SCN + 1
                    SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, CUR_TYP, &
     &                                              J11, SUR%IND_SRC(SUR%L_SCN-1), &
     &                                              SUR%SRC_TYP(SUR%L_SCN-1), -2, &
     &                                              SUR__FINE, IER )
                    SUR%L_SCN = SUR%L_SCN - 1
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 1721, IUER, 'SUR_ASTRO_SEQ', 'Trap '// &
     &                       'of internal control: error in SUR_SLEW_TIME ' )
                         RETURN
                    END IF
                    IF ( SLEW_TIME_MAX < 0.0D0 ) GOTO 4110
                  ELSE 
                    SLEW_TIME_MAX = 0.0D0
               END IF
!
! ------------ TIM_VIS -- how long the source was visible from the rise
! ------------            till now
!
! ------------ TOT_VIS -- the total visibility time
!
               TIM_VIS = 0.0D0
               TOT_VIS = 0.0D0
               DO 4130 J13=1,SUR%N_VIS(J11,CUR_TYP)
!
! --------------- TIM1 -- time elapsed from the J13-th rise
! --------------- TIM1 -- time elapsed from the J13-th set
!
                  TIM1 = (MJD_OBS - SUR%MJD_VIS(J13,SUR__RIS,J11,CUR_TYP))*86400.0D0 + &
     &                   (TAI_OBS - SUR%TAI_VIS(J13,SUR__RIS,J11,CUR_TYP))
                  TIM2 = (MJD_OBS - SUR%MJD_VIS(J13,SUR__SET,J11,CUR_TYP))*86400.0D0 + &
     &                   (TAI_OBS - SUR%TAI_VIS(J13,SUR__SET,J11,CUR_TYP))
                  IF ( TIM1 > 0.0D0  .AND.  TIM2 > 0.0D0 ) THEN
                       TIM_VIS = TIM_VIS + &
     &                              (SUR%MJD_VIS(J13,SUR__SET,J11,CUR_TYP) - &
     &                               SUR%MJD_VIS(J13,SUR__RIS,J11,CUR_TYP))*86400.0D0 + &
     &                              (SUR%TAI_VIS(J13,SUR__SET,J11,CUR_TYP) - &
     &                               SUR%TAI_VIS(J13,SUR__RIS,J11,CUR_TYP))
                    ELSE IF ( TIM1 > 0.0D0  .AND.  TIM2 < 0.0D0 ) THEN
                       TIM_VIS = TIM_VIS + &
     &                     (MJD_OBS - SUR%MJD_VIS(J13,SUR__RIS,J11,CUR_TYP))*86400.0D0 + &
     &                     (TAI_OBS - SUR%TAI_VIS(J13,SUR__RIS,J11,CUR_TYP))
                  END IF
                  TOT_VIS = TOT_VIS + (SUR%MJD_VIS(J13,SUR__SET,J11,CUR_TYP) - &
     &                                 SUR%MJD_VIS(J13,SUR__RIS,J11,CUR_TYP))*86400.0D0 + &
     &                                (SUR%TAI_VIS(J13,SUR__SET,J11,CUR_TYP) - &
     &                                 SUR%TAI_VIS(J13,SUR__RIS,J11,CUR_TYP))
 4130          CONTINUE
!
! ------------ Compute the history factor
!
               IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                    IF ( SUR%GAPS_SRC(J11) > 0.0D0 ) THEN
                         HIST_FACT = (TOT_VIS - TIM_VIS)/SUR%GAPS_SRC(J11) - SUR%NOBS_SRC(J11)
                         IF ( HIST_FACT < 0.0 ) HIST_FACT = 0.0
                      ELSE
                         HIST_FACT = 0.0D0
                    END IF
               END IF
!
! ------------ Set score
!
               SLEW_SRC(J11) = SLEW_TIME_MAX
               SCORE(J11) = 120000.0D0/(SLEW_TIME_MAX + MAX(0.0D0, SUR%PREOBS_LONG))
               IF ( SUR%ALGORITHM == 'ASTROMET_07' ) THEN
                    SCORE(J11) = 100.0D0
               END IF
!
! ------------ Update the score
!
               IF ( SUR%ALGORITHM == 'ASTROMET_01' ) THEN
!
! ----------------- Update of the score in the case of a low-declination source
!
                    IF ( SUR%SOU(J11)%DELTA < 20.0D0*DEG__TO__RAD ) THEN
                         SCORE(J11) = SCORE(J11) + &
     &                        100.D0*( 20.0D0*DEG__TO__RAD - SUR%SOU(J11)%DELTA )
                    END IF
!
                    IF ( SUR%NOBS_SRC(J11) .GE. SUR%SCAN_PER_SOURCE_NORM ) THEN
                         SCORE(J11) = SCORE(J11)/2.0D0**(SUR%SCAN_PER_SOURCE_NORM-SUR%NOBS_SRC(J11)+1)
                    END IF
                    IF ( TIM_VIS > SUR%GAPS_SRC(J11) .AND. HIST_FACT > 0.0D0 ) THEN
                         SCORE(J11) = SCORE(J11) + 1.0D0*(1.0D0+HIST_FACT)**8
                    END IF
                  ELSE IF ( SUR%ALGORITHM == 'ASTROMET_02' ) THEN
                    IF ( SUR%NOBS_SRC(J11) == 0         .AND. &
     &                   NUM_OBS_SOU(1) < SUR%NOBS_MIN .AND. &
     &                   TOT_VIS > 0.0D0               .AND. &
     &                   TIM_VIS > 0.0D0                     ) THEN
                         IF ( TIM_VIS < 0.1D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)*1000.0D0
                            ELSE IF ( TIM_VIS < 0.2D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)*500.0D0
                            ELSE IF ( TIM_VIS < 0.3D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)*200.0D0
                            ELSE IF ( TIM_VIS < 0.4D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)*50.0D0
                            ELSE IF ( TIM_VIS < 0.6D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)/10.0D0
                            ELSE IF ( TIM_VIS < 0.7D0*TOT_VIS ) THEN
                              SCORE(J11) = SCORE(J11)/100.0D0
                         END IF
                       ELSE IF ( SUR%NOBS_SRC(J11) > 0 ) THEN
                         IF ( TIM_LAST > GAP_MIN        .AND. &
     &                        TIM_LAST < 0.95*DUR       .AND. &
     &                        SUR%NOBS_SRC(J11) < GAP_NOR      ) THEN
                              IF ( MOD(J7,2) == 1 ) THEN
!
! -------------------------------- Primary   source list
!
                                   SCORE(J11) = SCORE(J11)*DEXP( (2.0D0*TIM_LAST/GAP_MIN)**2 )
                                 ELSE
!
! -------------------------------- Secondary source list
!
                                   SCORE(J11) = SCORE(J11)*DEXP( (TIM_LAST/(4.0D0*GAP_MIN))**2 )
                              END IF
                         END IF
                    END IF
!
! ----------------- Do not start putting a new source in the schedule if it is
! ----------------- too late -- less than xxx before the end of the scan
!
                    IF ( SUR%SCAN_PER_SOURCE_NORM .GE. 2  .AND. &
     &                   SUR%NOBS_SRC(J11) == 0  .AND. &
     &                  ( (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &                    (SUR%TAI_STOP - SUR%TAI_START) - &
     &                    (SUR%MJD_CUR  - SUR%MJD_START)*86400.0D0  - &
     &                    (SUR%TAI_CUR  - SUR%TAI_START) ) &
     &                   < (SUR%SCAN_PER_SOURCE_NORM-1.5)*GAP_MIN ) THEN
!
                         SCORE(J11) = SCORE(J11)/100.0D0
                    END IF
!
                    IF ( SUR%NOBS_SRC(J11) > GAP_NOR ) THEN
                         SCORE(J11) = SCORE(J11)/3000.0D0
                    END IF
                  ELSE IF ( ( SUR%ALGORITHM == 'ASTROMET_03' .OR.    &
     &                        SUR%ALGORITHM == 'ASTROMET_05' ) .AND. &
     &                      CUR_TYP == SUR__TYP_TAG                  ) THEN 
!
! ----------------- TIM_VIS  -- how long the source was visible from the rise till now
! ----------------- TOT_VIS  -- the total visibility time
! ----------------- FUT_VIS  -- how long the source will be visible
! ----------------- TIM_LAST -- time elapsed after observing that source last time
!
                    FUT_VIS  = TOT_VIS - TIM_VIS
!
! ----------------- Normal score in range [1, 50]
!
                    IF ( GAP_NOR > 1 ) THEN
                         FUT_FACT = FUT_VIS/GAP_NOR + 1 - SUR%NOBS_SRC(J11) 
                         ONS_FACT = TIM_VIS/GAP_NOR
                       ELSE 
                         FUT_FACT = 1.0D0
                         ONS_FACT = 1.0D0
                    END IF
!
                    SCORE(J11) = 500.0D0/(SLEW_TIME_MAX + MAX(0.0D0, SUR%PREOBS_LONG))
                    IF ( SUR%NOBS_SRC(J11) == 0 ) THEN
                         IF ( NUM_OBS_SOU(1) < SUR%NOBS_MIN .AND. NUM_OBS_SOU(1) .LT. SUR%NOBS_MAX ) THEN 
!
! --------------------------- The number of new sources is less than the first threshold
!
                              SCORE(J11) = 1.0D0*SCORE(J11)* &
     &                            (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**2
!
! --------------------------- Raise priority of source that just has risen
!
                              IF ( NUM_OBS_SOU(1) == 0 ) THEN
                                   IF ( ONS_FACT < 0.3 ) SCORE(J11) = 2.0*SCORE(J11) 
                                   IF ( ONS_FACT < 0.2 ) SCORE(J11) = 2.0*SCORE(J11) 
                                   IF ( ONS_FACT < 0.1 ) SCORE(J11) = 2.0*SCORE(J11) 
                              END IF
                              IF ( FUT_FACT < NSCA_MIN + 0.25  .AND. SUR%SOU(J11)%NSCA_MIN < 99 ) THEN
!
! -------------------------------- Discourage scheduling new sources if there is no 
! -------------------------------- time to complete it
!
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) < SUR%NOBS_MAX ) THEN
!
! --------------------------- The number of new sources is less than the second threshold
!
                              SCORE(J11) = 0.1D0*SCORE(J11)* &
     &                            (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**4
                              IF ( FUT_FACT < NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX ) THEN
                              IF ( J7 == 1 ) THEN
!
! --------------------------------- Do not schedule new sources 
! --------------------------------- if their number exceeded some threshold
!
                                   SCORE(J11) = -7
                                   IF ( IVRB .GE. 7  .AND.  CUR_TYP == SUR__TYP_TAG ) THEN
                                        WRITE ( 6, * ) 'Tag: ', SUR%SOU(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                      ELSE 
                                        WRITE ( 6, * ) 'Tag: ', SUR%SO2(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                   END IF
                                ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX+2 ) THEN
                                   SCORE(J11) = -8
                              END IF
                         END IF
                       ELSE
!
! ---------------------- This source has already been observed
!
                         IF ( GAP_NOR - SUR%NOBS_SRC(J11) > 0 ) THEN
                              FUT_FACT = FUT_VIS/GAP_NOR - SUR%NOBS_SRC(J11) + 1
                            ELSE 
                              FUT_FACT = 1.0D0
                         END IF
!
                         IF ( SUR%NOBS_SRC(J11) < SUR%SCAN_PER_SOURCE_NORM .AND. &
     &                        TIM_LAST > GAP_MIN                                ) THEN
                              IF ( SUR%SCAN_PER_SOURCE_NORM  > 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = MIN ( 16.0D0, 2.0D0*(TIM_LAST/GAP_NOR)**2 )
                                   SCORE(J11) = 64.0*SCORE(J11)*DEXP(ARG)
                                 ELSE IF ( SUR%SCAN_PER_SOURCE_NORM == 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = 1.0D0 - FUT_VIS/GAP_NOR
                                   SCORE(J11) = SCORE(J11)*DEXP(6.0D0*ARG)
                              END IF                                  
                            ELSE IF ( SUR%NOBS_SRC(J11) .GE. SUR%SCAN_PER_SOURCE_NORM ) THEN
                              SCORE(J11) = SCORE(J11)/1.D4
                         END IF
                    END IF
                    IF ( SLEW_SRC(J11) .GE. 2.0*(SUR%SOU(J11)%DUR + SUR%AVR_SLEW_TIME) ) THEN
!
! ---------------------- We can observe two sources while we slew to that...
!
                         SCORE(J11) = SCORE(J11)*1.D-5
                    END IF
                  ELSE IF ( SUR%ALGORITHM == 'ASTROMET_04' ) THEN
!
! ----------------- TIM_VIS  -- how long the source was visible from the rise till now
! ----------------- TOT_VIS  -- the total visibility time
! ----------------- FUT_VIS  -- how long the source will be visible
! ----------------- TIM_LAST -- time elapsed after observing that source last time
!
                    FUT_VIS  = TOT_VIS - TIM_VIS
                    IF ( GAP_NOR > 1 ) THEN
                         FUT_FACT = FUT_VIS/GAP_NOR + 1 - SUR%NOBS_SRC(J11) 
                       ELSE 
                         FUT_FACT = 1.0D0
                    END IF
!
! ----------------- Normal score in range [1, 10]
!
                    SCORE(J11) = 100.0D0/(SLEW_TIME_MAX + MAX(0.0D0, SUR%PREOBS_LONG))
                    IF ( SUR%NOBS_SRC(J11) == 0  .AND.  SUR%SOU(J11)%NOBS == 1 ) THEN
!
! ---------------------- The source has been observed in the previous session. 
! ---------------------- Put him in the 2nd priority
!
                         SCORE(J11) = 1.D4 + SCORE(J11) 
                       ELSE IF ( SUR%NOBS_SRC(J11) == 0  .AND.  SUR%SOU(J11)%NOBS == 0 ) THEN
                         IF ( NUM_OBS_SOU(1) < SUR%NOBS_MIN ) THEN
!
! --------------------------- The number of new sources is less than the first threshold
!
                              IF ( FUT_FACT .GE. NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = 1.0D0*SCORE(J11)* &
     &                                         (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**4
                                ELSE IF ( FUT_FACT < NSCA_MIN + 0.25 ) THEN
!
! -------------------------------- Discourage scheduling new sources if there is no 
! -------------------------------- time to complete it
!
                                   SCORE(J11) = SCORE(J11)/1.D4
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) < SUR%NOBS_MAX ) THEN
!
! --------------------------- The number of new sources is less than the second threshold
!
                              IF ( FUT_FACT .GE. NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = 0.1D0*SCORE(J11)* &
     &                                         (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**4
                                 ELSE IF ( FUT_FACT < NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX ) THEN
!
! --------------------------- Do not schedule new sources if their number exceeded some threshold
!
                              SCORE(J11) = -1.012D0
                         END IF
                       ELSE 
!
! ---------------------- The source has already been observed in this session
!
                         IF ( SUR%NOBS_SRC(J11) .GE. 1  .AND.  &
     &                        SUR%SOU(J11)%NOBS .GE. 1         ) THEN
!
! --------------------------- This source has been already observed enough.
!
                              SCORE(J11) = -2.0D0
                           ELSE
!
! --------------------------- This source has already been observed
!
                              IF ( GAP_NOR - SUR%NOBS_SRC(J11) > 0 ) THEN
                                   FUT_FACT = FUT_VIS/GAP_NOR - SUR%NOBS_SRC(J11) + 1
                                 ELSE 
                                   FUT_FACT = 1.0D0
                              END IF
!
                              IF ( SUR%NOBS_SRC(J11) < SUR%SCAN_PER_SOURCE_NORM .AND. &
     &                             TIM_LAST > GAP_NOR                                ) THEN
                                   IF ( FUT_FACT > 0.0 ) THEN
                                        SCORE(J11) = 1.D8 + SCORE(J11)*DEXP((TIM_LAST/GAP_NOR))
                                   END IF
                                 ELSE IF ( SUR%NOBS_SRC(J11) .GE. GAP_NOR ) THEN
                                   SCORE(J11) = SCORE(J11)/1.D4
                              END IF
                         END IF
                    END IF
                  ELSE IF ( SUR%ALGORITHM == 'ASTROMET_06' .AND. &
     &                      CUR_TYP == SUR__TYP_TAG              ) THEN 
                    ONS_FACT = 1.D0/(1.0D0 + SUR%NOBS_SRC(J11))
                    FUT_VIS  = 1.D0/(1.0D0 + SUR%NOBS_SRC(J11))
                    SCORE(J11) = FUT_VIS + TIM_LAST
                  ELSE IF ( SUR%ALGORITHM == 'ASTROMET_07' .AND. &
     &                      CUR_TYP == SUR__TYP_TAG              ) THEN 
                    FUT_VIS  = TOT_VIS - TIM_VIS
!
! ----------------- Normal score in range [1, 50]
!
                    IF ( GAP_NOR > 1 ) THEN
                         FUT_FACT = FUT_VIS/GAP_NOR + 1 - SUR%NOBS_SRC(J11) 
                         ONS_FACT = TIM_VIS/GAP_NOR
                       ELSE 
                         FUT_FACT = 1.0D0
                         ONS_FACT = 1.0D0
                    END IF
!
                    IF ( SUR%NOBS_SRC(J11) == 0 ) THEN
                         IF ( NUM_OBS_SOU(1) < SUR%NOBS_MIN .AND. NUM_OBS_SOU(1) .LT. SUR%NOBS_MAX ) THEN 
!
! --------------------------- The number of new sources is less than the first threshold
!
                              SCORE(J11) = 1.0D0*SCORE(J11)* &
     &                            (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**2
!
! --------------------------- Raise priority of source that just has risen
!
                              IF ( NUM_OBS_SOU(1) == 0 ) THEN
                                   IF ( ONS_FACT < 0.3 ) SCORE(J11) = 2.0*SCORE(J11) 
                                   IF ( ONS_FACT < 0.2 ) SCORE(J11) = 2.0*SCORE(J11) 
                                   IF ( ONS_FACT < 0.1 ) SCORE(J11) = 2.0*SCORE(J11) 
                              END IF
                              IF ( FUT_FACT < NSCA_MIN + 0.25  .AND. SUR%SOU(J11)%NSCA_MIN < 99 ) THEN
!
! -------------------------------- Discourage scheduling new sources if there is no 
! -------------------------------- time to complete it
!
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) < SUR%NOBS_MAX ) THEN
!
! --------------------------- The number of new sources is less than the second threshold
!
                              SCORE(J11) = 0.1D0*SCORE(J11)* &
     &                            (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**4
                              IF ( FUT_FACT < NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX ) THEN
                              IF ( J7 == 1 ) THEN
!
! --------------------------------- Do not schedule new sources 
! --------------------------------- if their number exceeded some threshold
!
                                   SCORE(J11) = -7
                                   IF ( IVRB .GE. 7  .AND.  CUR_TYP == SUR__TYP_TAG ) THEN
                                        WRITE ( 6, * ) 'Tag: ', SUR%SOU(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                      ELSE 
                                        WRITE ( 6, * ) 'Tag: ', SUR%SO2(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                   END IF
                                ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX+2 ) THEN
                                   SCORE(J11) = -8
                              END IF
                         END IF
                       ELSE
!
! ---------------------- This source has already been observed
!
                         IF ( GAP_NOR - SUR%NOBS_SRC(J11) > 0 ) THEN
                              FUT_FACT = FUT_VIS/GAP_NOR - SUR%NOBS_SRC(J11) + 1
                            ELSE 
                              FUT_FACT = 1.0D0
                         END IF
!
                         TIM_DIF = 1.D30
                         DO 4140 J14=SUR%L_SCN,1,-1
                            IF ( SUR%SRC_TYP(J14) == SUR__TYP_TAG ) THEN
                                 IF ( SUR%SOU(SUR%IND_SRC(J14))%J2000_NAME == JSOU_NAME ) THEN
                                      TIM_DIF = ( MJD_OBS - SUR%MJD_OBS_END(J14) )*86400.0D0 + &
     &                                          ( TAI_OBS - SUR%TAI_OBS_END(J14) )
                                      GOTO 8140
                                 END IF
                              ELSE IF ( SUR%SRC_TYP(J14) == SUR__TYP_SEC ) THEN
                                 IF ( SUR%SO2(SUR%IND_SRC(J14))%J2000_NAME == JSOU_NAME ) THEN
                                      TIM_DIF = ( MJD_OBS - SUR%MJD_OBS_END(J14) )*86400.0D0 + &
     &                                          ( TAI_OBS - SUR%TAI_OBS_END(J14) )
                                      GOTO 8140
                                 END IF
                            END IF
 4140                    CONTINUE 
 8140                    CONTINUE 
                         TIM_DIF_BEST = 43200.D0/NSCA_MIN
!
! ---------------------- Rangs from 0.0 to 1.0; 0.0 is the best
!
                         TIM_DIF_NRML = 2.0D0*ABS ( TIM_DIF - IDNINT ( TIM_DIF/TIM_DIF_BEST )*TIM_DIF_BEST )/TIM_DIF_BEST
                         IF ( TIM_DIF < TIM_DIF_BEST/2.D0 ) THEN
                              TIM_DIF_NRML = (TIM_DIF_BEST - TIM_DIF)/TIM_DIF_BEST
                         END IF
                         SCORE(J11) = SCORE(J11)/(0.1**2 + TIM_DIF_NRML**2)**2
!%%                       write ( 6, * ) 'HHH sou= ', JSOU_NAME, ' TIM_DIF_HR = ', sngl(tim_dif/3600.0d0), ' TD = ', sngl(tim_dif_nrml), ' up_score= ', sngl(1.0d0/(0.1**2 + tim_dif_nrml**2)**2) ! %%%%%%
!
                         IF ( SUR%NOBS_SRC(J11) < SUR%SCAN_PER_SOURCE_NORM .AND. &
     &                        TIM_LAST > GAP_MIN                                ) THEN
                              IF ( SUR%SCAN_PER_SOURCE_NORM  > 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = MIN ( 16.0D0, 2.0D0*(TIM_LAST/GAP_NOR)**2 )
                                   SCORE(J11) = 64.0*SCORE(J11)*DEXP(ARG)
                                 ELSE IF ( SUR%SCAN_PER_SOURCE_NORM == 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = 1.0D0 - FUT_VIS/GAP_NOR
                                   SCORE(J11) = SCORE(J11)*DEXP(6.0D0*ARG)
                              END IF                                  
                            ELSE IF ( SUR%NOBS_SRC(J11) .GE. SUR%SCAN_PER_SOURCE_NORM ) THEN
                              SCORE(J11) = SCORE(J11)/1.D4
                         END IF
                    END IF
                    IF ( SLEW_SRC(J11) .GE. 2.0*(SUR%SOU(J11)%DUR + SUR%AVR_SLEW_TIME) ) THEN
!
! ---------------------- We can observe two sources while we slew to that...
!
                         SCORE(J11) = SCORE(J11)*1.D-5
                    END IF
!!!!!!!!!!!!!!!!!!!!
                  ELSE IF ( SUR%ALGORITHM == 'ASTROMET_A5' .AND. &
     &                      CUR_TYP == SUR__TYP_TAG              ) THEN 
!
! ----------------- TIM_VIS  -- how long the source was visible from the rise till now
! ----------------- TOT_VIS  -- the total visibility time
! ----------------- FUT_VIS  -- how long the source will be visible
! ----------------- TIM_LAST -- time elapsed after observing that source last time
!
                    FUT_VIS  = TOT_VIS - TIM_VIS
!
! ----------------- Normal score in range [1, 50]
!
                    IF ( GAP_NOR > 1 ) THEN
                         FUT_FACT = FUT_VIS/GAP_NOR + 1 - SUR%NOBS_SRC(J11) 
                       ELSE 
                         FUT_FACT = 1.0D0
                    END IF
                    IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                         ARC = ARC_LEN_VEC ( SUR%SOU(J11)%S_VEC, &
     &                                       SUR%CAL(IND_SRC_CAL)%S_VEC )
                       ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                         ARC = ARC_LEN_VEC ( SUR%SO2(J11)%S_VEC, &
     &                                       SUR%CAL(IND_SRC_CAL)%S_VEC )
                    END IF              
                    IF ( ARC > SUR%EL_CHANGE_TSYS ) THEN
                         SLEW_TIME_MAX = SLEW_TIME_MAX + 15.0D0 + SUR%TROPO_SCAN_LEN
                    END IF
!
                    SCORE(J11) = 3000.0D0/(SLEW_TIME_MAX + MAX(0.0D0, SUR%PREOBS_LONG))
                    IF ( SUR%NOBS_SRC(J11) == 0 ) THEN
                         IF ( NUM_OBS_SOU(1) < SUR%NOBS_MIN ) THEN
!
! --------------------------- The number of new sources is less than the first threshold
!
                              SCORE(J11) = 1.0D0*SCORE(J11)* &
     &                                    (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**2
                              IF ( FUT_FACT < NSCA_MIN + 0.20 ) THEN
!
! -------------------------------- Discourage scheduling new sources if there is no 
! -------------------------------- time to complete it
!
                                   SCORE(J11) = SCORE(J11)/1.D3
                              END IF
                              IF ( TIM_VIS/GAP_MIN > 0.5 ) THEN
                                   SCORE(J11) = SCORE(J11)*DEXP( (3.6D0*TIM_VIS/GAP_MIN)**2)
                              END IF 
                            ELSE IF ( NUM_OBS_SOU(1) < SUR%NOBS_MAX ) THEN
!
! --------------------------- The number of new sources is less than the second threshold
!
                              SCORE(J11) = 0.1D0*SCORE(J11)* &
     &                            (FUT_FACT - SUR%SCAN_PER_SOURCE_NORM + 1)**4
                              IF ( FUT_FACT < NSCA_MIN + 0.25 ) THEN
                                   SCORE(J11) = SCORE(J11)/1.D6
                              END IF
                            ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX ) THEN
                              IF ( J7 == 1 ) THEN
!
! --------------------------------- Do not schedule new sources 
! --------------------------------- if their number exceeded some threshold
!
                                   SCORE(J11) = -7
                                   IF ( IVRB .GE. 7  .AND.  CUR_TYP == SUR__TYP_TAG ) THEN
                                        WRITE ( 6, * ) 'Tag: ', SUR%SOU(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                      ELSE 
                                        WRITE ( 6, * ) 'Tag: ', SUR%SO2(J11)%J2000_NAME, NUM_OBS_SOU(1), SUR%NOBS_MAX
                                   END IF
                                ELSE IF ( NUM_OBS_SOU(1) .GE. SUR%NOBS_MAX+2 ) THEN
                                   SCORE(J11) = -8
                              END IF
                         END IF
                       ELSE
!
! ---------------------- This source has already been observed
!
                         IF ( GAP_NOR - SUR%NOBS_SRC(J11) > 0 ) THEN
                              FUT_FACT = FUT_VIS/GAP_NOR - SUR%NOBS_SRC(J11) + 1
                            ELSE 
                              FUT_FACT = 1.0D0
                         END IF
!
                         IF ( SUR%NOBS_SRC(J11) < SUR%SCAN_PER_SOURCE_NORM .AND. &
     &                        TIM_LAST > GAP_MIN                                ) THEN
                              IF ( SUR%SCAN_PER_SOURCE_NORM  > 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = MIN ( 16.0D0, 2.0D0*(TIM_LAST/GAP_NOR)**2 )
                                   SCORE(J11) = 2.0*SCORE(J11)*DEXP(ARG)
                                 ELSE IF ( SUR%SCAN_PER_SOURCE_NORM == 2 .AND. FUT_FACT > 0.0 ) THEN
                                   ARG = 1.0D0 - FUT_VIS/GAP_NOR
                                   SCORE(J11) = SCORE(J11)*DEXP(6.0D0*ARG)
                              END IF
                            ELSE IF ( SUR%NOBS_SRC(J11) .GE. GAP_NOR ) THEN
                              SCORE(J11) = SCORE(J11)/1.D4
                         END IF
                    END IF
!                    IF ( CUR_TYP == SUR__TYP_TAG .AND. SUR%SOU(J11)%DEC > 10.0D0*DEG__TO__RAD ) THEN
!                         SCORE(J11) = SCORE(J11)*DSIN(DSIN( MAX(0.1D0, EL_MAX) )**2
!                    END IF
                    IF ( SLEW_SRC(J11) .GE. 2.0*(SUR%SOU(J11)%DUR + SUR%AVR_SLEW_TIME) ) THEN
!
! ---------------------- We can observe two sources while we slew to that...
!
                         SCORE(J11) = SCORE(J11)*1.D-5
                    END IF
                  ELSE IF ( SUR%ALGORITHM == 'GEODETIC_01'   .OR. &
     &                      SUR%ALGORITHM == 'GEODETIC_02'   .OR. &
     &                      SUR%ALGORITHM == 'GEODETIC_03'   .OR. &
     &                      SUR%ALGORITHM == 'GNSS_01'       .OR. &
     &                      SUR%ALGORITHM == 'GNSS_02'            ) THEN
                    IF ( SUR%NOBS_SRC(J11) > 0 ) THEN
                         IF ( TIM_LAST < GAP_MIN ) THEN
                              SCORE(J11) = SCORE(J11)/1.D6
                         END IF
                    END IF
                    DIST_MIN = PI2
                    DO 4150 J15=SUR%L_SCN,1,-1
                       TIM_DIF = (MJD_OBS - SUR%MJD_OBS_BEG(J15))*86400.0D0 + &
     &                           (TAI_OBS - SUR%TAI_OBS_BEG(J15))
                       IF ( TIM_DIF > 2.0*SUR%TROPO_BURST_INTERVAL .AND. &
     &                      SUR%TROPO_BURST_INTERVAL > 0.0D0          ) GOTO 4150
                       IF ( SUR%SRC_TYP(J15) == SUR__TYP_TAG ) THEN
                            DIST = ARC_LEN_VEC ( SUR%SOU(SUR%IND_SRC(J15))%S_VEC, SUR%SOU(J11)%S_VEC )
                          ELSE IF ( SUR%SRC_TYP(J15) == SUR__TYP_SEC ) THEN
                            DIST = ARC_LEN_VEC ( SUR%SO2(SUR%IND_SRC(J15))%S_VEC, SUR%SOU(J11)%S_VEC )
                          ELSE IF ( SUR%SRC_TYP(J15) == SUR__TYP_CAL ) THEN
                            DIST = ARC_LEN_VEC ( SUR%CAL(SUR%IND_SRC(J15))%S_VEC, SUR%SOU(J11)%S_VEC )
                       END IF
                       IF ( DIST < DIST_MIN ) THEN
                            DIST_MIN = DIST
                       END IF
 4150               CONTINUE 
                    SCORE(J11) = SCORE(J11)/10.0D0
                    IF ( SUR%ALGORITHM == 'GEODETIC_01' ) THEN
                         ARC_MAX = 0.66D0 ! 37.7 deg
                       ELSE IF ( SUR%ALGORITHM == 'GEODETIC_02' ) THEN
                         ARC_MAX = 0.35D0 ! 20.0  deg
                       ELSE IF ( SUR%ALGORITHM == 'GEODETIC_03' ) THEN
                         ARC_MAX = 1.05D0 ! 60.0  deg
                       ELSE IF ( SUR%ALGORITHM == 'GNSS_01' ) THEN
                         ARC_MAX = 0.52D0 ! 30.0  deg
                       ELSE IF ( SUR%ALGORITHM == 'GNSS_02' ) THEN
                         ARC_MAX = 0.26D0 ! 10.0  deg
                    END IF
                    IF ( FL_GEO_TEST ) THEN
                         ARC_MAX = 10.0D0
                         IF ( DIST_MIN < 2.0 ) SCORE(J11) = 1.D-4*SCORE(J11)
                         IF ( DIST_MIN > 2.5 ) SCORE(J11) = 1.D4*SCORE(J11)
                         IF ( DIST_MIN > 3.0 ) SCORE(J11) = 1.D8*SCORE(J11)
                    END IF
                    IF ( DIST_MIN < ARC_MAX ) THEN
                         SCORE(J11) = SCORE(J11)*((DIST_MIN + 0.01D0)/ARC_MAX)**4
                    END IF
                  ELSE IF ( ( SUR%ALGORITHM == 'ASTROMET_03' .OR. &
     &                        SUR%ALGORITHM == 'ASTROMET_05' .OR. &
     &                        SUR%ALGORITHM == 'ASTROMET_07'      ) .AND. &
     &                      CUR_TYP == SUR__TYP_SEC                       ) THEN 
                    IF ( SUR%NOBS_SO2(J11) > 0 ) THEN
                         IND_SC2  = SUR%IND_SCN_SO2(SUR%NOBS_SO2(J11),J11)
                         TIM_LAS2 = (MJD_OBS - SUR%MJD_OBS_BEG(IND_SC2))*86400.0D0 + &
     &                              (TAI_OBS - SUR%TAI_OBS_BEG(IND_SC2))
                         IF ( TIM_LAS2 < 0.5D0*GAP_MIN ) THEN
                              SCORE(J11) = 0.0D0
                            ELSE IF ( TIM_LAS2 < GAP_MIN ) THEN
                              SCORE(J11) = SCORE(J11)/16.0D0
                            ELSE 
                              SCORE(J11) = 16.0D0*SCORE(J11) 
                         END IF
                         IF ( SUR%NOBS_SO2(J11) .GE. GAP_NOR ) THEN
                              SCORE(J11) = 0.0D0
                         END IF
                       ELSE 
                         SCORE(J11) = 500.0D0/(SLEW_TIME_MAX + MAX(0.0D0, SUR%PREOBS_LONG))
                    END IF
                  ELSE IF ( SUR%ALGORITHM == 'SPACECRAFT_01'      ) THEN
                    IF ( J11 == IND_SOU_SPC ) THEN
                         SCORE(J11) = 1000.0D0
                       ELSE 
                         SCORE(J11) =    0.0D0
                    END IF
               END IF ! End of the cycle over the algorithms
               IF ( SUR%NOBS_SRC(J11) .LE. SUR%SCAN_PER_SOURCE_NORM  .AND. &
     &              CUR_TYP == SUR__TYP_TAG                               ) THEN
                    SCORE(J11) = SCORE(J11)*SUR%SOU(J11)%PRI
                    PRI = SUR%SOU(J11)%PRI
                 ELSE IF ( SUR%NOBS_SRC(J11) .LE. SUR%SCAN_PER_SOURCE_NORM  .AND. &
     &              CUR_TYP == SUR__TYP_SEC                                      ) THEN
                    SCORE(J11) = SCORE(J11)*SUR%SO2(J11)%PRI
                    PRI = SUR%SO2(J11)%PRI
               END IF
               IF ( IVRB .GE. 6  .AND.  CUR_TYP == SUR__TYP_TAG ) THEN
                    WRITE ( 6, 212 ) SUR%SOU(J11)%J2000_NAME, SCORE(J11), &
     &                               TIM_LAST/3600.0D0, FUT_FACT, ONS_FACT, PRI, &
     &                               SUR%NOBS_SRC(J11), J11, SUR__TYP_STR(SUR__TYP_TAG), &
     &                               SUR%SOU(J11)%EL_MIN/DEG__TO__RAD
 212                FORMAT ( 8X, ' Source: ',A, ' Score: ', 1PD11.4, &
     &                           ' Tim_last: ', 0PF6.2, ' Fut: ', 0PF5.2, &
     &                           ' Ons: ', 0PF5.2, ' PRI: ', 0PF9.1, ' # ', I1, &
     &                           ' J11= ',I5, 1X, A, 1X,' Sou_el_min: ', F5.1, ' deg'  )
                  ELSE IF ( IVRB .GE. 6  .AND.  CUR_TYP == SUR__TYP_SEC ) THEN
                    WRITE ( 6, 212 ) SUR%SO2(J11)%J2000_NAME, SCORE(J11), &
     &                               TIM_LAST/3600.0D0, FUT_FACT, ONS_FACT, PRI, &
     &                               SUR%NOBS_SO2(J11), J11, SUR__TYP_STR(SUR__TYP_SEC), &
     &                               SUR%SO2(J11)%EL_MIN/DEG__TO__RAD
               END IF
 4110       CONTINUE
!
! --------- Get the source index in SRC%SOU
!
            IND_SRC = MAX_LIST_R8 ( L_SOU, SCORE )
            IF ( IVRB .GE. 5 .AND.  CUR_TYP == SUR__TYP_TAG ) THEN 
                 WRITE ( 6, 216 ) SUR%L_SCN+1, NUM_OBS_SOU(1:4), &
     &                            SUR%SOU(IND_SRC)%J2000_NAME, &
     &                            SUR%NOBS_SRC(IND_SRC)+1, &
     &                            (MJD_OBS - SUR%MJD_START)*24.0 + &
     &                            (TAI_OBS - SUR%TAI_START)/3600.0, &
     &                            SCORE(IND_SRC), SUR__TYP_STR(CUR_TYP)
 216             FORMAT ( 'Scan: ', I4,' Nobs: ', 4(I3,1X), '  ', A, &
     &                    ' #Obs: ',I1,'  im: ', F5.2, ' Sc= ', 1PD11.4, ' Typ: ', A )
              ELSE IF ( IVRB .GE. 5 .AND.  CUR_TYP == SUR__TYP_SEC ) THEN 
                 WRITE ( 6, 216 ) SUR%L_SCN+1, NUM_OBS_SOU(1:4), &
     &                            SUR%SO2(IND_SRC)%J2000_NAME, &
     &                            SUR%NOBS_SO2(IND_SRC)+1, &
     &                            (MJD_OBS - SUR%MJD_START)*24.0 + &
     &                            (TAI_OBS - SUR%TAI_START)/3600.0, &
     &                            SCORE(IND_SRC), SUR__TYP_STR(CUR_TYP)
                 WRITE ( 6, * ) 'Sou: ', SUR%SO2(IND_SRC)%J2000_NAME, ' nobs_so2: ', int2(sur%nobs_so2(ind_src)), ' nsca_max: ', int2(sur%so2(ind_src)%nsca_max)
            END IF
            IF ( SCORE(IND_SRC) > 0.0D0 ) GOTO 870
!
! --------- No sources was found. Bad! 
!
            IF ( SUR%ALGORITHM == 'SPACECRAFT_01'      ) THEN
                 STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,  SUR%TAI_CUR, -2 )
                 WRITE ( 6, '(A)' ) ' '
                 WRITE ( 6, '(A)' ) '  Premature end of SUR_ASTRO_SEQ at '// &
      &                                STR1(1:19)//' because the spacecraft '// &
     &                              'is below the horizon' 
                 IER = 860
                 SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, CUR_TYP, &
     &                                           1, SUR%IND_SRC(SUR%L_SCN-1), &
     &                                           SUR%SRC_TYP(SUR%L_SCN-1), -2, &
     &                                           SUR__FINE, IER )
                 WRITE ( 6, * ) 'SLEW_TIME_MAX= ', SLEW_TIME_MAX
                 CALL ERR_LOG ( 1724, IUER, 'SUR_ASTRO_SEQ', 'Failure in schedule '// &
     &                          'preparation' )
                 RETURN
            END IF
            IF ( CUR_TYP == SUR__TYP_SEC ) THEN
! 
! -------------- Skip SUR%AVR_SLEW_TIME and try once again primary sources
!
                 IF ( IVRB .GE. 5 ) THEN
                      WRITE ( 6, * ) 'Switch to the next list of sources J7= ', INT2(J7), &
     &                               ' after scan ', INT2(SUR%L_SCN)
                 END IF
                 IF ( J7 > 2 ) THEN
                      SUR%TAI_CUR = SUR%TAI_CUR + SUR%AVR_SLEW_TIME
                      IF ( SUR%TAI_CUR  > 86400.0D0 ) THEN
                           SUR%TAI_CUR = SUR%TAI_CUR - 86400.D0
                           SUR%MJD_CUR = SUR%MJD_CUR + 1
                      END IF
                      IF ( IVRB .GE. 5 ) THEN
                           WRITE ( 6, * ) 'Skip ', SUR%AVR_SLEW_TIME, ' sec'
                      END IF
                 END IF
               ELSE
!
! -------------- Switch to the list of secondary sources and try again
!
                 IF ( IVRB .GE. 5 ) THEN
                      WRITE ( 6, * ) 'Switch to the list of secondary sources J7= ', INT2(J7), &
     &                               ' after scan ', INT2(SUR%L_SCN)
                 END IF
            END IF
!
            IF ( (SUR%MJD_CUR -  SUR%MJD_STOP)*86400.D0 + &
     &           (SUR%TAI_CUR - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > 0.0D0 ) THEN
                 IF ( IVRB .GE. 6 ) THEN
                      WRITE ( 6, * ) 'Last scan start: ', &
     &                                MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), -3 ), &
     &                               ' stop: ', &
     &                                MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), -3 ), &
     &                               ' -- too late, so we remove it'
                 END IF
!
! -------------- End of the session
!
                 IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                      SUR%L_OBS_TAG = SUR%L_OBS_TAG - 1
                    ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                      SUR%L_OBS_SEC = SUR%L_OBS_SEC - 1
                 END IF
                 IF ( SUR%L_SCN > 1 ) SUR%L_SCN = SUR%L_SCN - 1
                 SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
                 IF ( IVRB .GE. 6 ) WRITE ( 6, * ) 'JUMP-2 to end scheduling'
                 GOTO 810
            END IF
 470     CONTINUE
 870     CONTINUE
!
         IF ( SCORE(IND_SRC) == 0.D0 ) THEN
              WRITE ( 6, * ) ' SUR%L_SCN     = ', SUR%L_SCN
              WRITE ( 6, * ) ' SUR%MJD_CUR   = ', SUR%MJD_CUR, &
     &                       ' SUR%TAI_CUR   = ', SUR%TAI_CUR
              WRITE ( 6, * ) ' SUR%MJD_OB1   = ', SUR%MJD_OBS_BEG(SUR%L_SCN), &
     &                       ' SUR%TAI_OB1   = ', SUR%TAI_OBS_BEG(SUR%L_SCN)
              WRITE ( 6, * ) ' SUR%MJD_OB2   = ', SUR%MJD_OBS_END(SUR%L_SCN), &
     &                       ' SUR%TAI_OB2   = ', SUR%TAI_OBS_END(SUR%L_SCN)
!
              WRITE ( 6, * ) ' SUR%MJD_START = ', SUR%MJD_START, &
     &                       ' SUR%TAI_START = ', SUR%TAI_START
!
              WRITE ( 6, * ) ' SUR%MJD_STOP  = ', SUR%MJD_STOP, &
     &                       ' SUR%TAI_STOP  = ', SUR%TAI_STOP
!
              WRITE ( 6, * ) ' TIM = ', (SUR%MJD_CUR - SUR%MJD_START)*24.0D0 + &
     &                                  (SUR%TAI_CUR - SUR%TAI_START)/3600.0D0, &
     &                                ' hours '
              IF ( SUR%ALGORITHM == 'SPACECRAFT_01'      ) THEN
                   SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
                   IF ( IVRB > 0 ) THEN
                        STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,  SUR%TAI_CUR, -2 )
                        WRITE ( 6, '(A)' ) ' '
                        WRITE ( 6, '(A)' ) '  Premature end of SUR_ASTRO_SEQ at '// &
     &                                     STR1(1:19)//' because the spacecraft '// &
     &                                     'is below the horizon' 
                    END IF
                    CALL ERR_LOG ( 0, IUER ) 
                 ELSE
                   CALL ERR_LOG ( 1722, IUER, 'SUR_ASTRO_SEQ', 'Trap of '// &
     &                 'internal control: no sources were found' )
                   RETURN
              END IF
         END IF
         SLEW_TIME_MAX = SLEW_SRC(IND_SRC)
!
         IF ( IVRB == 6 ) THEN
              IER = -6
              SUR%L_SCN = SUR%L_SCN + 1
              SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, CUR_TYP, &
     &                                        IND_SRC, SUR%IND_SRC(SUR%L_SCN-1), &
     &                                        SUR%SRC_TYP(SUR%L_SCN-1), -2, &
     &                                        SUR__FINE, IER )
              SUR%L_SCN = SUR%L_SCN - 1
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1729, IUER, 'SUR_ASTRO_SEQ', 'Trap '// &
     &                       'of internal control: error in SUR_SLEW_TIME ' )
                   RETURN
              END IF
         END IF
!
         IF ( ( (MJD_OBS - SUR%MJD_CUR)*86400.D0 + &
     &               (TAI_OBS - SUR%TAI_CUR) ) < SLEW_TIME_MAX ) THEN
              MJD_OBS = SUR%MJD_CUR
              TAI_OBS = SUR%TAI_CUR + SLEW_TIME_MAX
              IF ( TAI_OBS > 86400.0D0 ) THEN
                   TAI_OBS = TAI_OBS - 86400.D0
                   MJD_OBS = MJD_OBS + 1
              END IF
         END IF
         FL_TAPE_CHANGE = .FALSE.
!
! ------ Check: is it time to change the tape?
!
         IF ( (MJD_OBS - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &        (TAI_OBS - SUR%TAI_TAPE_START_CUR) + MAX(0.0D0, SUR%PREOBS_LONG) + &
     &         SUR%SOU(IND_SRC)%DUR > SUR%TAPE_LENGTH ) THEN
!
! ----------- Yes, it is just time. Reset the current time
!
              TIM_DIF = (MJD_OBS - SUR%MJD_CUR)*86400.0D0 + &
     &                  (TAI_OBS - SUR%TAI_CUR)
              IF ( SUR%TAPE_CHANGE_TIME > 0.0D0 .AND. &
     &             TIM_DIF < SUR%TAPE_CHANGE_TIME ) THEN
!
                   SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) + SUR%TAPE_CHANGE_TIME
                   SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
                 ELSE
                   SUR%TAI_CUR = TAI_OBS
                   SUR%MJD_CUR = MJD_OBS
              END IF
              IF ( SUR%TAPE_CHANGE_TIME < TAPE_CHANGE_MIN_TIME ) THEN
!
! ---------------- That means we have a "faked" tape change
!
                   TAI_OBS = TAI_OBS + SUR%TAPE_CHANGE_TIME
              END IF
!
              IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                   SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0
                   SUR%MJD_CUR = SUR%MJD_CUR + 1
              END IF
!
! ----------- Increment the tape counter
!
              SUR%L_TAP = SUR%L_TAP + 1
!
! ----------- Set the tape start date
!
              SUR%MJD_TAPE_START_CUR = SUR%MJD_CUR
              SUR%TAI_TAPE_START_CUR = SUR%TAI_CUR
              IF ( SUR%TAPE_CHANGE_TIME > 0.0D0 ) THEN
                   FL_TAPE_CHANGE = .TRUE.
              END IF
              MJD_OBS = SUR%MJD_CUR 
              TAI_OBS = SUR%TAI_CUR 
         END IF
!
! ------ Adjustment for margins, Tsys measurements etc.
!
         IF ( SUR%PREOBS_SHORT > 0.0 .AND. SUR%SKIP_PREOBS_LONG > 0 ) THEN
              SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__SHORT
            ELSE
              SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
         END IF
         IF ( FL_TAPE_CHANGE ) SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__TAPE
!
         DO 4160 J16=1,SUR%L_STA
            IF ( SUR%STA(J16)%TAGALONE ) GOTO 4160
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, &
     &                      TAI_OBS+MAX(0.0D0, SUR%PREOBS_LONG), J16, IND_SRC, &
     &                      AZ_STA(J16), EL_STA(J16), HA_STA(J16), IER )
 4160    CONTINUE
!
         FL_LONG = .FALSE.
         IF ( SUR%SKIP_PREOBS_LONG > 0 ) THEN
              IF ( MOD((SUR%L_SCN+1),SUR%SKIP_PREOBS_LONG) == 0 ) THEN
                   SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
                 ELSE
                   DO 4170 J17=SUR%L_SCN,MAX(1,SUR%L_SCN-SUR%SKIP_PREOBS_LONG+1),-1
                      DO 4180 J18=1,SUR%L_STA
                         IF ( SUR%STA(J18)%TAGALONE ) GOTO 4180
                         IF ( SUR%SCAN_TYPE(J17) == SUR__LONG  .OR.  &
     &                        SUR%SCAN_TYPE(J17) == SUR__TAPE        ) THEN
                              FL_LONG = .TRUE.
                              GOTO 8110
                           ELSE IF ( SUR%SCAN_TYPE(J17) == SUR__SHORT   .AND. &
     &                        DABS(SUR%EL_OBS(J18,J17) - EL_STA(J18)) >       &
     &                             SUR%EL_CHANGE_TSYS                     ) THEN
                              SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
                              GOTO 8110
                          END IF
 4180                 CONTINUE
 4170              CONTINUE
 8110              CONTINUE
              END IF
         END IF
!
         IF ( FL_TAPE_CHANGE ) THEN
              CONTINUE 
            ELSE IF ( FL_LONG ) THEN
              TAI_OBS = TAI_OBS + MAX(0.0D0, SUR%PREOBS_LONG)
            ELSE 
              TAI_OBS = TAI_OBS + SUR%PREOBS_SHORT
         END IF
         UTC_OBS_INT = IDNINT ( TAI_OBS + SUR%UTC_M_TAI + 0.5000001D0 )
         IF ( MOD(UTC_OBS_INT, IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
              TAI_OBS = (UTC_OBS_INT/ IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                  - SUR%UTC_M_TAI
            ELSE
              TAI_OBS = UTC_OBS_INT - SUR%UTC_M_TAI
         END IF
         IF ( TAI_OBS > 86400.0D0 ) THEN
              TAI_OBS = TAI_OBS - 86400.0D0
              MJD_OBS = MJD_OBS + 1
         END IF
         SUR%MJD_CUR = MJD_OBS
         SUR%TAI_CUR = TAI_OBS
!
! ------ Compute final azimuth and elevation of the observation, since
! ------ now we know the time when it will happen
!
         DO 4190 J19=1,SUR%L_STA
            SUR%OBS_STA(J19,SUR%L_SCN+1) = 0
            IF ( SUR%STA(J19)%TAGALONE ) GOTO 4190
            SPL_STATUS = SUR%STATUS_SPL(CUR_TYP)
            SUR%STATUS_SPL(CUR_TYP) = 0
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, TAI_OBS, J19, &
     &                      IND_SRC, AZ, EL, HA, IER )
            SUR%STATUS_SPL(CUR_TYP) = SPL_STATUS 
            IF ( .NOT. SUR_CHECK_VIS ( SUR, J19, CUR_TYP, IND_SRC, AZ, EL, &
     &                                 HA, IER ) ) THEN
                 GOTO 4190          
            END IF
            DIF_EL = EL - SUR%STA(J19)%EL_CUR 
            IF ( SUR%STA(J19)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------------- Update accumulated azimuth in order to take into account
! -------------- the cable wrap
!
                 DIF_AZ = (AZ - SUR%STA(J19)%AZ_CUR)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J19)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J19)%AZ_ACC_MAX ) THEN
                           SUR%STA(J19)%AZ_ACC_CUR = SUR%STA(J19)%AZ_ACC_CUR + DIF_AZ
                           SUR%STA(J19)%HA_ACC_CUR = HA
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J19)%AZ_ACC_CUR = SUR%STA(J19)%AZ_ACC_CUR - &
     &                                               (PI2 - DIF_AZ)
                           DIF_AZ = (PI2 - DIF_AZ)
                           SUR%STA(J19)%HA_ACC_CUR = HA
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J19)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J19)%AZ_ACC_MIN ) THEN
                           SUR%STA(J19)%AZ_ACC_CUR = SUR%STA(J19)%AZ_ACC_CUR + DIF_AZ
                           SUR%STA(J19)%HA_ACC_CUR = HA
                        ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ clock-wise
!
                           SUR%STA(J19)%AZ_ACC_CUR = SUR%STA(J19)%AZ_ACC_CUR + &
     &                                              (PI2 + DIF_AZ)
                           DIF_AZ = (PI2 + DIF_AZ)
                           SUR%STA(J19)%HA_ACC_CUR = HA
                      END IF
                 END IF
!
                 IF ( EL < SUR%STA(J19)%EL_MIN .OR. &
     &                EL > SUR%STA(J19)%EL_MAX      ) THEN
!
!@                       WRITE ( 6, * ) ' Trap of internal control: station '// &
!@     &                                SUR%STA(J19)%NAME//' Epoch: '// &
!@     &                                MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -3 )// &
!@     &                                ' Elevation angle: ', EL/DEG__TO__RAD
!@                      CALL ERR_LOG ( 1734, IUER, 'SUR_ASTRO_SEQ', 'Trap of '// &
!@     &                    'internal control' )
!@                      RETURN
!!
                      SUR%EL_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%EL_CUR
                      SUR%AZ_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%AZ_CUR
                      SUR%HA_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%HA_CUR
                      SUR%AZ_ACC_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%HA_ACC_CUR
                      SUR%STA(J19)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                      SUR%STA(J19)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
!
                      SUR%SLEW_DUR(J19,SUR%L_SCN+1) = 0.0D0
                      GOTO 4190
                 END IF
!
! -------------- Compute the slew time
!
                 IF ( DABS(DIF_AZ) > SUR%STA(J19)%SLEW_RATE_AZ**2/SUR%STA(J19)%SLEW_ACCL_AZ ) THEN
                      SLEW_AZ = (DABS(DIF_AZ) - SUR%STA(J19)%SLEW_RATE_AZ**2/SUR%STA(J19)%SLEW_ACCL_AZ)/SUR%STA(J19)%SLEW_RATE_AZ + &
     &                          2.0D0*SUR%STA(J19)%SLEW_RATE_AZ/SUR%STA(J19)%SLEW_ACCL_AZ
                    ELSE
                      SLEW_AZ = 2.D0*DSQRT(DABS(DIF_AZ)/SUR%STA(J19)%SLEW_ACCL_AZ)
                 END IF
                 IF ( DABS(DIF_EL) > SUR%STA(J19)%SLEW_RATE_EL**2/SUR%STA(J19)%SLEW_ACCL_EL ) THEN
                      SLEW_EL = DABS(DIF_EL)/SUR%STA(J19)%SLEW_RATE_EL + &
     &                          SUR%STA(J19)%SLEW_RATE_EL/SUR%STA(J19)%SLEW_ACCL_EL
                    ELSE
                      SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J19)%SLEW_ACCL_EL)
                 END IF
                 SUR%SLEW_DUR(J19,SUR%L_SCN+1) = MAX(SLEW_AZ+SUR%STA(J19)%TIME_SETTLE_AZ,   &
     &                                               SLEW_EL+SUR%STA(J19)%TIME_SETTLE_EL) + &
     &                                           SUR%STA(J19)%POSTOB
                 SUR%HA_ACC_OBS(J19,SUR%L_SCN+1) = HA
               ELSE IF ( SUR%STA(J19)%MOUNT_TYPE == MT__EQUAT ) THEN
                 IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                      ALP = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%DELTA
                   ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                      ALP = SUR%SO2(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%SO2(SUR%IND_SRC(SUR%L_SCN))%DELTA
                   ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                      ALP = SUR%CAL(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%CAL(SUR%IND_SRC(SUR%L_SCN))%DELTA
                   ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_POC ) THEN
                      ALP = SUR%SOP(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%SOP(SUR%IND_SRC(SUR%L_SCN))%DELTA
                   ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_PLA ) THEN
                      ALP = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%DELTA
                 END IF
                 DIF_DEL = DEL - SUR%STA(J19)%DEL_CUR
                 DIF_HA = (HA - SUR%STA(J19)%HA_CUR)
                 IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      SUR%STA(J19)%HA_ACC_CUR = SUR%STA(J19)%HA_ACC_CUR + DIF_HA
                     ELSE
!
! -------------------- The shortest move is counter-clock-wise
!
                       IF ( SUR%STA(J19)%HA_ACC_CUR + DIF_HA > SUR%STA(J19)%AZ_ACC_MIN ) THEN
                            SUR%STA(J19)%HA_ACC_CUR = SUR%STA(J19)%HA_ACC_CUR + DIF_HA
                         ELSE
!
! ------------------------- The shortest way is not possible, move the longest way
! ------------------------- clock-wise
!
                           SUR%STA(J19)%HA_ACC_CUR = SUR%STA(J19)%HA_ACC_CUR + &
     &                                               (PI2 + DIF_HA)
                      END IF
                 END IF
!
! -------------- Compute the slew time
!
                 IF ( DABS(DIF_HA) > SUR%STA(J19)%SLEW_RATE_AZ**2/SUR%STA(J19)%SLEW_ACCL_AZ ) THEN
                      SLEW_HA = (DABS(DIF_HA) - SUR%STA(J19)%SLEW_RATE_AZ**2/SUR%STA(J19)%SLEW_ACCL_AZ)/SUR%STA(J19)%SLEW_RATE_AZ + &
     &                          2.0D0*SUR%STA(J19)%SLEW_RATE_AZ/SUR%STA(J19)%SLEW_ACCL_AZ
                    ELSE
                      SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J19)%SLEW_ACCL_AZ)
                 END IF
                 IF ( DABS(DIF_DEL) > SUR%STA(J19)%SLEW_RATE_EL**2/SUR%STA(J19)%SLEW_ACCL_EL ) THEN
                      SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J19)%SLEW_RATE_EL + &
     &                          SUR%STA(J19)%SLEW_RATE_EL/SUR%STA(J19)%SLEW_ACCL_EL
                    ELSE
                      SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J19)%SLEW_ACCL_EL)
                 END IF
                 SUR%SLEW_DUR(J19,SUR%L_SCN+1) = MAX(SLEW_HA+SUR%STA(J19)%TIME_SETTLE_AZ, &
     &                                               SLEW_DEL+SUR%STA(J19)%TIME_SETTLE_EL) + &
     &                                           SUR%STA(J19)%POSTOB
                 SUR%AZ_ACC_OBS(J19,SUR%L_SCN+1) = AZ
                 SUR%HA_ACC_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%HA_ACC_CUR
               ELSE IF ( SUR%STA(J19)%MOUNT_TYPE == MT__XY_E ) THEN
!
! -------------- XY-E mounting
!
                 IF ( DABS(DTAN(SUR%STA(J19)%EL_CUR)) < 1.D-6 ) THEN
                      A_LAST = P2I
                    ELSE
                      A_LAST = DATAN ( DCOS(SUR%STA(J19)%AZ_CUR)/DTAN(SUR%STA(J19)%EL_CUR) )
                      B_LAST = DASIN ( DSIN(SUR%STA(J19)%AZ_CUR)*DCOS(SUR%STA(J19)%EL_CUR) )
                 END IF
!
                 IF ( DABS(DTAN(EL)) < 1.D-6 ) THEN
                      A = P2I
                    ELSE
                      A = DATAN ( DCOS(AZ)/DTAN(EL) )
                      B = DASIN ( DSIN(AZ)*DCOS(EL) )
                 END IF
                 IF ( B < SUR%STA(J19)%EL_MIN .OR. &
     &                B > SUR%STA(J19)%EL_MAX      ) THEN
!
                      SUR%EL_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%EL_CUR
                      SUR%AZ_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%AZ_CUR
                      SUR%HA_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%HA_CUR
                      SUR%AZ_ACC_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J19,SUR%L_SCN+1) = SUR%STA(J19)%HA_ACC_CUR
                      SUR%STA(J19)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                      SUR%STA(J19)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
!
                      SUR%SLEW_DUR(J19,SUR%L_SCN+1) = 0.0D0
                      GOTO 4190
                 END IF
!
                 DIF_A = A - A_LAST
                 DIF_B = B - B_LAST
!
                 SLEW_A = DABS(DIF_A)/SUR%STA(J19)%SLEW_RATE_AZ + &
     &                     SUR%STA(J19)%SLEW_RATE_AZ/SUR%STA(J19)%SLEW_ACCL_AZ
                 SLEW_B = DABS(DIF_B)/SUR%STA(J19)%SLEW_RATE_EL + &
     &                     SUR%STA(J19)%SLEW_RATE_EL/SUR%STA(J19)%SLEW_ACCL_EL
                 SUR%SLEW_DUR(J19,SUR%L_SCN+1) = MAX(SLEW_A+SUR%STA(J19)%TIME_SETTLE_AZ,   &
     &                                               SLEW_B+SUR%STA(J19)%TIME_SETTLE_EL) + &
     &                                               SUR%STA(J19)%POSTOB
!
                 SUR%AZ_ACC_OBS(J19,SUR%L_SCN+1) = AZ
                 SUR%HA_ACC_OBS(J19,SUR%L_SCN+1) = HA
            END IF
            SUR%OBS_STA(J19,SUR%L_SCN+1) = SUR__USED
            SUR%SLEW_DUR(J19,SUR%L_SCN+1) = MAX(SLEW_AZ+SUR%STA(J19)%TIME_SETTLE_AZ,   &
     &                                          SLEW_EL+SUR%STA(J19)%TIME_SETTLE_EL) + &
     &                                      SUR%STA(J19)%POSTOB
 4190     CONTINUE
!
! ------ Update scan counter
!
         SUR%L_SCN = SUR%L_SCN + 1
         SUR%SLEW_SCA(SUR%L_SCN) = SLEW_TIME_MAX
         IF ( IVRB .GE. 5 ) THEN
              TC = CPU_TIMER  ( %VAL(2) ) 
              TW = WALL_TIMER ( %VAL(2) ) 
              WRITE ( 6, 210 ) TC, TW, SUR%L_SCN
 210          FORMAT ( '  SUR_ASTRO_SEQ    CPU_Time: ', F9.2, ' WALL_Time: ',F9.2, ' SUR%L_SCN= ', I4 )
              CALL FLUSH ( 6 ) 
         END IF
!
         IF ( SUR%L_SCN > SUR__M_SCN - 2) THEN
              CALL ERR_LOG ( 1734, IUER, 'SUR_ASTRO_SEQ', 'Trap of internal control: '// &
     &            'too many scans. Please increase parameter SUR__M_SCN' )
              RETURN
         END IF
         IF ( CUR_TYP == SUR__TYP_TAG ) THEN
              SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) + 1
              SUR%SOU(IND_SRC)%NOBS = SUR%SOU(IND_SRC)%NOBS + 1
              SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SRC),IND_SRC) = SUR%L_SCN
            ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
              SUR%L_OBS_SEC = SUR%L_OBS_SEC + 1
              SUR%NOBS_SO2(IND_SRC) = SUR%NOBS_SO2(IND_SRC) + 1
              SUR%SO2(IND_SRC)%NOBS = SUR%SO2(IND_SRC)%NOBS + 1
              SUR%IND_SCN_SO2(SUR%NOBS_SO2(IND_SRC),IND_SRC) = SUR%L_SCN
         END IF
         SUR%IND_SRC(SUR%L_SCN) = IND_SRC
!
         SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
         SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
         SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
         SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS
         IF ( CUR_TYP == SUR__TYP_TAG ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + SUR%SOU(IND_SRC)%DUR
            ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + SUR%SO2(IND_SRC)%DUR
         END IF
!       
         IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
         END IF
!
         SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
         SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
         SUR%SRC_TYP(SUR%L_SCN) = CUR_TYP
         IF ( CUR_TYP == SUR__TYP_TAG ) THEN
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
              IF ( IVRB .GE. 6 ) THEN 
                   STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), IER )
                   STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), IER )
                   WRITE ( 6, 123 ) SUR%L_SCN, 'pri', SUR%SOU(IND_SRC)%J2000_NAME, SUR%SOU(IND_SRC)%DUR, &
     &                              STR(1:21), STR1(1:21), SUR%SLEW_SCA(SUR%L_SCN)
 123               FORMAT ( 'Scan: ', I4, 1X, A, 1X, ' Sou: ', A, ' Dur: ', F6.1, &
     &                      ' Obs_tai_beg: ', A, ' Obs_tai_end: ', A, ' Slew: ' , F6.1 )
              END IF
            ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
              IF ( IVRB .GE. 6 ) THEN 
                   STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), IER )
                   STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), IER )
                   WRITE ( 6, 123 ) SUR%L_SCN, 'sec', SUR%SOU(IND_SRC)%J2000_NAME, SUR%SOU(IND_SRC)%DUR, &
     &                              STR(1:21), STR1(1:21), SUR%SLEW_SCA(SUR%L_SCN)
              END IF
              SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
         END IF
         IF ( SUR%L_SCN == 1 ) THEN
              SUR%MJD_TAPE_START_CUR = SUR%MJD_CUR 
              SUR%TAI_TAPE_START_CUR = SUR%TAI_CUR 
         END IF
!         IF ( IVRB .GE. 6 ) THEN 
!              WRITE ( 6, * ) 'SAS-1238 Scan: ', INT2(SUR%L_SCN), ' Ind: ', INT2(SUR%IND_SRC(SUR%L_SCN)), &
!     &                       ' Typ: ', INT2(SUR%SRC_TYP(SUR%L_SCN)), ' Dur= ', SUR%SOU(IND_SRC)%DUR
!         END IF
!
         IF ( CUR_TYP == SUR__TYP_TAG ) THEN
              SUR_SOU_DUR = SUR%SOU(IND_SRC)%DUR
            ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
              SUR_SOU_DUR = SUR%SO2(IND_SRC)%DUR
            ELSE 
              SUR_SOU_DUR = SUR%SCAN_LEN
         END IF
!
         IF ( SUR%ALGORITHM == 'SPACECRAFT_01'      ) THEN
!
! ----------- We do not want to have a short scan when we observe an spacecraft
!
              SUR_SOU_DUR = 0.0D0
         END IF
         IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.D0 + &
     &        (SUR%TAI_OBS_END(SUR%L_SCN) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > &
     &         SUR_SOU_DUR/2.0D0 ) THEN
              IF ( IVRB .GE. 6 ) THEN
                   WRITE ( 6, * ) 'Last scan start: ', &
     &                             MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), -3 ), &
     &                            ' stop: ', &
     &                             MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), -3 ), &
     &                            ' -- too late, so we remove it'
              END IF
!
! ----------- End of the session
!
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) - 1
              IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                   SUR%L_OBS_TAG = SUR%L_OBS_TAG - 1
                   SUR%L_SCN_SO1 = SUR%L_SCN_SO1 - 1
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                   SUR%L_OBS_SEC = SUR%L_OBS_SEC - 1
                   SUR%L_SCN_SO2 = SUR%L_SCN_SO2 - 1
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                   SUR%L_OBS_CAL = SUR%L_OBS_CAL - 1
              END IF
              SUR%L_SCN = SUR%L_SCN - 1
              IF ( FL_LAST_SCA_EXT ) THEN
                   SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
                   SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP - SUR%POSTSES_INTERVAL
              END IF
              SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
              CALL SUR_SET_CUR ( SUR, VTD, IER  )
              IF ( IVRB .GE. 6 ) WRITE ( 6, * ) 'JUMP-3 to end scheduling'
              GOTO 810
           ELSE IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.D0 + &
     &               (SUR%TAI_OBS_END(SUR%L_SCN) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL) ) > 0.0D0 ) THEN
              IF ( FL_LAST_SCA_EXT ) THEN
                   SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
                   SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP - SUR%POSTSES_INTERVAL
              END IF
              SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
         END IF
!
! ------ Set antennas current states
!
         CALL SUR_SET_CUR ( SUR, VTD, IER  )
         IF ( IVRB .GE. 6 ) THEN
              WRITE ( 6, * ) ' '
              CALL SUR_SLEW_REPORT ( SUR )
              WRITE ( 6, '("Scan ", I4, " Slew_time_max:  ", F6.1)' ) SUR%L_SCN, SLEW_TIME_MAX
              WRITE ( 6, * ) ' '
         END IF
         IF ( IVRB .GE. 5 ) THEN
              STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), -2 )
              STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), -2 )
              WRITE ( 6, 180 ) SUR%L_SCN, STR(1:25), STR1(1:24), SUR__TYP_STR(CUR_TYP)
 180          FORMAT ( 'SCAN ', I4, ' Obs_beg: ', A, ' Obs_end: ', A, ' Tag_type: ', A )
         END IF 
         IF ( SUR%ALGORITHM == 'ASTROMET_05' ) THEN
              IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                   ARC = ARC_LEN_VEC ( SUR%SOU(IND_SRC)%S_VEC, &
     &                                 SUR%CAL(IND_SRC_CAL)%S_VEC )
                 ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                   ARC = ARC_LEN_VEC ( SUR%SO2(IND_SRC)%S_VEC, &
     &                                 SUR%CAL(IND_SRC_CAL)%S_VEC )
              END IF              
              IF ( ARC > SUR%EL_CHANGE_TSYS ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL SUR_ASTRO_CAL ( SUR, VTD, IVRB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1735, IUER, 'SUR_ASTRO_SEQ', 'Failure '// &
     &                    'in search for  phase calibrator' )
                        RETURN
                   END IF 
              END IF
         END IF
 410  CONTINUE
 810  CONTINUE
      SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) '  End of SUR_ASTRO_SEQ'
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SUR_ASTRO_SEQ  !#!#
