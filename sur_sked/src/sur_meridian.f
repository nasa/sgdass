      SUBROUTINE SUR_MERIDIAN ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_MERIDIAN
! *                                                                      *
! *  ### 26-MAY-2010  SUR_MERIDIAN  v1.7 (c)  L. Petrov 28-JAN-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IVRB, IUER
      REAL*8     SANG, TANG, AZ, ELEV, HA, DTIM_RISE, DTIM_SET, &
     &           DEC_WGT, TIM_WGT, SLE_WGT, CUL_WGT, GAP(SUR__M_SOU)
      CHARACTER  STR*32, STR1*32, STR2*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, IND_SCN, UTC_OBS_INT, &
     &           NS, IND_SRC, MJD_OBS, MJD_BEG, I_STA, K_STA, &
     &           MJD_STOP_SAVE, IS, SCA_STA_PREV, SPL_STATUS, IER
      REAL*8     D1, D2, SCORE(SUR__M_SOU), IND_R8(SUR__M_SOU), TAI_OBS, &
     &           TAI_BEG, SLEW_TIME_MAX, SLEW_TIME_SCN(SUR__M_SOU), &
     &           TROP_BURST_LEN, TIM_CUL, TIM_SCA, TIM_LAST, TIM_DIF, TIM_LO, &
     &           DIF_AZ, DIF_HA, TAI_STOP_SAVE, ELEV_REF, DEL, DIST_AZ, DIST_HA, &
     &           AZ_ACC_CUR, HA_ACC_CUR
      REAL*8     DIF_A, DIF_B, DIF_EL, DIF_DEL, A_LAST, B_LAST, A, B
      REAL*8     SLEW_EL, SLEW_AZ, SLEW_HA, SLEW_DEL, SLEW_A, SLEW_B
      LOGICAL*1  FL_TAPE_CHANGE, FL_LONG, FL_SLEW_PRINT, FL_LAST_SCA_EXT
      REAL*8     SHARE__MIN_DUR, SHARE__TROPO
      PARAMETER  ( SHARE__MIN_DUR = 0.8  )
      PARAMETER  ( SHARE__TROPO   = 0.95 )
      PARAMETER  ( D1 =  0.0D0*DEG__TO__RAD )
      PARAMETER  ( D2 = 30.0D0*DEG__TO__RAD )
      TYPE     ( SUR__TYPE ) :: SUR_SAVE
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, SUR_MERIDIAN_SEC
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      REAL*8   , EXTERNAL :: SUR_SLEW_TIME, SUR_GET_ELEV_REF, SUR_ASTRO_SCORE
!
      FL_SLEW_PRINT   = .TRUE.
      FL_LAST_SCA_EXT = .TRUE.
      CALL GETENVAR ( 'SUR_PROHIBIT_LAST_SCAN_EXTENSION', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_LAST_SCA_EXT = .TRUE.
           IF ( IVRB > 0 ) THEN
                WRITE ( 6, * ) 'SUR_MERIDIAN: prohibit last scan extension'
           END IF
      END IF
!
! --- Compute time of upper culmination
!
      DO 410 J1=1,SUR%L_SOU
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, SUR%MJD_CUR, &
     &                   SUR%TAI_CUR, SUR%REF_STA, J1, AZ, ELEV, HA, &
     &                   IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1741, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &            'compute hourly angle for source '// &
     &             SUR%SOU(J1)%J2000_NAME )
              RETURN
         END IF
         SUR%SOU(J1)%MJD_UP_CULM = SUR%MJD_CUR
         SUR%SOU(J1)%TAI_UP_CULM = SUR%TAI_CUR - HA*RAD__TO__SEC/1.002738D0
         IF ( SUR%SOU(J1)%TAI_UP_CULM  < 0.0D0 ) THEN
              SUR%SOU(J1)%MJD_UP_CULM = SUR%SOU(J1)%MJD_UP_CULM - 1
              SUR%SOU(J1)%TAI_UP_CULM = SUR%SOU(J1)%TAI_UP_CULM + 86400.0D0
         END IF
         IF ( SUR%SOU(J1)%TAI_UP_CULM > 86400.0D0 ) THEN
              SUR%SOU(J1)%MJD_UP_CULM = SUR%SOU(J1)%MJD_UP_CULM + 1
              SUR%SOU(J1)%TAI_UP_CULM = SUR%SOU(J1)%TAI_UP_CULM - 86400.0D0
         END IF
!
         IF ( IVRB .GE. 5 ) THEN
              STR = MJDSEC_TO_DATE ( SUR%SOU(J1)%MJD_UP_CULM, &
     &                               SUR%SOU(J1)%TAI_UP_CULM, IER )
              WRITE ( 6, 110 ) J1, SUR%SOU(J1)%J2000_NAME, STR(1:21), &
     &                         SUR%SOU(J1)%NOBS
 110          FORMAT ( I4,') Source: ',A, ' Upper culmination at ', A, &
     &                       ' Nobs: ', I4 )
         END IF
!
         SUR%NOBS_SRC(J1) = SUR%SOU(J1)%NOBS
 410  CONTINUE
!
      IF ( SUR%TROPO_RANGE == 12 ) THEN
           TROP_BURST_LEN = 3*SUR%AVR_SLEW_TROPO_TIME + 2*SUR%TROPO_SCAN_LEN
         ELSE 
           TROP_BURST_LEN = 5*SUR%AVR_SLEW_TROPO_TIME + 4*SUR%TROPO_SCAN_LEN
      END IF
!
! === Process primary sources
!
      MJD_OBS = SUR%MJD_CUR
      TAI_OBS = SUR%TAI_CUR
!
      DO 420 J2=1,SUR__M_SOU
         NS = 0
         IS = 0
!
! ------ Insert a block of troposphere calibrators if it is time to do it,
! ------ unless we are close to the end of the session
!
         IF ( SUR%ALGORITHM == 'IMAGING_01' .AND. J2 == 1 ) THEN
              CONTINUE 
            ELSE IF ( SUR%ALGORITHM == 'IMAGING_01'  .AND.  J2 > SUR%L_SOU ) THEN
              GOTO 420
            ELSE 
              IF ( (MJD_OBS - SUR%MJD_TROPO_CUR)*86400.0D0 + &
     &             (TAI_OBS - SUR%TAI_TROPO_CUR) + SUR%PREOBS_LONG + &
     &              SUR%SCAN_LEN > SUR%TROPO_BURST_INTERVAL    .AND. &
     &              SUR%TROPO_BURST_INTERVAL > 0.0D0           .AND. &
     &             (SUR%MJD_STOP - SUR%MJD_CUR)*86400.0D0 + &
     &             (SUR%TAI_STOP - SUR%TAI_CUR) > SHARE__TROPO*SUR%TROPO_BURST_INTERVAL ) THEN
!
                   IF ( IVRB .GE. 8 ) THEN
                        STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,       SUR%TAI_CUR,       -2 )
                        STR2 = MJDSEC_TO_DATE ( SUR%MJD_TROPO_CUR, SUR%TAI_TROPO_CUR, -2 )
                        WRITE ( 6, * ) 'SUR_MERIDIAN-1 Started troposphere burst, because '// &
     &                                 'time_cur= ', STR1(1:19)//' time_tropo_cur= ', STR2(1:19)// &
     &                                 ' scan_len= ', sngl(sur%scan_len), ' tbi= ', &
     &                                 SNGL(SUR%TROPO_BURST_INTERVAL), ' tci= ', &
     &                                 (MJD_OBS - SUR%MJD_TROPO_CUR)*86400.0D0 + &
     &                                 (TAI_OBS - SUR%TAI_TROPO_CUR) + SUR%PREOBS_LONG + &
     &                                 SUR%SCAN_LEN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 1742, IUER, 'SUR_MEIRDIAN', 'Error in '// &
     &                      'computing azimuth and elevation' )
                        RETURN
                   END IF
              END IF
         END IF
!
! ------ Check whether it is time to finish scheduling primary sources
! ------ and rush for the troposphere calibrators at the end of the experiment
!
         IF ( SUR%ALGORITHM == 'IMAGING_01' ) THEN
              IF ( (SUR%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &             (SUR%TAI_CUR - SUR%TAI_STOP) + &
     &              SUR%SCAN_LEN > 0  ) THEN
                    IF ( IVRB .GE. 8 ) THEN
                         STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,  SUR%TAI_CUR,       -2 )
                         STR2 = MJDSEC_TO_DATE ( SUR%MJD_STOP, SUR%TAI_STOP, -2 )
                         WRITE ( 6, * ) 'SUR_MEDIDIAN-2 Started troposphere burst, because '// &
     &                         'time_cur= '//STR1(1:19)//' time_stop= ', STR2(1:19), &
     &                         ' scan_len = ', SUR%SCAN_LEN 
                    END IF
                    GOTO 820
              END IF
            ELSE 
              IF ( (SUR%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &             (SUR%TAI_CUR - SUR%TAI_STOP) + &
     &              SUR%SCAN_LEN + TROP_BURST_LEN > 0  ) THEN
                    IF ( IVRB .GE. 8 ) THEN
                         STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,  SUR%TAI_CUR,       -2 )
                         STR2 = MJDSEC_TO_DATE ( SUR%MJD_STOP, SUR%TAI_STOP, -2 )
                         WRITE ( 6, * ) 'SUR_MEDIDIAN-3 Started troposphere burst, because '// &
     &                         'time_cur= '//STR1(1:19)//' time_stop= ', STR2(1:19), &
     &                         ' scan_len = ', SUR%SCAN_LEN 
                    END IF
                    GOTO 820
              END IF
         END IF
         FL_TAPE_CHANGE = .FALSE.
!
         SLEW_TIME_SCN = -1.D8
         DO 430 J3=1,SUR%L_SOU
            IF ( SUR%NOBS_SRC(J3) > 0 ) THEN
                 IND_SCN  = SUR%IND_SCN_SRC(SUR%NOBS_SRC(J3),J3)
                 IF ( IND_SCN > 0 ) THEN
                      TIM_LAST = (MJD_OBS - SUR%MJD_OBS_BEG(IND_SCN))*86400.0D0 + &
     &                           (TAI_OBS - SUR%TAI_OBS_BEG(IND_SCN))
                    ELSE
                      TIM_LAST = 10.0D0*86400.0D0
                 END IF
                 IF ( TIM_LAST < SUR%SOU(J3)%GAP_MIN ) THEN
                      GOTO 430
                 END IF 
            END IF
!
            K_STA = 0
            DO 440 J4=1,SUR%L_STA
               IF ( SUR%STA(J4)%TAGALONE ) GOTO 440
!
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, MJD_OBS, TAI_OBS, &
     &                         J4, J3, AZ, ELEV, HA, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1743, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &                   'computing azimuth and elevation' )
                    RETURN
               END IF
               IF ( IVRB .GE. 17 ) THEN
                    WRITE ( 6, * ) 'SM-165 Sou= ', SUR%SOU(J3)%J2000_NAME, ' Sta: ', SUR%STA(J4)%NAME, &
     &                             ' Elev= ', SNGL(ELEV/DEG__TO__RAD), ' Azim= ', SNGL(AZ/DEG__TO__RAD)
               END IF
!
               IF ( ELEV < SUR%SOU(J3)%EL_MIN ) THEN
                    IS = -1
                    GOTO 440
               END IF
!
               IF ( SUR%STA(J4)%MOUNT_TYPE == MT__ALTAZ ) THEN
                    IF ( ELEV < SUR%STA(J4)%EL_MIN .OR.  &
     &                   ELEV > SUR%STA(J4)%EL_MAX       ) THEN
                         IS = -2
                         GOTO 440
                    END IF
               END IF
               IF ( SUR%STA(J4)%MOUNT_TYPE == MT__ALTAZ ) THEN
                    IF ( DABS(SUR%STA(J4)%AZ_CUR) < 20.0D0 ) THEN
                         DIF_AZ = (AZ - SUR%STA(J4)%AZ_CUR)
                         DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                         IF ( DABS(DABS(DIF_AZ) - PI__NUM) < SUR%AZIM_180_MARGIN ) THEN
                              IS = -3
                              GOTO 440
                         END IF
                    END IF
               END IF
!
! ------------ Now, let us perform a more stringent check
!
               IER = -1
               IF ( .NOT. SUR_CHECK_VIS ( SUR, J4, SUR__TYP_TAG, J3, AZ, &
     &                                    ELEV, HA, IER ) ) THEN
!
! ----------------- Does not see? Go to the next station
!
                    IF ( IVRB .GE. 17 ) THEN
                         WRITE ( 6, * ) 'SM-219 Sou= ', SUR%SOU(J3)%J2000_NAME, ' Sta: ', SUR%STA(J4)%NAME, &
     &                             ' not visible' 
                    END IF
                    IS = -4
                    GOTO 440
               END IF
               K_STA = K_STA + 1
 440        CONTINUE
            IF ( K_STA < SUR%SOU(J3)%MIN_STA ) THEN
!
! -------------- Do not consider sources which have too few stations that
! -------------- see it
!
                 IF ( IVRB .GE. 17 ) THEN
                      WRITE ( 6, 270 ) SUR%SOU(J3)%J2000_NAME, K_STA 
 270                  FORMAT ( 'Discarded source ', A, ' because it ', &
                               'is visible at ', I2, ' stations only' )
                 END IF
                 GOTO 430
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            SLEW_TIME_SCN(J3) = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_TAG, &
     &                                 J3, SUR%IND_SRC(SUR%L_SCN), &
     &                                 SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                 SUR__FINE, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1744, IUER, 'SUR_MERIDIAN', 'Trap '// &
     &               'of internal control: error in SUR_SLEW_TIME ' )
                 RETURN
            END IF
            IF ( SLEW_TIME_SCN(J3) .LE. 0.0D0 ) THEN
                 IF ( IVRB .GE. 17 ) THEN
                      WRITE ( 6, 280 ) SUR%SOU(J3)%J2000_NAME,  SLEW_TIME_SCN(J3) 
 280                  FORMAT ( 'Discared source ', A, ' because ', &
                               'slewing time: ', F6.1 )
                 END IF
                 GOTO 430
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            ELEV_REF = SUR_GET_ELEV_REF ( SUR, VTD, SUR__TYP_TAG, J3, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1745, IUER, 'SUR_MERIDIAN', 'Trap '// &
     &               'of internal control: error in SUR_GET_ELEV_REF ' )
                 RETURN
            END IF
!
            NS = NS + 1
!
            SCORE(NS) = SUR_ASTRO_SCORE ( SUR, SLEW_TIME_SCN(J3), ELEV_REF, &
     &                                    SUR__TYP_TAG, J3 )
            SCORE(NS) = SCORE(NS)*SUR%SOU(J3)%PRI
            IF ( SUR%NOBS_SRC(J3) < SUR%SOU(J3)%NSCA_MAX ) THEN
                 IF ( SUR%SOU(J3)%NOBS > 0 .AND. SUR%SOU(J3)%NSCA_MAX > 1 ) THEN
!
! ------------------- Encourage reobserving old observations
!
                      SCORE(NS) = 1000.0D0*SCORE(NS)
                    ELSE IF ( SUR%NOBS_SRC(J3) > 0 ) THEN
!
! ------------------- Encourage the second scan
!
                      SCORE(NS) = 100.0D0*SCORE(NS)
                 END IF
               ELSE 
                 SCORE(NS) = 0.0D0
            END IF
            IF ( SUR%ALGORITHM == 'IMAGING_ZZ' ) THEN
                 IF ( IDNINT(SUR%SOU(J3)%PRI) == J3 ) THEN
                      SCORE(NS) = 1000.0
                    ELSE
                      SCORE(NS) = 0.0D0
                 END IF
            END IF
            IND_R8(NS) = J3 + 1.D-5
            IF ( IVRB .GE. 5 ) THEN
                 WRITE ( 6, 140 ) SUR%L_SCN, NS, SUR%SOU(J3)%J2000_NAME, SCORE(NS), &
     &                            K_STA, SUR%NOBS_SRC(J3), SLEW_TIME_SCN(J3), &
     &                            ELEV_REF/DEG__TO__RAD, &
     &                            SUR%ELEV_MAX(J3,1)/DEG__TO__RAD
 140             FORMAT ( 'Scan ', I4,' Variant: ', I3, ' Source: ', A, &
     &                    ' Score: ', F12.0, ' K_STA: ', I2, ' N_SRC: ', I2, &
     &                    ' Slew: ', F5.1, &
     &                    ' Elev_ref: ', F5.1, ' deg ', &
     &                    ' Elev_max: ', F5.1, ' deg. Check' )
            END IF
 430     CONTINUE
!
         IF ( NS > 0 ) THEN
            CALL SORT8 ( NS, SCORE, IND_R8 )
            CALL ERR_PASS ( IUER, IER )
            IF ( SCORE(NS) == 0.0 ) THEN
                 IF ( IVRB .GE. 5 ) THEN
                      WRITE ( 6, * ) 'Searching the secoundary source list'
                 END IF
                 CALL ERR_PASS ( IUER, IER )
                 IS = SUR_MERIDIAN_SEC ( SUR, VTD, IVRB, IER )
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 1746, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &                    'an attempt to insert a secondary target' )
                      RETURN
                 END IF
!
                 IF ( IS == 0 ) THEN
                      IF ( IVRB .GE. 4 ) THEN
                           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, IER )
                           WRITE ( 6, * ) 'Skip ', SNGL(SUR%SCAN_LEN), &
     &                                    ' at ', STR(1:19)
                      END IF
                      SUR%TAI_CUR = SUR%TAI_CUR + SUR%SCAN_LEN
                      SUR_SAVE = SUR  ! Save the SUR object in the case we have to walk out
                 END IF
                 MJD_OBS = SUR%MJD_CUR
                 TAI_OBS = SUR%TAI_CUR 
                 GOTO 420
            END IF
            IND_SRC = IND_R8(NS)
!
! --------- Check: is it time to change the tape?
!
            IF ( (MJD_OBS - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &           (TAI_OBS - SUR%TAI_TAPE_START_CUR) + SUR%PREOBS_LONG + &
     &            SUR%SOU(IND_SRC)%DUR > SUR%TAPE_LENGTH ) THEN
!
! -------------- Yes, it is just time. Reset the current time
!
                 TIM_DIF = (MJD_OBS - SUR%MJD_CUR)*86400.0D0 + &
     &                     (TAI_OBS - SUR%TAI_CUR)
                 IF ( SUR%TAPE_CHANGE_TIME > 0.0D0 .AND. &
     &                TIM_DIF < SUR%TAPE_CHANGE_TIME ) THEN
!
                      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) + SUR%TAPE_CHANGE_TIME
                      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
                    ELSE
                      SUR%TAI_CUR = TAI_OBS
                      SUR%MJD_CUR = MJD_OBS
                 END IF
!
                 IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                      SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0
                      SUR%MJD_CUR = SUR%MJD_CUR + 1
                 END IF
!
! -------------- Increment the tape counter
!
                 SUR%L_TAP = SUR%L_TAP + 1
!
! -------------- Set the tape start date
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
            IF ( IVRB .GE. 5 ) THEN
                 ELEV_REF = SUR_GET_ELEV_REF ( SUR, VTD, SUR__TYP_TAG, IND_SRC, IER )
                 STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                 WRITE ( 6, 160 ) SUR%L_SCN, SUR%SOU(IND_SRC)%J2000_NAME, SCORE(NS), &
     &                            STR(1:19), SLEW_TIME_SCN(IND_SRC), IND_SRC, &
     &                            ELEV_REF/DEG__TO__RAD, &
     &                            SUR%ELEV_MAX(IND_SRC,1)/DEG__TO__RAD
 160             FORMAT ( 'Scan ', I4, ' Source: ',A,' Score: ', F12.0, 2X, A, &
     &                    ' Slew: ', F6.1, ' Ind_src: ', I4, &
     &                    ' Elev_ref: ', F5.1, ' deg  Elev_max: ', F5.1, ' deg. Inserted' )
            END IF
!
            MJD_OBS = SUR%MJD_CUR
            TAI_OBS = SUR%TAI_CUR
!
            SUR_SAVE = SUR  ! Save the SUR object in the case we have to walk out
!
! --------- Update scan counter
!
            SUR%L_SCN = SUR%L_SCN + 1
            SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
            SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) + 1
            SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SRC),IND_SRC) = SUR%L_SCN
!
            SUR%IND_SRC(SUR%L_SCN) = IND_SRC
            SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
            SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
            SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
!
            IF ( FL_TAPE_CHANGE ) THEN
                 SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS + SLEW_TIME_SCN(IND_SRC)
                 SUR%SCAN_TYPE(SUR%L_SCN) = SUR__TAPE
               ELSE 
                 SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS + SLEW_TIME_SCN(IND_SRC) &
     &                                        + SUR%PREOBS_LONG
                 SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
            END IF
!
            UTC_OBS_INT = IDNINT ( SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
            IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
                 SUR%TAI_OBS_BEG(SUR%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                        - SUR%UTC_M_TAI
               ELSE
                 SUR%TAI_OBS_BEG(SUR%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
            END IF
            SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                   SUR%SOU(IND_SRC)%DUR
            IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                 SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
            END IF
            SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
            SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
            SUR%SRC_TYP(SUR%L_SCN) = SUR__TYP_TAG
            SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
!
! --------- Compute final azimuth and elevation of the observation, since
! --------- now we know the time when it will happen
!
            K_STA = 0
            DO 450 J5=1,SUR%L_STA
               SUR%OBS_STA(J5,SUR%L_SCN) = 0
               IF ( SUR%STA(J5)%TAGALONE ) GOTO 450
!
               IF ( SUR%L_SCN > 1 ) THEN
                    SCA_STA_PREV = 0
                    DO 460 J6=SUR%L_SCN-1,1,-1
                       IF ( SUR%OBS_STA(J5,J6) == SUR__USED ) THEN
                            SCA_STA_PREV = J6
                            GOTO 860
                       END IF
 460                CONTINUE 
 860                CONTINUE 
               END IF
               CALL ERR_PASS ( IUER, IER )
               SPL_STATUS = SUR%STATUS_SPL(SUR__TYP_TAG)
               SUR%STATUS_SPL(SUR__TYP_TAG) = 0
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, MJD_OBS, TAI_OBS, J5, &
     &                         IND_SRC, AZ, ELEV, HA, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1747, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &                   'computing azimuth and elevation' )
                    RETURN
               END IF
               SUR%STATUS_SPL(SUR__TYP_TAG) = SPL_STATUS 
               IF ( SUR%STA(J5)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ----------------- Update accumulative aziumth in order to take into account
! ----------------- cable wrap
!
                    DIF_EL = (ELEV - SUR%STA(J5)%EL_CUR)
                    DIF_AZ = (AZ - SUR%STA(J5)%AZ_CUR)
                    DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                    IF ( DIF_AZ > 0.0D0 ) THEN
!
! ---------------------- The shortest move is clock-wise
!
                        IF ( SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J5)%AZ_ACC_MAX ) THEN
                             AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ
                             HA_ACC_CUR = HA
                             DIST_AZ = DIF_AZ
                          ELSE
!
! -------------------------- The shortest way is not possible, move the longest way
! -------------------------- counter-clock-wise
!
                             AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR - &
     &                                                (PI2 - DIF_AZ)
                             HA_ACC_CUR = HA
                             DIST_AZ = (PI2 - DIF_AZ)
                         END IF
                       ELSE
!
! --------------------- The shortest move is counter-clock-wise
!
                        IF ( SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J5)%AZ_ACC_MIN ) THEN
                             AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ
                             HA_ACC_CUR = HA
                             DIST_AZ = -DIF_AZ
                           ELSE
!
! ------------------------- The shortest way is not possible, move the longest way
! ------------------------- clock-wise
!
                             AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + (PI2 + DIF_AZ)
                             HA_ACC_CUR = HA
                             DIST_AZ = (PI2 + DIF_AZ)
                         END IF
                    END IF
!
                    IF ( ELEV < SUR%STA(J5)%EL_MIN .OR. &
     &                   ELEV > SUR%STA(J5)%EL_MAX      ) THEN
!
                         IF ( SCA_STA_PREV > 0 ) THEN
                              SUR%EL_OBS(J5,SUR%L_SCN) = SUR%EL_OBS(J5,SCA_STA_PREV) 
                              SUR%AZ_OBS(J5,SUR%L_SCN) = SUR%AZ_OBS(J5,SCA_STA_PREV) 
                              SUR%HA_OBS(J5,SUR%L_SCN) = SUR%HA_OBS(J5,SCA_STA_PREV) 
                              SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = SUR%AZ_ACC_OBS(J5,SCA_STA_PREV) 
                              SUR%HA_ACC_OBS(J5,SUR%L_SCN) = SUR%HA_ACC_OBS(J5,SCA_STA_PREV) 
                         END IF
                         GOTO 450
                    END IF
!
! ----------------- Compute the slew time of Az/El mounting
!
                    IF ( DABS(DIST_AZ) > SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ ) THEN
                         SLEW_AZ = ( DABS(DIST_AZ) - SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                               2.0D0*SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                        ELSE
                         SLEW_AZ = 2.D0*DSQRT(DABS(DIST_AZ)/SUR%STA(J5)%SLEW_ACCL_AZ)
                    END IF
                    IF ( DABS(DIF_EL) > SUR%STA(J5)%SLEW_RATE_EL**2/SUR%STA(J5)%SLEW_ACCL_EL ) THEN
                         SLEW_EL = DABS(DIF_EL - SUR%STA(J5)%SLEW_RATE_EL**2/SUR%STA(J5)%SLEW_ACCL_EL)/ &
     &                                  SUR%STA(J5)%SLEW_RATE_EL + &
     &                                  2.D0*SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                       ELSE
                         SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J5)%SLEW_ACCL_EL)
                    END IF
                    SUR%SLEW_DUR(J5,SUR%L_SCN) = MAX(SLEW_AZ+SUR%STA(J5)%TIME_SETTLE_AZ, &
     &                                               SLEW_EL+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                           SUR%STA(J5)%POSTOB
                    TIM_LO = 0.0D0
                    IF ( SCA_STA_PREV > 0 ) THEN
                         TIM_LO = (SUR%TAI_OBS_END(SUR%L_SCN-1) - SUR%TAI_OBS_END(SCA_STA_PREV)) + &
     &                            (SUR%MJD_OBS_END(SUR%L_SCN-1) - SUR%MJD_OBS_END(SCA_STA_PREV))*86400.0D0
                    END IF
                    SUR%SLEW_DUR(J5,SUR%L_SCN) = MAX ( 1.00001D0 + SUR%STA(J5)%POSTOB, &
     &                                                 SUR%SLEW_DUR(J5,SUR%L_SCN) - TIM_LO )
!
                    IF ( FL_SLEW_PRINT ) THEN
                         WRITE ( 6, 210 ) SUR%L_SCN, SUR%STA(J5)%NAME, DIF_EL/DEG__TO__RAD, &
     &                            DIST_AZ/DEG__TO__RAD, SLEW_EL, SLEW_AZ, &
     &                            SUR%SLEW_DUR(J5,SUR%L_SCN), AZ/DEG__TO__RAD, &
     &                            AZ_ACC_CUR/DEG__TO__RAD, ELEV/DEG__TO__RAD, &
     &                            SCA_STA_PREV, TIM_LO
 210                     FORMAT ( 'SUR_MER_503 SCA: ', I4, ' Sta: ', A, &
     &                            ' Dif_el: ', F7.2, ' Dif_az: ', F7.2, &
     &                            ' Slew_el: ', F5.1, ' Slew_az: ', F5.1, ' Slew: ', f5.1, &
     &                            ' az: ', f6.1, ' azc: ', f7.1, ' el: ', f5.1, &
     &                            ' Sca_prev: ', I4, ' Tim_lo= ', F6.1 )
                    END IF
                    SUR%HA_ACC_OBS(J5,SUR%L_SCN) = HA
                  ELSE IF ( SUR%STA(J5)%MOUNT_TYPE == MT__EQUAT ) THEN
                    DEL = SUR%SOU(IND_SRC)%DELTA
                    DIF_DEL = DEL - SUR%STA(J5)%DEL_CUR
                    DIF_HA  = (HA - SUR%STA(J5)%HA_CUR)
                    IF ( DABS(DIF_HA) < P2I ) THEN
!
! ---------------------- The shortest move is clock-wise
!
                         HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + DIF_HA
                       ELSE
!
! ---------------------- The shortest move is counter-clock-wise
!
                         IF ( SUR%STA(J5)%HA_ACC_CUR + DIF_HA > SUR%STA(J5)%AZ_ACC_MIN ) THEN
                              HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + DIF_HA
                              DIST_HA = -DIF_HA
                            ELSE
!
! --------------------------- The shortest way is not possible, move the longest way
! --------------------------- clock-wise
!
                              HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + (PI2 + DIF_HA)
                              DIST_HA = (PI2 + DIF_HA)
                        END IF
                   END IF
                   IF ( SUR%SOU(IND_SRC)%DELTA < SUR%STA(J5)%EL_MIN .OR. &
     &                  SUR%SOU(IND_SRC)%DELTA > SUR%STA(J5)%EL_MAX      ) THEN
!
                        IF ( SCA_STA_PREV > 0 ) THEN
                             SUR%EL_OBS(J5,SUR%L_SCN) = SUR%EL_OBS(J5,SCA_STA_PREV) 
                             SUR%AZ_OBS(J5,SUR%L_SCN) = SUR%AZ_OBS(J5,SCA_STA_PREV) 
                             SUR%HA_OBS(J5,SUR%L_SCN) = SUR%HA_OBS(J5,SCA_STA_PREV) 
                             SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = SUR%AZ_ACC_OBS(J5,SCA_STA_PREV) 
                             SUR%HA_ACC_OBS(J5,SUR%L_SCN) = SUR%HA_ACC_OBS(J5,SCA_STA_PREV) 
                        END IF
                        GOTO 450
                   END IF
!
! ---------------- Compute slew tine for equat mounting
!
                   IF ( DABS(DIF_HA) > SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ ) THEN
                        SLEW_HA = (DABS(DIF_HA) - SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                             2.0D0*SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                       ELSE
                        SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J5)%SLEW_ACCL_AZ)
                   END IF
                   IF ( DABS(DIF_DEL) > SUR%STA(J5)%SLEW_RATE_EL**2/SUR%STA(J5)%SLEW_ACCL_EL ) THEN
                         SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J5)%SLEW_RATE_EL + &
     &                              SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                       ELSE
                         SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J5)%SLEW_ACCL_EL)
                   END IF
                   SUR%SLEW_DUR(J5,SUR%L_SCN) = MAX(SLEW_HA+SUR%STA(J5)%TIME_SETTLE_AZ, &
     &                                              SLEW_DEL+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                          SUR%STA(J5)%POSTOB
                   SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = AZ
                 ELSE IF ( SUR%STA(J5)%MOUNT_TYPE == MT__XY_E ) THEN
!
! ---------------- XY-E mounting
!
                   IF ( DABS(DTAN(SUR%STA(J5)%EL_CUR)) < 1.D-6 ) THEN
                        A_LAST = P2I
                      ELSE
                        A_LAST = DATAN ( DCOS(SUR%STA(J5)%AZ_CUR)/DTAN(SUR%STA(J5)%EL_CUR) )
                        B_LAST = DASIN ( DSIN(SUR%STA(J5)%AZ_CUR)*DCOS(SUR%STA(J5)%EL_CUR) )
                   END IF
!
                   IF ( DABS(DTAN(ELEV)) < 1.D-6 ) THEN
                        A = P2I
                      ELSE
                        A = DATAN ( DCOS(AZ)/DTAN(ELEV) )
                        B = DASIN ( DSIN(AZ)*DCOS(ELEV) )
                   END IF
                   IF ( B < SUR%STA(J5)%EL_MIN .OR. &
     &                  B > SUR%STA(J5)%EL_MAX      ) THEN
!
                        SUR%EL_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%EL_CUR
                        SUR%AZ_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%AZ_CUR
                        SUR%HA_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%HA_CUR
                        SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%AZ_ACC_CUR
                        SUR%HA_ACC_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%HA_ACC_CUR
                        SUR%STA(J5)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                        SUR%STA(J5)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
!
                        SUR%SLEW_DUR(J5,SUR%L_SCN) = 0.0D0
                        GOTO 450
                   END IF
                   AZ_ACC_CUR = SUR%STA(J5)%AZ_CUR
                   HA_ACC_CUR = SUR%STA(J5)%HA_CUR
!
                   DIF_A = A - A_LAST
                   DIF_B = B - B_LAST
!
! ---------------- Compute slew tine for XY-E mounting
!
                   SLEW_A = DABS(DIF_A)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                      SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                   SLEW_B = DABS(DIF_B)/SUR%STA(J5)%SLEW_RATE_EL + &
     &                      SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                   SUR%SLEW_DUR(J5,SUR%L_SCN) = MAX(SLEW_A+SUR%STA(J5)%TIME_SETTLE_AZ,   &
     &                                              SLEW_B+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                          SUR%STA(J5)%POSTOB
!
                   SUR%HA_ACC_OBS(J5,SUR%L_SCN) = HA
                   SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = AZ
               END IF
               IF ( .NOT. SUR_CHECK_VIS ( SUR, J5, SUR__TYP_TAG, IND_SRC, &
     &                                    AZ, ELEV, HA, IER ) ) THEN
!
! ----------------- The J5-th station does not see the IND_SRC -th source
!
                   IF ( SCA_STA_PREV > 0 ) THEN
                        SUR%EL_OBS(J5,SUR%L_SCN) = SUR%EL_OBS(J5,SCA_STA_PREV) 
                        SUR%AZ_OBS(J5,SUR%L_SCN) = SUR%AZ_OBS(J5,SCA_STA_PREV) 
                        SUR%HA_OBS(J5,SUR%L_SCN) = SUR%HA_OBS(J5,SCA_STA_PREV) 
                        SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = SUR%AZ_ACC_OBS(J5,SCA_STA_PREV) 
                        SUR%HA_ACC_OBS(J5,SUR%L_SCN) = SUR%HA_ACC_OBS(J5,SCA_STA_PREV) 
                   END IF
                   GOTO 450
               END IF
!
               K_STA = K_STA + 1
               SUR%STA(J5)%AZ_CUR  = AZ
               SUR%STA(J5)%EL_CUR  = ELEV
               SUR%STA(J5)%HA_CUR  = HA
               SUR%STA(J5)%AZ_ACC_CUR = AZ_ACC_CUR
               SUR%STA(J5)%HA_ACC_CUR = HA_ACC_CUR
               SUR%STA(J5)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
               SUR%STA(J5)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
               SUR%EL_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%EL_CUR
               SUR%AZ_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%AZ_CUR
               SUR%HA_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%HA_CUR
               SUR%AZ_ACC_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%AZ_ACC_CUR
               SUR%HA_ACC_OBS(J5,SUR%L_SCN) = SUR%STA(J5)%HA_ACC_CUR
               SUR%OBS_STA(J5,SUR%L_SCN) = SUR__USED
               SUR%SCA_PREV(J5,SUR%L_SCN) = 0
               IF ( SUR%L_SCN > 1 ) THEN
                    SUR%SCA_PREV(J5,SUR%L_SCN) = SCA_STA_PREV
               END IF
 450        CONTINUE
            IF ( K_STA < SUR%SOU(IND_SRC)%MIN_STA ) THEN
!
! -------------- Alas! Too few stations. We have to play back
!
                 SUR = SUR_SAVE
                 IF ( IVRB .GE. 5 ) THEN
                      WRITE ( 6, * ) 'SUR_MERIDIAN: Un-insert source '//SUR%SOU(IND_SRC)%J2000_NAME, &
     &                               ' becauase it is up only at ', INT2(K_STA), ' stations' 
                 END IF
                 GOTO 420
            END IF
            IF ( IVRB .GE. 6 ) THEN
                 WRITE ( 6, * ) ' '
                 CALL SUR_SLEW_REPORT ( SUR )
                 WRITE ( 6, * ) ' '
             END IF
          ELSE
!
! --------- No more sources remained in the primary list
!
            IF ( IVRB .GE. 2 ) THEN
                 STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                 WRITE ( 6, 150 ) STR(1:21)
 150             FORMAT ( ' No primary sources to observe at ', A )
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            IS = SUR_MERIDIAN_SEC ( SUR, VTD, IVRB, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1748, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &               'an attempt to insert a secondary target' )
                 RETURN
            END IF
!
            IF ( IS == 0 ) THEN
                 IF ( IVRB .GE. 2 ) THEN
                      STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                      WRITE ( 6, 155 ) STR(1:21), SUR%SCAN_LEN
 155                  FORMAT ( ' No secondary sources to observe at ', A, &
     &                         ' Skip ', F5.0, ' sec' )
                 END IF
!
! -------------- Skip time. What else can we do?
!
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) + &
     &                                        SUR%SCAN_LEN
                 IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0 ) THEN
                      SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                      SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
                 END IF
                 SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
                 SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
                 IND_SRC = 0
            END IF
            MJD_OBS = SUR%MJD_CUR
            TAI_OBS = SUR%TAI_CUR
         END IF
         IF ( IVRB .GE. 4 ) THEN
              STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), IER )
              STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), IER )
              IF ( IND_SRC .LE. 0 ) THEN
                   WRITE ( 6, * ) 'SUR_MERIDIAN(579): IND_SRC=', IND_SRC
                 ELSE
                   WRITE ( 6, 180 ) SUR%L_SCN, SUR%SOU(IND_SRC)%J2000_NAME, &
     &                              STR(1:21), STR1(1:21)
              END IF
 180          FORMAT ( 'SCAN ', I4, 1X, A, ' Obs_beg: ', A, ' Obs_end: ', A )
         END IF 
 420  CONTINUE
 820  CONTINUE
!
! --- Insert a burst of troposphere calibrators
!
      IF ( SUR%TROPO_BURST_INTERVAL > 0.0D0 ) THEN
           IF ( IVRB .GE. 8 ) THEN
                STR1 = MJDSEC_TO_DATE ( SUR%MJD_CUR,       SUR%TAI_CUR,       -2 )
                STR2 = MJDSEC_TO_DATE ( SUR%MJD_TROPO_CUR, SUR%TAI_TROPO_CUR, -2 )
                WRITE ( 6, * ) 'SUR_MEDIDIAN Started troposphere burst, because '// &
     &                         'we cannot find a primary source at '//STR1(1:19)
           END IF
           CALL ERR_PASS ( IUER, IER )
           CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1749, IUER, 'SUR_MERIDIAN', 'Error in '// &
          &         'an attempt to insert a burst of troposphere calibrators' )
                RETURN
           END IF
      END IF
!
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
!
! --- Try to schedule secondary sources
!
      SUR%NOBS_SO2 = 0
      DO 480 J8=1,SUR__M_SOU
         IF ( SUR%ALGORITHM == 'IMAGING_01' ) GOTO  480 ! Not in imaging mode, sorry
         CALL ERR_PASS ( IUER, IER )
         IS = SUR_MERIDIAN_SEC ( SUR, VTD, IVRB, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1750, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &            'an attempt to insert a secondary target' )
              RETURN
         END IF
         IF ( IS == 0 ) THEN
!
! ----------- We did not find any secondary scan. Extend the last scan to the
! ----------- stop time and that is it. What can we do more?
!
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP
              IF ( FL_LAST_SCA_EXT .AND. &
     &             (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_OBS_BEG(SUR%L_SCN))*86400.0D0 + &
     &             (SUR%TAI_OBS_END(SUR%L_SCN) - SUR%TAI_OBS_BEG(SUR%L_SCN)) > &
     &             1.5D0*SUR%SCAN_LEN ) THEN
!
                   IF ( SUR%MJD_OBS_END(SUR%L_SCN) == SUR%MJD_OBS_BEG(SUR%L_SCN) ) THEN
                        SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                               1.5D0*SUR%SCAN_LEN
                      ELSE 
                        SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                               1.5D0*SUR%SCAN_LEN - 86400.0D0
                   END IF
                   IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.D0 ) THEN
                        SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                        SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
                   END IF
              END IF
              GOTO 880
         END IF
 480  CONTINUE
 880  CONTINUE
!
      SUR%MJD_STOP = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_STOP = SUR%TAI_OBS_END(SUR%L_SCN)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_MERIDIAN  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SUR_MERIDIAN_SEC ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Function SUR_MERIDIAN_SEC
! *                                                                      *
! * ## 11-JUN-2011 SUR_MERIDIAN_SEC v2.5 (c)  L. Petrov  28-JAN-2019 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  SUR_MERIDIAN_SEC
      INTEGER*4  IVRB, IUER
      REAL*8     SANG, TANG, AZ, ELEV, HA, DTIM_RISE, DTIM_SET, &
     &           DEC_WGT, TIM_WGT, SLE_WGT, CUL_WGT, &
     &           DIF_AZ, DIF_HA, TIM_LAST, TAI_OBS, TAI_TAPE_START_SAVE, &
     &           SLEW_TIME_MAX, SLEW_TIME_SCN(SUR__M_SOU)
      REAL*8     DIF_A, DIF_B, DIF_EL, DIF_DEL, A_LAST, B_LAST, A, B, &
     &           DIST_AZ, DIST_HA, DEL, TIM_LO, AZ_ACC_CUR, HA_ACC_CUR
      REAL*8     SLEW_EL, SLEW_AZ, SLEW_HA, SLEW_DEL, SLEW_A, SLEW_B
      CHARACTER  STR*128
      LOGICAL*1  FL_TAPE_CHANGE
      REAL*8     SHARE__MIN_DUR, SHARE__TROPO, SHARE__GAP
      PARAMETER  ( SHARE__MIN_DUR = 0.8   )
      PARAMETER  ( SHARE__TROPO   = 0.75  )
      PARAMETER  ( SHARE__GAP     = 0.667 )
      REAL*8     SCORE(SUR__M_SOU), IND_R8(SUR__M_SOU)
      INTEGER*4  J1, J2, J3, J4, J5, NS, K_STA, IND_SCN, IND_SRC, &
     &           MJD_OBS, MJD_TAPE_START_SAVE, SCA_STA_PREV, SPL_STATUS, IER
      LOGICAL*1  FL_SEE 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      REAL*8   , EXTERNAL :: SUR_SLEW_TIME
!
! --- Check: is it time to change the tape?
!
      MJD_OBS = SUR%MJD_CUR
      TAI_OBS = SUR%TAI_CUR
      MJD_TAPE_START_SAVE = SUR%MJD_TAPE_START_CUR 
      TAI_TAPE_START_SAVE = SUR%TAI_TAPE_START_CUR 
!
! --- Check: is it time to change the tape?
!
      IF ( (SUR%MJD_CUR - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &     (SUR%TAI_CUR - SUR%TAI_TAPE_START_CUR) + SUR%PREOBS_LONG + &
     &      SUR%SCAN_LEN*2.0 > SUR%TAPE_LENGTH ) THEN
!
! --------- Yes, it is just time. Reset the current time
!
            SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) + SUR%TAPE_CHANGE_TIME
            SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
!
            IF ( SUR%TAI_CUR > 86400.0D0 ) THEN
                 SUR%TAI_CUR = SUR%TAI_CUR - 86400.0D0
                 SUR%MJD_CUR = SUR%MJD_CUR + 1
            END IF
!
! --------- Increment the tape counter
!
            SUR%L_TAP = SUR%L_TAP + 1
!
! --------- Set the tape start date
!
            SUR%MJD_TAPE_START_CUR = SUR%MJD_CUR
            SUR%TAI_TAPE_START_CUR = SUR%TAI_CUR
            FL_TAPE_CHANGE = .TRUE.
          ELSE 
            FL_TAPE_CHANGE = .FALSE.
      END IF
!
      NS = 0
      DO 410 J1=1,SUR%L_SO2
         SCORE(J1) = 0.0D0
         IF ( (SUR%MJD_STOP*86400.0D0 + SUR%TAI_STOP) - (SUR%MJD_CUR*86400.0D0 + SUR%TAI_CUR) < &
     &                      SUR%AVR_SLEW_TIME + SHARE__MIN_DUR*SUR%SO2(J1)%DUR ) THEN
!
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP
!
              IF ( FL_TAPE_CHANGE ) THEN
                   SUR%L_TAP = SUR%L_TAP - 1
                   SUR%MJD_TAPE_START_CUR = MJD_TAPE_START_SAVE
                   SUR%TAI_TAPE_START_CUR = TAI_TAPE_START_SAVE
              END IF
              IF ( IVRB .GE. 6 ) THEN
                   WRITE ( 6, * ) 'sur .. sec c1 remained only ', &
     &                            SNGL( (SUR%MJD_STOP*86400.0D0 + SUR%TAI_STOP) - &
     &                                  ( SUR%MJD_CUR*86400.0D0 + SUR%TAI_CUR)    ), &
     &                            ' sec before the session end'
              END IF
!
              SUR_MERIDIAN_SEC = 0
              CALL ERR_LOG ( 0, IUER )
              RETURN
         END IF
         IF ( SUR%NOBS_SO2(J1) > 0 ) THEN
              IND_SCN  = SUR%IND_SCN_SO2(SUR%NOBS_SO2(J1),J1)
              IF ( IND_SCN > 0 ) THEN
                   TIM_LAST = (SUR%MJD_CUR - SUR%MJD_OBS_BEG(IND_SCN))*86400.0D0 + &
     &                        (SUR%TAI_CUR - SUR%TAI_OBS_BEG(IND_SCN))
                 ELSE
                   TIM_LAST = 10.0D0*86400.0D0
              END IF
              IF ( TIM_LAST < SHARE__GAP*SUR%SO2(J1)%GAP_MIN ) THEN
                   IF ( IVRB .GE. 5 ) THEN
                        WRITE ( 6, * ) 'SUR_MERIDIAN SEC Sou: ', SUR%SO2(J1)%J2000_NAME, &
     &                                ' Tim_last= ', sngl(tim_last/3600.0d0), ' hours'
                   END IF
                   GOTO 410
              END IF
         END IF
!
         K_STA = 0
         DO 420 J2=1,SUR%L_STA
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, SUR__TYP_SEC, SUR%MJD_CUR, &
     &                      SUR%TAI_CUR + SUR%AVR_SLEW_TIME, J2, J1, AZ, &
     &                      ELEV, HA, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1753, IUER, 'SUR_MERIDIAN', 'Error in '// &
     &               'an attempt to compute AZ/EL' )
                 RETURN
            END IF
            IF ( SUR_CHECK_VIS ( SUR, J2, SUR__TYP_SEC, J1, &
     &                           AZ, ELEV, HA, IER ) ) THEN
                 K_STA = K_STA + 1
            END IF
 420     CONTINUE
         IF ( K_STA < SUR%SO2(J1)%MIN_STA ) THEN       
              IF ( IVRB .GE. 6 ) THEN
                   WRITE ( 6, * ) 'SUR_MERIDIAN SEC Sou: ', SUR%SO2(J1)%J2000_NAME, &
     &                           ' K_STA= ', K_STA
              END IF
              GOTO 410
         END IF
         SLEW_TIME_SCN(J1) = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_SEC, &
     &                                   J1, SUR%IND_SRC(SUR%L_SCN), &
     &                                   SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                   SUR__FINE, IER )
         IF ( SLEW_TIME_SCN(J1) .LE. 0 ) THEN
              IF ( IVRB .GE. 6 ) THEN
                   WRITE ( 6, * ) 'SUR_MERIDIAN SEC Sou: ', SUR%SO2(J1)%J2000_NAME, &
     &                           ' SLEW_TIME= ', SNGL(SLEW_TIME_SCN(J1))
              END IF
              GOTO 410
         END IF
         NS = NS + 1
         SCORE(NS) = -1000.0*SUR%NOBS_SO2(J1) + 1000.0/(1.0D0 + SLEW_TIME_SCN(J1))
         IND_R8(NS) = J1
 410  CONTINUE 
!
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) 'sur .. sec c2 ns = ', ns
      END IF
      IF ( NS == 0 ) THEN
           SUR_MERIDIAN_SEC = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) 'sur .. sec c3 ns = ', ns
      END IF
!
      CALL SORT8 ( NS, SCORE, IND_R8 )
      IND_SRC = IND_R8(NS)
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) 'sur .. sec c4 max_score= ', score(ns), ' ind_src= ', ind_src
      END IF
!
      SUR%L_SCN = SUR%L_SCN + 1
!
      IF ( FL_TAPE_CHANGE ) THEN
            SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
            SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR + SLEW_TIME_SCN(IND_SRC)
            SUR%SCAN_TYPE(SUR%L_SCN) = SUR__TAPE
          ELSE 
            SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
            SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR + SLEW_TIME_SCN(IND_SRC) &
     &                                   + SUR%PREOBS_LONG
            SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
      END IF

      IF ( ( SUR%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &     ( SUR%TAI_CUR + SUR%AVR_SLEW_TIME + SUR%SO2(IND_SRC)%DUR - SUR%TAI_STOP) > 0.0D0 ) THEN
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP
        ELSE
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_CUR
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_CUR + SUR%AVR_SLEW_TIME + SUR%SO2(IND_SRC)%DUR
      END IF
      IF ( (SUR%MJD_STOP - SUR%MJD_OBS_END(SUR%L_SCN))*86400.0D0 + &
     &     (SUR%TAI_STOP - SUR%TAI_OBS_END(SUR%L_SCN)) < &
     &     SHARE__MIN_DUR*SUR%SO2(IND_SRC)%DUR ) THEN
           SUR%L_SCN = SUR%L_SCN - 1
           SUR_MERIDIAN_SEC = 0
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
      END IF
!
      SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_SEC, &
     &                                IND_SRC, SUR%IND_SRC(SUR%L_SCN), &
     &                                SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                SUR__FINE, IER )
      IF ( SLEW_TIME_MAX .LE. 0.0D0 ) THEN
           SUR%L_SCN = SUR%L_SCN - 1
!
           WRITE ( 6, * ) ' SLEW_TIME_MAX = ', SLEW_TIME_MAX 
           CALL ERR_LOG ( 1762, IUER, 'SUR_MERIDIAN_SEC', 'Trap of '// &
     &         'internal control: slew_time_max is not positive' )
           RETURN 
      END IF
!
      IF ( IVRB .GE. 4 ) THEN
           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, IER  )
           WRITE ( 6, 170 ) SUR%SO2(IND_SRC)%J2000_NAME, STR(1:24), &
     &                      SUR%L_SCN
 170       FORMAT ( ' Secondary source ', A, ' at ', A, ' Scan: ', I4 )
      END IF
!
      SUR%SRC_TYP(SUR%L_SCN) = SUR__TYP_SEC
      SUR%IND_SRC(SUR%L_SCN) = IND_SRC
      SUR%NOBS_SO2(IND_SRC) = SUR%NOBS_SO2(IND_SRC) + 1
      SUR%IND_SCN_SO2(SUR%NOBS_SO2(IND_SRC),IND_SRC) = SUR%L_SCN
!
      DO 430 J3=1,SUR%L_STA
         CALL ERR_PASS ( IUER, IER )
         SPL_STATUS = SUR%STATUS_SPL(SUR__TYP_SEC)
         SUR%STATUS_SPL(SUR__TYP_SEC) = 0
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, SUR__TYP_SEC, SUR%MJD_OBS_BEG(SUR%L_SCN), &
     &                   SUR%TAI_OBS_BEG(SUR%L_SCN), J3, IND_SRC, &
     &                   AZ, ELEV, HA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1763, IUER, 'SUR_MERIDIAN_SEC', 'Error in '// &
     &             'computing azimuth and elevation' )
              RETURN
         END IF
         SUR%STATUS_SPL(SUR__TYP_SEC) = SPL_STATUS 
         FL_SEE = .TRUE.
         IF ( SUR%STA(J3)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ----------- Update accumulative aziumth in order to take into account
! ----------- cable wrap
!
              DIF_AZ = (AZ - SUR%STA(J3)%AZ_CUR)
              DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
              IF ( DIF_AZ > 0.0D0 ) THEN
!
! ---------------- The shortest move is clock-wise
!
                   IF ( SUR%STA(J3)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J3)%AZ_ACC_MAX ) THEN
                        AZ_ACC_CUR = SUR%STA(J3)%AZ_ACC_CUR + DIF_AZ
                        HA_ACC_CUR = HA
                        DIST_AZ = DIF_AZ
                      ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- counter-clock-wise
!
                        AZ_ACC_CUR = SUR%STA(J3)%AZ_ACC_CUR - (PI2 - DIF_AZ)
                        HA_ACC_CUR = HA
                        DIST_AZ = (PI2 - DIF_AZ)
                   END IF
                 ELSE
!
! ---------------- The shortest move is counter-clock-wise
!
                   IF ( SUR%STA(J3)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J3)%AZ_ACC_MIN ) THEN
                        AZ_ACC_CUR = SUR%STA(J3)%AZ_ACC_CUR + DIF_AZ
                        HA_ACC_CUR = HA
                        DIST_AZ = -DIF_AZ
                     ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- clock-wise
!
                        AZ_ACC_CUR = SUR%STA(J3)%AZ_ACC_CUR + (PI2 + DIF_AZ)
                        HA_ACC_CUR = HA
                        DIST_AZ = (PI2 + DIF_AZ)
                   END IF
              END IF
!
              IF ( ELEV < SUR%STA(J3)%EL_MIN .OR. &
     &             ELEV > SUR%STA(J3)%EL_MAX      ) THEN
!
                   FL_SEE = .FALSE.
              END IF
!
! ----------- Compute the slew time of Az/El mounting
!
              IF ( DABS(DIST_AZ) > SUR%STA(J3)%SLEW_RATE_AZ**2/SUR%STA(J3)%SLEW_ACCL_AZ ) THEN
                   SLEW_AZ = ( DABS(DIST_AZ) - SUR%STA(J3)%SLEW_RATE_AZ**2/SUR%STA(J3)%SLEW_ACCL_AZ)/SUR%STA(J3)%SLEW_RATE_AZ + &
     &                         2.0D0*SUR%STA(J3)%SLEW_RATE_AZ/SUR%STA(J3)%SLEW_ACCL_AZ
                  ELSE
                    SLEW_AZ = 2.D0*DSQRT(DABS(DIST_AZ)/SUR%STA(J3)%SLEW_ACCL_AZ)
              END IF
              IF ( DABS(DIF_EL) > SUR%STA(J3)%SLEW_RATE_EL**2/SUR%STA(J3)%SLEW_ACCL_EL ) THEN
                   SLEW_EL = DABS(DIF_EL)/SUR%STA(J3)%SLEW_RATE_EL + &
     &                       SUR%STA(J3)%SLEW_RATE_EL/SUR%STA(J3)%SLEW_ACCL_EL
                 ELSE
                   SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J3)%SLEW_ACCL_EL)
              END IF
              SUR%SLEW_DUR(J3,SUR%L_SCN) = MAX(SLEW_AZ+SUR%STA(J3)%TIME_SETTLE_AZ,   &
     &                                         SLEW_EL+SUR%STA(J3)%TIME_SETTLE_EL) + &
     &                                     SUR%STA(J3)%POSTOB
              SUR%HA_ACC_OBS(J3,SUR%L_SCN) = HA
            ELSE IF ( SUR%STA(J3)%MOUNT_TYPE == MT__EQUAT ) THEN
              DEL = SUR%SO2(IND_SRC)%DELTA
              DIF_DEL = DEL - SUR%STA(J3)%DEL_CUR
              DIF_HA = (HA - SUR%STA(J3)%HA_CUR)
              DIF_HA = DIF_HA - PI2*IDNINT(DIF_AZ/PI2)
              IF ( DIF_HA > 0.0D0 ) THEN
!
! ---------------- The shortest move is clock-wise
!
                   IF ( SUR%STA(J3)%HA_ACC_CUR + DIF_AZ < SUR%STA(J3)%AZ_ACC_MAX ) THEN
                        SUR%STA(J3)%HA_ACC_CUR = SUR%STA(J3)%HA_ACC_CUR + DIF_HA
                        DIST_HA = DIF_HA
                      ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- counter-clock-wise
!
                        SUR%STA(J3)%HA_ACC_CUR = SUR%STA(J3)%HA_ACC_CUR - &
     &                                           (PI2 - DIF_HA)
                        DIST_HA = (PI2 - DIF_HA)
                   END IF
                 ELSE
!
! ---------------- The shortest move is counter-clock-wise
!
                   IF ( SUR%STA(J3)%HA_ACC_CUR + DIF_HA > SUR%STA(J3)%AZ_ACC_MIN ) THEN
                        SUR%STA(J3)%HA_ACC_CUR = SUR%STA(J3)%HA_ACC_CUR + DIF_HA
                        DIST_HA = -DIF_HA
                      ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- clock-wise
!
                        SUR%STA(J3)%HA_ACC_CUR = SUR%STA(J3)%HA_ACC_CUR + &
     &                                          (PI2 + DIF_HA)
                        DIST_HA = (PI2 + DIF_HA)
                   END IF
              END IF
              IF ( SUR%SO2(IND_SRC)%DELTA < SUR%STA(J3)%EL_MIN .OR. &
     &             SUR%SO2(IND_SRC)%DELTA > SUR%STA(J3)%EL_MAX      ) THEN
                   FL_SEE = .FALSE.
              END IF
!
! ----------- Compute slew tine for equat mounting
!
              IF ( DABS(DIF_HA) > SUR%STA(J3)%SLEW_RATE_AZ**2/SUR%STA(J3)%SLEW_ACCL_AZ ) THEN
                   SLEW_HA = (DABS(DIF_HA) - SUR%STA(J3)%SLEW_RATE_AZ**2/SUR%STA(J3)%SLEW_ACCL_AZ)/SUR%STA(J3)%SLEW_RATE_AZ + &
     &                       2.0D0*SUR%STA(J3)%SLEW_RATE_AZ/SUR%STA(J3)%SLEW_ACCL_AZ
                 ELSE
                   SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J3)%SLEW_ACCL_AZ)
              END IF
              IF ( DABS(DIF_DEL) > SUR%STA(J3)%SLEW_RATE_EL**2/SUR%STA(J3)%SLEW_ACCL_EL ) THEN
                   SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J3)%SLEW_RATE_EL + &
     &                        SUR%STA(J3)%SLEW_RATE_EL/SUR%STA(J3)%SLEW_ACCL_EL
                 ELSE
                  SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J3)%SLEW_ACCL_EL)
              END IF
              SUR%SLEW_DUR(J3,SUR%L_SCN) = MAX(SLEW_HA+SUR%STA(J3)%TIME_SETTLE_AZ, &
     &                                         SLEW_DEL+SUR%STA(J3)%TIME_SETTLE_EL) + &
     &                                     SUR%STA(J3)%POSTOB
              SUR%AZ_ACC_OBS(J3,SUR%L_SCN) = AZ
            ELSE IF ( SUR%STA(J3)%MOUNT_TYPE == MT__XY_E ) THEN
!
! ----------- XY-E mounting
!
              IF ( DABS(DTAN(SUR%STA(J3)%EL_CUR)) < 1.D-6 ) THEN
                   A_LAST = P2I
                 ELSE
                   A_LAST = DATAN ( DCOS(SUR%STA(J3)%AZ_CUR)/DTAN(SUR%STA(J3)%EL_CUR) )
                   B_LAST = DASIN ( DSIN(SUR%STA(J3)%AZ_CUR)*DCOS(SUR%STA(J3)%EL_CUR) )
              END IF
!              
              IF ( DABS(DTAN(ELEV)) < 1.D-6 ) THEN
                   A = P2I
                 ELSE
                   A = DATAN ( DCOS(AZ)/DTAN(ELEV) )
                   B = DASIN ( DSIN(AZ)*DCOS(ELEV) )
              END IF
!
              DIF_A = A - A_LAST
              DIF_B = B - B_LAST
!
! ----------- Compute slew time for XY-E mounting
!
              SLEW_A = DABS(DIF_A)/SUR%STA(J3)%SLEW_RATE_AZ + &
     &                 SUR%STA(J3)%SLEW_RATE_AZ/SUR%STA(J3)%SLEW_ACCL_AZ
              SLEW_B = DABS(DIF_B)/SUR%STA(J3)%SLEW_RATE_EL + &
     &                 SUR%STA(J3)%SLEW_RATE_EL/SUR%STA(J3)%SLEW_ACCL_EL
              SUR%SLEW_DUR(J3,SUR%L_SCN) = MAX(SLEW_A+SUR%STA(J3)%TIME_SETTLE_AZ, &
     &                                         SLEW_B+SUR%STA(J3)%TIME_SETTLE_EL) + &
     &                                     SUR%STA(J3)%POSTOB
!
              AZ_ACC_CUR = AZ
              HA_ACC_CUR = HA
         END IF
!
         IF ( SUR%L_SCN > 1 ) THEN
              SCA_STA_PREV = 0
              DO 440 J4=SUR%L_SCN-1,1,-1
                 IF ( SUR%OBS_STA(J3,J4) == SUR__USED ) THEN
                      SCA_STA_PREV = J4
                      GOTO 840
                  END IF
 440          CONTINUE 
 840          CONTINUE 
         END IF
         TIM_LO = 0.0D0
         IF ( SCA_STA_PREV > 0 ) THEN
              TIM_LO = (SUR%TAI_OBS_END(SUR%L_SCN-1) - SUR%TAI_OBS_END(SCA_STA_PREV)) + &
     &                 (SUR%MJD_OBS_END(SUR%L_SCN-1) - SUR%MJD_OBS_END(SCA_STA_PREV))*86400.0D0
         END IF
         SUR%SLEW_DUR(J3,SUR%L_SCN) = MAX ( 1.00001D0 + SUR%STA(J3)%POSTOB, &
     &                                      SUR%SLEW_DUR(J3,SUR%L_SCN) - TIM_LO )
!
         IF ( FL_SEE ) THEN
              SUR%STA(J3)%AZ_CUR  = AZ
              SUR%STA(J3)%EL_CUR  = ELEV
              SUR%STA(J3)%HA_CUR  = HA
              SUR%STA(J3)%ALP_CUR = SUR%SO2(IND_SRC)%ALPHA
              SUR%STA(J3)%DEL_CUR = SUR%SO2(IND_SRC)%DELTA
              SUR%EL_OBS(J3,SUR%L_SCN) = SUR%STA(J3)%EL_CUR
              SUR%AZ_OBS(J3,SUR%L_SCN) = SUR%STA(J3)%AZ_CUR
              SUR%HA_OBS(J3,SUR%L_SCN) = SUR%STA(J3)%HA_CUR
              SUR%AZ_ACC_OBS(J3,SUR%L_SCN) = AZ_ACC_CUR
              SUR%HA_ACC_OBS(J3,SUR%L_SCN) = HA_ACC_CUR
              SUR%IND_TAP(SUR%L_SCN)    = SUR%L_TAP
              SUR%OBS_STA(J3,SUR%L_SCN) = SUR__USED
              IF ( SUR%L_SCN > 1 ) THEN
                   DO 450 J5=SUR%L_SCN-1,1,-1
                      IF ( SUR%OBS_STA(J3,J5) == SUR__USED ) THEN
                           SUR%SCA_PREV(J3,SUR%L_SCN) = J5
                           GOTO 850
                      END IF
 450               CONTINUE 
 850               CONTINUE 
              END IF
            ELSE
!
! ----------- The J3-th station does not see the source
!
              IF ( SUR%L_SCN > 1 ) THEN
                   SUR%EL_OBS(J3,SUR%L_SCN) = SUR%EL_OBS(J3,SUR%L_SCN-1)
                   SUR%AZ_OBS(J3,SUR%L_SCN) = SUR%AZ_OBS(J3,SUR%L_SCN-1) 
                   SUR%HA_OBS(J3,SUR%L_SCN) = SUR%HA_OBS(J3,SUR%L_SCN-1) 
                   SUR%AZ_ACC_OBS(J3,SUR%L_SCN) = SUR%AZ_ACC_OBS(J3,SUR%L_SCN-1) 
                   SUR%HA_ACC_OBS(J3,SUR%L_SCN) = SUR%HA_ACC_OBS(J3,SUR%L_SCN-1) 
              END IF
         END IF
 430  CONTINUE
!
      SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_BEG(SUR%L_SCN)
      SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%SO2(IND_SRC)%DUR
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
      END IF
!
      SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
!
      SUR_MERIDIAN_SEC = 1
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   FUNCTION  SUR_MERIDIAN_SEC  !#!#
