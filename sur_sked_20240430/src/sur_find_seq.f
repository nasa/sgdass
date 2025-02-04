      SUBROUTINE SUR_FIND_SEQ ( SUR, VTD, MJD_STOP, TAI_STOP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_FIND_SEQ 
! *                                                                      *
! *  ### 11-OCT-2005  SUR_FIND_SEQ  v2.1 (c)  L. Petrov 22-SEP-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MJD_STOP, IUER
      REAL*8     TAI_STOP
      REAL*8     SCORE(SUR__M_SOU), SLEW_STA(SUR__M_STA), &
     &           SLEW_SRC(SUR__M_SOU), IND_SCORE(SUR__M_SOU), SLEW_TIME_MAX
      REAL*8     AZ, EL, HA, SLEW_EL, SLEW_AZ, &
     &           DIST_EL, DIST_AZ, DIST_HA, DIST_DEL, TAI_OBS, &
     &           DIF_HA, DIF_AZ, TIM_DIF, EL_SCAN_MIN, ALP, DEL, &
     &           AZ_STA(SUR__M_STA), EL_STA(SUR__M_STA), HA_STA(SUR__M_STA)
      REAL*8     AZ__MARGIN, EL__MARGIN, PREOBS_DUR
      PARAMETER  ( AZ__MARGIN = 2.0D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 0.7D0*DEG__TO__RAD  )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, MULT_TRY, MJD_OBS, IND_SLEW, &
     &           IND_SRC, K_SOU, CUR_TYP, LAST_SRC, UTC_OBS_INT, IER
      LOGICAL*4  FL_TAPE_CHANGE, FL_LONG
      INTEGER*4  TRY
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*23
      LOGICAL*4  SUR_CHECK_VIS 
      INTEGER*4, EXTERNAL :: MAX_LIST_R8, ILEN, I_LEN
!
!@      MULT_TRY = MAX ( 2, IDNINT(SUR%SCAN_LEN/SUR%AVR_SLEW_TIME) )
      MULT_TRY = 2
!
      CUR_TYP = SUR__TYP_TAG   ! Current type: target source
      FL_TAPE_CHANGE = .FALSE. ! No tape change should happen immediately
!
      DO 410 J1=2,SUR%L_SOU*2
         SCORE(J1) = 0.0D0
         TRY = 0
         DO 420 J2=1,SUR__M_SOU
            TRY = TRY + 1
!
! --------- Set approximate time for the next observation
!
            MJD_OBS = SUR%MJD_CUR
            IF ( TRY > 2 ) THEN
                 TAI_OBS = SUR%TAI_CUR + SUR%AVR_SLEW_TIME*((J2+1)/2)
              ELSE 
                 TAI_OBS = SUR%TAI_CUR 
            END IF
!
            IF ( TAI_OBS > 86400.0D0 ) THEN
                 MJD_OBS = MJD_OBS + 1
                 TAI_OBS = TAI_OBS - 86400.0D0
            END IF
!
            IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                 LAST_SRC = SUR%L_CAL
               ELSE
                 IF ( MOD(TRY,MULT_TRY) .NE. 0 ) THEN
                      CUR_TYP = SUR__TYP_TAG 
                      LAST_SRC = SUR%L_SOU
                    ELSE 
                      CUR_TYP = SUR__TYP_SEC 
                      LAST_SRC = SUR%L_SO2
                 END IF
            END IF
!
! --------- The number of sources is different depending on mode: 
! --------- target or calibrator
!
            K_SOU = 0
            DO 430 J3=1,LAST_SRC
               IF ( CUR_TYP == SUR__TYP_TAG  ) THEN 
!
! ----------------- If this target source has already been used -- skip it
! ----------------- If the calibrator source has been used -- this is OK
!
                    IF ( SUR%SOU(J3)%FL_USE ) GOTO 430
               END IF
               IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                    IF ( SUR%SO2(J3)%FL_USE ) GOTO 430
               END IF
!
               EL_SCAN_MIN = PI2
               DO 440 J4=1,SUR%L_STA
                  IF ( SUR%STA(J4)%TAGALONE ) GOTO 440
                  SLEW_STA(J4) = -1.0D0
!
! --------------- Compute for each station azimuth and elevation
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, TAI_OBS, &
     &                            J4, J3, AZ, EL, HA, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL ERR_LOG ( 1681, IUER, 'SUR_FIND_SEQ', &
     &                     'Error in computing azimuth and elevation' ) 
                       RETURN 
                  END IF
!
! --------------- Check, whether the J4-th station does see the source
!
                  IF ( EL < SUR%STA(J4)%EL_MIN + EL__MARGIN  .OR. &
     &                 EL > SUR%STA(J4)%EL_MAX - EL__MARGIN       ) THEN
!
! -------------------- Does not see? Go to the next source
!
                       GOTO 430
                  END IF
                  EL_SCAN_MIN = MIN(EL_SCAN_MIN, EL)
!
! --------------- More stringent check
!
                  IER = -1 
                  IF ( .NOT. SUR_CHECK_VIS ( SUR, J4, CUR_TYP, J3, AZ, EL, HA, IER ) ) THEN
!
! -------------------- Does not see? Go to the next source
!
                       GOTO 430
                  END IF
!
                  IF ( SUR%STA(J4)%MOUNT_TYPE == MT__ALTAZ ) THEN
                       DIST_EL = DABS(EL - SUR%STA(J4)%EL_CUR)
                       IF ( DIST_EL > SUR%STA(J4)%SLEW_RATE_EL**2/SUR%STA(J4)%SLEW_ACCL_EL ) THEN
                            SLEW_EL = DIST_EL/SUR%STA(J4)%SLEW_RATE_EL + &
     &                                SUR%STA(J4)%SLEW_RATE_EL/SUR%STA(J4)%SLEW_ACCL_EL
                          ELSE 
                            SLEW_EL = 2.D0*DSQRT(DIST_EL/SUR%STA(J4)%SLEW_ACCL_EL)
                       END IF
!
! -------------------- Compute slewing time for azimuth, keeping in mind
! -------------------- cable wrap limits
!
                       DIF_AZ = (AZ - SUR%STA(J4)%AZ_CUR) 
                       DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
!
! -------------------- Check whether the shortest azimuthal move is possible
!
                       IF ( DIF_AZ > 0.0D0 ) THEN
                            IF ( SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J4)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                                 DIST_AZ = DIF_AZ
                              ELSE 
                                 DIST_AZ = (PI2 - DIF_AZ)
                            END IF
                          ELSE ! if ( DIF_AZ .LE. 0.0D0 ) then
                            IF ( SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J4)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                                 DIST_AZ = -DIF_AZ
                               ELSE 
                                 DIST_AZ = (PI2 + DIF_AZ)
                            END IF
                       END IF
!                       
                       IF ( DIST_AZ > SUR%STA(J4)%SLEW_RATE_AZ**2/SUR%STA(J4)%SLEW_ACCL_AZ ) THEN
                            SLEW_AZ = DIST_AZ/SUR%STA(J4)%SLEW_RATE_AZ + &
     &                                SUR%STA(J4)%SLEW_RATE_AZ/SUR%STA(J4)%SLEW_ACCL_AZ
                          ELSE 
                            SLEW_AZ = 2.D0*DSQRT(DIST_AZ/SUR%STA(J4)%SLEW_ACCL_AZ)
                       END IF
                     ELSE IF ( SUR%STA(J4)%MOUNT_TYPE == MT__EQUAT ) THEN
                       IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                            ALP = SUR%SOU(J3)%ALPHA
                            DEL = SUR%SOU(J3)%DELTA
                         ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                            ALP = SUR%SO2(J3)%ALPHA
                            DEL = SUR%SO2(J3)%DELTA
                         ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                            ALP = SUR%CAL(J3)%ALPHA
                            DEL = SUR%CAL(J3)%DELTA
                       END IF
!
! -------------------- Equatorial mounting
!
                       DIST_DEL = DABS(DEL - SUR%STA(J4)%DEL_CUR)
!
! -------------------- Compute slewing time for declination and store it in
! -------------------- SLEW_EL
!
                       IF ( DIST_DEL > SUR%STA(J4)%SLEW_RATE_EL**2/SUR%STA(J4)%SLEW_ACCL_EL ) THEN
                            SLEW_EL = DIST_EL/SUR%STA(J4)%SLEW_RATE_EL + &
     &                                SUR%STA(J4)%SLEW_RATE_EL/SUR%STA(J4)%SLEW_ACCL_EL
                          ELSE 
                            SLEW_EL = 2.D0*DSQRT(DIST_EL/SUR%STA(J4)%SLEW_ACCL_EL)
                       END IF
!
! -------------------- Compute slewing time for hour angle keeping in mind
! -------------------- cable wrap limits. Store it in SLEW_AZ
!
                       DIF_HA = (HA - SUR%STA(J4)%HA_CUR) 
                       DIF_HA = DIF_HA - PI2*IDNINT(DIF_HA/PI2)
!
! -------------------- Check whether the shortest hour angle move is possible
!
                       IF ( DIF_HA > 0.0D0 ) THEN
                            IF ( SUR%STA(J4)%HA_ACC_CUR + DIF_HA < SUR%STA(J4)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                                 DIST_HA = DIF_HA
                              ELSE 
                                 DIST_HA = (PI2 - DIF_HA)
                            END IF
                          ELSE ! if ( DIF_HA .LE. 0.0D0 ) then
                            IF ( SUR%STA(J4)%HA_ACC_CUR + DIF_HA > SUR%STA(J4)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                                 DIST_HA = -DIF_HA
                               ELSE 
                                 DIST_HA = (PI2 + DIF_HA)
                            END IF
                       END IF
!                       
                       IF ( DIST_HA > SUR%STA(J4)%SLEW_RATE_AZ**2/SUR%STA(J4)%SLEW_ACCL_AZ ) THEN
                            SLEW_AZ = DIST_HA/SUR%STA(J4)%SLEW_RATE_AZ + &
     &                                SUR%STA(J4)%SLEW_RATE_AZ/SUR%STA(J4)%SLEW_ACCL_AZ
                          ELSE 
                            SLEW_AZ = 2.D0*DSQRT(DIST_HA/SUR%STA(J4)%SLEW_ACCL_AZ)
                       END IF
                     ELSE 

                       CALL ERR_LOG ( 1682, IUER, 'SUR_FIND_SEQ', &
     &                     'Unsupported mounting type' )
                       RETURN 
                  END IF
!
                  IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
                       SLEW_STA(J4) = MAX ( SLEW_EL+SUR%STA(J4)%TIME_SETTLE_EL, &
     &                                      SLEW_AZ + SUR%STA(J4)%TIME_SETTLE_AZ) + &
     &                                MAX ( SUR__SLEW_EXTRA_TIM, + SUR%STA(J4)%POSTOB )
                     ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
                       SLEW_STA(J4) = MAX ( SLEW_EL+SUR%STA(J4)%TIME_SETTLE_EL, &
     &                                      SLEW_AZ+SUR%STA(J4)%TIME_SETTLE_AZ  ) + &
     &                                SUR%STA(J4)%POSTOB 
                  END IF
 440           CONTINUE 
!
! ------------ Find maximum slewing time over all stations
!
               IND_SLEW = MAX_LIST_R8 ( SUR%L_STA, SLEW_STA )
               SLEW_TIME_MAX = SLEW_STA(IND_SLEW)
               K_SOU = K_SOU + 1
!
               IF ( FL_TAPE_CHANGE ) THEN
!
! ----------------- If the tape has been immediately changed, set slewing
! ----------------- time to zero, since we assume it was enough time during
! ----------------- tape change
!
                    SLEW_SRC(J3)  = (MJD_OBS - SUR%MJD_OBS_END(SUR%L_SCN))*86400.0D0 + &
     &                              (TAI_OBS - SUR%TAI_OBS_END(SUR%L_SCN))
                    SLEW_TIME_MAX = 0.0D0
                    SCORE(K_SOU)  = 0.01D0
                  ELSE 
!
! ----------------- Set score for non tape-change case
!
                    SLEW_SRC(J3) = SLEW_TIME_MAX
                    SCORE(K_SOU) = 100.0D0/MAX(1.0D0,(SLEW_TIME_MAX - SUR%PREOBS_SHORT))
               END IF
               IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
                    SCORE(K_SOU) = SCORE(K_SOU) + 0.2D0*EL_SCAN_MIN/DEG__TO__RAD
                  ELSE IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_02' ) THEN
                    SCORE(K_SOU) = SCORE(K_SOU) + 0.002D0*EL_SCAN_MIN/DEG__TO__RAD
               END IF
!
               IF ( CUR_TYP == SUR__TYP_TAG  ) THEN
!
! ----------------- Update of the score in the case of a low-declination source
!
                    IF ( SUR%SOU(J3)%DELTA < 20.0D0*DEG__TO__RAD ) THEN
                         SCORE(K_SOU) = SCORE(K_SOU) + &
     &                       30.0D0*( 20.0D0*DEG__TO__RAD - SUR%SOU(J3)%DELTA )
                    END IF
               END IF
!!!
!               IF ( CUR_TYP == SUR__TYP_SEC ) THEN
!                    IF ( SUR%SO2(J3)%DELTA < 20.0D0*DEG__TO__RAD ) THEN
!                         SCORE(K_SOU) = SCORE(K_SOU) + &
!     &                       10.0D0*( 20.0D0*DEG__TO__RAD - SUR%SO2(J3)%DELTA )
!                    END IF
!               END IF
!!!
               IND_SCORE(K_SOU) = J3
 430        CONTINUE 
            IF ( K_SOU > 0 ) GOTO 820
 420     CONTINUE 
 820     CONTINUE 
!
         IF ( K_SOU == 0 ) THEN
              CALL ERR_LOG ( 1683, IUER, 'SUR_FIND_SEQ', 'Trap of internal '// &
     &            'control: no sources were found' )
              RETURN 
         END IF
!
! ------ Get the source index in SRC%SOU or SRC%SO2
!
         IND_SRC = IND_SCORE( MAX_LIST_R8 ( K_SOU, SCORE ) )
         IF ( FL_TAPE_CHANGE ) THEN
              SLEW_TIME_MAX = 0.0D0
            ELSE 
              SLEW_TIME_MAX = SLEW_SRC(IND_SRC) 
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
         END IF
!
! ------ Check: is it time to change the tape?
!
         IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01'                 .AND. &
     &        ( (MJD_OBS - SUR%MJD_TAPE_START_CUR)*86400.0D0 + &
     &          (TAI_OBS - SUR%TAI_TAPE_START_CUR) + SUR%PREOBS_LONG + &
     &           SUR%SCAN_LEN > SUR%TAPE_LENGTH ) ) THEN
!
! ----------- Yes, it is just time. Reset the current time
!
              TIM_DIF = (MJD_OBS - SUR%MJD_CUR)*86400.0D0 + &
     &                  (TAI_OBS - SUR%TAI_CUR)
              IF ( SUR%TAPE_CHANGE_TIME > 0.0D0  .AND. &
     &             TIM_DIF < SUR%TAPE_CHANGE_TIME ) THEN
!
                   SUR%TAI_CUR = SUR%TAI_CUR + SUR%TAPE_CHANGE_TIME
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
! ----------- Increment the tape counter
!
              SUR%L_TAP = SUR%L_TAP + 1
!
! ----------- Set the tape start date
!
              SUR%MJD_TAPE_START_CUR = SUR%MJD_CUR 
              SUR%TAI_TAPE_START_CUR = SUR%TAI_CUR 
              IF ( SUR%TAPE_CHANGE_TIME > 0.0D0 ) FL_TAPE_CHANGE = .TRUE.
              GOTO 410
         END IF
!
! ------ Adjustment for margins, Tsys measurements etc.
!
         IF ( FL_TAPE_CHANGE ) THEN
              SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__TAPE
            ELSE 
              IF ( SUR%PREOBS_SHORT > 0.0 .AND. SUR%SKIP_PREOBS_LONG > 0 ) THEN
                   SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
                   PREOBS_DUR = SUR%PREOBS_SHORT
                   FL_LONG = .FALSE.
                 ELSE
                   SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__SHORT 
                   PREOBS_DUR = SUR%PREOBS_LONG
                   FL_LONG = .FALSE.
              END IF
              DO 450 J5=1,SUR%L_STA
                 IF ( SUR%STA(J5)%TAGALONE ) GOTO 450
                 CALL ERR_PASS ( IUER, IER )
                 CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, &
     &                           TAI_OBS+PREOBS_DUR, J5, &
     &                           IND_SRC, AZ_STA(J5), EL_STA(J5), HA_STA(J5), &
     &                           IER )
 450          CONTINUE 
!
              DO 460 J6=SUR%L_SCN,MAX(1,SUR%L_SCN-SUR%SKIP_PREOBS_LONG+1),-1
                 DO 470 J7=1,SUR%L_STA
                    IF ( SUR%STA(J7)%TAGALONE ) GOTO 470
                    IF ( SUR%SCAN_TYPE(J6) == SUR__LONG  .OR.  &
     &                   SUR%SCAN_TYPE(J6) == SUR__TAPE        ) THEN
                         FL_LONG = .TRUE.
                         GOTO 860
                      ELSE IF ( SUR%SCAN_TYPE(J6) == SUR__SHORT        .AND. &
     &                          DABS(SUR%EL_OBS(J7,J6) - EL_STA(J7)) >       &
     &                          SUR%EL_CHANGE_TSYS                     ) THEN
                         SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
                         GOTO 860
                    END IF
 470             CONTINUE 
 460          CONTINUE 
 860          CONTINUE 
!
              IF ( .NOT. FL_LONG                    .AND. &
     &             SUR%L_SCN > SUR%SKIP_PREOBS_LONG       ) THEN
                   SUR%SCAN_TYPE(SUR%L_SCN+1) = SUR__LONG
              END IF
         END IF
!
         IF ( SUR%SCAN_TYPE(SUR%L_SCN+1) == SUR__SHORT ) THEN
              TAI_OBS = TAI_OBS + SUR%PREOBS_SHORT
            ELSE 
              TAI_OBS = TAI_OBS + SUR%PREOBS_LONG
         END IF
         UTC_OBS_INT = IDNINT ( TAI_OBS - SUR%UTC_M_TAI + 1.0D0 )
         IF ( MOD(UTC_OBS_INT,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
              TAI_OBS = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                  + SUR%UTC_M_TAI
            ELSE 
              TAI_OBS = UTC_OBS_INT + SUR%UTC_M_TAI
         END IF
         IF ( TAI_OBS > 86400.0D0 ) THEN
              TAI_OBS = TAI_OBS - 86400.0D0
              MJD_OBS = MJD_OBS + 1
         END IF
!
         FL_TAPE_CHANGE = .FALSE.
!
! ------ Compute final azimuth and elevation of the observation, since
! ------ now we know the time when it will happen
!
         DO 480 J8=1,SUR%L_STA
            IF ( SUR%STA(J8)%TAGALONE ) GOTO 480
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, TAI_OBS, J8, &
     &                      IND_SRC, AZ, EL, HA, IER )
!
! --------- Update accumaulative aziumth in order to take into account 
! --------- cable wrap
!
            IF ( SUR%STA(J8)%MOUNT_TYPE == MT__ALTAZ ) THEN
                 DIF_AZ = (AZ - SUR%STA(J8)%AZ_CUR) 
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J8)%AZ_ACC_MAX ) THEN
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + &
     &                                              DIF_AZ
                           SUR%STA(J8)%HA_ACC_CUR = HA
                         ELSE 
!
! ------------------------ The shortest way is not possible, move the longest way 
! ------------------------ counter-clock-wise
!
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR - &
     &                                              (PI2 - DIF_AZ)
                           SUR%STA(J8)%HA_ACC_CUR = HA
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J8)%AZ_ACC_MIN ) THEN
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + &
     &                                              DIF_AZ
                           SUR%STA(J8)%HA_ACC_CUR = HA
                         ELSE 
!
! ------------------------ The shortest way is not possible, move the longest way 
! ------------------------ clock-wise
!
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + &
     &                                              (PI2 + DIF_AZ)
                           SUR%STA(J8)%HA_ACC_CUR = HA
                     END IF
                END IF
              ELSE IF ( SUR%STA(J8)%MOUNT_TYPE == MT__EQUAT ) THEN
                 DIF_HA = (HA - SUR%STA(J8)%HA_CUR) 
                 DIF_HA = DIF_HA - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J8)%HA_ACC_CUR + DIF_AZ < SUR%STA(J8)%AZ_ACC_MAX ) THEN
                           SUR%STA(J8)%HA_ACC_CUR = SUR%STA(J8)%HA_ACC_CUR + DIF_HA
                         ELSE 
!
! ------------------------ The shortest way is not possible, move the longest way 
! ------------------------ counter-clock-wise
!
                           SUR%STA(J8)%HA_ACC_CUR = SUR%STA(J8)%HA_ACC_CUR - &
     &                                              (PI2 - DIF_HA)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J8)%HA_ACC_CUR + DIF_HA > SUR%STA(J8)%AZ_ACC_MIN ) THEN
                           SUR%STA(J8)%HA_ACC_CUR = SUR%STA(J8)%HA_ACC_CUR + DIF_HA
                         ELSE 
!
! ----------------------- The shortest way is not possible, move the longest way 
! ----------------------- clock-wise
!
                          SUR%STA(J8)%HA_ACC_CUR = SUR%STA(J8)%HA_ACC_CUR + &
     &                                             (PI2 + DIF_HA)
                     END IF
                END IF
            END IF
!
            IF ( EL < SUR%STA(J8)%EL_MIN .OR. &
     &           EL > SUR%STA(J8)%EL_MAX      ) THEN
!
                 WRITE ( 6, * ) ' Trap of internal control: station '// &
     &                            SUR%STA(J8)%NAME//' Epoch: '// &
     &                            MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )// &
     &                          ' Elevation angle: ', EL/DEG__TO__RAD
                 WRITE ( 6, * ) ' TRY=',TRY, ' CUR_TYP=',CUR_TYP
!@               CALL ERR_LOG ( 1684, IUER, 'SUR_FIND_SEQ', 'Trap of '// &
!@     &               'internal control' )
!@                   RETURN 
            END IF
!
            IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                 ALP = SUR%SOU(IND_SRC)%ALPHA
                 DEL = SUR%SOU(IND_SRC)%DELTA
              ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                 ALP = SUR%SO2(IND_SRC)%ALPHA
                 DEL = SUR%SO2(IND_SRC)%DELTA
              ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                 ALP = SUR%CAL(IND_SRC)%ALPHA
                 DEL = SUR%CAL(IND_SRC)%DELTA
            END IF
!
            SUR%STA(J8)%AZ_CUR  = AZ
            SUR%STA(J8)%EL_CUR  = EL
            SUR%STA(J8)%HA_CUR  = HA
            SUR%STA(J8)%ALP_CUR = ALP
            SUR%STA(J8)%DEL_CUR = DEL
            SUR%EL_OBS(J8,SUR%L_SCN+1) = SUR%STA(J8)%EL_CUR 
            SUR%AZ_OBS(J8,SUR%L_SCN+1) = SUR%STA(J8)%AZ_CUR 
            SUR%HA_OBS(J8,SUR%L_SCN+1) = SUR%STA(J8)%HA_CUR 
            SUR%AZ_ACC_OBS(J8,SUR%L_SCN+1) = SUR%STA(J8)%AZ_ACC_CUR 
            SUR%HA_ACC_OBS(J8,SUR%L_SCN+1) = SUR%STA(J8)%HA_ACC_CUR 
 480     CONTINUE 
!
! ------ Update scan counter 
!
         SUR%L_SCN = SUR%L_SCN + 1
         IF ( CUR_TYP == SUR__TYP_TAG ) THEN
              SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
              SUR%SOU(IND_SRC)%FL_USE = .TRUE.
            ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
              SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
              SUR%SO2(IND_SRC)%FL_USE = .TRUE.
            ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
!
! ----------- Set epoch of the last calibrator obseration (now)
!
              SUR%MJD_CALIB_START_CUR = MJD_OBS
              SUR%TAI_CALIB_START_CUR = TAI_OBS
         END IF
!
         SUR%IND_SRC(SUR%L_SCN) = IND_SRC
         SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
         SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
         SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
         SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS
         SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + SUR%SCAN_LEN
         IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
         END IF
         SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN) 
         SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) 
         SUR%SRC_TYP(SUR%L_SCN) = CUR_TYP
         IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
              SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
              SUR%L_SCN_CAL = SUR%L_SCN_CAL + 1
         END IF
         SUR%OBS_STA(1:SUR__M_STA,SUR%L_SCN) = SUR__USED
!
!@         IF ( SUR%L_OBS_TAG .GE. SUR%L_SOU ) GOTO 810 ! end of work
         IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.D0 + &
     &        (SUR%TAI_OBS_END(SUR%L_SCN) - SUR%TAI_STOP - SUR%POSTSES_INTERVAL) > 0.0D0 ) THEN
!
! ----------- End of the session
!
              SUR%L_SCN = SUR%L_SCN - 1
              SUR%L_OBS_TAG = SUR%L_OBS_TAG - 1
              SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
              GOTO 810
         END IF
!
! ------ Check is it this time to observe the next amplitude calibrator
!
         IF ( (MJD_OBS - SUR%MJD_CALIB_START_CUR)*86400.0D0 + &
     &        (TAI_OBS - SUR%TAI_CALIB_START_CUR) + SUR%PREOBS_LONG + SUR%SCAN_LEN > &
     &        SUR%CALIB_LEN ) THEN
!
              CUR_TYP = SUR__TYP_CAL
            ELSE 
!
! ----------- The next source swill be target (not amplitude calibrator)
!
              CUR_TYP = SUR__TYP_TAG 
         END IF
 410  CONTINUE 
 810  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_FIND_SEQ  !#!#
