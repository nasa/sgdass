      FUNCTION   SUR_SLEW_TIME ( SUR, VTD, CUR_TYP, IND_SRC, IND_SRC_LAST, &
                                 LAST_TYP, IND_STA, PROC_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_SLEW_TIME                                              *
! *                                                                      *
! * ### 19-JAN-2008  SUR_SLEW_TIME  v4.6 (c)  L. Petrov  26-MAR-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IND_STA, PROC_CODE, IUER
      REAL*8     SUR_SLEW_TIME
      INTEGER*4  CUR_TYP, IND_SRC, IND_SRC_LAST, LAST_TYP
      INTEGER*4  J1, J2, J3, J4, J5, MJD_OBS, IND_SLEW, N_ITER, K_STA, &
     &           SCA_STA_PREV, IER
      REAL*8     SLEW_TIME_MAX, SLEW_STA(SUR__M_STA), TAI_OBS, AZ, EL, HA, &
     &           A, B, DIST_A, DIST_B, DIST_EL, DIST_DEL, DIST_AZ, DIST_HA, &
     &           A_LAST, B_LAST, ALP, DEL, DIST_AZ_1ST(SUR__M_STA), TIM_LO
      REAL*8     AZ__MARGIN, EL__MARGIN
      PARAMETER  ( AZ__MARGIN = 0.25D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 0.25D0*DEG__TO__RAD  )
      REAL*8     EL_SCAN_MIN, DIF_AZ, DIF_HA, SLEW_EL, SLEW_AZ
      CHARACTER  STR*128, STR1*128
      LOGICAL*1  FL_VLBA, FL_CHECK
      INTEGER*4  MV 
      PARAMETER  ( MV = 11 )
      CHARACTER  VLBA_STA(MV)*8
!
! --- VLBA stations and GBT
!
      DATA       VLBA_STA / &
     &                      'BR-VLBA ', &
     &                      'FD-VLBA ', &
     &                      'HN-VLBA ', &
     &                      'KP-VLBA ', &
     &                      'LA-VLBA ', &
     &                      'MK-VLBA ', &
     &                      'NL-VLBA ', &
     &                      'OV-VLBA ', &
     &                      'PIETOWN ', &
     &                      'SC-VLBA ', &
     &                      'GBT-VLBA' &
     &                     /
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*1, EXTERNAL :: SUR_CHECK_VIS
      INTEGER*4, EXTERNAL :: MAX_LIST_R8, LTM_DIF
!
! --- Set approximate slewing time for the first iteration
!
      SUR_SLEW_TIME = SUR%AVR_SLEW_TIME
      IF ( PROC_CODE == SUR__FINE ) THEN
           N_ITER = 2
         ELSE IF ( PROC_CODE == SUR__2STA   ) THEN
           N_ITER = 2
         ELSE IF ( PROC_CODE == SUR__3STA   ) THEN
           N_ITER = 2
         ELSE IF ( PROC_CODE == SUR__COARSE ) THEN
           N_ITER = 2
      END IF
!
! --- Two iterations for computing slewing time
!
      DIST_AZ_1ST = 0.0D0 ! Initializtion of distance over azimuth in the 1st iteration
      DO 410 J1=1,N_ITER
         MJD_OBS = SUR%MJD_CUR
         TAI_OBS = SUR%TAI_CUR + SUR_SLEW_TIME
!
         IF ( TAI_OBS > 86400.0D0 ) THEN
              MJD_OBS = MJD_OBS + 1
              TAI_OBS = TAI_OBS - 86400.0D0
         END IF
!
         EL_SCAN_MIN = PI2
         DO 420 J2=1,SUR%L_STA
            FL_VLBA = .FALSE.
            IF ( LTM_DIF ( 0, MV, VLBA_STA, SUR%STA(J2)%NAME ) > 0 ) THEN
                 FL_VLBA = .TRUE.
            END IF
!
! --------- Get the index of the last used scan
!
            SCA_STA_PREV = 0
            IF ( SUR%L_SCN > 1 ) THEN
                 DO 430 J3=SUR%L_SCN-1,1,-1
                    IF ( SUR%OBS_STA(J2,J3) == SUR__USED ) THEN
                         SCA_STA_PREV = J3
                         GOTO 830
                    END IF
 430             CONTINUE 
 830             CONTINUE 
            END IF
!
! --------- Get time left-over -- time between the end time of the last scan 
! --------- the J2-th station observed and the end time of last common scan
!
            TIM_LO = 0.0D0
            IF ( SCA_STA_PREV > 11110 ) THEN
                 TIM_LO = (SUR%TAI_OBS_END(SUR%L_SCN-1) - SUR%TAI_OBS_END(SCA_STA_PREV)) + &
     &                    (SUR%MJD_OBS_END(SUR%L_SCN-1) - SUR%MJD_OBS_END(SCA_STA_PREV))*86400.0D0
            END IF
!
            SLEW_STA(J2) = -1.0D0 ! Initialization
!
! --------- The program works in two modes: all stations, except tag-alone,
! --------- or in this station only mode.
!
            IF ( IND_STA == 0 .OR. IND_STA == -1 .OR. IND_STA == -2 ) THEN
                 CONTINUE 
                 IF ( SUR%STA(J2)%TAGALONE ) THEN 
                      IF ( IUER == 860 ) THEN
                           WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 1
                      END IF
                      GOTO 420 ! ???????
                 END IF
               ELSE
                 IF ( IND_STA .NE. J2 ) THEN
                      IF ( IUER == 860 ) THEN
                           WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 2
                      END IF
                      GOTO 420
                 END IF
            END IF
!
! --------- Compute for each station azimuth, elevation and hour angle
!
            CALL ERR_PASS ( IUER, IER )
            IF ( IER == -6 ) IER = -1
            IF ( IUER == 860 ) IER = -1
            CALL SUR_AZEL ( SUR, VTD, CUR_TYP, MJD_OBS, TAI_OBS, &
                            J2, IND_SRC, AZ, EL, HA, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1761, IUER, 'SUR_SLEW_TIME', &
                     'Error in computing azimuth and elevation' )
                 SUR_SLEW_TIME = -1.0D0
                 RETURN
            END IF
            IF ( FL_VLBA .AND. SUR%STA(J2)%EL_CUR < -100.0D0 ) THEN
!
! -------------- VLBA should start the 1st scan in the neutral sector
!
                 IF ( AZ < SUR%STA(J2)%AZ_RANGE(2) + AZ__MARGIN .OR. &
     &                AZ > SUR%STA(J2)%AZ_RANGE(3) - AZ__MARGIN      ) THEN
                      SUR_SLEW_TIME = -21.0D0
                      IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                           CALL ERR_LOG ( 0, IUER )
                           RETURN
                      END IF
                 END IF
            END IF
!
            IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                 IF ( EL < SUR%SOU(IND_SRC)%EL_MIN + EL__MARGIN )  THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           SLEW_STA(J2) = 0.0D0
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 3
 210                            FORMAT ( '    Sta: ', A, ' El: ', F7.2, ' Reason: ', I2 )
                           END IF
                           GOTO 420
                        ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                            .NOT. SUR%STA(J2)%STICKY                   ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 4
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                           IF ( SUR%STA(J2)%TAGALONE ) THEN
                                IF ( IUER == 860 ) THEN
                                     WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 5
                                END IF
                                GOTO 420
                           END IF
                           SUR_SLEW_TIME = -2.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
               ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                 IF ( EL < SUR%SO2(IND_SRC)%EL_MIN + EL__MARGIN ) THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 6
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 7
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
                           SUR_SLEW_TIME = -3.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
               ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                 IF ( EL < SUR%CAL(IND_SRC)%EL_MIN + EL__MARGIN ) THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 8
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                            .NOT. SUR%STA(J2)%STICKY                   ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 9
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
                           SUR_SLEW_TIME = -4.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
               ELSE IF ( CUR_TYP == SUR__TYP_POC ) THEN
                 IF ( EL < SUR%SOP(IND_SRC)%EL_MIN + EL__MARGIN ) THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 10
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                           SUR_SLEW_TIME = -5.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
               ELSE IF ( CUR_TYP == SUR__TYP_PLA ) THEN
                 IF ( EL < SUR%SOP(IND_SRC)%EL_MIN + EL__MARGIN ) THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 11
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                           SUR_SLEW_TIME = -25.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
            END IF
!
! --------- Check, whether the J2-th station does see the source
!
            IER = -1
            FL_CHECK = SUR_CHECK_VIS ( SUR, J2, CUR_TYP, IND_SRC, AZ, EL, &
     &                                 HA, IER ) 
            IF ( SUR%STA(J2)%MOUNT_TYPE == MT__ALTAZ ) THEN
                 IF ( EL < SUR%STA(J2)%EL_MIN + EL__MARGIN ) FL_CHECK = .FALSE.
                 IF ( EL > SUR%STA(J2)%EL_MAX - EL__MARGIN ) FL_CHECK = .FALSE.
            END IF
            IF ( .NOT. FL_CHECK ) THEN
!
! -------------- Does not see? Set the flag
!
                 IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                      IF ( IUER == 860 ) THEN
                           WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 12
                      END IF
                      SLEW_STA(J2) = 0.0D0
                      GOTO 420
                    ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                        .NOT. SUR%STA(J2)%STICKY                   ) THEN
!
                      IF ( IUER == 860 ) THEN
                           WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 13
                      END IF
                      SLEW_STA(J2) = 0.0D0
                      GOTO 420
                    ELSE
                      IF ( IUER == 860 ) THEN
                           WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 14
                      END IF
                      IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
                      SUR_SLEW_TIME = -6.0D0
                      IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                           CALL ERR_LOG ( 0, IUER )
                           RETURN
                      END IF
                 END IF
            END IF
!
            IF ( SUR%STA(J2)%MOUNT_TYPE == MT__ALTAZ ) THEN
!@                 IF ( AZ < SUR%STA(J2)%AZ_ACC_MIN + AZ__MARGIN .OR. &
!@     &                AZ > SUR%STA(J2)%AZ_ACC_MAX - AZ__MARGIN      ) THEN
!@!
!@                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!@!
!@                           IF ( IUER == 860 ) THEN
!@                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, AZ/DEG__TO__RAD, 1
!@ 220                            FORMAT ( '    Sta: ', A, ' AZ: ', F7.2, ' Reason: ', I2 )
!@                           END IF
!@                           SLEW_STA(J2) = 0.0D0
!@                           GOTO 420
!@                         ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
!@     &                        .NOT. SUR%STA(J2)%STICKY ) THEN
!@!
!@                           IF ( IUER == 860 ) THEN
!@                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, AZ/DEG__TO__RAD, 2
!@                           END IF
!@                           SLEW_STA(J2) = 0.0D0
!@                           GOTO 420
!@                         ELSE
!@                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
!@                           SUR_SLEW_TIME = -7.0D0
!@                           CALL ERR_LOG ( 0, IUER )
!@                           RETURN
!@                      END IF
!@                 END IF
!
                 DIST_EL = DABS(EL - SUR%STA(J2)%EL_CUR)
                 IF ( SUR%STA(J2)%EL_CUR < -100.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      DIST_EL = 0.0D0
                 END IF
                 IF ( DIST_EL > SUR%STA(J2)%SLEW_RATE_EL**2/SUR%STA(J2)%SLEW_ACCL_EL ) THEN
                      SLEW_EL = (DIST_EL - SUR%STA(J2)%SLEW_RATE_EL**2/SUR%STA(J2)%SLEW_ACCL_EL)/ &
     &                          SUR%STA(J2)%SLEW_RATE_EL + &
     &                          2.D0*SUR%STA(J2)%SLEW_RATE_EL/SUR%STA(J2)%SLEW_ACCL_EL
                    ELSE
                      SLEW_EL = 2.D0*DSQRT(DIST_EL/SUR%STA(J2)%SLEW_ACCL_EL)
                 END IF
!
! -------------- Compute slewing time for azimuth, keeping in mind
! -------------- cable wrap limits
!
                 DIF_AZ = (AZ - SUR%STA(J2)%AZ_CUR)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DABS(DABS(DIF_AZ) - PI__NUM) < SUR%AZIM_180_MARGIN .AND. &
     &                DABS(SUR%STA(J2)%AZ_CUR) < 20.0D0                  .AND. &
     &                SUR%ALGORITHM .NE. 'SPACECRAFT_01'                       ) THEN
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
                           IF ( IUER == 860 .OR. IUER == 865 ) THEN
                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, AZ/DEG__TO__RAD, 31
 220                            FORMAT ( '    Sta: ', A, ' AZ: ', F7.2, ' Reason: ', I2 )
                           END IF
                           SUR_SLEW_TIME = -8.1D0
                           CALL ERR_LOG ( 0, IUER )
                           RETURN
                        ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                            .NOT. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 .OR. IUER == 865 ) THEN
                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, AZ/DEG__TO__RAD, 32
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                        ELSE
                          IF ( SUR%STA(J2)%TAGALONE ) THEN
                               IF ( IUER == 860 ) THEN
                                    WRITE ( 6, 210 ) SUR%STA(J2)%NAME, EL/DEG__TO__RAD, 15
                               END IF
                               GOTO 420
                           END IF
                           SUR_SLEW_TIME = -8.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
!
! -------------- Check whether the shortest azimuthal move is possible
!
                 IF ( DIF_AZ > 0.0D0 ) THEN
                      IF ( SUR%STA(J2)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J2)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                           DIST_AZ = DIF_AZ
                        ELSE
                           DIST_AZ = (PI2 - DIF_AZ)
                      END IF
                    ELSE ! if ( DIF_AZ .LE. 0.0D0 ) then
                      IF ( SUR%STA(J2)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J2)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                           DIST_AZ = -DIF_AZ
                         ELSE
                           DIST_AZ = (PI2 + DIF_AZ)
                      END IF
                 END IF
                 IF ( SUR%STA(J2)%AZ_ACC_CUR < -100.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      DIF_AZ  = 0.0D0
                      DIST_AZ = 0.0D0
                 END IF
!
                 IF ( DIST_AZ > SUR%STA(J2)%SLEW_RATE_AZ**2/SUR%STA(J2)%SLEW_ACCL_AZ ) THEN
                      SLEW_AZ = (DIST_AZ - SUR%STA(J2)%SLEW_RATE_AZ**2/SUR%STA(J2)%SLEW_ACCL_AZ)/SUR%STA(J2)%SLEW_RATE_AZ + &
     &                          2.0D0*SUR%STA(J2)%SLEW_RATE_AZ/SUR%STA(J2)%SLEW_ACCL_AZ
                    ELSE
                      SLEW_AZ = 2.D0*DSQRT(DIST_AZ/SUR%STA(J2)%SLEW_ACCL_AZ)
                 END IF
                 IF ( J1 == 2 .AND. DABS(DIST_AZ_1ST(J2) - DIST_AZ) > PI__NUM )THEN
                      SUR_SLEW_TIME = -19.0D0
                      IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                            CALL ERR_LOG ( 0, IUER )
                            RETURN
                      END IF
                 END IF
!
                 IF ( SUR%ALGORITHM .NE. 'SPACECRAFT_01'         .AND. &
     &                DIST_AZ .GE. PI__NUM - SUR%AZIM_180_MARGIN .AND. &
     &                DIST_AZ .LE. PI__NUM + SUR%AZIM_180_MARGIN       ) THEN
!
                      SUR_SLEW_TIME = -20.0D0
                      IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                            CALL ERR_LOG ( 0, IUER )
                            RETURN
                      END IF
                 END IF
                 IF ( J1 == 1 ) DIST_AZ_1ST(J2) = DIST_AZ
               ELSE IF ( SUR%STA(J2)%MOUNT_TYPE == MT__EQUAT ) THEN
                 IF ( HA < SUR%STA(J2)%AZ_ACC_MIN + AZ__MARGIN .OR. &
     &                HA > SUR%STA(J2)%AZ_ACC_MAX - AZ__MARGIN      ) THEN
!
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 230 ) SUR%STA(J2)%NAME, HA/DEG__TO__RAD, 1
 230                            FORMAT ( '    Sta: ', A, ' HA: ', F7.2, ' Reason: ', I2 )
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                         ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                             .NOT. SUR%STA(J2)%STICKY                   ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 230 ) SUR%STA(J2)%NAME, HA/DEG__TO__RAD, 2
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                         ELSE
                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
                           SUR_SLEW_TIME = -9.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                     END IF
                 END IF
                 IF ( SUR%STA(J2)%DEL_CUR > -PI__NUM ) THEN
                      ALP = SUR%STA(J2)%ALP_CUR 
                      DEL = SUR%STA(J2)%DEL_CUR 
                    ELSE
                      IF ( LAST_TYP == SUR__TYP_TAG ) THEN
                           ALP = SUR%SOU(IND_SRC_LAST)%ALPHA
                           DEL = SUR%SOU(IND_SRC_LAST)%DELTA
                        ELSE IF ( LAST_TYP == SUR__TYP_SEC ) THEN
                           ALP = SUR%SO2(IND_SRC_LAST)%ALPHA
                           DEL = SUR%SO2(IND_SRC_LAST)%DELTA
                        ELSE IF ( LAST_TYP == SUR__TYP_CAL ) THEN
                           ALP = SUR%CAL(IND_SRC_LAST)%ALPHA
                           DEL = SUR%CAL(IND_SRC_LAST)%DELTA
                        ELSE IF ( LAST_TYP == SUR__TYP_POC ) THEN
                           ALP = SUR%SOP(IND_SRC_LAST)%ALPHA
                           DEL = SUR%SOP(IND_SRC_LAST)%DELTA
                        ELSE IF ( LAST_TYP == SUR__TYP_CAL ) THEN
                           ALP = SUR%PLA(IND_SRC_LAST)%ALPHA
                           DEL = SUR%PLA(IND_SRC_LAST)%DELTA
                      END IF
                 END IF
!
! -------------- Equatorial mounting
!
                 DIST_DEL = DABS(DEL - SUR%STA(J2)%DEL_CUR)
!
! -------------- Compute slewing time for declination and store it in
! -------------- SLEW_EL
!
                 IF ( DIST_DEL > SUR%STA(J2)%SLEW_RATE_EL**2/SUR%STA(J2)%SLEW_ACCL_EL ) THEN
                      SLEW_EL = DIST_DEL/SUR%STA(J2)%SLEW_RATE_EL + &
     &                          SUR%STA(J2)%SLEW_RATE_EL/SUR%STA(J2)%SLEW_ACCL_EL
                    ELSE
                      SLEW_EL = 2.D0*DSQRT(DIST_DEL/SUR%STA(J2)%SLEW_ACCL_EL)
                 END IF
                 IF ( SUR%STA(J2)%EL_CUR < -100.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      SLEW_EL = 0.0D0
                 END IF
!
! -------------- Compute slewing time for hour angle keeping in mind
! -------------- cable wrap limits. Store it in SLEW_AZ
!
                 DIF_HA = (HA - SUR%STA(J2)%HA_CUR)
!
! -------------- Check whether the shortest hour angle move is possible
!
                  IF ( DABS(DIF_HA) < P2I ) THEN
                       DIST_HA = DABS(DIF_HA)
                    ELSE ! if ( DIF_HA .LE. 0.0D0 ) then
                      IF ( SUR%STA(J2)%HA_ACC_CUR + DIF_HA > SUR%STA(J2)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                           DIST_HA = -DIF_HA
                         ELSE
                           DIST_HA = (PI2 + DIF_HA)
                      END IF
                 END IF
!
                 IF ( DIST_HA > SUR%STA(J2)%SLEW_RATE_AZ**2/SUR%STA(J2)%SLEW_ACCL_AZ ) THEN
                      SLEW_AZ = DIST_HA/SUR%STA(J2)%SLEW_RATE_AZ + &
     &                          SUR%STA(J2)%SLEW_RATE_AZ/SUR%STA(J2)%SLEW_ACCL_AZ
                    ELSE
                      SLEW_AZ = 2.D0*DSQRT(DIST_HA/SUR%STA(J2)%SLEW_ACCL_AZ)
                 END IF
                 IF ( SUR%STA(J2)%AZ_CUR < -100.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      SLEW_AZ = 0.0D0
                 END IF
               ELSE IF ( SUR%STA(J2)%MOUNT_TYPE == MT__XY_E ) THEN
!
! -------------- XY-E mounting
!
                 IF ( DABS(DTAN(SUR%STA(J2)%EL_CUR)) < 1.D-6 ) THEN
                      A_LAST = P2I
                    ELSE
                      A_LAST = DATAN ( DCOS(SUR%STA(J2)%AZ_CUR)/DTAN(SUR%STA(J2)%EL_CUR) )
                      B_LAST = DASIN ( DSIN(SUR%STA(J2)%AZ_CUR)*DCOS(SUR%STA(J2)%EL_CUR) )
                 END IF
!
                 IF ( DABS(DTAN(EL)) < 1.D-6 ) THEN
                      A = P2I
                    ELSE
                      A = DATAN ( DCOS(AZ)/DTAN(EL) )
                      B = DASIN ( DSIN(AZ)*DCOS(EL) )
                 END IF
!
!@                 IF ( A < SUR%STA(J2)%AZ_ACC_MIN + AZ__MARGIN .OR. &
!@     &                A > SUR%STA(J2)%AZ_ACC_MAX - AZ__MARGIN      ) THEN
!@!
!@                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
!@     &                     SUR%STA(J2)%STICKY                         ) THEN
!@!
!@                           SLEW_STA(J2) = 0.0D0
!@                           IF ( IUER == 860 ) THEN
!@                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, A/DEG__TO__RAD, 33
!@                           END IF
!@                           GOTO 420
!@                         ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
!@     &                     .NOT. SUR%STA(J2)%STICKY                           ) THEN
!@!
!@                           IF ( IUER == 860 ) THEN
!@                                WRITE ( 6, 220 ) SUR%STA(J2)%NAME, A/DEG__TO__RAD, 34
!@                           END IF
!@                           SLEW_STA(J2) = 0.0D0
!@                           GOTO 420
!@                         ELSE
!@                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
!@                           SUR_SLEW_TIME = -10.0D0
!@                           CALL ERR_LOG ( 0, IUER )
!@                           RETURN
!@                      END IF
!@                 END IF
!
                 IF ( B < SUR%STA(J2)%EL_MIN + EL__MARGIN .OR. &
     &                B > SUR%STA(J2)%EL_MAX - EL__MARGIN      ) THEN
!
                      IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. SUR%STA(J2)%STICKY ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, B/DEG__TO__RAD, 16
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                         ELSE IF ( ( IND_STA == -1 .OR. IND_STA == -2 ) .AND. &
     &                     .NOT. SUR%STA(J2)%STICKY                           ) THEN
!
                           IF ( IUER == 860 ) THEN
                                WRITE ( 6, 210 ) SUR%STA(J2)%NAME, B/DEG__TO__RAD, 17
                           END IF
                           SLEW_STA(J2) = 0.0D0
                           GOTO 420
                         ELSE
                           IF ( SUR%STA(J2)%TAGALONE ) GOTO 420
                           SUR_SLEW_TIME = -11.0D0
                           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA ) ) THEN
                                CALL ERR_LOG ( 0, IUER )
                                RETURN
                           END IF
                      END IF
                 END IF
!
                 DIST_A = DABS(A - A_LAST)
                 DIST_B = DABS(B - B_LAST)
!
                 SLEW_AZ = DIST_A/SUR%STA(J2)%SLEW_RATE_AZ + &
     &                     SUR%STA(J2)%SLEW_RATE_AZ/SUR%STA(J2)%SLEW_ACCL_AZ
                 SLEW_EL = DIST_B/SUR%STA(J2)%SLEW_RATE_EL + &
     &                     SUR%STA(J2)%SLEW_RATE_EL/SUR%STA(J2)%SLEW_ACCL_EL
               ELSE
                 CALL ERR_LOG ( 1641, IUER, 'SUR_SLEW_TIME', &
     &               'Unsupported mounting type >>'//SUR%STA(J2)%MOUNT_TYPE//'<< '// &
     &               ' for station '//SUR%STA(J2)%NAME )
                 SUR_SLEW_TIME = -12.0D0
                 RETURN
            END IF
!
            IF ( SUR%ALGORITHM == 'FRINGE_SEARCH_01' ) THEN
                 SLEW_STA(J2) = MAX ( SLEW_EL + SUR%STA(J2)%TIME_SETTLE_EL,    &
     &                                SLEW_AZ + SUR%STA(J2)%TIME_SETTLE_AZ ) + &
     &                          MAX ( SUR__SLEW_EXTRA_TIM, SUR%STA(J2)%PREOB + SUR%STA(J2)%POSTOB )
               ELSE
                 SLEW_STA(J2) = MAX ( SLEW_EL + SUR%STA(J2)%TIME_SETTLE_EL,    &
     &                                SLEW_AZ + SUR%STA(J2)%TIME_SETTLE_AZ ) + &
     &                                SUR%STA(J2)%PREOB + SUR%STA(J2)%POSTOB
                 IF ( FL_VLBA .AND. SLEW_STA(J2) > 8.0 ) THEN
!
! ------------------- A fiddle for VLBA
!
                      SLEW_STA(J2) = SLEW_STA(J2) - 2.0D0
                 END IF
            END IF
!
! --------- Correct slewing time at a given station for left-over time from 
! --------- the last scan that station observed. 
! --------- NB: we want slewing time to be at least one second long
!
            SLEW_STA(J2) = MAX ( 0.999999D0, SLEW_STA(J2) - TIM_LO )
            IF ( ( IUER  == 860 .OR. IUER  == -6 ) .AND. J1 == N_ITER ) THEN
                 IF ( SUR%STA(J2)%MOUNT_TYPE == MT__EQUAT ) THEN
                      DIST_AZ = DIST_HA
                      DIST_EL = DIST_DEL
                 END IF
                 IF ( SUR%STA(J2)%MOUNT_TYPE == MT__XY_E ) THEN
                      DIST_AZ = DIST_A
                      DIST_EL = DIST_B
                 END IF
                 WRITE ( 6, 110 ) SUR%L_SCN, SUR%STA(J2)%NAME, DIST_EL/DEG__TO__RAD, &
     &                            DIST_AZ/DEG__TO__RAD, SLEW_EL, SLEW_AZ, &
     &                            SLEW_STA(J2), AZ/DEG__TO__RAD, &
     &                            SUR%STA(J2)%AZ_CUR/DEG__TO__RAD, EL/DEG__TO__RAD, TIM_LO
 110             FORMAT ( 'SCA: ', I4, ' Sta: ', A, &
     &                    ' Dif_el: ', F7.2, ' Dif_az: ', F7.2, &
     &                    ' Slew_el: ', F5.1, ' Slew_az: ', F5.1, ' Slew: ', f5.1, &
     &                    ' az: ', f6.1, ' azc: ', f7.1, ' el: ', f5.1, ' Tim_lo: ', F6.1 )
            END IF
 420     CONTINUE
!
! ------ Find maximum slewing time over all stations
!
         IF ( IND_STA == 0  .OR.  IND_STA == -1 .OR.  IND_STA == -2 ) THEN
              IND_SLEW = MAX_LIST_R8 ( SUR%L_STA, SLEW_STA )
              SUR_SLEW_TIME = SLEW_STA(IND_SLEW)
            ELSE
               SUR_SLEW_TIME = SLEW_STA(IND_STA)
         END IF
 410  CONTINUE
!
      IF ( IND_STA == -2 ) THEN
           K_STA = 0
           DO 440 J4=1,SUR%L_STA
              IF ( SLEW_STA(J4) > 0.0D0 ) THEN
                   K_STA = K_STA + 1
              END IF
 440       CONTINUE 
           IF ( .NOT. ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA  ) ) THEN
                IF ( CUR_TYP == SUR__TYP_TAG ) THEN
                     IF ( K_STA < SUR%SOU(IND_SRC)%MIN_STA ) SUR_SLEW_TIME = -13.0D0
                   ELSE IF ( CUR_TYP == SUR__TYP_SEC ) THEN
                     IF ( K_STA < SUR%SO2(IND_SRC)%MIN_STA ) SUR_SLEW_TIME = -14.0D0
                   ELSE IF ( CUR_TYP == SUR__TYP_CAL ) THEN
                     IF ( K_STA < SUR%CAL(IND_SRC)%MIN_STA ) SUR_SLEW_TIME = -15.0D0
                   ELSE IF ( CUR_TYP == SUR__TYP_POC ) THEN
                     IF ( K_STA < SUR%SOP(IND_SRC)%MIN_STA ) SUR_SLEW_TIME = -16.0D0
                   ELSE IF ( CUR_TYP == SUR__TYP_PLA ) THEN
                     IF ( K_STA < SUR%PLA(IND_SRC)%MIN_STA ) SUR_SLEW_TIME = -17.0D0
                 END IF
            END IF
      END IF
!
      IF ( PROC_CODE == SUR__2STA  .OR.  PROC_CODE == SUR__3STA  ) THEN
           SUR_SLEW_TIME = -1.01D0
           CALL SORT_R8 ( SUR%L_STA, SLEW_STA )
           K_STA = 0
           DO 450 J5=1,SUR%L_STA
              IF ( SLEW_STA(J5) > 0.9 ) THEN
                   K_STA = K_STA  + 1
                   IF ( K_STA == 2  .AND.  PROC_CODE == SUR__2STA ) THEN
                        SUR_SLEW_TIME = SLEW_STA(J5)
                        GOTO 850
                     ELSE IF ( K_STA == 3  .AND.  PROC_CODE == SUR__3STA ) THEN
                        SUR_SLEW_TIME = SLEW_STA(J5)
                        GOTO 850
                   END IF
              END IF
 450       CONTINUE 
 850       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER)
      RETURN
      END  FUNCTION  SUR_SLEW_TIME  !#!#
