      SUBROUTINE SUR_ASTRO_FIRST ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_FIRST 
! *                                                                      *
! * ### 12-JAN-2007 SUR_ASTRO_FIRST  v1.10 (c) L. Petrov 20-FEB-2024 ### *
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
      REAL*8     SCORE(SUR__M_SOU), SCORE_MAX
      CHARACTER  STR*128
      REAL*8       SUR__EL_MIN_FIRST, SUR__EL_MAX_FIRST, &
     &             SUR__AZ_MIN_FIRST, SUR__AZ_MAX_FIRST, &
     &             SUR__EL_OPT_FIRST, SUR__AZ_OPT_FIRST
      PARAMETER  ( SUR__EL_MIN_FIRST =  10.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__EL_MAX_FIRST =  84.0D0*DEG__TO__RAD )
      PARAMETER  ( SUR__EL_OPT_FIRST =  50.0D0*DEG__TO__RAD )
      REAL*8     AZ(SUR__M_STA), EL(SUR__M_STA), HA(SUR__M_STA)
      REAL*8     SUR__A_FIRST, SUR__B_FIRST, EL__MARGIN, AZ__MARGIN
      PARAMETER  ( SUR__A_FIRST = 1.0D0 )
      PARAMETER  ( SUR__B_FIRST = 0.1D0 )
      PARAMETER  ( EL__MARGIN = 0.5*DEG__TO__RAD )
      PARAMETER  ( AZ__MARGIN = 4.0*DEG__TO__RAD )
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
      LOGICAL*1  FL_USED
      INTEGER*4  J1, J2, J3, K_STA, I_TYP, IND_SOU, IVAL, SPL_STATUS, IER
      LOGICAL*4  SUR_CHECK_VIS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      IF ( SUR%STA(1)%NAME(1:4) == 'ATCA'     .OR. &
     &     SUR%STA(1)%NAME(1:6) == 'PARKES'        ) THEN
           SUR__AZ_MIN_FIRST =  -40.0D0*DEG__TO__RAD
           SUR__AZ_MAX_FIRST =  170.0D0*DEG__TO__RAD
           SUR__AZ_OPT_FIRST =   30.0D0*DEG__TO__RAD
         ELSE IF ( SUR%STA(1)%NAME(1:8) == 'HOBART26' .OR. &
     &             SUR%STA(1)%NAME(1:7) == 'HOB_MK5'  .OR. &
     &             SUR%STA(1)%NAME(1:7) == 'HART15M'  .OR. &
     &             SUR%STA(1)%NAME(1:8) == 'HOBART13'       ) THEN
           SUR__AZ_MIN_FIRST =  -40.0D0*DEG__TO__RAD
           SUR__AZ_MAX_FIRST =  170.0D0*DEG__TO__RAD
           SUR__AZ_OPT_FIRST =   30.0D0*DEG__TO__RAD
         ELSE
           SUR__AZ_MIN_FIRST =  40.0D0*DEG__TO__RAD
           SUR__AZ_MAX_FIRST = 320.0D0*DEG__TO__RAD
           SUR__AZ_OPT_FIRST = 180.0D0*DEG__TO__RAD
      END IF
!
      SCORE_MAX = -1.D16
      IND_SOU   = 0
      I_TYP = SUR__TYP_TAG 
      SUR%SLEW_DUR = 0.0D0
      DO 410 J1=1,SUR%L_SOU
         SCORE(J1) = 1.0D0
         SUR%NOBS_SRC(J1) = SUR%SOU(J1)%NOBS
         IF ( SUR%ALGORITHM == 'ASTROMET_03' .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_03' .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_02' .OR. &
     &        SUR%ALGORITHM == 'GEODETIC_01'      ) THEN
              IF ( SUR%NOBS_SRC(J1) .GE. SUR%SOU(J1)%NSCA_MAX ) GOTO 410
         END IF            
         IF ( SUR%STA(1)%NAME(1:4) == 'ATCA'  .OR. SUR%STA(1)%NAME(1:6) == 'PARKES' ) THEN
              IF ( SUR%SOU(J1)%DELTA < 20.0D0*DEG__TO__RAD ) THEN
                   SCORE(J1) = SCORE(J1) + &
     &                  10.0D0*(20.0D0*DEG__TO__RAD - SUR%SOU(J1)%DELTA )**2
              END IF
         END IF
!
         IF ( SUR%ALGORITHM == 'GNSS_01' .OR. &
     &        SUR%ALGORITHM == 'GNSS_02'      ) THEN
              IF ( DABS( (SUR%MJD_CUR - SUR%SOU(J1)%MJD_EPOCH)*86400.0D0 + &
     &                    ( SUR%TAI_CUR +           &
     &                      SUR%AVR_SLEW_TIME +     &
     &                      SUR%SOU(J1)%DUR/2.0D0 - &
     &                      SUR%SOU(J1)%TAI_EPOCH)  ) > SUR%SOU(J1)%RANGE ) THEN
                   GOTO 410
              END IF
         END IF
!
         K_STA = 0
         DO 420 J2=1,SUR%L_STA
            FL_USED = .TRUE.
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, I_TYP, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                      J2, J1, AZ(J2), EL(J2), HA(J2), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1681, IUER, 'SUR_ASTRO_FIRST', 'Error in '// &
     &               'computing azimuth and elevation' ) 
                 RETURN 
            END IF
            IF ( LTM_DIF ( 0, MV, VLBA_STA, SUR%STA(J2)%NAME ) > 0 ) THEN
!
! -------------- VLBA should start the 1st scan in the neutral sector
!
                 IF ( AZ(J2) < SUR%STA(J2)%AZ_RANGE(2) + AZ__MARGIN .OR. &
     &                AZ(J2) > SUR%STA(J2)%AZ_RANGE(3) - AZ__MARGIN      ) THEN
!
! ------------------- We put a very small priority, but not zero in order 
! ------------------- to prevent a situation that the schedule cannot start
! ------------------- because there is no sich a source. In that case we
! ------------------- take whatever source we can pick.
!
                      SCORE(J1) = 1.0D-6
                 END IF
            END IF 
!
            IF ( SUR%STA(J2)%LAT_GDT < 0.0 .AND. AZ(J2) > 300.0D0*DEG__TO__RAD ) THEN
                 AZ(J2) = AZ(J2) - PI2
            END IF
!
            IF ( SUR%STA(J2)%MOUNT_TYPE == MT__EQUAT ) THEN
                 IF ( HA(J2) < SUR%STA(J2)%AZ_ACC_MIN  .OR.  &
     &                HA(J2) > SUR%STA(J2)%AZ_ACC_MAX        ) THEN
                      FL_USED = .FALSE.
                 END IF
                 IF ( SUR%SOU(J1)%DELTA < SUR%STA(J2)%EL_MIN .OR. &
     &                SUR%SOU(J1)%DELTA > SUR%STA(J2)%EL_MAX      ) THEN
                      IF ( .NOT. SUR%STA(J2)%TAGALONE ) THEN
                           FL_USED = .TRUE.
                      END IF
                 END IF
               ELSE IF ( SUR%STA(J2)%MOUNT_TYPE == MT__ALTAZ ) THEN
                 IF ( EL(J2) < SUR%STA(J2)%EL_MIN  .OR.  &
     &                EL(J2) > SUR%STA(J2)%EL_MAX        ) THEN
                      IF ( .NOT. SUR%STA(J2)%TAGALONE ) THEN
                           IF ( IVRB .GE. 8 ) THEN
                                WRITE ( 6, * ) 'SUR_ASTRO_FIRST sou: ', SUR%SOU(J1)%J2000_NAME, ' at station ', &
     &                                          SUR%STA(J2)%NAME, ' Elevation: ', SNGL(EL(J2)/DEG__TO__RAD)
                                END IF
                           FL_USED = .TRUE.
                      END IF
                 END IF
            END IF
            SCORE(J1) = SCORE(J1) + &
     &          1.D0/(SUR__A_FIRST**2 + (AZ(J2) - SUR__AZ_OPT_FIRST)**2) + &
     &          1.D0/(SUR__B_FIRST**2 + (EL(J2) - SUR__EL_OPT_FIRST)**2)
            IF ( SUR%STA(J2)%NAME(1:4) == 'ATCA' .OR. &
     &           SUR%STA(J2)%NAME(1:6) == 'PARKES' ) THEN
                 IF ( AZ(J2) < SUR__AZ_MIN_FIRST .OR. AZ(J2) > SUR__AZ_MAX_FIRST ) THEN
                      SCORE(J1) = SCORE(J1) - 1.D8
                 END IF 
               ELSE
                 IF ( AZ(J2) <  SUR__AZ_MIN_FIRST .OR. AZ(J2) > SUR__AZ_MAX_FIRST ) THEN
                      SCORE(J1) = 1.D-4*SCORE(J1)
                 END IF 
            END IF 
            IF ( FL_USED ) K_STA = K_STA + 1
 420     CONTINUE 
!
         IF ( SUR%ALGORITHM == 'SPACECRAFT_01' ) THEN
              CALL CHIN ( SUR%SOU(J1)%J2000_NAME(4:10), IVAL )
              IVAL = IVAL - SUR%UTC_M_TAI
              SCORE(J1) = 1.0
              IF ( ABS(IVAL) < 200 ) THEN
                   SCORE(J1) = 1.D2
                   IF ( ABS(IVAL) < 100 ) THEN
                        SCORE(J1) = 1.D4
                        IF ( ABS(IVAL) < 50 ) THEN
                             SCORE(J1) = 1.D5
                             IF ( ABS(IVAL) < 20 ) THEN
                                  SCORE(J1) = 1.D6
                             END IF              
                        END IF              
                   END IF              
              END IF              
         END IF
!
         IF ( K_STA < SUR%SOU(J1)%MIN_STA ) THEN
              SCORE(J1) = 0.0
         END IF
         IF ( SCORE(J1) > SCORE_MAX ) THEN
              SCORE_MAX = SCORE(J1)
              IND_SOU = J1
         END IF
!
         IF ( IVRB .GE. 6 ) THEN
              WRITE ( 6, * ) 'SUR_ASTRO_FIRST Sou: '//SUR%SOU(J1)%J2000_NAME, ' SCORE: ',SCORE(J1) ! %%%
         END IF
 410  CONTINUE 
!
      IF ( IND_SOU == 0 ) THEN
           CALL ERR_LOG ( 1682, IUER, 'SUR_ASTRO_FIRST', 'No suitable '// &
     &         'source to start has been found' ) 
           RETURN 
      END IF
!
      SUR%L_SCN = 1
      SUR%L_OBS_TAG = 1
      SUR%SOU(IND_SOU)%FL_USE = .TRUE.
      SUR%IND_SRC(SUR%L_SCN)  = IND_SOU
      SUR%L_TAP = 1
      SUR%IND_TAP(SUR%L_SCN)  = SUR%L_TAP 
      SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
      SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_CUR
      SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR
      IF ( SUR%PRESES_INTERVAL .LE. 0.0 ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_CUR + &
     &                                  SUR%SOU(IND_SOU)%DUR + SUR%PREOBS_LONG
         ELSE 
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_CUR + &
     &                                  SUR%SOU(IND_SOU)%DUR
      END IF
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
      END IF
      SUR%SRC_TYP(SUR%L_SCN) = I_TYP
      SUR%L_SCN_SO1 = 1
!    
      SUR%MJD_TAPE_START_CUR = SUR%MJD_OBS_BEG(SUR%L_SCN)
      SUR%TAI_TAPE_START_CUR = SUR%TAI_OBS_BEG(SUR%L_SCN) ! - SUR__TAPE_MOTION 
      IF ( SUR%TAI_TAPE_START_CUR < 0.0D0 ) THEN
           SUR%MJD_TAPE_START_CUR = SUR%MJD_TAPE_START_CUR - 1 
           SUR%TAI_TAPE_START_CUR = SUR%TAI_TAPE_START_CUR + 86400.0D0
      END IF
!
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN) 
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN) 
!
      DO 430 J3=1,SUR%L_STA
!
! ------ Computation of final azimuth and elevaion
!
         SPL_STATUS = SUR%STATUS_SPL(SUR__TYP_TAG)
         SUR%STATUS_SPL(SUR__TYP_TAG) = 0
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, I_TYP, SUR%MJD_CUR, SUR%TAI_CUR, &
     &                   J3, IND_SOU, AZ(J3), EL(J3), HA(J3), IER )
         SUR%STATUS_SPL(SUR__TYP_TAG) = SPL_STATUS 
         IF ( .NOT. SUR_CHECK_VIS ( SUR, J3, I_TYP, IND_SOU, AZ(J3), &
     &                              EL(J3), HA(J3), IER ) ) THEN
               SUR%STA(J3)%EL_CUR = 0.0D0
               SUR%STA(J3)%AZ_CUR = 0.0D0
               SUR%STA(J3)%AZ_ACC_CUR = 0.0D0
               SUR%STA(J3)%HA_ACC_CUR = 0.0D0
               SUR%STA(J3)%ALP_CUR = 0.0D0
               SUR%STA(J3)%DEL_CUR = 0.0D0
               SUR%STA(J3)%HA_CUR  = 0.0D0
               GOTO 430
         END IF
!
         SUR%STA(J3)%EL_CUR = EL(J3)
         SUR%STA(J3)%AZ_CUR = AZ(J3)
         SUR%STA(J3)%AZ_ACC_CUR = AZ(J3)
         SUR%STA(J3)%HA_ACC_CUR = HA(J3)
!
         SUR%STA(J3)%ALP_CUR = SUR%SOU(IND_SOU)%ALPHA
         SUR%STA(J3)%DEL_CUR = SUR%SOU(IND_SOU)%DELTA
         SUR%STA(J3)%HA_CUR  = HA(J3)
         SUR%EL_OBS(J3,SUR%L_SCN) = EL(J3)
         SUR%AZ_OBS(J3,SUR%L_SCN) = AZ(J3)
         SUR%HA_OBS(J3,SUR%L_SCN) = HA(J3)
         SUR%AZ_ACC_OBS(J3,SUR%L_SCN) = SUR%STA(J3)%AZ_ACC_CUR
         SUR%HA_ACC_OBS(J3,SUR%L_SCN) = SUR%STA(J3)%HA_ACC_CUR 
         SUR%OBS_STA(J3,SUR%L_SCN) = SUR__USED 
 430  CONTINUE 
      IF ( SUR%PRESES_INTERVAL .LE. 0.0 ) THEN
           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
         ELSE 
           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__FIRST
      END IF
      SUR%NOBS_SRC(IND_SOU) = SUR%NOBS_SRC(IND_SOU) + 1
      SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SOU),IND_SOU) = SUR%L_SCN
!      
      IF ( IVRB .GE. 4 ) THEN
           STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
           WRITE ( 6, * ) ' First source: '//SUR%SOU(IND_SOU)%J2000_NAME// &
     &                    ' Time: '//STR(1:24)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SUR_ASTRO_FIRST  !#!#
