      SUBROUTINE SUR_ASTRO_FILL ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_FILL
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 03-OCT-2025  SUR_ASTRO_FILL v1.0 (d) L. Petrov 03-OCT-2025 ###  *
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
      LOGICAL*1  FL_SOU(SUR__M_SOU)
      CHARACTER  C_ABB(SUR__M_SOU)*3, STR*128, STR1*128
      REAL*8     SAVE_TAI_CUR, SLEW_TIME_MAX, ARC, SCORE, SCORE_MAX, &
     &           SLEW_TIME_BEST, DUR_MAIN_SCAN, AZ, EL, HA, DIF_AZ, DIF_EL, &
     &           DIF_HA, DIF_DEL, SLEW_AZ, SLEW_EL, ALP, DEL, SLEW_A, SLEW_B, &
     &           SLEW_DEL, SLEW_HA, A, B, A_LAST, B_LAST, DIF_A, DIF_B
      INTEGER*4    M_SCA_BACK
      PARAMETER  ( M_SCA_BACK = 4 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, L_SAT, K_STA, SAVE_MJD_CUR, &
     &           MIN_STA_SAVE(SUR__M_SOU), IND_SOU_BEST, SPL_STATUS, &
     &           IND_LAST_SCN(SUR__M_STA), IND_LAST_SCN_SBR, IER
      REAL*8,    EXTERNAL ::  SUR_SLEW_TIME, ARC_LEN_VEC
      INTEGER*4, EXTERNAL ::  LTM_DIF
      CHARACTER, EXTERNAL ::  MJDSEC_TO_DATE*30
!
      IF ( SUR%L_SCN < 2 ) THEN
           RETURN
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      K_STA = 0
      DO 410 J1=1,SUR%L_STA
         IF ( SUR%OBS_STA(J1,SUR%L_SCN) == SUR__USED ) THEN
              SUR%STA(J1)%FLAGGED = .TRUE.  ! we flag the station that participated in the previous scan
            ELSE
              SUR%STA(J1)%FLAGGED = .FALSE.
              K_STA = K_STA + 1
         END IF
 410  CONTINUE 
      IF ( K_STA < 2 ) THEN
           RETURN
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      SAVE_MJD_CUR = SUR%MJD_CUR
      SAVE_TAI_CUR = SUR%TAI_CUR
      SUR%MJD_CUR = SUR%MJD_OBS_BEG(1)
      SUR%TAI_CUR = SUR%TAI_OBS_BEG(1)
      FL_SOU = .FALSE.
      L_SAT  = 0
      DO 420 J2=1,SUR%L_SCN
         IF ( J2 .GT. SUR%L_SCN-M_SCA_BACK ) THEN
              IF ( SUR%ALGORITHM == 'GNSS_01' .OR. &
                   SUR%ALGORITHM == 'GNSS_02' .OR. &
                   SUR%ALGORITHM == 'GNSS_03'      ) THEN
                   CALL ADD_CLIST ( SUR__M_SOU, L_SAT, C_ABB, SUR%SOU(SUR%IND_SRC(J2))%J2000_NAME(1:3), IER )
              END IF
              FL_SOU(SUR%IND_SRC(J2)) = .TRUE.
         END IF
!
         DO 430 J3=1,SUR%L_STA
            IF ( SUR%OBS_STA(J3,J2) == SUR__USED ) THEN
                 IND_LAST_SCN(J3) = J2
                 IF ( .NOT. SUR%STA(J3)%FLAGGED ) THEN
                      IF ( ( (SUR%MJD_OBS_END(J2) - SUR%MJD_CUR)*86400.0D0 + &
     &                       (SUR%TAI_OBS_END(J2) - SUR%TAI_CUR)             ) > 0.0D0 ) THEN
                           SUR%MJD_CUR = SUR%MJD_OBS_END(J2)
                           SUR%TAI_CUR = SUR%TAI_OBS_END(J2)
                      END IF
                      IND_LAST_SCN_SBR = J2
                 END IF
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
      SCORE_MAX    = -1.0D0
      IND_SOU_BEST = 0
      DO 440 J4=1,SUR%L_SOU
         SUR%SOU(J4)%MIN_STA = K_STA
         MIN_STA_SAVE(J4) = SUR%SOU(J4)%MIN_STA
         IF ( FL_SOU(J4) ) GOTO 440
         IF ( L_SAT > 0 ) THEN
              IF ( LTM_DIF ( 0, L_SAT, C_ABB, SUR%SOU(J4)%J2000_NAME(1:3) ) > 0 ) GOTO 440
         END IF
         IF ( SUR%ALGORITHM == 'GNSS_01' .OR. &
              SUR%ALGORITHM == 'GNSS_02' .OR. &
              SUR%ALGORITHM == 'GNSS_03'      ) THEN
              IF ( DABS( (SUR%MJD_CUR - SUR%SOU(J4)%MJD_EPOCH)*86400.0D0 + &
     &                   ( SUR%TAI_CUR +           &
     &                     SUR%AVR_SLEW_TIME +     &
     &                     SUR%SOU(J4)%DUR/2.0D0 - &
     &                     SUR%SOU(J4)%TAI_EPOCH)  ) > SUR%SOU(J4)%RANGE ) THEN
                   GOTO 440
              END IF
         END IF
!
         SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_TAG, &
     &                                   J4, SUR%IND_SRC(IND_LAST_SCN_SBR), &
     &                                   SUR%SRC_TYP(IND_LAST_SCN_SBR), -2, &
     &                                   SUR__FINE, IER )
         ARC = ARC_LEN_VEC ( SUR%SOU(J4)%S_VEC, SUR%SOU(SUR%IND_SRC(IND_LAST_SCN_SBR))%S_VEC )
         IF ( SLEW_TIME_MAX > 1.0D0 ) THEN
              SCORE = 1.D4/SLEW_TIME_MAX
              IF ( ARC < 0.3 ) THEN
                   SCORE = 0.1D0*SCORE
              END IF
              IF ( SCORE > SCORE_MAX ) THEN
                   SCORE_MAX = SCORE
                   IND_SOU_BEST = J4
                   SLEW_TIME_BEST = SLEW_TIME_MAX
              END IF
         END IF
 440  CONTINUE 
!
      IF ( IND_SOU_BEST > 0 ) THEN
           DO 450 J5=1,SUR%L_STA
              SUR%OBS_STA(J5,SUR%L_SCN+1) = 0
              IF ( SUR%STA(J5)%FLAGGED  ) THEN
                   SUR%EL_OBS(J5,SUR%L_SCN+1)     = SUR%EL_OBS(J5,IND_LAST_SCN(J5)) 
                   SUR%AZ_OBS(J5,SUR%L_SCN+1)     = SUR%AZ_OBS(J5,IND_LAST_SCN(J5))
                   SUR%HA_OBS(J5,SUR%L_SCN+1)     = SUR%HA_OBS(J5,IND_LAST_SCN(J5))
                   SUR%AZ_ACC_OBS(J5,SUR%L_SCN+1) = SUR%AZ_ACC_OBS(J5,IND_LAST_SCN(J5))
                   SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = SUR%HA_ACC_OBS(J5,IND_LAST_SCN(J5))
                   GOTO 450
              END IF
!
              IF ( SUR%STA(J5)%TAGALONE ) GOTO 450
              SPL_STATUS = SUR%STATUS_SPL(SUR__TYP_TAG)
              SUR%STATUS_SPL(SUR__TYP_TAG) = 0
              CALL ERR_PASS ( IUER, IER )
              CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, SUR%MJD_CUR, &
     &                        SUR%TAI_CUR + SLEW_TIME_BEST, J5, &
     &                        IND_SOU_BEST, AZ, EL, HA, IER )
              IF ( EL < SUR%SOU(IND_LAST_SCN_SBR)%EL_MIN ) THEN
                   STR  = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR + SLEW_TIME_BEST, -2 )
                   WRITE ( 6, * ) 'Date: ', STR(1:19), ' Sou: ', SUR%SOU(IND_SOU_BEST)%J2000_NAME, &
     &                            ' El= ', EL/DEG__TO__RAD, ' deg  el_min= ', &
     &                            SNGL(SUR%SOU(IND_SOU_BEST)%EL_MIN/DEG__TO__RAD), &
     &                            ' deg sta: ', SUR%STA(J5)%NAME, ' SCA= ', INT2(SUR%L_SCN+1)
                   CALL ERR_LOG ( 1791, IUER, 'SUR_ASTRO_FILL', 'Trap of internal '// &
     &                           'control: too low elevation' )
                   RETURN 
              END IF
              SUR%STATUS_SPL(SUR__TYP_TAG) = SPL_STATUS 
!
              DIF_EL = EL - SUR%STA(J5)%EL_CUR 
              IF ( SUR%STA(J5)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ---------------- Update accumulated azimuth in order to take into account
! ---------------- the cable wrap
!
                   DIF_AZ = (AZ - SUR%STA(J5)%AZ_CUR)
                   DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                   IF ( DIF_AZ > 0.0D0 ) THEN
!
! --------------------- The shortest move is clock-wise
!
                        IF ( SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J5)%AZ_ACC_MAX ) THEN
                             SUR%STA(J5)%AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ
                             SUR%STA(J5)%HA_ACC_CUR = HA
                           ELSE
!
! -------------------------- The shortest way is not possible, move the longest way
! -------------------------- counter-clock-wise
!
                             SUR%STA(J5)%AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR - &
     &                                                 (PI2 - DIF_AZ)
                             DIF_AZ = (PI2 - DIF_AZ)
                             SUR%STA(J5)%HA_ACC_CUR = HA
                        END IF
                      ELSE
!
! --------------------- The shortest move is counter-clock-wise
!
                        IF ( SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J5)%AZ_ACC_MIN ) THEN
                             SUR%STA(J5)%AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + DIF_AZ
                             SUR%STA(J5)%HA_ACC_CUR = HA
                          ELSE
!
! -------------------------- The shortest way is not possible, move the longest way
! -------------------------- clock-wise
!
                             SUR%STA(J5)%AZ_ACC_CUR = SUR%STA(J5)%AZ_ACC_CUR + &
     &                                                (PI2 + DIF_AZ)
                             DIF_AZ = (PI2 + DIF_AZ)
                             SUR%STA(J5)%HA_ACC_CUR = HA
                        END IF
                   END IF
!
                   IF ( EL < SUR%STA(J5)%EL_MIN .OR. &
     &                  EL > SUR%STA(J5)%EL_MAX      ) THEN
!
                        SUR%EL_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%EL_CUR
                        SUR%AZ_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%AZ_CUR
                        SUR%HA_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%HA_CUR
                        SUR%AZ_ACC_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%AZ_ACC_CUR
                        SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%HA_ACC_CUR
                        SUR%STA(J5)%ALP_CUR = SUR%SOU(IND_SOU_BEST)%ALPHA
                        SUR%STA(J5)%DEL_CUR = SUR%SOU(IND_SOU_BEST)%DELTA
!
                        SUR%SLEW_DUR(J5,SUR%L_SCN+1) = 0.0D0
                        GOTO 450
                   END IF
!
! ---------------- Compute the slew time
!
                   IF ( DABS(DIF_AZ) > SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ ) THEN
                        SLEW_AZ = (DABS(DIF_AZ) - SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                            2.0D0*SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                      ELSE
                        SLEW_AZ = 2.D0*DSQRT(DABS(DIF_AZ)/SUR%STA(J5)%SLEW_ACCL_AZ)
                   END IF
                   IF ( DABS(DIF_EL) > SUR%STA(J5)%SLEW_RATE_EL**2/SUR%STA(J5)%SLEW_ACCL_EL ) THEN
                        SLEW_EL = DABS(DIF_EL)/SUR%STA(J5)%SLEW_RATE_EL + &
     &                            SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                      ELSE
                        SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J5)%SLEW_ACCL_EL)
                   END IF
                   SUR%SLEW_DUR(J5,SUR%L_SCN+1) = MAX(SLEW_AZ+SUR%STA(J5)%TIME_SETTLE_AZ,   &
     &                                                 SLEW_EL+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                             SUR%STA(J5)%POSTOB
                   SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = HA
                 ELSE IF ( SUR%STA(J5)%MOUNT_TYPE == MT__EQUAT ) THEN
                   ALP = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                   DEL = SUR%SOU(SUR%IND_SRC(SUR%L_SCN))%DELTA
                   DIF_DEL = DEL - SUR%STA(J5)%DEL_CUR
                   DIF_HA = (HA - SUR%STA(J5)%HA_CUR)
                   IF ( DIF_HA > 0.0D0 ) THEN
!
! --------------------- The shortest move is clock-wise
!
                        SUR%STA(J5)%HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + DIF_HA
                       ELSE
!
! --------------------- The shortest move is counter-clock-wise
!
                         IF ( SUR%STA(J5)%HA_ACC_CUR + DIF_HA > SUR%STA(J5)%AZ_ACC_MIN ) THEN
                              SUR%STA(J5)%HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + DIF_HA
                           ELSE
!
! -------------------------- The shortest way is not possible, move the longest way
! -------------------------- clock-wise
!
                               SUR%STA(J5)%HA_ACC_CUR = SUR%STA(J5)%HA_ACC_CUR + &
     &                                                 (PI2 + DIF_HA)
                        END IF
                   END IF
!
! ---------------- Compute the slew time
!
                   IF ( DABS(DIF_HA) > SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ ) THEN
                        SLEW_HA = (DABS(DIF_HA) - SUR%STA(J5)%SLEW_RATE_AZ**2/SUR%STA(J5)%SLEW_ACCL_AZ)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                            2.0D0*SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                      ELSE
                        SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J5)%SLEW_ACCL_AZ)
                   END IF
                   IF ( DABS(DIF_DEL) > SUR%STA(J5)%SLEW_RATE_EL**2/SUR%STA(J5)%SLEW_ACCL_EL ) THEN
                        SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J5)%SLEW_RATE_EL + &
     &                            SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                      ELSE
                        SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J5)%SLEW_ACCL_EL)
                   END IF
                   SUR%SLEW_DUR(J5,SUR%L_SCN+1) = MAX(SLEW_HA+SUR%STA(J5)%TIME_SETTLE_AZ, &
     &                                                 SLEW_DEL+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                             SUR%STA(J5)%POSTOB
                   SUR%AZ_ACC_OBS(J5,SUR%L_SCN+1) = AZ
                   SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%HA_ACC_CUR
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
                   IF ( DABS(DTAN(EL)) < 1.D-6 ) THEN
                        A = P2I
                      ELSE
                        A = DATAN ( DCOS(AZ)/DTAN(EL) )
                        B = DASIN ( DSIN(AZ)*DCOS(EL) )
                   END IF
                   IF ( B < SUR%STA(J5)%EL_MIN .OR. &
     &                  B > SUR%STA(J5)%EL_MAX      ) THEN
!
                        SUR%EL_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%EL_CUR
                        SUR%AZ_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%AZ_CUR
                        SUR%HA_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%HA_CUR
                        SUR%AZ_ACC_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%AZ_ACC_CUR
                        SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = SUR%STA(J5)%HA_ACC_CUR
                        SUR%STA(J5)%ALP_CUR = SUR%SOU(IND_SOU_BEST)%ALPHA
                        SUR%STA(J5)%DEL_CUR = SUR%SOU(IND_SOU_BEST)%DELTA
!
                        SUR%SLEW_DUR(J5,SUR%L_SCN+1) = 0.0D0
                        GOTO 450
                   END IF
!
                   DIF_A = A - A_LAST
                   DIF_B = B - B_LAST
!
                   SLEW_A = DABS(DIF_A)/SUR%STA(J5)%SLEW_RATE_AZ + &
     &                       SUR%STA(J5)%SLEW_RATE_AZ/SUR%STA(J5)%SLEW_ACCL_AZ
                   SLEW_B = DABS(DIF_B)/SUR%STA(J5)%SLEW_RATE_EL + &
     &                       SUR%STA(J5)%SLEW_RATE_EL/SUR%STA(J5)%SLEW_ACCL_EL
                   SUR%SLEW_DUR(J5,SUR%L_SCN+1) = MAX(SLEW_A+SUR%STA(J5)%TIME_SETTLE_AZ,   &
     &                                                 SLEW_B+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                                 SUR%STA(J5)%POSTOB
!
                   SUR%AZ_ACC_OBS(J5,SUR%L_SCN+1) = AZ
                   SUR%HA_ACC_OBS(J5,SUR%L_SCN+1) = HA
               END IF
               SUR%OBS_STA(J5,SUR%L_SCN+1) = SUR__USED
               SUR%SLEW_DUR(J5,SUR%L_SCN+1) = MAX(SLEW_AZ+SUR%STA(J5)%TIME_SETTLE_AZ,   &
     &                                            SLEW_EL+SUR%STA(J5)%TIME_SETTLE_EL) + &
     &                                        SUR%STA(J5)%POSTOB
 450       CONTINUE
!
           DUR_MAIN_SCAN = (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_CUR)*86400.0D0 + &
     &                     (SUR%TAI_OBS_END(SUR%L_SCN) - SUR%TAI_CUR)
!
! -------- Update scan counter
!
           SUR%L_SCN = SUR%L_SCN + 1
           SUR%SLEW_SCA(SUR%L_SCN) = SLEW_TIME_BEST
!
           SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
           SUR%NOBS_SRC(IND_SOU_BEST) = SUR%NOBS_SRC(IND_SOU_BEST) + 1
           SUR%SOU(IND_SOU_BEST)%NOBS = SUR%SOU(IND_SOU_BEST)%NOBS + 1
           SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SOU_BEST),IND_SOU_BEST) = SUR%L_SCN
!
           SUR%IND_SRC(SUR%L_SCN) = IND_SOU_BEST
           SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
           SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR + SLEW_TIME_BEST
           SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%SOU(IND_SOU_BEST)%DUR
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_CUR
!       
           IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
                SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
           END IF
!
           IF ( SUR%MJD_OBS_END(SUR%L_SCN-1)*86400.D0 + SUR%TAI_OBS_END(SUR%L_SCN-1) > &
     &          SUR%MJD_OBS_END(SUR%L_SCN)*86400.0D0  + SUR%TAI_OBS_END(SUR%L_SCN)     ) THEN
                SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN-1)
                SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN-1)
             ELSE
                SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
                SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
           END IF
           SUR%SRC_TYP(SUR%L_SCN)   = SUR__TYP_TAG
           SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG 

           IF ( IVRB .GE. 6 ) THEN   
                STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), IER )
                STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), IER )
                WRITE ( 6, 123 ) SUR%L_SCN, 'pri', SUR%SOU(IND_SOU_BEST)%J2000_NAME, SUR%SOU(IND_SOU_BEST)%DUR, &
     &                           STR(1:21), STR1(1:21), SUR%SLEW_SCA(SUR%L_SCN)
 123            FORMAT ( 'Scan: ', I4, 1X, A, 1X, ' Sou: ', A, ' Dur: ', F6.1, &
     &                   ' Obs_tai_beg: ', A, ' Obs_tai_end: ', A, ' Slew: ' , F6.1 )
           END IF
         ELSE
           SUR%MJD_CUR = SAVE_MJD_CUR 
           SUR%TAI_CUR = SAVE_TAI_CUR 
      END IF
!
! --- Restore station flags
!
      DO 460 J6=1,SUR%L_STA
         SUR%STA(J6)%FLAGGED = .FALSE.
 460  CONTINUE 
!
! --- Restore the minimum number of observations per source
!
      DO 470 J7=1,SUR%L_SOU
         SUR%SOU(J7)%MIN_STA = MIN_STA_SAVE(J7)
 470  CONTINUE 
!
! --- Set antennas current states
!
      CALL SUR_SET_CUR ( SUR, VTD, IER  )
      IF ( IVRB .GE. 6 ) THEN
           WRITE ( 6, * ) ' '
           CALL SUR_SLEW_REPORT ( SUR )
           WRITE ( 6, '("Scan ", I4, " Slew_time_max:  ", F6.1, " nobs_src: ", I4 )' ) SUR%L_SCN, &
     &                SLEW_TIME_BEST, SUR%NOBS_SRC(IND_SOU_BEST)
           WRITE ( 6, * ) ' '
      END IF
      IF ( IVRB .GE. 5 ) THEN
           STR  = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), SUR%TAI_OBS_BEG(SUR%L_SCN), -2 )
           STR1 = MJDSEC_TO_DATE ( SUR%MJD_OBS_END(SUR%L_SCN), SUR%TAI_OBS_END(SUR%L_SCN), -2 )
           WRITE ( 6, 180 ) SUR%L_SCN, STR(1:25), STR1(1:24), SUR__TYP_STR(SUR__TYP_TAG)
 180       FORMAT ( 'ScAN ', I4, ' Obs_beg: ', A, ' Obs_end: ', A, ' Tag_type: ', A )
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_ASTRO_FILL  !#!#
