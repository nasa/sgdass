      SUBROUTINE SUR_SET_CUR ( SUR, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_SET_CUR
! *                                                                      *
! *  ### 25-NOV-2010  SUR_SET_CUR  v2.2 (c)  L. Petrov  18-JAN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     AZ__MARGIN, EL__MARGIN
      PARAMETER  ( AZ__MARGIN = 0.25D0*DEG__TO__RAD )
      PARAMETER  ( EL__MARGIN = 0.25D0*DEG__TO__RAD  )
      REAL*8     AZ_ACC_PRE, HA_ACC_PRE, HA_PRE, AZ_PRE, EL_PRE, DIST_AZ, &
     &           A, B, A_LAST, B_LAST, ALP, DEL, &
     &           DIF_DEL, DIF_EL, DIF_AZ, DIF_HA, DIF_A, DIF_B, &
     &           SLEW_AZ, SLEW_EL, SLEW_HA, SLEW_DEL, SLEW_A, SLEW_B
      INTEGER*4  IUER
      INTEGER*4  J1, J2, J3, ITURN, IER
      CHARACTER  MJDSEC_TO_DATE*30
!
      SUR%MJD_CUR = SUR%MJD_START
      SUR%TAI_CUR = SUR%TAI_START
      HA_PRE = 0.0
      DO 410 J1=1,SUR%L_STA
         DO 420 J2=SUR%L_SCN,1,-1
            IF ( SUR%OBS_STA(J1,J2) == SUR__USED  .OR. &
     &           ( SUR%SOU_POCAL(J2) > 0  .AND.  J1 == 1 ) ) THEN
                 IF ( SUR%MJD_OBS_END(J2)*86400.0D0 + SUR%TAI_OBS_END(J2) > &
     &                SUR%MJD_CUR*86400.0D0 + SUR%TAI_CUR ) THEN
                      SUR%MJD_CUR = SUR%MJD_OBS_END(J2)
                      SUR%TAI_CUR = SUR%TAI_OBS_END(J2)
                 END IF
                 AZ_ACC_PRE = -101.0D0
                 HA_ACC_PRE = -101.0D0
                 SUR%SCA_PREV(J1,SUR%L_SCN) = 0
                 IF ( J2 > 1 ) THEN
                      DO 430 J3=J2-1,1,-1
                         IF ( SUR%OBS_STA(J1,J3) == SUR__USED  .OR. &
     &                        ( SUR%SOU_POCAL(J3) > 0  .AND.  J1 == 1 ) ) THEN
                              AZ_ACC_PRE = SUR%AZ_ACC_OBS(J1,J3) 
                              HA_ACC_PRE = SUR%HA_ACC_OBS(J1,J3) 
                              HA_PRE     = SUR%HA_OBS(J1,J3) 
                              AZ_PRE     = SUR%AZ_OBS(J1,J3) 
                              EL_PRE     = SUR%EL_OBS(J1,J3) 
                              SUR%SCA_PREV(J1,SUR%L_SCN) = J3
                              GOTO 830
                         END IF
 430                  CONTINUE 
 830                  CONTINUE 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 IF ( SUR%SOU_POCAL(J2) .NE. 0  .AND. J1 == 1 ) THEN
                      CALL SUR_AZEL ( SUR, VTD, SUR__TYP_POC, &
     &                                SUR%MJD_OBS_END(J2), SUR%TAI_OBS_END(J2), &
     &                                J1, SUR%SOU_POCAL(J2), SUR%STA(J1)%AZ_CUR, &
     &                                SUR%STA(J1)%EL_CUR, SUR%STA(J1)%HA_CUR, &
     &                                IER )
                    ELSE 
                      CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J2), &
     &                                SUR%MJD_OBS_END(J2), SUR%TAI_OBS_END(J2), &
     &                                J1, SUR%IND_SRC(J2), SUR%STA(J1)%AZ_CUR, &
     &                                SUR%STA(J1)%EL_CUR, SUR%STA(J1)%HA_CUR, &
     &                                IER )
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 2711, IUER, 'SUR_SET_CUR', 'Error in '// &
     &                    'computing elevation angle and azimuth' )
                      RETURN 
                 END IF
                 DIF_AZ = (SUR%STA(J1)%AZ_CUR - AZ_ACC_PRE)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
!
! -------------- Check whether the shortest azimuthal move is possible
!
                 IF ( DIF_AZ > 0.0D0 ) THEN
                      IF ( AZ_ACC_PRE + DIF_AZ < SUR%STA(J1)%AZ_ACC_MAX - AZ__MARGIN  ) THEN
                           DIST_AZ = DIF_AZ
                        ELSE
                           DIST_AZ = DIF_AZ - PI2
                      END IF
                    ELSE ! if ( DIF_AZ .LE. 0.0D0 ) then
                      IF ( AZ_ACC_PRE + DIF_AZ > SUR%STA(J1)%AZ_ACC_MIN + AZ__MARGIN ) THEN
                           DIST_AZ = DIF_AZ
                        ELSE
                           DIST_AZ = DIF_AZ + PI2
                      END IF
                 END IF
                 DIF_HA = (SUR%STA(J1)%HA_CUR - HA_PRE )
                 DIF_HA = DIF_HA - PI2*IDNINT(DIF_HA/PI2)
!
                 IF ( AZ_ACC_PRE < -50.0D0 ) THEN
!
! ------------------- Special case of the first observation in the experiment
!
                      SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_CUR
                      SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_CUR
                    ELSE
                      SUR%STA(J1)%AZ_ACC_CUR = AZ_ACC_PRE + DIST_AZ
                      SUR%STA(J1)%HA_ACC_CUR = HA_ACC_PRE + DIF_HA
                 END IF
!                 
                 IF ( SUR%STA(J1)%MOUNT_TYPE == MT__ALTAZ ) THEN
                      DIF_EL = DABS(SUR%STA(J1)%EL_CUR - EL_PRE)
!
! ------------------- Compute the slew time
!
                      IF ( DABS(DIST_AZ) > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                           SLEW_AZ = ( DABS(DIST_AZ) - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                               2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                         ELSE
                           SLEW_AZ = 2.D0*DSQRT(DABS(DIST_AZ)/SUR%STA(J1)%SLEW_ACCL_AZ)
                      END IF
                      IF ( DABS(DIF_EL) > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                           SLEW_EL = DABS(DIF_EL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                               SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                         ELSE
                           SLEW_EL = 2.D0*DSQRT(DABS(DIF_EL)/SUR%STA(J1)%SLEW_ACCL_EL)
                      END IF
                      SUR%SLEW_DUR(J1,SUR%L_SCN) = MAX(SLEW_AZ+SUR%STA(J1)%TIME_SETTLE_AZ,   &
     &                                                 SLEW_EL+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                             SUR%STA(J1)%POSTOB
                      SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_CUR
                   ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__EQUAT ) THEN
                      IF ( SUR%SRC_TYP(J2) == SUR__TYP_TAG ) THEN
                           DEL = SUR%SOU(SUR%SRC_TYP(J2))%DELTA 
                        ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_SEC ) THEN
                           DEL = SUR%SO2(SUR%SRC_TYP(J2))%DELTA
                        ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_CAL ) THEN
                           DEL = SUR%CAL(SUR%SRC_TYP(J2))%DELTA
                        ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_POC ) THEN
                           DEL = SUR%SOP(SUR%SRC_TYP(J2))%DELTA
                        ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_PLA ) THEN
                           DEL = SUR%PLA(SUR%SRC_TYP(J2))%DELTA
                      END IF
                      DIF_DEL = DEL - SUR%STA(J1)%DEL_CUR
!
                      IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------------ The shortest move is clock-wise
!
                           IF ( SUR%STA(J1)%HA_ACC_CUR + DIF_AZ < SUR%STA(J1)%AZ_ACC_MAX ) THEN
                                SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + DIF_HA
                              ELSE
!
! ----------------------------- The shortest way is not possible, move the longest way
! ----------------------------- counter-clock-wise
!
                                SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR - &
     &                                                   (PI2 - DIF_HA)
                           END IF
                         ELSE
!
! ------------------------ The shortest move is counter-clock-wise
!
                           IF ( SUR%STA(J1)%HA_ACC_CUR + DIF_HA > SUR%STA(J1)%AZ_ACC_MIN ) THEN
                                SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + DIF_HA
                              ELSE
!
! ----------------------------- The shortest way is not possible, move the longest way
! ----------------------------- clock-wise
!
                                SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_ACC_CUR + &
     &                                                   (PI2 + DIF_HA)
                           END IF
                      END IF
!
! ------------------- Compute slew tine for equat mounting
!
                      IF ( DABS(DIF_HA) > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                           SLEW_HA = (DABS(DIF_HA) - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                               2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                         ELSE
                           SLEW_HA = 2.D0*DSQRT(DABS(DIF_HA)/SUR%STA(J1)%SLEW_ACCL_AZ)
                      END IF
                      IF ( DABS(DIF_DEL) > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                           SLEW_DEL = DABS(DIF_DEL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                                SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                         ELSE
                           SLEW_DEL = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J1)%SLEW_ACCL_EL)
                      END IF
                      SUR%SLEW_DUR(J1,SUR%L_SCN) = MAX(SLEW_HA+SUR%STA(J1)%TIME_SETTLE_AZ,    &
     &                                                 SLEW_DEL+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                             SUR%STA(J1)%POSTOB
                      SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_CUR
                      SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_CUR
                   ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__XY_E  ) THEN
!
! ------------------- XY-E mounting
!
                      IF ( DABS(DTAN(EL_PRE)) < 1.D-6 ) THEN
                           A_LAST = P2I
                         ELSE
                           A_LAST = DATAN ( DCOS(AZ_PRE)/DTAN(EL_PRE) )
                           B_LAST = DASIN ( DSIN(AZ_PRE)*DCOS(EL_PRE) )
                      END IF
!
                      IF ( DABS(DTAN(SUR%STA(J1)%EL_CUR)) < 1.D-6 ) THEN
                           A = P2I
                         ELSE
                           A = DATAN ( DCOS(SUR%STA(J1)%AZ_CUR)/DTAN(SUR%STA(J1)%EL_CUR) )
                           B = DASIN ( DSIN(SUR%STA(J1)%AZ_CUR)*DCOS(SUR%STA(J1)%EL_CUR) )
                      END IF
!
                      DIF_A = A - A_LAST
                      DIF_B = B - B_LAST
!
! ------------------- Compute slew tine for XY-E mounting
!
                      SLEW_A = DABS(DIF_A)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                          SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                      SLEW_B = DABS(DIF_B)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                          SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                      SUR%SLEW_DUR(J1,SUR%L_SCN) = MAX(SLEW_A+SUR%STA(J1)%TIME_SETTLE_AZ, &
     &                                                 SLEW_B+SUR%STA(J1)%TIME_SETTLE_EL) + &
     &                                             SUR%STA(J1)%POSTOB
!
                      SUR%STA(J1)%AZ_ACC_CUR = SUR%STA(J1)%AZ_CUR
                      SUR%STA(J1)%HA_ACC_CUR = SUR%STA(J1)%HA_CUR
                 END IF
!
                 IF ( SUR%SRC_TYP(J2) == SUR__TYP_TAG ) THEN
                      SUR%STA(J1)%ALP_CUR = SUR%SOU(SUR%SRC_TYP(J2))%ALPHA
                      SUR%STA(J1)%DEL_CUR = SUR%SOU(SUR%SRC_TYP(J2))%DELTA 
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_SEC ) THEN
                      SUR%STA(J1)%ALP_CUR = SUR%SO2(SUR%SRC_TYP(J2))%ALPHA
                      SUR%STA(J1)%DEL_CUR = SUR%SO2(SUR%SRC_TYP(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_CAL ) THEN
                      SUR%STA(J1)%ALP_CUR = SUR%CAL(SUR%SRC_TYP(J2))%ALPHA
                      SUR%STA(J1)%DEL_CUR = SUR%CAL(SUR%SRC_TYP(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_POC ) THEN
                      SUR%STA(J1)%ALP_CUR = SUR%SOP(SUR%SRC_TYP(J2))%ALPHA
                      SUR%STA(J1)%DEL_CUR = SUR%SOP(SUR%SRC_TYP(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_PLA ) THEN
                      ALP = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%ALPHA
                      DEL = SUR%PLA(SUR%IND_SRC(SUR%L_SCN))%DELTA
                 END IF
!
                 SUR%EL_OBS(J1,SUR%L_SCN) = SUR%STA(J1)%EL_CUR
                 SUR%AZ_OBS(J1,SUR%L_SCN) = SUR%STA(J1)%AZ_CUR
                 SUR%HA_OBS(J1,SUR%L_SCN) = SUR%STA(J1)%HA_CUR
!
                 SUR%AZ_ACC_OBS(J1,SUR%L_SCN) = SUR%STA(J1)%AZ_ACC_CUR
                 SUR%HA_ACC_OBS(J1,SUR%L_SCN) = SUR%STA(J1)%HA_ACC_CUR
!
                 GOTO 820
             END IF
 420     CONTINUE 
 820     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_SET_CUR  !#!#
