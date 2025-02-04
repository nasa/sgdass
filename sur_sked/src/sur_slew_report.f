      SUBROUTINE SUR_SLEW_REPORT ( SUR )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_SLEW_REPORT
! *                                                                      *
! * ### 25-NOV-2010 SUR_SLEW_REPORT  v2.0 (c)  L. Petrov 19-SEP-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      REAL*8     EL_PRE(SUR__M_STA), AZ_ACC_PRE(SUR__M_STA),  &
     &           HA_ACC_PRE(SUR__M_STA), DEC_PRE(SUR__M_STA), &
     &           EL_CUR(SUR__M_STA), AZ_ACC_CUR(SUR__M_STA),    &
     &           HA_ACC_CUR(SUR__M_STA), DEC_CUR(SUR__M_STA), &
     &           SLEW_EL(SUR__M_STA), SLEW_AZ(SUR__M_STA), &
     &           DIST_EL, DIST_AZ, DIST_HA, DEL_PRE, DEL_CUR, &
     &           ALP_CUR, DIF_DEL
      CHARACTER  DAT_CUR*30, DAT_PRE*30, SOU_NAM*10
      INTEGER*4  IND_PRE(SUR__M_STA), IND_CUR(SUR__M_STA), &
     &           J1, J2, J3, J4, J5, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      WRITE ( 6, 150 ) SUR%L_SCN
 150  FORMAT ( 'Sur_Slew  Scan: ', I4 )
!
      DO 410 J1=1,SUR%L_STA
         IND_PRE(J1) = 0
         IND_CUR(J1) = 0
         DO 420 J2=SUR%L_SCN,1,-1
            IF ( SUR%OBS_STA(J1,J2) == SUR__USED         .OR. &
     &           ( SUR%SOU_POCAL(J2) > 0  .AND.  J1 == 1 )    ) THEN
!
                 EL_CUR(J1)     = SUR%EL_OBS(J1,J2)
                 AZ_ACC_CUR(J1) = SUR%AZ_ACC_OBS(J1,J2)
                 HA_ACC_CUR(J1) = SUR%HA_ACC_OBS(J1,J2)
                 IND_CUR(J1) = J2
                 IF ( SUR%SRC_TYP(J2) == SUR__TYP_TAG ) THEN
                      ALP_CUR = SUR%SOU(SUR%IND_SRC(J2))%ALPHA
                      DEL_CUR = SUR%SOU(SUR%IND_SRC(J2))%DELTA 
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_SEC ) THEN
                      ALP_CUR = SUR%SO2(SUR%IND_SRC(J2))%ALPHA
                      DEL_CUR = SUR%SO2(SUR%IND_SRC(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_CAL ) THEN
                      ALP_CUR = SUR%CAL(SUR%IND_SRC(J2))%ALPHA
                      DEL_CUR = SUR%CAL(SUR%IND_SRC(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_POC ) THEN
                      ALP_CUR = SUR%SOP(SUR%IND_SRC(J2))%ALPHA
                      DEL_CUR = SUR%SOP(SUR%IND_SRC(J2))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J2) == SUR__TYP_PLA ) THEN
                      ALP_CUR = SUR%PLA(SUR%IND_SRC(J2))%ALPHA
                      DEL_CUR = SUR%PLA(SUR%IND_SRC(J2))%DELTA
                 END IF
                 GOTO 820
             END IF
 420     CONTINUE 
 820     CONTINUE 
!
         DO 430 J3=IND_CUR(J1)-1,1,-1
            IF ( J3 < 1 ) THEN
                 GOTO 830
            END IF
            IF ( SUR%OBS_STA(J1,J3) == SUR__USED  .OR.     &
     &           ( SUR%SOU_POCAL(J3) > 0  .AND.  J1 == 1 ) ) THEN
!
                 EL_PRE(J1)     = SUR%EL_OBS(J1,J3)
                 AZ_ACC_PRE(J1) = SUR%AZ_ACC_OBS(J1,J3)
                 HA_ACC_PRE(J1) = SUR%HA_ACC_OBS(J1,J3)
                 IF ( SUR%SRC_TYP(J3) == SUR__TYP_TAG ) THEN
                      DEL_PRE = SUR%SOU(SUR%IND_SRC(J3))%DELTA 
                   ELSE IF ( SUR%SRC_TYP(J3) == SUR__TYP_SEC ) THEN
                      DEL_PRE = SUR%SO2(SUR%IND_SRC(J3))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J3) == SUR__TYP_CAL ) THEN
                      DEL_PRE = SUR%CAL(SUR%IND_SRC(J3))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J3) == SUR__TYP_POC ) THEN
                      DEL_PRE = SUR%SOP(SUR%IND_SRC(J3))%DELTA
                   ELSE IF ( SUR%SRC_TYP(J3) == SUR__TYP_PLA ) THEN
                      DEL_PRE = SUR%PLA(SUR%IND_SRC(J3))%DELTA
                 END IF
                 IND_PRE(J1) = J3
                 GOTO 830
             END IF
 430     CONTINUE 
 830     CONTINUE 
!
         SOU_NAM = '????????'
         IF ( IND_CUR(J1) > 0 ) THEN
              IF ( SUR%SRC_TYP(IND_CUR(J1)) == SUR__TYP_TAG ) THEN
                   SOU_NAM = SUR%SOU(SUR%IND_SRC(IND_CUR(J1)))%J2000_NAME
                 ELSE IF ( SUR%SRC_TYP(IND_CUR(J1)) == SUR__TYP_SEC ) THEN
                   SOU_NAM = SUR%SO2(SUR%IND_SRC(IND_CUR(J1)))%J2000_NAME
                 ELSE IF ( SUR%SRC_TYP(IND_CUR(J1)) == SUR__TYP_CAL ) THEN
                   SOU_NAM = SUR%CAL(SUR%IND_SRC(IND_CUR(J1)))%J2000_NAME
              END IF
!
              IF ( SUR%SOU_POCAL(IND_CUR(J1)) > 0 .AND. J1 == 1 ) THEN
                   SOU_NAM = SUR%SOP(SUR%SOU_POCAL(IND_CUR(J1)))%J2000_NAME
              END IF
         END IF
!
         IF ( IND_PRE(J1) == 0 .AND. IND_CUR(J1) > 0 ) THEN
              DAT_CUR = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(IND_CUR(J1)), &
     &                                   SUR%TAI_OBS_BEG(IND_CUR(J1)), IER )
              WRITE ( 6, 110 ) SUR%L_SCN, SOU_NAM, DAT_CUR(1:19), &
     &                         SUR%STA(J1)%NAME
 110          FORMAT ( 'Scan ', I4, 2X, A, 2X, A, 2X, A, &
     &                 '  Initial Slewing state' )
           ELSE IF ( IND_CUR(J1) == 0 ) THEN
              WRITE ( 6, 120 ) SUR%L_SCN, SOU_NAM, ' ', &
     &                         SUR%STA(J1)%NAME
 120          FORMAT ( 'Scan ', I4, 2X, A, 2X, A, 2X, A, &
     &                 '  Scan is skipped, no slewing' )
           ELSE IF ( IND_CUR(J1) >  0 ) THEN
              DIST_EL = DABS(EL_CUR(J1) - EL_PRE(J1))
              DIST_AZ = DABS(AZ_ACC_CUR(J1) - AZ_ACC_PRE(J1))
              DIST_HA = DABS(HA_ACC_CUR(J1) - HA_ACC_PRE(J1))
              IF ( SUR%STA(J1)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! ---------------- Az/El mounting
!
                   IF ( DIST_EL > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                        SLEW_EL(J1) = (DIST_EL - SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                                2.D0*SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                      ELSE
                        SLEW_EL(J1) = 2.D0*DSQRT(DIST_EL/SUR%STA(J1)%SLEW_ACCL_EL)
                   END IF
                   IF ( DIST_AZ > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                        SLEW_AZ(J1) = (DIST_AZ - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                                2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                      ELSE
                        SLEW_AZ(J1) = 2.D0*DSQRT(DIST_AZ/SUR%STA(J1)%SLEW_ACCL_AZ)
                   END IF
                   SLEW_AZ(J1) = SLEW_AZ(J1) + SUR%STA(J1)%TIME_SETTLE_AZ
                   SLEW_EL(J1) = SLEW_EL(J1) + SUR%STA(J1)%TIME_SETTLE_EL
                 ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__EQUAT ) THEN
!
! ---------------- Equatorial mounting
!
                   DIF_DEL = DEL_CUR - DEL_PRE
!
! ---------------- Compute slew tine for equat mounting
!
                   IF ( DABS(DIST_HA) > SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ ) THEN
                        SLEW_AZ(J1) = (DABS(DIST_HA) - SUR%STA(J1)%SLEW_RATE_AZ**2/SUR%STA(J1)%SLEW_ACCL_AZ)/SUR%STA(J1)%SLEW_RATE_AZ + &
     &                             2.0D0*SUR%STA(J1)%SLEW_RATE_AZ/SUR%STA(J1)%SLEW_ACCL_AZ
                      ELSE
                        SLEW_AZ(J1) = 2.D0*DSQRT(DABS(DIST_HA)/SUR%STA(J1)%SLEW_ACCL_AZ)
                   END IF
                   IF ( DABS(DIF_DEL) > SUR%STA(J1)%SLEW_RATE_EL**2/SUR%STA(J1)%SLEW_ACCL_EL ) THEN
                        SLEW_EL(J1) = DABS(DIF_DEL)/SUR%STA(J1)%SLEW_RATE_EL + &
     &                                SUR%STA(J1)%SLEW_RATE_EL/SUR%STA(J1)%SLEW_ACCL_EL
                      ELSE
                        SLEW_EL(J1) = 2.D0*DSQRT(DABS(DIF_DEL)/SUR%STA(J1)%SLEW_ACCL_EL)
                   END IF
                 ELSE IF ( SUR%STA(J1)%MOUNT_TYPE == MT__XY_E ) THEN
              END IF
!
              DAT_PRE = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(IND_PRE(J1)), &
     &                                   SUR%TAI_OBS_BEG(IND_PRE(J1)), IER )
              DAT_CUR = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(IND_CUR(J1)), &
     &                                   SUR%TAI_OBS_BEG(IND_CUR(J1)), IER )
!
              IF ( SUR%OBS_STA(J1,SUR%L_SCN) == SUR__USED  .OR. &
     &             ( SUR%SOU_POCAL(SUR%L_SCN) > 0  .AND.  J1 == 1 ) ) THEN
!
                   WRITE ( 6, 130 ) SUR%L_SCN, SOU_NAM, DAT_CUR(1:19), &
     &                              SUR%STA(J1)%NAME, &
     &                              SLEW_EL(J1), SLEW_AZ(J1), SUR%STA(J1)%POSTOB, &
     &                              DIST_EL/DEG__TO__RAD, DIST_AZ/DEG__TO__RAD, &
     &                              AZ_ACC_PRE(J1)/DEG__TO__RAD, &
     &                              AZ_ACC_CUR(J1)/DEG__TO__RAD, &
     &                              EL_PRE(J1)/DEG__TO__RAD, &
     &                              EL_CUR(J1)/DEG__TO__RAD, SUR%SLEW_DUR(J1,SUR%L_SCN), &
     &                              HA_ACC_CUR(J1)/DEG__TO__RAD, HA_ACC_PRE(J1)/DEG__TO__RAD
 130               FORMAT  ( 'Scan ', I4, 2X, A, 2X, A, 2X, A, &
     &                       '  Slew_el: ', F6.1, ' Slew_az: ', F6.1, ' Postob: ', F5.1 &
     &                       ' De: ', F4.0, ' Da: ', F5.0, &
     &                       ' Ap: ', F9.3, ' Ac: ', F9.3, &
     &                       ' Ep: ', F7.2, ' Ec: ', F6.2, &
     &                       ' Slew_tot: ', F6.1, &
     &                       ' ha= ', f7.1, 1x,  f7.1 ) ! %%%%
                 ELSE 
                   WRITE ( 6, 140 ) SUR%L_SCN, SOU_NAM, DAT_CUR(1:19), &
     &                              SUR%STA(J1)%NAME
 140               FORMAT  ( 'Scan ', I4, 2X, A, 2X, A, 2X, A, &
     &                       '  Slew_el: Skipped' )
              END IF
         END IF
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE  SUR_SLEW_REPORT  !#!  
