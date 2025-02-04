      SUBROUTINE SUR_TRANSIT  ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SUR_TRANSIT
! *                                                                      *
! *  ### 26-MAY-2010  SUR_TRANSIT  v1.2 (c)  L. Petrov  26-JAN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      INCLUDE   'getpar.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      TYPE     ( SUR__TYPE ) :: SUR_2ND
      INTEGER*4  IVRB, IUER
      REAL*8     SANG, TANG, AZ, ELEV, HA, DTIM_RISE, DTIM_SET, &
     &           DEC_WGT, TIM_WGT, SLE_WGT, GAP(SUR__M_SOU)
      CHARACTER  STR*32
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, NTB, UTC_OBS_INT, &
     &           NS, IND_SRC, MJD_OBS, MJD_BEG, I_STA, K_STA, N_SCA, &
     &           MJD_STOP_SAVE, IER
      REAL*8     D1, D2, SCORE(SUR__M_SOU), IND_R8(SUR__M_SOU), TAI_OBS, &
     &           TAI_BEG, SLEW_TIME_MAX, SLEW_TIME_1ST(SUR__M_SOU), &
     &           SLEW_TIME_2ND(SUR__M_SOU), TROP_BURST_LEN, GAP_HALF, &
     &           SLEW_EL, SLEW_AZ, DIF_AZ, DIF_HA, TAI_STOP_SAVE, SCAN_DUR, DT
      REAL*8     SHARE__MIN_DUR
      PARAMETER  ( SHARE__MIN_DUR = 0.8 )
      PARAMETER  ( D1 =  0.0D0*DEG__TO__RAD )
      PARAMETER  ( D2 = 30.0D0*DEG__TO__RAD )
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      LOGICAL*4, EXTERNAL :: SUR_CHECK_VIS
      REAL*8   , EXTERNAL :: SUR_SLEW_TIME
!
      SUR_2ND = SUR
      SUR_2ND%L_SCN = 0
!
      DO 410 J1=1,SUR%L_SOU
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, SUR%MJD_CUR, &
     &                   SUR%TAI_CUR, SUR%REF_STA, J1, AZ, ELEV, HA, &
     &                   IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1611, IUER, 'SUR_TRANSIT', 'Error in '// &
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
     &                               SUR%SOU(J1)%TAI_UP_CULM, -2 )
              WRITE ( 6, 110 ) J1, SUR%SOU(J1)%J2000_NAME, STR(1:21)
 110          FORMAT ( I4,') Source: ',A, ' Upper culmination at ', A )
         END IF
 410  CONTINUE
!
! --- Search for the first source
!
      NS = 0
      GAP_HALF = ( (SUR%MJD_STOP - SUR%MJD_START)*86400.0D0 + &
     &             (SUR%TAI_STOP - SUR%TAI_START)             )/2.0D0
      DO 420 J2=1,SUR%L_SOU
         DTIM_RISE = -1.D6
         DTIM_SET  = -1.D6
         CALL ERR_PASS ( IUER, IER )
         SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_TAG, &
     &                                   J2, SUR%IND_SRC(SUR%L_SCN), &
     &                                   SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                   SUR__FINE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1612, IUER, 'SUR_TRANSIT', 'Trap '// &
     &            'of internal control: error in SUR_SLEW_TIME ' )
              RETURN
         END IF
         IF ( SLEW_TIME_MAX < 0.0D0 ) GOTO 420
!
! ------ Get DTIM_RISE -- interval of time from the source rise to the current time.
! ------                  Positive interval means the source has risen.
! ------ Get DTIM_SET  -- interval of time between source set to the current time.
! ------                  Positive interval means the source has NOT yet set
!
         IF ( SUR%N_VIS(J2,SUR__TYP_TAG) == 0 ) GOTO 420
         DO 430 J3=1,SUR%N_VIS(J2,SUR__TYP_TAG)
            DTIM_RISE =   (SUR%MJD_CUR - SUR%MJD_VIS(J3,SUR__RIS,J2,SUR__TYP_TAG))*86400.0D0 &
     &                  + (SUR%TAI_CUR - SUR%TAI_VIS(J3,SUR__RIS,J2,SUR__TYP_TAG))
            DTIM_SET  =   (SUR%MJD_VIS(J3,SUR__SET,J2,SUR__TYP_TAG) - SUR%MJD_CUR)*86400.0D0 &
     &                  + (SUR%TAI_VIS(J3,SUR__SET,J2,SUR__TYP_TAG) - SUR%TAI_CUR)
            IF ( DTIM_RISE > 0.0D0 .AND. DTIM_SET > 0.0D0 ) GOTO 830
 430     CONTINUE
 830     CONTINUE
!
         IF ( SUR%SOU(J2)%DELTA > D1 ) THEN
              GAP(J2) = SUR%SOU(J2)%GAP_NOR
            ELSE
              GAP(J2) = SUR%SOU(J2)%GAP_MIN
         END IF
         IF ( IVRB .GE. 5 ) THEN
              CALL CLRCH ( STR )
              IF ( DTIM_RISE < 0.0D0    ) STR(1:1) = '@'
              IF ( DTIM_SET  < GAP_HALF ) STR(2:2) = '#'
!
              WRITE ( 6, 120 ) J2, STR(1:2), SUR%SOU(J2)%J2000_NAME, &
     &                         DTIM_RISE/3600.0D0, DTIM_SET/3600.0D0
 120          FORMAT ( I4, ' ) ', A, ' Source: ', A, ' Dtim_rise: ', F6.2, &
     &                ' Dtim_set: ', F6.2, ' hours' )
         END IF
         IF ( DTIM_RISE < 0.0D0 .OR. DTIM_SET < GAP_HALF ) GOTO 420
!
         NS = NS + 1
         IF ( SUR%SOU(J2)%DELTA < 30.0D0*DEG__TO__RAD ) THEN
              DEC_WGT = (30.0D0 - SUR%SOU(J2)%DELTA/DEG__TO__RAD)**2
            ELSE
              DEC_WGT = 1.0D0
         END IF
         TIM_WGT = 1.D6/(1.0D0 + DTIM_SET - GAP_HALF)
         SCORE(NS)  = DEC_WGT + TIM_WGT
         IND_R8(NS) = J2 + 1.D-5
 420  CONTINUE
!
      IF ( NS .LE. 0 ) THEN
           CALL ERR_LOG ( 1613, IUER, 'SUR_TRANSIT', 'Did not find '// &
     &         'any sourse to begin with' )
           RETURN
      END IF
!
      CALL SORT8 ( NS, SCORE, IND_R8 )
      IND_SRC = IND_R8(NS)
!
      MJD_OBS = SUR%MJD_CUR
      TAI_OBS = SUR%TAI_CUR
!
      CALL ERR_PASS ( IUER, IER )
      SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_TAG, &
     &                                IND_SRC, SUR%IND_SRC(SUR%L_SCN), &
     &                                SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                SUR__FINE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1614, IUER, 'SUR_TRANSIT', 'Trap '// &
     &         'of internal control: error in SUR_SLEW_TIME ' )
           RETURN
      END IF
      IF ( SLEW_TIME_MAX < 0.0D0 ) THEN
           WRITE ( 6, * ) ' SLEW_TIME_MAX = ', SLEW_TIME_MAX, ' ss= ', sur__typ_tag ! %%%
           CALL ERR_LOG ( 1615, IUER, 'SUR_TRANSIT', 'Trap '// &
     &         'of internal control: SLEW_TIME_MAX is negative for '// &
     &         'source '//SUR%SOU(IND_SRC)%J2000_NAME )
           RETURN
      END IF
!
      TAI_BEG = TAI_OBS
      MJD_BEG = MJD_OBS
!
      TAI_OBS = TAI_OBS + SLEW_TIME_MAX
      MJD_OBS = MJD_OBS
      IF ( TAI_OBS > 86400.0D0 ) THEN
           TAI_OBS = TAI_OBS - 86400.0D0
           MJD_OBS = MJD_OBS - 1
      END IF
      IF ( IVRB .GE. 5 ) THEN
           STR = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           WRITE ( 6, 130 ) SUR%SOU(IND_SRC)%J2000_NAME, SCORE(NS), &
     &                      STR(1:19), SLEW_TIME_MAX
 130       FORMAT ( '1st obs. Source: ',A,' Score: ', F10.1, &
     &              '  ', A, ' Slew: ', F6.1 )
      END IF
!
      K_STA = 0
      DO 440 J4=1,SUR%L_STA
         SUR%OBS_STA(J4,SUR%L_SCN+1) = 0
         IF ( SUR%STA(J4)%TAGALONE ) GOTO 440
         CALL ERR_PASS ( IUER, IER )
         CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, MJD_OBS, TAI_OBS, J4, &
     &                   IND_SRC, AZ, ELEV, HA, IER )
         IF ( SUR%STA(J4)%MOUNT_TYPE == MT__ALTAZ ) THEN
              IF ( ELEV < SUR%STA(J4)%EL_MIN .OR. &
     &             ELEV > SUR%STA(J4)%EL_MAX      ) THEN
!
                   GOTO 440
              END IF
!
! ----------- Update accumulative aziumth in order to take into account
! ----------- cable wrap
!
              DIF_AZ = (AZ - SUR%STA(J4)%AZ_CUR)
              DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
              IF ( DIF_AZ > 0.0D0 ) THEN
!
! ---------------- The shortest move is clock-wise
!
                   IF ( SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J4)%AZ_ACC_MAX ) THEN
                        SUR%STA(J4)%AZ_ACC_CUR = SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ
                      ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- counter-clock-wise
!
                        SUR%STA(J4)%AZ_ACC_CUR = SUR%STA(J4)%AZ_ACC_CUR - &
     &                                            (PI2 - DIF_AZ)
                   END IF
                 ELSE
!
! ---------------- The shortest move is counter-clock-wise
!
                   IF ( SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J4)%AZ_ACC_MIN ) THEN
                        SUR%STA(J4)%AZ_ACC_CUR = SUR%STA(J4)%AZ_ACC_CUR + DIF_AZ
                     ELSE
!
! --------------------- The shortest way is not possible, move the longest way
! --------------------- clock-wise
!
                        SUR%STA(J4)%AZ_ACC_CUR = SUR%STA(J4)%AZ_ACC_CUR + &
     &                                           (PI2 + DIF_AZ)
                   END IF
              END IF
              IF ( ELEV < SUR%STA(J4)%EL_MIN .OR. &
     &             ELEV > SUR%STA(J4)%EL_MAX      ) THEN
!
                   SUR%EL_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%EL_CUR
                   SUR%AZ_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_CUR
                   SUR%HA_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_CUR
                   SUR%AZ_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_ACC_CUR
                   SUR%HA_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_ACC_CUR
                   GOTO 440
              END IF
            ELSE IF ( SUR%STA(J4)%MOUNT_TYPE == MT__EQUAT ) THEN
              DIF_HA = (HA - SUR%STA(J4)%HA_CUR)
              IF ( DABS(DIF_HA) < P2I ) THEN
!
! ---------------- The shortest move is clock-wise
!
                   SUR%STA(J4)%HA_ACC_CUR = SUR%STA(J4)%HA_ACC_CUR + DIF_HA
                 ELSE
!
! ---------------- The shortest move is counter-clock-wise
!
                   IF ( SUR%STA(J4)%HA_ACC_CUR + DIF_HA > SUR%STA(J4)%AZ_ACC_MIN ) THEN
                        SUR%STA(J4)%HA_ACC_CUR = SUR%STA(J4)%HA_ACC_CUR + DIF_HA
                      ELSE
!
! -------------------- The shortest way is not possible, move the longest way
! -------------------- clock-wise
!
                       SUR%STA(J4)%HA_ACC_CUR = SUR%STA(J4)%HA_ACC_CUR + &
     &                                          (PI2 + DIF_HA)
                  END IF
              END IF
              IF ( SUR%SOU(IND_SRC)%DELTA < SUR%STA(J4)%EL_MIN .OR. &
     &             SUR%SOU(IND_SRC)%DELTA > SUR%STA(J4)%EL_MAX      ) THEN
!
                   SUR%EL_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%EL_CUR
                   SUR%AZ_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_CUR
                   SUR%HA_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_CUR
                   SUR%AZ_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_ACC_CUR
                   SUR%HA_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_ACC_CUR
                   SUR%STA(J4)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                   SUR%STA(J4)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                   GOTO 440
              END IF
         END IF
         IF ( .NOT. SUR_CHECK_VIS ( SUR, J4, SUR__TYP_TAG, IND_SRC, &
     &                              AZ, ELEV, HA, IER ) ) THEN
!
! ----------- The J4-th station does not the IND_SRC -th source
!
!
              SUR%EL_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%EL_CUR
              SUR%AZ_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_CUR
              SUR%HA_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_CUR
              SUR%AZ_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%AZ_ACC_CUR
              SUR%HA_ACC_OBS(J4,SUR%L_SCN) = SUR%STA(J4)%HA_ACC_CUR
              GOTO 440
         END IF
!
         K_STA = K_STA + 1
         SUR%STA(J4)%AZ_CUR  = AZ
         SUR%STA(J4)%EL_CUR  = ELEV
         SUR%STA(J4)%HA_CUR  = HA
         SUR%STA(J4)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
         SUR%STA(J4)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
         SUR%EL_OBS(J4,SUR%L_SCN+1) = SUR%STA(J4)%EL_CUR
         SUR%AZ_OBS(J4,SUR%L_SCN+1) = SUR%STA(J4)%AZ_CUR
         SUR%HA_OBS(J4,SUR%L_SCN+1) = SUR%STA(J4)%HA_CUR
         SUR%AZ_ACC_OBS(J4,SUR%L_SCN+1) = SUR%STA(J4)%AZ_ACC_CUR
         SUR%HA_ACC_OBS(J4,SUR%L_SCN+1) = SUR%STA(J4)%HA_ACC_CUR
         SUR%OBS_STA(J4,SUR%L_SCN+1) = SUR__USED
 440  CONTINUE
      IF ( K_STA < SUR%SOU(IND_SRC)%MIN_STA ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( K_STA, STR )
           CALL ERR_LOG ( 1616, IUER, 'SUR_TRANSIT', 'Trap of internal '// &
     &         'control: too few stations ('//STR(1:I_LEN(STR))// &
     &         '0 are able to see source '//SUR%SOU(IND_SRC)%J2000_NAME )
           RETURN
      END IF
!
! --- Update scan counter
!
      SUR%L_SCN = SUR%L_SCN + 1
      SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LONG
      SUR%L_OBS_TAG = SUR%L_OBS_TAG + 1
      SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) + 1
      SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SRC),IND_SRC) = SUR%L_SCN
!
      SUR%IND_SRC(SUR%L_SCN) = IND_SRC
      SUR%IND_TAP(SUR%L_SCN) = SUR%L_TAP
      SUR%MJD_OBS_BEG(SUR%L_SCN) = MJD_OBS
      SUR%MJD_OBS_END(SUR%L_SCN) = MJD_OBS
      SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS
      UTC_OBS_INT = IDNINT ( SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
      IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
           SUR%TAI_OBS_BEG(SUR%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                  - SUR%UTC_M_TAI
         ELSE
           SUR%TAI_OBS_BEG(SUR%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
      END IF
!
      SUR%TAI_OBS_END(SUR%L_SCN) = TAI_OBS + SUR%SOU(IND_SRC)%DUR
      IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
      END IF
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
      SUR%SRC_TYP(SUR%L_SCN) = SUR__TYP_TAG
      SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
!
! --- Update SUR_2ND scan counter for the 2nd transit
!
      SUR_2ND%L_SCN   = 1
      SUR_2ND%MJD_OBS_BEG(SUR_2ND%L_SCN) = MJD_OBS
      SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + GAP_HALF
      SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) = MJD_OBS
      SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) + GAP_HALF
      SUR_2ND%MJD_CUR = SUR%MJD_OBS_END(SUR_2ND%L_SCN)
      SUR_2ND%TAI_CUR = SUR%TAI_OBS_END(SUR_2ND%L_SCN)
!
      SUR_2ND%NOBS_SRC(IND_SRC) = SUR_2ND%NOBS_SRC(IND_SRC) + 1
      SUR_2ND%IND_SCN_SRC(SUR_2ND%NOBS_SRC(IND_SRC),IND_SRC) = SUR_2ND%L_SCN
!
      SUR_2ND%IND_SRC(SUR_2ND%L_SCN) = IND_SRC
      SUR_2ND%IND_TAP(SUR_2ND%L_SCN) = SUR_2ND%L_TAP
      IF ( SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) > 86400.0D0 ) THEN
           SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) = SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) - 86400.0D0
           SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) = SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) + 1
      END IF
      SUR_2ND%MJD_CUR = SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN)
      SUR_2ND%TAI_CUR = SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN)
      SUR_2ND%SRC_TYP(SUR_2ND%L_SCN) = SUR__TYP_TAG
      SUR_2ND%OBS_STA(1:SUR__M_STA,SUR_2ND%L_SCN) = 0
      SUR_2ND%L_SCN_SO1 = SUR_2ND%L_SCN_SO1 + 1
!
! === Process other sources
!
      NTB = 1 ! The number of troposphere calibrators bursts
      TROP_BURST_LEN = 5*SUR%AVR_SLEW_TROPO_TIME + 4*SUR%TROPO_SCAN_LEN
!
      DO 450 J5=2,SUR__M_SOU
         NS = 0
!
         IF ( NTB == 1 .AND. &
     &        (MJD_OBS - SUR%MJD_TROPO_CUR)*86400.0D0 + &
     &        (TAI_OBS - SUR%TAI_TROPO_CUR) + SUR%PREOBS_LONG + &
     &        SUR%SOU(IND_SRC)%DUR > SUR%TROPO_BURST_INTERVAL   ) THEN
!
! ----------- Insert a burst of troposphere calibrators
!
              CALL ERR_PASS ( IUER, IER )
              CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1617, IUER, 'SUR_TRANSIT', 'Error in '// &
     &                 'an attempt to insert a burst of troposphere calibrators' )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL SUR_ASTRO_TROPO ( SUR_2ND, VTD, IVRB, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1618, IUER, 'SUR_TRANSIT', 'Error in '// &
    &                  'an attempt to insert a burst of troposphere '// &
    &                  'calibrators for the second part of the schedule' )
                   RETURN
              END IF
!
              NTB = NTB + 1
         END IF
!
         DO 460 J6=1,SUR%L_SOU
            IF ( SUR%NOBS_SRC(J6) > 0 ) GOTO 460
            DTIM_RISE = -1.D6
            DTIM_SET  = -1.D6
!
! --------- Get DTIM_RISE -- interval of time from the source rise to the current time.
! ---------                  Positive interval means the source has risen.
! --------- Get DTIM_SET  -- interval of time between source set to the current time.
! ---------                  Positive interval means the source has NOT yet set
!
            DO 470 J7=1,SUR%N_VIS(J6,SUR__TYP_TAG)
               DTIM_RISE =   (SUR%MJD_CUR - SUR%MJD_VIS(J7,SUR__RIS,J6,SUR__TYP_TAG))*86400.0D0 &
     &                     + (SUR%TAI_CUR - SUR%TAI_VIS(J7,SUR__RIS,J6,SUR__TYP_TAG))
               DTIM_SET  =   (SUR%MJD_VIS(J7,SUR__SET,J6,SUR__TYP_TAG) - SUR%MJD_CUR)*86400.0D0 &
     &                     + (SUR%TAI_VIS(J7,SUR__SET,J6,SUR__TYP_TAG) - SUR%TAI_CUR)
               IF ( DTIM_RISE > 0.0D0 .AND. DTIM_SET > 0.0D0 ) GOTO 870
 470        CONTINUE
 870        CONTINUE
!
            IF ( SUR%SOU(J6)%DELTA > D1 ) THEN
                 GAP(J6) = SUR%SOU(J6)%GAP_NOR
               ELSE
                 GAP(J6) = SUR%SOU(J6)%GAP_MIN
            END IF
            IF ( DTIM_RISE < 0.0D0 .OR. DTIM_SET < GAP_HALF ) GOTO 460
!
            CALL ERR_PASS ( IUER, IER )
            SLEW_TIME_1ST(J6) = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_TAG, &
     &                                   J6, SUR%IND_SRC(SUR%L_SCN), &
     &                                   SUR%SRC_TYP(SUR%L_SCN), -1, &
     &                                   SUR__FINE, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1619, IUER, 'SUR_TRANSIT', 'Trap '// &
     &               'of internal control: error in SUR_SLEW_TIME ' )
                 RETURN
            END IF
!
            CALL ERR_PASS ( IUER, IER )
            SLEW_TIME_2ND(J6) = SUR_SLEW_TIME ( SUR_2ND, VTD, SUR__TYP_TAG, &
     &                                   J6, SUR_2ND%IND_SRC(SUR_2ND%L_SCN), &
     &                                   SUR_2ND%SRC_TYP(SUR%L_SCN), -1, &
     &                                   SUR__FINE, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 1620, IUER, 'SUR_TRANSIT', 'Trap '// &
     &               'of internal control: error in SUR_SLEW_TIME ' )
                 RETURN
            END IF
!
            NS = NS + 1
            IF ( IVRB .GE. 5 ) THEN
                 WRITE ( 6, 140 ) J5, NS, SUR%SOU(J6)%J2000_NAME, &
     &                            DTIM_RISE/3600.0D0, DTIM_SET/3600.0D0
 140             FORMAT ( 'Scan ', I3,' Variant: ', I3, ' Source: ', A, &
     &                    ' Dtim_rise: ', F5.2, ' Dtim_set: ', &
     &                    F5.2, ' hours' )
            END IF
            IF ( SUR%SOU(J6)%DELTA < 30.0D0*DEG__TO__RAD ) THEN
                 DEC_WGT = (30.0D0 - SUR%SOU(J6)%DELTA/DEG__TO__RAD)**2
               ELSE
                 DEC_WGT = 1.0D0
            END IF
            TIM_WGT = 1.D6/(1.0D0 + DTIM_SET - GAP(J6))
            SLE_WGT = 1.D4/(SLEW_TIME_1ST(J6) + SLEW_TIME_2ND(J6))
!
            SCORE(NS)  = DEC_WGT + TIM_WGT + SLE_WGT
            IND_R8(NS) = J6 + 1.D-5
 460     CONTINUE
!
         IF ( NS == 0 ) THEN
              IF ( IVRB .GE. 4 ) THEN
                   STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                   WRITE ( 6, 150 ) STR(1:21)
 150               FORMAT ( 'No more primary sources to observe at ', A )
              END IF
              GOTO 850
         END IF
         CALL SORT8 ( NS, SCORE, IND_R8 )
         IND_SRC = IND_R8(NS)
!
         IF ( IVRB .GE. 5 ) THEN
              STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
              WRITE ( 6, 160 ) J5, SUR%SOU(IND_SRC)%J2000_NAME, SCORE(NS), &
     &                         STR(1:19), SLEW_TIME_1ST(IND_SRC)
 160          FORMAT ( 'Scan ', I3, ' Source: ',A,' Score: ', F10.1, 2X, A, &
     &                 ' Slew: ', F6.1 )
         END IF
!
         MJD_OBS = SUR%MJD_CUR
         TAI_OBS = SUR%TAI_CUR
!
! ------ Update scan counter
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
         SUR%TAI_OBS_BEG(SUR%L_SCN) = TAI_OBS + SLEW_TIME_1ST(IND_SRC) &
     &                                + SUR%PREOBS_LONG
         UTC_OBS_INT = IDNINT ( SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
         IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
              SUR%TAI_OBS_BEG(SUR%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                     - SUR%UTC_M_TAI
            ELSE
              SUR%TAI_OBS_BEG(SUR%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
         END IF
         SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                SUR%SOU(IND_SRC)%DUR
         IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
         END IF
         SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
         SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
         SUR%SRC_TYP(SUR%L_SCN) = SUR__TYP_TAG
         SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
!
! ------ Compute final azimuth and elevation of the observation, since
! ------ now we know the time when it will happen
!
         DO 480 J8=1,SUR%L_STA
            SUR%OBS_STA(J8,SUR%L_SCN) = 0
            IF ( SUR%STA(J8)%TAGALONE ) GOTO 480
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, SUR__TYP_TAG, MJD_OBS, TAI_OBS, J8, &
     &                      IND_SRC, AZ, ELEV, HA, IER )
            IF ( SUR%STA(J8)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------------- Update accumulative aziumth in order to take into account
! -------------- cable wrap
!
                 DIF_AZ = (AZ - SUR%STA(J8)%AZ_CUR)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J8)%AZ_ACC_MAX ) THEN
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR - &
     &                                               (PI2 - DIF_AZ)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J8)%AZ_ACC_MIN ) THEN
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + DIF_AZ
                        ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ clock-wise
!
                           SUR%STA(J8)%AZ_ACC_CUR = SUR%STA(J8)%AZ_ACC_CUR + &
     &                                              (PI2 + DIF_AZ)
                      END IF
                 END IF
!
                 IF ( ELEV < SUR%STA(J8)%EL_MIN .OR. &
     &                ELEV > SUR%STA(J8)%EL_MAX      ) THEN
!
                      SUR%EL_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%EL_CUR
                      SUR%AZ_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_CUR
                      SUR%HA_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_CUR
                      SUR%AZ_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_ACC_CUR
                      GOTO 480
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
                IF ( SUR%SOU(IND_SRC)%DELTA < SUR%STA(J8)%EL_MIN .OR. &
     &               SUR%SOU(IND_SRC)%DELTA > SUR%STA(J8)%EL_MAX      ) THEN
!
                     SUR%EL_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%EL_CUR
                     SUR%AZ_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_CUR
                     SUR%HA_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_CUR
                     SUR%AZ_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_ACC_CUR
                     SUR%HA_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_ACC_CUR
                     SUR%STA(J8)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                     SUR%STA(J8)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                     GOTO 480
                END IF
            END IF
            IF ( .NOT. SUR_CHECK_VIS ( SUR, J8, SUR__TYP_TAG, IND_SRC, &
     &                                 AZ, ELEV, HA, IER ) ) THEN
!
! -------------- The J8-th station does not see the IND_SRC -th source
!
!
                SUR%EL_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%EL_CUR
                SUR%AZ_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_CUR
                SUR%HA_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_CUR
                SUR%AZ_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_ACC_CUR
                SUR%HA_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_ACC_CUR
                SUR%STA(J8)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                SUR%STA(J8)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                GOTO 480
            END IF
!
            SUR%STA(J8)%AZ_CUR  = AZ
            SUR%STA(J8)%EL_CUR  = ELEV
            SUR%STA(J8)%HA_CUR  = HA
            SUR%STA(J8)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
            SUR%STA(J8)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
            SUR%EL_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%EL_CUR
            SUR%AZ_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_CUR
            SUR%HA_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_CUR
            SUR%AZ_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%AZ_ACC_CUR
            SUR%HA_ACC_OBS(J8,SUR%L_SCN) = SUR%STA(J8)%HA_ACC_CUR
            SUR%OBS_STA(J8,SUR%L_SCN) = SUR__USED
 480     CONTINUE
!
! ------ Update SUR_2ND scan counter for the 2nd transit
!
         SUR_2ND%L_SCN   = SUR_2ND%L_SCN + 1
         SUR_2ND%MJD_OBS_BEG(SUR_2ND%L_SCN) = SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN-1)
         SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) = SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN-1) &
     &                                        + SLEW_TIME_2ND(IND_SRC) &
     &                                        + SUR%PREOBS_LONG
         UTC_OBS_INT = IDNINT ( SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
         IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
              SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                             - SUR%UTC_M_TAI
            ELSE
              SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
         END IF
         IF ( SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) > 86400.0D0 ) THEN
              SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) = SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) - 86400.0D0
              SUR_2ND%MJD_OBS_BEG(SUR_2ND%L_SCN) = SUR_2ND%MJD_OBS_BEG(SUR_2ND%L_SCN) + 1
         END IF
         SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) = MJD_OBS
         SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) = SUR_2ND%TAI_OBS_BEG(SUR_2ND%L_SCN) + &
     &                                        SUR_2ND%SOU(IND_SRC)%DUR
         IF ( SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) > 86400.0D0 ) THEN
              SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) = SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN) - 86400.0D0
              SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) = SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN) + 1
         END IF
         SUR_2ND%MJD_CUR = SUR%MJD_OBS_END(SUR_2ND%L_SCN)
         SUR_2ND%TAI_CUR = SUR%TAI_OBS_END(SUR_2ND%L_SCN)
!
         SUR_2ND%NOBS_SRC(IND_SRC) = SUR_2ND%NOBS_SRC(IND_SRC) + 1
         SUR_2ND%IND_SCN_SRC(SUR_2ND%NOBS_SRC(IND_SRC),IND_SRC) = SUR_2ND%L_SCN
!
         SUR_2ND%IND_SRC(SUR_2ND%L_SCN) = IND_SRC
         SUR_2ND%IND_TAP(SUR_2ND%L_SCN) = SUR_2ND%L_TAP
         SUR_2ND%MJD_CUR = SUR_2ND%MJD_OBS_END(SUR_2ND%L_SCN)
         SUR_2ND%TAI_CUR = SUR_2ND%TAI_OBS_END(SUR_2ND%L_SCN)
         SUR_2ND%SRC_TYP(SUR_2ND%L_SCN) = SUR__TYP_TAG
         SUR_2ND%OBS_STA(1:SUR__M_STA,SUR_2ND%L_SCN) = 0
         SUR_2ND%L_SCN_SO1 = SUR_2ND%L_SCN_SO1 + 1
!
         IF ( (SUR_2ND%MJD_OBS_BEG(1) - SUR%MJD_CUR)*86400.0D0 + &
     &        (SUR_2ND%TAI_OBS_BEG(1) - SUR%TAI_CUR) < 2.0D0*SUR%AVR_SLEW_TIME ) THEN
!
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) - 1
              SUR%L_SCN     = SUR%L_SCN - 1

              SUR_2ND%NOBS_SRC(IND_SRC) = SUR_2ND%NOBS_SRC(IND_SRC) - 1
              SUR_2ND%L_SCN = SUR_2ND%L_SCN - 1
              SUR_2ND%L_SCN_SO1 = SUR_2ND%L_SCN_SO1 - 1
              IF ( IVRB .GE. 4 ) THEN
                   STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                   WRITE ( 6, '(A)' ) 'No time to start the 2nd observation for scan at '//STR(1:21)
              END IF
              GOTO 850
         END IF
         IF ( (SUR_2ND%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &        (SUR_2ND%TAI_CUR - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) - 1.5*TROP_BURST_LEN > 0.0D0  ) THEN
!
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) - 1
              SUR%L_SCN     = SUR%L_SCN - 1
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 - 1
              SUR_2ND%NOBS_SRC(IND_SRC) = SUR_2ND%NOBS_SRC(IND_SRC) - 1
              SUR_2ND%L_SCN = SUR_2ND%L_SCN - 1
              SUR_2ND%L_SCN_SO1 = SUR_2ND%L_SCN_SO1 - 1
              IF ( IVRB .GE. 4 ) THEN
                   STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                   WRITE ( 6, '(A)' ) 'No time to stop the 2nd observation for scan at '//STR(1:21)
                   STR = MJDSEC_TO_DATE ( SUR_2ND%MJD_CUR, SUR_2ND%TAI_CUR, -2 )
                   WRITE ( 6, '(A)' ) 'Current time for the 2nd scan: '//STR(1:21)
              END IF
              GOTO 850
         END IF
         IF ( (SUR%MJD_CUR - SUR%MJD_START)*86400.0D0 + &
     &        (SUR%TAI_CUR - SUR%TAI_START) > GAP_HALF - TROP_BURST_LEN/2.0D0 ) THEN
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) - 1
              SUR%L_SCN     = SUR%L_SCN - 1
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 - 1
              SUR_2ND%NOBS_SRC(IND_SRC) = SUR_2ND%NOBS_SRC(IND_SRC) - 1
              SUR_2ND%L_SCN = SUR_2ND%L_SCN - 1
              SUR_2ND%L_SCN_SO1 = SUR_2ND%L_SCN_SO1 - 1
              IF ( IVRB .GE. 4 ) THEN
                   STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                   WRITE ( 6, '(A)' ) 'No time for the 2nd observation for scan at '//STR(1:21)
              END IF
              GOTO 850
         END IF
!
         IF ( IVRB .GE. 5 ) THEN
              STR = MJDSEC_TO_DATE ( SUR_2ND%MJD_CUR, SUR_2ND%TAI_CUR, -2 )
              WRITE ( 6, 165 ) 'ScaN', J5, SUR_2ND%SOU(IND_SRC)%J2000_NAME, &
     &                         STR(1:19)
 165          FORMAT ( A, I4, ' Source: ',A, ' Time: ', A )
         END IF
 450  CONTINUE
 850  CONTINUE
!
! --- Insert a burst of troposphere calibrators
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1621, IUER, 'SUR_TRANSIT', 'Error in '// &
     &         'an attempt to insert a burst of troposphere calibrators' )
           RETURN
      END IF
      NTB = NTB + 1
!
      SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
      SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
!
      DO 490 J9=1,SUR_2ND%L_SCN
         SUR%L_SCN   = SUR%L_SCN + 1
         SUR%IND_SRC(SUR%L_SCN) = SUR_2ND%IND_SRC(J9)
         SUR%IND_TAP(SUR%L_SCN) = SUR_2ND%L_TAP
         SUR%SRC_TYP(SUR%L_SCN) = SUR_2ND%SRC_TYP(J9)
         IND_SRC = SUR%IND_SRC(SUR%L_SCN)
         SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR%SRC_TYP(SUR%L_SCN), &
     &                   SUR%IND_SRC(SUR%L_SCN), SUR%IND_SRC(SUR%L_SCN-1), &
     &                   SUR%SRC_TYP(SUR%L_SCN-1), -1, SUR__FINE, IER )
         IF ( J9 == 1 ) THEN
              IF ( SLEW_TIME_MAX > (SUR_2ND%MJD_OBS_BEG(J9) - SUR%MJD_CUR)*86400.0D0 + &
     &                             (SUR_2ND%TAI_OBS_BEG(J9) - SUR%TAI_CUR)             ) THEN
                   write ( 6, * ) ' slew_time_max = ', slew_time_max
                   write ( 6, * ) ' mjd_cur/tai_cur = ', sur%mjd_cur, sur%tai_cur  ! %%%%
                   write ( 6, * ) ' mjd_2nd/tai_2nd = ', sur_2nd%mjd_obs_beg(j9), sur_2nd%tai_obs_beg(j9) ! %%%
!                   CALL ERR_LOG ( 1622, IUER, 'SUR_TRANSIT', 'Not enough '// &
!     &                 'time to start the second run' )
!                   RETURN
              END IF
              SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR_2ND%MJD_OBS_BEG(J9)
              SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR_2ND%TAI_OBS_BEG(J9)
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR_2ND%MJD_OBS_END(J9)
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR_2ND%TAI_OBS_END(J9)
            ELSE
              SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
              SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR &
     &                                     + SLEW_TIME_MAX &
     &                                     + SUR%PREOBS_LONG
              UTC_OBS_INT = IDNINT ( SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
              IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
                   SUR%TAI_OBS_BEG(SUR%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                         - SUR%UTC_M_TAI
                ELSE
                   SUR%TAI_OBS_BEG(SUR%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
              END IF
              IF ( SUR%TAI_OBS_BEG(SUR%L_SCN) > 86400.0D0 ) THEN
                   SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) - 86400.0D0
                   SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_OBS_BEG(SUR%L_SCN) + 1
              END IF
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_BEG(SUR%L_SCN)
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &           ( SUR_2ND%MJD_OBS_END(J9) - SUR_2ND%MJD_OBS_BEG(J9) )*86400.0D0 + &
     &           ( SUR_2ND%TAI_OBS_END(J9) - SUR_2ND%TAI_OBS_BEG(J9) )
!
              IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
                   SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                   SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
              END IF
         END IF
!
         IF ( SUR_2ND%SRC_TYP(J9) == SUR__TYP_TAG ) THEN
              SUR%NOBS_SRC(IND_SRC) = SUR%NOBS_SRC(IND_SRC) + 1
              SUR%IND_SCN_SRC(SUR%NOBS_SRC(IND_SRC),IND_SRC) = SUR%L_SCN
         END IF
!
         DO 4100 J10=1,SUR%L_STA
            SUR%OBS_STA(J10,SUR%L_SCN) = 0
            IF ( SUR%STA(J10)%TAGALONE ) GOTO 4100
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, SUR_2ND%SRC_TYP(J9), MJD_OBS, TAI_OBS, &
     &                      J10, IND_SRC, AZ, ELEV, HA, IER )
            IF ( SUR%STA(J10)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------------- Update accumulative aziumth in order to take into account
! -------------- cable wrap
!
                 DIF_AZ = (AZ - SUR%STA(J10)%AZ_CUR)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J10)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J10)%AZ_ACC_MAX ) THEN
                           SUR%STA(J10)%AZ_ACC_CUR = SUR%STA(J10)%AZ_ACC_CUR + DIF_AZ
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J10)%AZ_ACC_CUR = SUR%STA(J10)%AZ_ACC_CUR - &
     &                                               (PI2 - DIF_AZ)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J10)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J10)%AZ_ACC_MIN ) THEN
                           SUR%STA(J10)%AZ_ACC_CUR = SUR%STA(J10)%AZ_ACC_CUR + DIF_AZ
                        ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ clock-wise
!
                           SUR%STA(J10)%AZ_ACC_CUR = SUR%STA(J10)%AZ_ACC_CUR + &
     &                                              (PI2 + DIF_AZ)
                      END IF
                 END IF
!
                 IF ( ELEV < SUR%STA(J10)%EL_MIN .OR. &
     &                ELEV > SUR%STA(J10)%EL_MAX      ) THEN
!
                      SUR%EL_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%EL_CUR
                      SUR%AZ_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_CUR
                      SUR%HA_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_CUR
                      SUR%AZ_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_ACC_CUR
                      GOTO 4100
                 END IF
               ELSE IF ( SUR%STA(J10)%MOUNT_TYPE == MT__EQUAT ) THEN
                 DIF_HA = (HA - SUR%STA(J10)%HA_CUR)
                 DIF_HA = DIF_HA - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J10)%HA_ACC_CUR + DIF_AZ < SUR%STA(J10)%AZ_ACC_MAX ) THEN
                           SUR%STA(J10)%HA_ACC_CUR = SUR%STA(J10)%HA_ACC_CUR + DIF_HA
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J10)%HA_ACC_CUR = SUR%STA(J10)%HA_ACC_CUR - &
     &                                              (PI2 - DIF_HA)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J10)%HA_ACC_CUR + DIF_HA > SUR%STA(J10)%AZ_ACC_MIN ) THEN
                           SUR%STA(J10)%HA_ACC_CUR = SUR%STA(J10)%HA_ACC_CUR + DIF_HA
                         ELSE
!
! ----------------------- The shortest way is not possible, move the longest way
! ----------------------- clock-wise
!
                          SUR%STA(J10)%HA_ACC_CUR = SUR%STA(J10)%HA_ACC_CUR + &
     &                                             (PI2 + DIF_HA)
                     END IF
                END IF
                IF ( SUR%SOU(IND_SRC)%DELTA < SUR%STA(J10)%EL_MIN .OR. &
     &               SUR%SOU(IND_SRC)%DELTA > SUR%STA(J10)%EL_MAX      ) THEN
!
                     SUR%EL_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%EL_CUR
                     SUR%AZ_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_CUR
                     SUR%HA_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_CUR
                     SUR%AZ_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_ACC_CUR
                     SUR%HA_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_ACC_CUR
                     SUR%STA(J10)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                     SUR%STA(J10)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                     GOTO 4100
                END IF
            END IF
            IF ( .NOT. SUR_CHECK_VIS ( SUR, J10, SUR_2ND%SRC_TYP(J9), &
     &                                 IND_SRC, AZ, ELEV, HA, IER ) ) THEN
!
! -------------- The J10-th station does not the IND_SRC -th source
!
!
                SUR%EL_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%EL_CUR
                SUR%AZ_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_CUR
                SUR%HA_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_CUR
                SUR%AZ_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_ACC_CUR
                SUR%HA_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_ACC_CUR
                SUR%STA(J10)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                SUR%STA(J10)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                GOTO 4100
            END IF
!
            SUR%STA(J10)%AZ_CUR  = AZ
            SUR%STA(J10)%EL_CUR  = ELEV
            SUR%STA(J10)%HA_CUR  = HA
            SUR%STA(J10)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
            SUR%STA(J10)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
            SUR%EL_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%EL_CUR
            SUR%AZ_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_CUR
            SUR%HA_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_CUR
            SUR%AZ_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%AZ_ACC_CUR
            SUR%HA_ACC_OBS(J10,SUR%L_SCN) = SUR%STA(J10)%HA_ACC_CUR
            SUR%OBS_STA(J10,SUR%L_SCN) = SUR__USED
 4100    CONTINUE
!
         SUR%SRC_TYP(SUR%L_SCN) = SUR_2ND%SRC_TYP(J9)
         IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
              SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
              SUR%L_SCN_CAL = SUR%L_SCN_CAL + 1
         END IF
!
         SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
         SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
!
         IF ( IVRB .GE. 5 ) THEN
              STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
              IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                  WRITE ( 6, 167 ) 'SCAN_2ND', J9, SUR%SOU(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(1)
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                  WRITE ( 6, 167 ) 'SCAN_2ND', J9, SUR%SO2(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(2)
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                  WRITE ( 6, 167 ) 'SCAN_2ND', J9, SUR%CAL(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(3)
              END IF
 167          FORMAT ( A, I3, ' Source: ',A, ' Time: ', A, '  Type: ', A )
         END IF
 490  CONTINUE
!
! --- Insert the last burst of troposphere calibrators
!
      MJD_STOP_SAVE = SUR%MJD_STOP
      TAI_STOP_SAVE = SUR%TAI_STOP 
!
! --- Adjust stop time. The dates may be shifted towards future.
! --- So we allow to schedule the burst of calibrators beyond the
! --- current stop date.
!
      SUR%TAI_STOP = SUR%TAI_STOP + TROP_BURST_LEN
      IF ( SUR%TAI_STOP > 86400.0D0 ) THEN
           SUR%TAI_STOP = SUR%TAI_STOP - 86400.0D0
           SUR%MJD_STOP = SUR%MJD_STOP - 1
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SUR_ASTRO_TROPO ( SUR, VTD, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1623, IUER, 'SUR_TRANSIT', 'Error in '// &
     &         'an attempt to insert a burst of troposphere calibrators' )
           RETURN
      END IF
      SUR%MJD_STOP = MJD_STOP_SAVE
      SUR%TAI_STOP = TAI_STOP_SAVE
      NTB = NTB + 1
!
      SUR_2ND = SUR
      SUR%MJD_CUR   = SUR%MJD_START
      SUR%TAI_CUR   = SUR%TAI_START
      SUR%L_SCN     = 0
      SUR%L_SCN_SO1 = 0
      SUR%L_SCN_SO2 = 0
      SUR%L_SCN_CAL = 0
      DO 510 J1=1,SUR%L_STA
         SUR%STA(J1)%EL_CUR      = -101.D0
         SUR%STA(J1)%AZ_CUR      = -101.D0
         SUR%STA(J1)%ALP_CUR     = -101.D0
         SUR%STA(J1)%DEL_CUR     = -101.D0
         SUR%STA(J1)%HA_CUR      = -101.D0
         SUR%STA(J1)%AZ_ACC_CUR  = -101.D0
         SUR%STA(J1)%HA_ACC_CUR  = -101.D0
 510  CONTINUE
!
      DO 4110 J11=1,SUR_2ND%L_SCN
         SUR%L_SCN   = SUR%L_SCN + 1
         SUR%IND_SRC(SUR%L_SCN) = SUR_2ND%IND_SRC(J11)
         SUR%IND_TAP(SUR%L_SCN) = SUR_2ND%L_TAP
         SUR%SRC_TYP(SUR%L_SCN) = SUR_2ND%SRC_TYP(J11)
         IND_SRC = SUR_2ND%IND_SRC(SUR%L_SCN)
         IF ( J11 == 1 ) THEN
              SLEW_TIME_MAX = 0.0D0
              SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_START
              SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_START + SUR%PRESES_INTERVAL
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR_2ND%MJD_OBS_BEG(SUR%L_SCN)
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                     ( SUR_2ND%TAI_OBS_END(J11) - &
     &                                       SUR_2ND%TAI_OBS_BEG(J11)   )
            ELSE
              SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR_2ND%SRC_TYP(J11), &
     &                        SUR%IND_SRC(SUR%L_SCN), SUR%IND_SRC(SUR%L_SCN-1), &
     &                        SUR%SRC_TYP(SUR%L_SCN-1), -1, SUR__FINE, IER )
              SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
              SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR &
     &                                     + SLEW_TIME_MAX &
     &                                     + SUR%PREOBS_LONG
              UTC_OBS_INT = IDNINT ( SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%UTC_M_TAI + 1.0D0 )
              IF ( MOD(UTC_OBS_INT ,IDNINT(SUR%START_ROUNDING)) .NE. 0 ) THEN
                   SUR%TAI_OBS_BEG(SUR%L_SCN) = (UTC_OBS_INT /IDNINT(SUR%START_ROUNDING)+1)*IDNINT(SUR%START_ROUNDING) &
     &                                          - SUR%UTC_M_TAI
                 ELSE
                   SUR%TAI_OBS_BEG(SUR%L_SCN) = UTC_OBS_INT - SUR%UTC_M_TAI
              END IF
!
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_BEG(SUR%L_SCN)
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + &
     &                                     ( SUR_2ND%TAI_OBS_END(J11) - &
     &                                       SUR_2ND%TAI_OBS_BEG(J11)   )
         END IF
         IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
              SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
              SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
         END IF
!
         IF ( IVRB .GE. 5 ) THEN
              STR = MJDSEC_TO_DATE ( SUR%MJD_OBS_BEG(SUR%L_SCN), &
     &                               SUR%TAI_OBS_BEG(SUR%L_SCN), -2 )
              IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
                  WRITE ( 6, 169 ) 'SCAN_FIN', J11, SUR%SOU(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(1), SLEW_TIME_MAX
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
                  WRITE ( 6, 169 ) 'SCAN_FIN', J11, SUR%SO2(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(2), SLEW_TIME_MAX
                ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
                  WRITE ( 6, 169 ) 'SCAN_FIN', J11, SUR%CAL(IND_SRC)%J2000_NAME, &
     &                              STR(1:19), SUR__TYP_STR(3), SLEW_TIME_MAX
              END IF
 169          FORMAT ( A, I3, ' Source: ',A, ' Time: ', A, '  Type: ', A, &
     &                 ' Slew: ', F6.1 )
         END IF
!
         DO 4120 J12=1,SUR%L_STA
            IF ( SUR%OBS_STA(J12,SUR%L_SCN) == SUR__UND ) GOTO 4120
            IF ( SUR%STA(J12)%TAGALONE ) GOTO 4120
!
            CALL ERR_PASS ( IUER, IER )
            CALL SUR_AZEL ( SUR, VTD, SUR%SRC_TYP(J11), SUR%MJD_CUR, &
     &                      SUR%TAI_CUR, J12, IND_SRC, AZ, ELEV, HA, IER )
            IF ( SUR%STA(J12)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------------- Update accumulative azimuth in order to take into account
! -------------- cable wrap
!
                 IF ( SUR%STA(J12)%AZ_ACC_CUR < -100.0D0 ) THEN
!
! ------------------- This means that this was the first scan
!
                      SUR%STA(J12)%AZ_CUR = AZ
                      SUR%STA(J12)%AZ_ACC_CUR = AZ
                      IF ( SUR%STA(J12)%AZ_ACC_CUR > 1.5D0*PI__NUM ) THEN
                           SUR%STA(J12)%AZ_ACC_CUR = AZ - PI2
                      END IF
                 END IF
!
                 DIF_AZ = (AZ - SUR%STA(J12)%AZ_CUR)
                 DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_AZ == 0.0D0 ) THEN
                      CONTINUE 
                   ELSE IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J12)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J12)%AZ_ACC_MAX ) THEN
                           SUR%STA(J12)%AZ_ACC_CUR = SUR%STA(J12)%AZ_ACC_CUR + DIF_AZ
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J12)%AZ_ACC_CUR = SUR%STA(J12)%AZ_ACC_CUR - &
     &                                               (PI2 - DIF_AZ)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J12)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J12)%AZ_ACC_MIN ) THEN
                           SUR%STA(J12)%AZ_ACC_CUR = SUR%STA(J12)%AZ_ACC_CUR + DIF_AZ
                        ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ clock-wise
!
                           SUR%STA(J12)%AZ_ACC_CUR = SUR%STA(J12)%AZ_ACC_CUR + &
     &                                              (PI2 + DIF_AZ)
                      END IF
                 END IF
!
                 IF ( ELEV < SUR%STA(J12)%EL_MIN .OR. &
     &                ELEV > SUR%STA(J12)%EL_MAX      ) THEN
!
                      SUR%EL_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%EL_CUR
                      SUR%AZ_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_CUR
                      SUR%HA_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_CUR
                      SUR%AZ_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_ACC_CUR
                      SUR%OBS_STA(J12,SUR%L_SCN) = SUR__UND 
                      GOTO 4120
                 END IF
               ELSE IF ( SUR%STA(J12)%MOUNT_TYPE == MT__EQUAT ) THEN
                 IF ( SUR%STA(J12)%HA_ACC_CUR < -100.0D0 ) THEN
!
! ------------------- This means that this was the first scan
!
                      SUR%STA(J12)%HA_ACC_CUR = HA
                      SUR%STA(J12)%HA_CUR = HA
                 END IF
!
                 DIF_HA = (HA - SUR%STA(J12)%HA_CUR)
                 DIF_HA = DIF_HA - PI2*IDNINT(DIF_AZ/PI2)
                 IF ( DIF_HA == 0.0D0 ) THEN
                      CONTINUE 
                   ELSE IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                      IF ( SUR%STA(J12)%HA_ACC_CUR + DIF_AZ < SUR%STA(J12)%AZ_ACC_MAX ) THEN
                           SUR%STA(J12)%HA_ACC_CUR = SUR%STA(J12)%HA_ACC_CUR + DIF_HA
                         ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                           SUR%STA(J12)%HA_ACC_CUR = SUR%STA(J12)%HA_ACC_CUR - &
     &                                              (PI2 - DIF_HA)
                      END IF
                    ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                      IF ( SUR%STA(J12)%HA_ACC_CUR + DIF_HA > SUR%STA(J12)%AZ_ACC_MIN ) THEN
                           SUR%STA(J12)%HA_ACC_CUR = SUR%STA(J12)%HA_ACC_CUR + DIF_HA
                         ELSE
!
! ----------------------- The shortest way is not possible, move the longest way
! ----------------------- clock-wise
!
                          SUR%STA(J12)%HA_ACC_CUR = SUR%STA(J12)%HA_ACC_CUR + &
     &                                             (PI2 + DIF_HA)
                     END IF
                 END IF
                 IF ( SUR%SOU(IND_SRC)%DELTA < SUR%STA(J12)%EL_MIN .OR. &
     &                SUR%SOU(IND_SRC)%DELTA > SUR%STA(J12)%EL_MAX      ) THEN
!
                      SUR%EL_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%EL_CUR
                      SUR%AZ_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_CUR
                      SUR%HA_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_CUR
                      SUR%AZ_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_ACC_CUR
                      SUR%HA_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_ACC_CUR
                      SUR%STA(J12)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                      SUR%STA(J12)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                      SUR%OBS_STA(J12,SUR%L_SCN) = SUR__UND 
                      GOTO 4120
                END IF
            END IF
!
            IF ( .NOT. SUR_CHECK_VIS ( SUR, J12, SUR%SRC_TYP(J11), IND_SRC, &
     &                                 AZ, ELEV, HA, IER ) ) THEN
!
! -------------- The J12-th station does not the IND_SRC -th source
!
!
                 SUR%EL_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%EL_CUR
                 SUR%AZ_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_CUR
                 SUR%HA_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_CUR
                 SUR%AZ_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_ACC_CUR
                 SUR%HA_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_ACC_CUR
                 SUR%STA(J12)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
                 SUR%STA(J12)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
                 SUR%OBS_STA(J12,SUR%L_SCN) = SUR__UND 
                 GOTO 4120
            END IF
            IF ( SUR%L_SCN == 1 ) THEN
!
! -------------- At VLBA the first scan should start wiht azimuth
! -------------- in range [90.0, 270] to ensure that the azimuth of the 
! -------------- cable wrap is in a predictable position
!
                 IF ( SUR%STA(J12)%NAME == 'BR-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'FD-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'HN-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'KP-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'LA-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'MK-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'NL-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'OV-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'PIETOWN ' .OR. &
     &                SUR%STA(J12)%NAME == 'SC-VLBA ' .OR. &
     &                SUR%STA(J12)%NAME == 'GBT-VLBA'      ) THEN
                      IF ( AZ < 0.5D0*PI__NUM .OR. AZ > 1.5D0*PI__NUM ) THEN
                           SUR%OBS_STA(J12,SUR%L_SCN) = SUR__UND 
                           GOTO 4120
                      END IF 
                 END IF 
            END IF
!
            SUR%STA(J12)%AZ_CUR  = AZ
            SUR%STA(J12)%EL_CUR  = ELEV
            SUR%STA(J12)%HA_CUR  = HA
            SUR%STA(J12)%ALP_CUR = SUR%SOU(IND_SRC)%ALPHA
            SUR%STA(J12)%DEL_CUR = SUR%SOU(IND_SRC)%DELTA
            SUR%EL_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%EL_CUR
            SUR%AZ_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_CUR
            SUR%HA_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_CUR
            SUR%AZ_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%AZ_ACC_CUR
            SUR%HA_ACC_OBS(J12,SUR%L_SCN) = SUR%STA(J12)%HA_ACC_CUR
            SUR%OBS_STA(J12,SUR%L_SCN) = SUR__USED
 4120    CONTINUE
!
         IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_TAG ) THEN
              SUR%L_SCN_SO1 = SUR%L_SCN_SO1 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_SEC ) THEN
              SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
            ELSE IF ( SUR%SRC_TYP(SUR%L_SCN) == SUR__TYP_CAL ) THEN
              SUR%L_SCN_CAL = SUR%L_SCN_CAL + 1
         END IF
!
         SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
         SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
 4110 CONTINUE
!
      SUR%NOBS_SO2 = 0
      DO 4130 J13=1,SUR__M_SOU
         DO 4140 J14=1,SUR%L_SO2
            IF ( ( SUR%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &           ( SUR%TAI_CUR + SUR%AVR_SLEW_TIME + SHARE__MIN_DUR*SUR%SO2(J14)%DUR &
     &             - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > 0.0D0 ) THEN
                   SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
                   SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP - SUR%POSTSES_INTERVAL
                   SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
                   GOTO 8130
            END IF
!
! --------- Check whether the source sdfrom the secondary list was in the 
! --------- priamray list and have been already observed
!
            DO 4150 J15=1,SUR%L_SOU
               IF ( SUR%SOU(J15)%J2000_NAME == SUR%SO2(J14)%J2000_NAME ) THEN
                    IF ( SUR%NOBS_SRC(J15) > 0 ) GOTO 4140
               END IF
 4150       CONTINUE 
!
            IF ( SUR%NOBS_SO2(J14) > 0 ) GOTO 4140
!
            K_STA = 0
            DO 4160 J16=1,SUR%L_STA
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, SUR__TYP_SEC, SUR%MJD_CUR, &
     &                         SUR%TAI_CUR + SUR%AVR_SLEW_TIME, J16, J14, AZ, &
     &                         ELEV, HA, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 1625, IUER, 'SUR_TRANSIT', 'Error in '// &
     &                  'an attempt to compute AZ/EL' )
                    RETURN
               END IF
               IF ( SUR_CHECK_VIS ( SUR, J16, SUR__TYP_SEC, J14, &
     &                              AZ, ELEV, HA, IER ) ) THEN
                    K_STA = K_STA + 1
               END IF
 4160       CONTINUE
            IF ( K_STA < SUR%SO2(J14)%MIN_STA ) GOTO 4140
!
            SUR%L_SCN = SUR%L_SCN + 1
            IF ( ( SUR%MJD_CUR - SUR%MJD_STOP)*86400.0D0 + &
     &           ( SUR%TAI_CUR + SUR%AVR_SLEW_TIME + SUR%SO2(J14)%DUR - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > 0.0D0 ) THEN
                 SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP - SUR%POSTSES_INTERVAL
              ELSE
                 SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_CUR
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_CUR + SUR%AVR_SLEW_TIME + SUR%SO2(J14)%DUR
            END IF
            IF ( (SUR%MJD_STOP - SUR%MJD_OBS_END(SUR%L_SCN))*86400.0D0 + &
     &           (SUR%TAI_STOP - SUR%TAI_OBS_END(SUR%L_SCN)) < &
     &           SHARE__MIN_DUR*SUR%SO2(J14)%DUR ) THEN
                 SUR%L_SCN = SUR%L_SCN - 1
                 GOTO 4140
            END IF
!
            IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                 SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
            END IF
!
            IF ( IVRB .GE. 4 ) THEN
                 STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
                 WRITE ( 6, 170 ) J13, STR, SUR%SO2(J14)%J2000_NAME
 170             FORMAT ( I2,' Secondary source ', A, ' at ', A )
            END IF
!
            SLEW_TIME_MAX = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_SEC, &
     &                                      J14, SUR%IND_SRC(SUR%L_SCN), &
     &                                      SUR%SRC_TYP(SUR%L_SCN), -1, SUR__FINE, IER )
            SUR%SRC_TYP(SUR%L_SCN) = SUR__TYP_SEC
            SUR%IND_SRC(SUR%L_SCN) = J14
            SUR%NOBS_SO2(J14) = SUR%NOBS_SO2(J14) + 1
            SUR%IND_SCN_SRC(SUR%NOBS_SO2(J14),J14) = SUR%L_SCN
!
            SUR%MJD_OBS_BEG(SUR%L_SCN) = SUR%MJD_CUR
            SUR%TAI_OBS_BEG(SUR%L_SCN) = SUR%TAI_CUR + SLEW_TIME_MAX
!
            DO 4170 J17=1,SUR%L_STA
               CALL ERR_PASS ( IUER, IER )
               CALL SUR_AZEL ( SUR, VTD, SUR__TYP_SEC, SUR%MJD_OBS_BEG(SUR%L_SCN), &
     &                         SUR%TAI_OBS_BEG(SUR%L_SCN), J17, J14, AZ, ELEV, HA, IER )
               IF ( SUR%STA(J17)%MOUNT_TYPE == MT__ALTAZ ) THEN
!
! -------------- Update accumulative aziumth in order to take into account
! -------------- cable wrap
!
                    DIF_AZ = (AZ - SUR%STA(J17)%AZ_CUR)
                    DIF_AZ = DIF_AZ - PI2*IDNINT(DIF_AZ/PI2)
                    IF ( DIF_AZ > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                         IF ( SUR%STA(J17)%AZ_ACC_CUR + DIF_AZ < SUR%STA(J17)%AZ_ACC_MAX ) THEN
                              SUR%STA(J17)%AZ_ACC_CUR = SUR%STA(J17)%AZ_ACC_CUR + DIF_AZ
                            ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                              SUR%STA(J17)%AZ_ACC_CUR = SUR%STA(J17)%AZ_ACC_CUR - &
     &                                                  (PI2 - DIF_AZ)
                         END IF
                       ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                         IF ( SUR%STA(J17)%AZ_ACC_CUR + DIF_AZ > SUR%STA(J17)%AZ_ACC_MIN ) THEN
                              SUR%STA(J17)%AZ_ACC_CUR = SUR%STA(J17)%AZ_ACC_CUR + DIF_AZ
                           ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ clock-wise
!
                              SUR%STA(J17)%AZ_ACC_CUR = SUR%STA(J17)%AZ_ACC_CUR + &
     &                                                 (PI2 + DIF_AZ)
                         END IF
                    END IF
!
                    IF ( ELEV < SUR%STA(J17)%EL_MIN .OR. &
     &                   ELEV > SUR%STA(J17)%EL_MAX      ) THEN
!
                         SUR%EL_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%EL_CUR
                         SUR%AZ_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_CUR
                         SUR%HA_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_CUR
                         SUR%AZ_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_ACC_CUR
                         SUR%HA_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_ACC_CUR
                         GOTO 4170
                    END IF
                  ELSE IF ( SUR%STA(J17)%MOUNT_TYPE == MT__EQUAT ) THEN
                    DIF_HA = (HA - SUR%STA(J17)%HA_CUR)
                    DIF_HA = DIF_HA - PI2*IDNINT(DIF_AZ/PI2)
                    IF ( DIF_HA > 0.0D0 ) THEN
!
! ------------------- The shortest move is clock-wise
!
                         IF ( SUR%STA(J17)%HA_ACC_CUR + DIF_AZ < SUR%STA(J17)%AZ_ACC_MAX ) THEN
                              SUR%STA(J17)%HA_ACC_CUR = SUR%STA(J17)%HA_ACC_CUR + DIF_HA
                            ELSE
!
! ------------------------ The shortest way is not possible, move the longest way
! ------------------------ counter-clock-wise
!
                              SUR%STA(J17)%HA_ACC_CUR = SUR%STA(J17)%HA_ACC_CUR - &
     &                                                 (PI2 - DIF_HA)
                         END IF
                       ELSE
!
! ------------------- The shortest move is counter-clock-wise
!
                         IF ( SUR%STA(J17)%HA_ACC_CUR + DIF_HA > SUR%STA(J17)%AZ_ACC_MIN ) THEN
                              SUR%STA(J17)%HA_ACC_CUR = SUR%STA(J17)%HA_ACC_CUR + DIF_HA
                            ELSE
!
! ----------------------- The shortest way is not possible, move the longest way
! ----------------------- clock-wise
!
                             SUR%STA(J17)%HA_ACC_CUR = SUR%STA(J17)%HA_ACC_CUR + &
     &                                                (PI2 + DIF_HA)
                        END IF
                    END IF
                    IF ( SUR%SO2(J14)%DELTA < SUR%STA(J17)%EL_MIN .OR. &
     &                   SUR%SO2(J14)%DELTA > SUR%STA(J17)%EL_MAX      ) THEN
!
                         SUR%EL_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%EL_CUR
                         SUR%AZ_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_CUR
                         SUR%HA_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_CUR
                         SUR%AZ_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_ACC_CUR
                         SUR%HA_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_ACC_CUR
                         SUR%STA(J17)%ALP_CUR = SUR%SO2(J14)%ALPHA
                         SUR%STA(J17)%DEL_CUR = SUR%SO2(J14)%DELTA
                         GOTO 4170
                   END IF
               END IF
!
               SUR%STA(J17)%AZ_CUR  = AZ
               SUR%STA(J17)%EL_CUR  = ELEV
               SUR%STA(J17)%HA_CUR  = HA
               SUR%STA(J17)%ALP_CUR = SUR%SO2(J13)%ALPHA
               SUR%STA(J17)%DEL_CUR = SUR%SO2(J13)%DELTA
               SUR%EL_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%EL_CUR
               SUR%AZ_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_CUR
               SUR%HA_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_CUR
               SUR%AZ_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%AZ_ACC_CUR
               SUR%HA_ACC_OBS(J17,SUR%L_SCN) = SUR%STA(J17)%HA_ACC_CUR
               SUR%OBS_STA(J17,SUR%L_SCN) = SUR__USED
 4170       CONTINUE
!
            SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_BEG(SUR%L_SCN)
            SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + SUR%SO2(J14)%DUR
            IF ( SUR%TAI_OBS_END(SUR%L_SCN) > 86400.0D0 ) THEN
                 SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_END(SUR%L_SCN) - 86400.0D0
                 SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_OBS_END(SUR%L_SCN) + 1
            END IF
!
            SUR%L_SCN_SO2 = SUR%L_SCN_SO2 + 1
            SUR%MJD_CUR = SUR%MJD_OBS_END(SUR%L_SCN)
            SUR%TAI_CUR = SUR%TAI_OBS_END(SUR%L_SCN)
!
! --------- Goto to the next scan
!
            GOTO 4130
 4140    CONTINUE
!
! ------ We did not find any secondary scan. Extend the last scan to the
! ------ stop time and that is it. What can we do more?
!
! ------ Double the scan time for the last scan...
!
         DT = (SUR%MJD_STOP - SUR%MJD_OBS_END(SUR%L_SCN))*86400.0D0 + &
     &        (SUR%TAI_STOP - SUR%POSTSES_INTERVAL - SUR%TAI_OBS_END(SUR%L_SCN)) 
         SCAN_DUR = ( SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_OBS_BEG(SUR%L_SCN) )*86400.D0 + &
     &              ( SUR%TAI_OBS_END(SUR%L_SCN) - SUR%TAI_OBS_BEG(SUR%L_SCN) )
!
! ------ ... but make it no longer than stop time
!
         SCAN_DUR = SCAN_DUR + MIN ( DT, SCAN_DUR )
         SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_OBS_BEG(SUR%L_SCN) + SCAN_DUR 
         IF ( IVRB .GE. 4 ) THEN
              STR = MJDSEC_TO_DATE ( SUR%MJD_CUR, SUR%TAI_CUR, -2 )
              WRITE ( 6, 180 ) STR(1:21)
 180          FORMAT ( 'No more secondary sources to observe at ', A )
         END IF
         GOTO 8130
 4130 CONTINUE
 8130 CONTINUE
!
! --- Remove scans that are acidentally went beyond the session stop time
!
      N_SCA = SUR%L_SCN
      DO 4180 J18=N_SCA,1,-1
         IF ( (SUR%MJD_OBS_END(J18) - SUR%MJD_STOP)*86400.0D0 + &
     &        (SUR%TAI_OBS_END(J18) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > SUR%TROPO_SCAN_LEN/2.0D0 ) THEN
              IF ( IVRB .GE. 4 ) THEN
                   WRITE ( 6, 190 ) SUR%L_SCN 
 190               FORMAT ( 'Removed scan ', I4,' because it went beyond ', &
     &                      'the stop time' )
              END IF
              SUR%L_SCN = J18-1
         END IF
 4180 CONTINUE 
!
! --- Check if the stop time for the last scan is still over the session stop time
!
      IF ( (SUR%MJD_OBS_END(SUR%L_SCN) - SUR%MJD_STOP)*86400.0D0 + &
     &     (SUR%TAI_OBS_END(SUR%L_SCN) - (SUR%TAI_STOP - SUR%POSTSES_INTERVAL)) > 0.0D0 ) THEN
           IF ( IVRB .GE. 4 ) THEN
                WRITE ( 6, 1100 ) SUR%L_SCN
 1100           FORMAT ( 'Adjusted last scan ', I4,' because it went ', &
     &                   'beyond the stop time' )
           END IF
           SUR%MJD_OBS_END(SUR%L_SCN) = SUR%MJD_STOP
           SUR%TAI_OBS_END(SUR%L_SCN) = SUR%TAI_STOP - SUR%POSTSES_INTERVAL
      END IF
      SUR%SCAN_TYPE(SUR%L_SCN) = SUR__LAST
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_TRANSIT  !#!#

