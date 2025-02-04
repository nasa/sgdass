      SUBROUTINE UPDATE_THEOR_DELAY ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  UPDATE_THEOR_DELAY 
! *                                                                      *
! * ### 24-JAN-2010 UPDATE_THEOR_DELAY v1.2 (c) L. Petrov 25-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'vtd.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      TYPE    ( VTD__TYPE ) :: VTD
      TYPE    ( VTD__OBS_TYPE ) :: OBS_TYP
      INTEGER*4  IUER
      INTEGER*4  L_STA, L_SOU, MJD_UTC_BEG, MJD_UTC_END, MJD_BEG, MJD_END, &
     &           MJD_TAI_OBS, MJD_UTC_OBS, MBUF, IP, IP1, IP2, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, L_ACM, &
     &           CDP_NUMBER(MAX_ARC_STA), MJD_BEG_INIT, MJD_END_INIT, &
     &           IND_STA, IER
!      
      INTEGER*2  ICONT_I2, IERR_I2
      PARAMETER  ( MBUF = 256 )
      LOGICAL*2  FL_11, FL_FOUND, FL_ECC_FOUND
      CHARACTER  VTD_CONF_SES_SAVE*128, STR*128, BUF(MBUF)*128, &
     &           C_STA(MAX_ARC_STA)*8, STA_ACM(M_ACM)*8, STA_NAME*8, &
     &           VTD_CONF_SES_VAR*128
      REAL*8     UTC_BEG, UTC_END, TAI_BEG, TAI_END, TAI_OBS, UTC_OBS, &
     &           FJD_BEG, FJD_END, PRES_VAL, TEMP_VAL, TMIN, UTC_MINUS_TAI, &
     &           DELAY_THR, RATE_THR, DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           CLOOF_ACM(M_ACM), CLODR_ACM(M_ACM), TIME_BEG, TIME_END, &
     &           TIME_ECC_BEG, TIME_ECC_END, ECC_TRS(3,MAX_ARC_STA), &
     &           TAI_BEG_INIT, TAI_END_INIT, TIME_REF, TT, TT2
      REAL*8       EPSILON_0, JUL_YEAR__TO__SEC, UTCMTAI_MAX
      PARAMETER  ( EPSILON_0 = 0.4090928041D0 )  ! rad
      PARAMETER  ( JUL_YEAR__TO__SEC = 365.25D0*86400.0D0 ) ! Julian year
      PARAMETER  ( UTCMTAI_MAX = 120.0D0 ) ! maximum value of UTCMTAI
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL UN_CURSES ()
      CALL USE_PARFIL ( 'OR' ) 
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_INIT ( VTD,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8311, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &         'an atttempt to initialize VTD' )
           RETURN 
      END IF
!
! --- Check envirnoment variable with the name of the VTD configuration file
!
      CALL GETENVAR ( 'PSOLVE_CHECK_VCAT', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:1) == 'Y' ) THEN
           WRITE ( 6, * ) 'UPDATE_THEOR_DELAY-1:         VTD_CONF_SES= '//TRIM(VTD_CONF_SES)
           CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
      END IF
!
      CALL GETENVAR ( 'VTD_CONF_SES', VTD_CONF_SES_VAR )
      IF ( STR(1:1) == 'Y' ) THEN
           WRITE ( 6, * ) 'UPDATE_THEOR_DELAY-2: env var VTD_CONF_SES= '//TRIM(VTD_CONF_SES_VAR)
           CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
      END IF
!
      IF ( ILEN(VTD_CONF_SES_VAR) .NE. 0 ) VTD_CONF_SES = VTD_CONF_SES_VAR
      IF ( STR(1:1) == 'Y' ) THEN
           WRITE ( 6, * ) 'UPDATE_THEOR_DELAY-3:         VTD_CONF_SES= '//TRIM(VTD_CONF_SES)
           CALL HIT_CONT ( 'Hit any key to continue', 0 ) 
      END IF
!
      WRITE ( 6, '(A)' ) 'Update theoretical model... VTD: '//TRIM(VTD_CONF_SES)
!
! --- Parse VTD condiguration file
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_CONF ( VTD_CONF_SES, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8312, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &         'an attempt to parse VTD configuration file '//VTD_CONF_SES )
           RETURN 
      END IF
!
      CALL GETENVAR ( 'VTD_TEST', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%TEST(1) )
      END IF
!
! --- Check for a special kludge variable
!
      CALL GETENVAR ( 'VTD_DEBUG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%IVRB )
      END IF
!
      VTD_CONF_SES_SAVE = VTD_CONF_SES
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_3 ( 'R'  )
      CALL USE_GLBFIL_4 ( 'RC'  )
      FL_VTD_SES = .TRUE.
      VTD_CONF_SES = VTD_CONF_SES_SAVE
      CALL USE_GLBFIL_4 ( 'OWC'  )
!
      L_STA = NUMSTA
      L_SOU = NUMSTR
!
      CALL OBSTM ( FJD_BEG, FJD_END )
      CALL JD_TO_MJD_SEC  ( FJD_BEG, MJD_UTC_BEG, UTC_BEG )
      CALL JD_TO_MJD_SEC  ( FJD_END, MJD_UTC_END, UTC_END )
!
      DO 410 J1=1,NUMSTA
         C_STA(J1) = ISITN_CHR(J1)
         CALL VTD_NAME_REPAIR ( C_STA(J1) )
 410  CONTINUE 
      MJD_BEG_INIT = MJD_UTC_BEG
      TAI_BEG_INIT = UTC_BEG - UTCMTAI_MAX 
      IF ( TAI_BEG_INIT < 0.0D0 ) THEN
           TAI_BEG_INIT = TAI_BEG_INIT + 86400.0D0
           MJD_BEG_INIT = MJD_BEG_INIT - 1
      END IF
      MJD_END_INIT = MJD_UTC_END
      TAI_END_INIT = UTC_END + UTCMTAI_MAX 
      IF ( TAI_END_INIT < 0.0D0 ) THEN
           TAI_END_INIT = TAI_END_INIT + 86400.0D0
           MJD_END_INIT = MJD_END_INIT - 1
      END IF
!
! --- Load parameters of the theoretical models
! 
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_LOAD ( VTD, L_STA, C_STA, L_SOU, ISTRN_CHR, &
     &                MJD_BEG_INIT, TAI_BEG_INIT, &
     &                MJD_END_INIT, TAI_END_INIT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8313, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &         'an attempt to load files with auxiliary information '// &
     &         'need for computation of theoretical path delay' )
           RETURN 
      END IF
      IF ( VTD%CONF%FINAM_LEAPSEC .NE. VTD__NERS_STR ) THEN
!
! -------- Load leap second file from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_LEAPSEC ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8314, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &              'an attempt to load leap second file '//VTD%CONF%FINAM_LEAPSEC )
                RETURN
           END IF
!
! -------- Transform the begin date from UTC to TAI
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_UTC_TO_TAI ( VTD, MJD_UTC_BEG, UTC_BEG, TAI_BEG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8315, IUER, 'UPDATE_THEOR_DELAY', 'Error in an '// &
     &              'attempt to get UTC-minus TAI' )
                RETURN
           END IF
           IF ( TAI_BEG < 0.0D0 ) THEN
                TAI_BEG = TAI_BEG + 86400.0D0
                MJD_BEG = MJD_UTC_BEG - 1
              ELSE
                MJD_BEG = MJD_UTC_BEG
           END IF
!
! -------- Transform the end date from UTC to TAI
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_UTC_TO_TAI ( VTD, MJD_UTC_END, UTC_END, TAI_END, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8316, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &              'an attempt to get UTC-minus TAI' )
                RETURN
           END IF
           IF ( TAI_END < 0.0D0 ) THEN
                TAI_END = TAI_END + 86400.0D0
                MJD_END = MJD_UTC_END - 1
              ELSE
                MJD_END = MJD_UTC_END
           END IF
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_GET_UTCMTAI ( VTD%NERS, UTC_BEG + (MJD_UTC_BEG - J2000__MJD)*86400.0D0, &
     &                             UTC_M_TAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8317, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &              'an attempt to get UTC-minus TAI' )
                RETURN
           END IF
           TAI_BEG = UTC_BEG - UTC_M_TAI
           IF ( TAI_BEG > 0.0D0 ) THEN
                MJD_BEG = MJD_UTC_BEG
              ELSE
                MJD_BEG = MJD_UTC_BEG - 1
                TAI_BEG = TAI_BEG + 86400.0D0
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_GET_UTCMTAI ( VTD%NERS, UTC_END + (MJD_UTC_END - J2000__MJD)*86400.0D0, &
     &                             UTC_M_TAI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8318, IUER, 'UPDATE_THEOR_DELAY', 'Error in '// &
     &              'an attempt to get UTC-minus TAI' )
                RETURN
           END IF
           TAI_END = UTC_END - UTC_M_TAI
           IF ( TAI_END > 0.0D0 ) THEN
                MJD_END = MJD_UTC_END
              ELSE
                MJD_END = MJD_UTC_END - 1
                TAI_END = TAI_END + 86400.0D0
           END IF
      END IF
      UTC_MINUS_TAI = UTC_BEG - TAI_BEG
      IF ( UTC_MINUS_TAI >  43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI - 86400.0D0
      IF ( UTC_MINUS_TAI < -43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI + 86400.0D0
      TIME_BEG = (MJD_BEG - J2000__MJD)*86400.0D0 + (TAI_BEG - 43200.0D0)
      TIME_END = (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END - 43200.0D0)
!
! --- Open OBSFIL file
! 
      FL_11 = KBIT( PRE_IBATCH, INT2(11) ) 
      CALL SBIT ( PRE_IBATCH, INT2(11), 0 )
      CALL ACS_OBSFIL ( 'O' )
      IF ( FL_11 ) CALL SBIT ( PRE_IBATCH, INT2(11), 1 )
!
      L_ACM = 0
      DO 420 J2=1,NUMSTA
         IF ( J2 == 1 ) THEN
              ICONT_I2 = 1
           ELSE 
              ICONT_I2 = 0
         END IF
         CALL GETCARD ( INT2(1), 'ACM ', ICONT_I2, STR, IERR_I2 )
         IF ( IERR_I2 == 0 ) THEN
              READ ( STR, 110 ) IP, L_ACM, IP, STA_ACM(J2), CLOOF_ACM(J2), &
     &                          CLODR_ACM(J2)
 110          FORMAT ( 5X, I2, 1X, I2, 1X, I2, 1X, A8, 1X, D23.15, 1X, D23.15 )
            ELSE IF ( IERR_I2 == 1 ) THEN
              GOTO 820
            ELSE IF ( IERR_I2 == -3 ) THEN
              GOTO 820
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(IERR_I2), STR )
              CALL ERR_LOG ( 8319, IUER, 'UPDATE_THEOR_DELAY', 'Error '// &
     &             STR(1:I_LEN(STR))//' in an attempt to read ACM card' )
              RETURN 
         END IF
         L_ACM = L_ACM + 1
 420  CONTINUE 
 820  CONTINUE 
!
! --- Compute path delay
!
      DO 430 J3=1,NUMOBS
         CALL USE_OBSFIL ( IOBSFIL, J3, 'R' )
         IF ( J3 == 1 ) THEN
              TMIN = FJD + FRACTC
         END IF
!
! ------ NB: we need to transform FJD+FRACTC by parts, otherwise
! ------ we will lose precision which will result in an additional
! ------ noise in delay with rms 10-15 ps!!!
!
         CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
         UTC_OBS = UTC_OBS + FRACT*86400.0D0
         UTC_M_TAI = UTC_MINUS_TAI
         TAI_OBS = UTC_OBS - UTC_M_TAI
         IF ( TAI_OBS < 0.0D0 ) THEN
              TAI_OBS = TAI_OBS + 86400.0D0
              MJD_TAI_OBS = MJD_UTC_OBS - 1
            ELSE
              MJD_TAI_OBS = MJD_UTC_OBS
         END IF
         TAI_OBS = UTC_OBS - UTC_M_TAI
!
! ------ Load meteorological parameters of the first station into the VTD record
!
         PRES_VAL = ATMPR(1)*100.0D0
         TEMP_VAL = TEMPC(1) + 273.16D0
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_METEO_IN ( VTD, C_STA(ISITE(1)), PRES_VAL, TEMP_VAL, &
     &                       TEMP_VAL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8320, IUER, 'UPDATE_THEOR_DELAY', 'Error in an '// &
     &            'attempt to load meteorological parameters for station '// &
     &             C_STA(ISITE(1)) )
              RETURN 
         END IF
!
! ------ Load meteorological parameters of the first station into the VTD record
!
         PRES_VAL = ATMPR(2)*100.0D0
         TEMP_VAL = TEMPC(2) + 273.16D0
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_METEO_IN ( VTD, C_STA(ISITE(2)), PRES_VAL, TEMP_VAL, &
     &                       TEMP_VAL, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8321, IUER, 'UPDATE_THEOR_DELAY', 'Error in an '// &
     &            'attempt to load meteorological parameters for station '// &
     &             C_STA(ISITE(2)) )
              RETURN 
         END IF
!
! ------ Set fields of OBS_TYP
!
         CALL SET_OBSTYP ( OBS_TYP )
!
! ------ Now compute path delay
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_DELAY ( ISTRN_CHR(ISTAR), C_STA(ISITE(1)), &
     &                    C_STA(ISITE(2)), MJD_TAI_OBS, TAI_OBS, OBS_TYP, &
     &                    VTD, DELAY_THR, RATE_THR, DER_DEL, DER_RAT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8322, IUER, 'UPDATE_THEOR_DELAY', 'Error in an '// &
     &            'attempt to compute VLBI time delay' )
              RETURN 
         END IF
!
         DT = DELAY_THR*1.D6
         RT = RATE_THR
         IF ( VTD%CONF%TEST(1) == 1 ) THEN
!
! ----------- Special kludge
!
              RELP(1) = DER_DEL(30)
         END IF
!
! ------ Delay derivative
!
         BP(1,1,1) = DER_DEL(VTD__DER_ST1X)
         BP(2,1,1) = DER_DEL(VTD__DER_ST1Y)
         BP(3,1,1) = DER_DEL(VTD__DER_ST1Z)
         BP(1,2,1) = DER_DEL(VTD__DER_ST2X)
         BP(2,2,1) = DER_DEL(VTD__DER_ST2Y)
         BP(3,2,1) = DER_DEL(VTD__DER_ST2Z)
         SP(1,1)   = DER_DEL(VTD__DER_RA)
         SP(2,1)   = DER_DEL(VTD__DER_DL)
         ROTP(1,1) = DER_DEL(VTD__DER_E2)
         ROTP(2,1) = DER_DEL(VTD__DER_E1)
         ROTP(3,1) = -DER_DEL(VTD__DER_E3)*UT1__TO__E3
         NUTP(1,1) = -( DER_DEL(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &                  DER_DEL(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &                  DSIN(EPSILON_0)
         NUTP(2,1) =  DER_DEL(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &                DER_DEL(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
         AP(1,1)   = DER_DEL(VTD__DER_AT1)
         AP(2,1)   = DER_DEL(VTD__DER_AT2)
         AGRAD_PART(1,1,1) = -DER_DEL(VTD__DER_ATN1)
         AGRAD_PART(2,1,1) = DER_DEL(VTD__DER_ATN2)
         AGRAD_PART(1,2,1) = DER_DEL(VTD__DER_ATE1)
         AGRAD_PART(2,2,1) = DER_DEL(VTD__DER_ATE2)
         AXOFP(1,1) = DER_DEL(VTD__DER_AXF1)
         AXOFP(2,1) = DER_DEL(VTD__DER_AXF2)
         FEED_ANG(1) = DER_DEL(VTD__FEED1)
         FEED_ANG(2) = DER_DEL(VTD__FEED2)
!
! ------ Delay rate derivative
!
         BP(1,1,2) =  DER_RAT(VTD__DER_ST1X)
         BP(2,1,2) =  DER_RAT(VTD__DER_ST1Y)
         BP(3,1,2) =  DER_RAT(VTD__DER_ST1Z)
         BP(1,2,2) =  DER_RAT(VTD__DER_ST2X)
         BP(2,2,2) =  DER_RAT(VTD__DER_ST2Y)
         BP(3,2,2) =  DER_RAT(VTD__DER_ST2Z)
!
         SP(1,2)   =  DER_RAT(VTD__DER_RA)
         SP(2,2)   =  DER_RAT(VTD__DER_DL)
!
         ROTP(1,2) =  DER_RAT(VTD__DER_E2)
         ROTP(2,2) =  DER_RAT(VTD__DER_E1)
         ROTP(3,2) = -DER_RAT(VTD__DER_E3)*UT1__TO__E3
!
         NUTP(1,2) = -( DER_RAT(VTD__DER_E1)*DSIN(VTD%MOM%S_ANG) + &
     &                  DER_RAT(VTD__DER_E2)*DCOS(VTD%MOM%S_ANG)   )* &
     &                  DSIN(EPSILON_0)
         NUTP(2,2) =   DER_RAT(VTD__DER_E1)*DCOS(VTD%MOM%S_ANG) - &
     &                 DER_RAT(VTD__DER_E2)*DSIN(VTD%MOM%S_ANG)
!
         AP(1,2)   =  DER_RAT(VTD__DER_AT1)
         AP(2,2)   =  DER_RAT(VTD__DER_AT2)
         AGRAD_PART(1,1,2) = -DER_RAT(VTD__DER_ATN1)
         AGRAD_PART(2,1,2) =  DER_RAT(VTD__DER_ATN2)
         AGRAD_PART(1,2,2) = -DER_RAT(VTD__DER_ATE1)
         AGRAD_PART(2,2,2) =  DER_RAT(VTD__DER_ATE2)
         AXOFP(1,2) = DER_RAT(VTD__DER_AXF1)
         AXOFP(2,2) = DER_RAT(VTD__DER_AXF2)
!
         AZ(1)   = VTD%STA(ISITE(1))%AZ
         AZ(2)   = VTD%STA(ISITE(2))%AZ
         ELEV(1) = VTD%STA(ISITE(1))%ELEV
         ELEV(2) = VTD%STA(ISITE(2))%ELEV
!
         UV_COOR(1) = VTD%UV_COOR(1)
         UV_COOR(2) = VTD%UV_COOR(2)
!
         UT1_M_TAI = VTD%MOM%UT1_M_TAI
         X_POLE    = VTD%MOM%XPL/MAS__TO__RAD
         Y_POLE    = VTD%MOM%YPL/MAS__TO__RAD
         UT1_RATE  = VTD%MOM%UT1_RATE
         XP_RATE   = VTD%MOM%XPL_RATE
         YP_RATE   = VTD%MOM%YPL_RATE
!!  write ( 6, * ) 'Sta: ', vtd%sta(isite(1))%ivs_name, ' Az= ', az(1), ' Fa= ', feed_ang(1); call pause ( 'update_theor_delay 377' ) ! %%%%%%
!
! ------ Initializing contribution of a priori clock model
!
         TAU_ACM  = 0.0D0
         RATE_ACM = 0.0D0
         IF ( L_ACM > 0 ) THEN
!
! ----------- Adding contributions from a priori clok model to theoretical
! ----------- time delay and delay rate
!
! ----------- TT  -- proper time of the first station from TMIN epoch to the moment
! -----------        of observation (in sec)
! ----------- TT2 -- proper time of the second station from TMIN epoch to the
! -----------        moment of observation (in sec)
!
              TT  = ((FJD - TMIN) + FRACTC)*86400.0D0
              TT2 = TT + DT*1.D-6
!
! ----------- IP1 -- index of the first station of the baseine in
! -----------        the list of stations for which a priori clock model
! -----------        has been applied
!
              IP1 = LTM_DIF ( 0, L_ACM, STA_ACM, ISITN_CHR(ISITE(1)) )
              IF ( IP1 .GT. 0 ) THEN
!
! ---------------- New correction of theoreticals due to up to date ACM
!
                   TAU_ACM  = TAU_ACM  - CLOOF_ACM(IP1) - CLODR_ACM(IP1)*TT
                   RATE_ACM = RATE_ACM - CLODR_ACM(IP1)
              END IF
!
! ----------- The same for the second station. But NB sign!
!
              IP2 = LTM_DIF ( 0, L_ACM, STA_ACM, ISITN_CHR(ISITE(2)) )
              IF ( IP2 .GT. 0 ) THEN
                   TAU_ACM  = TAU_ACM  + CLOOF_ACM(IP2) + CLODR_ACM(IP2)*TT2
                   RATE_ACM = RATE_ACM + CLODR_ACM(IP2)
              END IF
!
! ----------- Correction for theoreticals. NB units for DT!
!
              DT = ( DT*1.D-6 + TAU_ACM  ) *1.D6
              RT = ( RT       + RATE_ACM )
         END IF
!
         CALL USE_OBSFIL ( IOBSFIL, J3, 'W' )
 430  CONTINUE 
      CALL ACS_OBSFIL ( 'C' )
!
      DO 440 J4=1,L_STA
         FL_FOUND = .FALSE.
         DO 450 J5=1,VTD%L_STA
            IF ( VTD%STA(J5)%IVS_NAME == C_STA(J4) ) THEN
                 FL_FOUND = .TRUE.
                 VSITEC(1,J4) = VTD%STA(J5)%COO_TRS(1,1) 
                 VSITEC(2,J4) = VTD%STA(J5)%COO_TRS(2,1)
                 VSITEC(3,J4) = VTD%STA(J5)%COO_TRS(3,1)
!
                 VSITEV(1,J4) = VTD%STA(J5)%VEL_TRS(1)*JUL_YEAR__TO__SEC
                 VSITEV(2,J4) = VTD%STA(J5)%VEL_TRS(2)*JUL_YEAR__TO__SEC
                 VSITEV(3,J4) = VTD%STA(J5)%VEL_TRS(3)*JUL_YEAR__TO__SEC
                 CALL CLRCH (                         MONUMENTS_CHR(J4)      )
                 CALL INCH  ( VTD%STA(J5)%CDP_NUMBER, MONUMENTS_CHR(J4)(1:4) )
!
! -------------- Search for eccentricity
!
                 FL_ECC_FOUND = .FALSE.
                 DO 460 J6=1,VTD%STA(J5)%N_ECC 
                    TIME_ECC_BEG = &
     &                  ( VTD%STA(J5)%ECC_MJD_BEG(J6) - J2000__MJD)*86400.0D0 + &
     &                  ( VTD%STA(J5)%ECC_TAI_BEG(J6) - 43200.0D0)
                    TIME_ECC_END = &
     &                  ( VTD%STA(J5)%ECC_MJD_END(J6) - J2000__MJD)*86400.0D0 + &
     &                  ( VTD%STA(J5)%ECC_TAI_END(J6) - 43200.0D0)
!
                    IF ( TIME_BEG .GE. TIME_ECC_BEG .AND. &
     &                   TIME_END .LE. TIME_ECC_END       ) THEN
!
                         ECC_TRS(1,J4) = VTD%STA(J5)%ECC_TRS(1,J6)
                         ECC_TRS(2,J4) = VTD%STA(J5)%ECC_TRS(2,J6)
                         ECC_TRS(3,J4) = VTD%STA(J5)%ECC_TRS(3,J6)
!
                         FL_ECC_FOUND = .TRUE.
                    END IF
 460             CONTINUE 
                 IF ( .NOT. FL_ECC_FOUND ) THEN
                       write ( 6, * ) ' VTD%STA(J5)%N_ECC = ', VTD%STA(J5)%N_ECC 
                       write ( 6, * ) ' time_ecc_beg = ',time_ecc_beg
                       write ( 6, * ) ' time_ecc_end = ',time_ecc_end
                       write ( 6, * ) ' time_beg = ', time_beg, ' time_end = ', time_end 
                       CALL ERR_LOG ( 8323, IUER, 'UPDATE_THEOR_DELAY', &
     &                     'No eccentricity was found for station '// &
     &                      VTD%STA(J5)%IVS_NAME//' for the range [ '// &
     &                      MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -3 )//', '// &
     &                      MJDSEC_TO_DATE ( MJD_END, TAI_END, -3 )//'] ' )
                       RETURN 
                 END IF
!
                 CDP_NUMBER(J4) = VTD%STA(J5)%CDP_NUMBER
                 GOTO 850
            END IF
 450     CONTINUE 
 850     CONTINUE 
         IF ( .NOT. FL_FOUND ) THEN
              CALL ERR_LOG ( 8324, IUER, 'UPDATE_THEOR_DELAY', 'Coordinates '// &
     &            'of the participated station '//C_STA(J4)//' were '// &
     &            'not found in the catalogue '//VTD%CONF%FINAM_STACOO )
              RETURN 
         END IF
!
         DO 470 J7=1,L_STA
            IF ( J7 == 1 ) THEN
                 ICONT_I2 = 1 
               ELSE 
                 ICONT_I2 = 0
            END IF
            CALL GETCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
            IF ( IERR_I2 .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( INT4(IERR_I2), STR )
                 CALL ERR_LOG ( 8325, IUER, 'UPDATE_THEOR_DELAY', 'Error in reading '// &
     &               'SITE namefile card: '//STR )
                 RETURN 
            END IF
            IF ( STR(6:13) == VTD%STA(J4)%IVS_NAME ) THEN
                 WRITE ( UNIT=STR(15:18), FMT='(I4)'    ) CDP_NUMBER(J4) 
                 WRITE ( UNIT=STR(29:38), FMT='(F10.4)' ) ECC_TRS(1,J4)  
                 WRITE ( UNIT=STR(40:49), FMT='(F10.4)' ) ECC_TRS(2,J4)  
                 WRITE ( UNIT=STR(51:60), FMT='(F10.4)' ) ECC_TRS(3,J4)  
                 STR(62:63) = 'XY'
            END IF
            ICONT_I2 = 4
            CALL PUTCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
            IF ( IERR_I2 .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( INT4(IERR_I2), STR )
                 CALL ERR_LOG ( 8326, IUER, 'UPDATE_THEOR_DELAY', 'Error in writing '// &
     &               'SITE namefile card: '//STR )
                 RETURN 
            END IF
 470     CONTINUE 
 440  CONTINUE 
!
      DO 480 J8=1,L_SOU
         FL_FOUND = .FALSE.
         DO 490 J9=1,VTD%L_SOU
            IF ( ISTRN_CHR(J8) == VTD%SOU(J9)%IVS_NAME ) THEN
                 FL_FOUND = .TRUE.
                 VSTARC(1,J8) = VTD%SOU(J9)%ALPHA
                 VSTARC(2,J8) = VTD%SOU(J9)%DELTA
                 GOTO 890
            END IF
 490     CONTINUE 
 890     CONTINUE 
         IF ( .NOT. FL_FOUND ) THEN
              CALL ERR_LOG ( 8327, IUER, 'UPDATE_THEOR_DELAY', 'Coordinates '// &
     &            'of the observed source '//ISITN_CHR(J8)//' were '// &
     &            'not found in the catalogue '//VTD%CONF%FINAM_SOUCOO )
              RETURN 
         END IF
 480  CONTINUE 
!
      CALL USE_PARFIL ( 'WC' ) 
      CALL USE_GLBFIL_4 ( 'OR'  )
!
! --- Update station coordinates
!
      DO 4100 J10=1,L_STA
         IND_STA = 0
         DO 4110 J11=1,VTD%L_STA
            TIME_REF = (   (MJD_BEG - VTD%STA(J11)%MJD_REF)*86400.0D0   &
     &                   + (TAI_BEG - VTD%STA(J11)%TAI_REF)             &
     &                   + (MJD_BEG - VTD%STA(J11)%MJD_REF)*86400.0D0   &
     &                   + (TAI_BEG - VTD%STA(J11)%TAI_REF)           )/2.0D0
            STA_NAME = ISITN_CHR(J10)
            CALL VTD_NAME_REPAIR ( STA_NAME )
            IF ( STA_NAME == VTD%STA(J11)%IVS_NAME ) THEN
                 IND_STA = J11
                 VSITEC(1,J10) = VTD%STA(J11)%COO_TRS(1,1)
                 VSITEC(2,J10) = VTD%STA(J11)%COO_TRS(2,1)
                 VSITEC(3,J10) = VTD%STA(J11)%COO_TRS(3,1)
!
                 VSITEV(1,J10) = VTD%STA(J11)%VEL_TRS(1)*JUL_YEAR__TO__SEC
                 VSITEV(2,J10) = VTD%STA(J11)%VEL_TRS(2)*JUL_YEAR__TO__SEC
                 VSITEV(3,J10) = VTD%STA(J11)%VEL_TRS(3)*JUL_YEAR__TO__SEC
                 VAXOF(J10)    = VTD%STA(J11)%AXIS_OFFSET
!
                 NVSITEC(1,J10) = VTD%STA(J11)%COO_TRS(1,1) + &
     &                            VTD%STA(J11)%VEL_TRS(1)*TIME_REF
                 NVSITEC(2,J10) = VTD%STA(J11)%COO_TRS(2,1) + &
     &                            VTD%STA(J11)%VEL_TRS(2)*TIME_REF
                 NVSITEC(3,J10) = VTD%STA(J11)%COO_TRS(3,1) + &
     &                            VTD%STA(J11)%VEL_TRS(3)*TIME_REF
!
                 WRITE ( UNIT=LMONUMENTS(J10)(1:4), FMT='(I4)', &
     &                   IOSTAT=IER ) VTD%STA(J11)%CDP_NUMBER
            END IF
 4110    CONTINUE 
 4100 CONTINUE 
      CALL USE_GLBFIL_4 ( 'WC'  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE UPDATE_THEOR_DELAY  !#!#  
