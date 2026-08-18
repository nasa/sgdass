!
! >> include-block for nutation estimation
! >> L. Petrov  12-JAN-2000       v 3.5  11-OCT-2003 19:00:08
!
        INTEGER*4  M_NW, M_TRM, M_PT, M_LS, M_TYP, M_PRT, M_TID, &
     &             M_EOP, M_ADH, MALL_EOP, M_PAR
        PARAMETER  ( M_NW  =  2048 )
        PARAMETER  ( M_TRM =   128 )
        PARAMETER  ( M_PT  =  4096 )
        PARAMETER  ( M_LS  =   256 )
        PARAMETER  ( M_TYP =    21 )
        PARAMETER  ( M_TID =   256 )
        PARAMETER  ( M_EOP =    64 )
        PARAMETER  ( M_ADH =   256 )
        PARAMETER  ( MALL_EOP = 3*M_EOP )
        PARAMETER  ( M_PRT =  2048 )
        PARAMETER  ( M_PAR = 16384 )
        REAL*8       J2000_JD_TDB, NUT_AMPL_MIN, TID_AMPL_MIN
        PARAMETER  ( J2000_JD_TDB = 2451545.0D0 )
        PARAMETER  ( NUT_AMPL_MIN = 1.D-12 )
        PARAMETER  ( TID_AMPL_MIN = 0.0001 )
        REAL*8     FREQ_LOW, AMPL_LOW
!!        PARAMETER  ( FREQ_LOW = FREQ_CUTOFF/2.0D0 )
        PARAMETER  ( FREQ_LOW = 1.D-14 ) ! rad/sec
        PARAMETER  ( AMPL_LOW = 1.D-14 ) ! rad
        REAL*8       PRC_PSI__RATE, PRC_EPS__RATE
        PARAMETER  ( PRC_PSI__RATE =  7.725650327D-12 ) ! rad/sec IERS 1996 +cor
        PARAMETER  ( PRC_EPS__RATE = -7.195790557D-14 ) ! rad/sec IERS 1996 +cor
        CHARACTER    USER_NUT_LABEL*27
        PARAMETER (  USER_NUT_LABEL = 'USER_NUT v 3.00  2003.03.02' )
!@        REAL*8       EPSILON_0 ! nominal inclination ecliptique to equator
!@        PARAMETER  ( EPSILON_0 = 0.4090928041D0 )
!
        INTEGER*4  PSI__EPS, RET__PRG, TRA__ADD
        PARAMETER  ( PSI__EPS = 1 )
        PARAMETER  ( RET__PRG = 2 )
        PARAMETER  ( TRA__ADD = 3 )
!
        INTEGER*4  OFF__NUT, PRC__NUT, NPS__NUT, NPC__NUT, NES__NUT, &
     &             NEC__NUT, AXC__NUT, AXS__NUT, AYC__NUT, AYS__NUT, &
     &             AUC__NUT, AUS__NUT, HXC__NUT, HXS__NUT, HYC__NUT, &
     &             HYS__NUT, HUC__NUT, HUS__NUT, SPX__NUT, SPY__NUT, &
     &             SUT__NUT, PIN__NUT, RIN__NUT, POU__NUT, ROU__NUT
        PARAMETER  ( OFF__NUT =  1 )
        PARAMETER  ( PRC__NUT =  2 )
        PARAMETER  ( NPC__NUT =  3 )
        PARAMETER  ( NPS__NUT =  4 )
        PARAMETER  ( NEC__NUT =  5 )
        PARAMETER  ( NES__NUT =  6 )
        PARAMETER  ( HXC__NUT =  7 )
        PARAMETER  ( HXS__NUT =  8 )
        PARAMETER  ( HYC__NUT =  9 )
        PARAMETER  ( HYS__NUT = 10 )
        PARAMETER  ( HUC__NUT = 11 )
        PARAMETER  ( HUS__NUT = 12 )
        PARAMETER  ( AXC__NUT = 13 )
        PARAMETER  ( AXS__NUT = 14 )
        PARAMETER  ( AYC__NUT = 15 )
        PARAMETER  ( AYS__NUT = 16 )
        PARAMETER  ( AUC__NUT = 17 )
        PARAMETER  ( AUS__NUT = 18 )
        PARAMETER  ( SPX__NUT = 19 )
        PARAMETER  ( SPY__NUT = 20 )
        PARAMETER  ( SUT__NUT = 21 )
!
        PARAMETER  ( PIN__NUT =  3 )
        PARAMETER  ( RIN__NUT =  4 )
        PARAMETER  ( POU__NUT =  5 )
        PARAMETER  ( ROU__NUT =  6 )
!
        INTEGER*4  NPC__PAR, NPS__PAR, NEC__PAR, NES__PAR, &
     &             PIN__PAR, RIN__PAR, POU__PAR, ROU__PAR, &
     &             PRP__PAR, PRE__PAR, TID__COS, TID__SIN
        PARAMETER  ( NPC__PAR = 1 )
        PARAMETER  ( NPS__PAR = 2 )
        PARAMETER  ( NEC__PAR = 3 )
        PARAMETER  ( NES__PAR = 4 )
        PARAMETER  ( RIN__PAR = 1 )
        PARAMETER  ( PIN__PAR = 2 )
        PARAMETER  ( ROU__PAR = 3 )
        PARAMETER  ( POU__PAR = 4 )
        PARAMETER  ( PRP__PAR = 1 )
        PARAMETER  ( PRE__PAR = 2 )
        PARAMETER  ( TID__COS = 1 )
        PARAMETER  ( TID__SIN = 2 )
!
	INTEGER*4  NUT__KND, TID__KND, ADH__KND
	PARAMETER  ( NUT__KND = 1 )
	PARAMETER  ( TID__KND = 2 )
	PARAMETER  ( ADH__KND = 3 )
!
	INTEGER*4  INC__ACT, EXC__ACT, UNDF__ACT
	PARAMETER  ( UNDF__ACT =  0 )
	PARAMETER  ( INC__ACT  =  1 )
	PARAMETER  ( EXC__ACT  = -1 )
!
        INTEGER*4    PSI__PAR, EPS__PAR
        PARAMETER  ( PSI__PAR = 1 )
        PARAMETER  ( EPS__PAR = 2 )
!
        INTEGER*4  WRI__NUT, REA__NUT
        PARAMETER  ( WRI__NUT = 1 )
        PARAMETER  ( REA__NUT = 2 )
!
	INTEGER*4    TIDES__UNDF, TIDES__CALC, TIDES__MTH
	PARAMETER  ( TIDES__UNDF = 0  )
	PARAMETER  ( TIDES__CALC = 91 )
	PARAMETER  ( TIDES__MTH  = 99 )
!
        TYPE      NUT__REC
            INTEGER*4  FIRST_FIELD
            INTEGER*4  CAL_MODE
            INTEGER*4  SIDELOBE_MODE
!
            REAL*8     FREQ_CUTOFF
            REAL*8     NUT_FREQ_MAX
            REAL*8     NUTAMP_CUTOFF
            REAL*8     TIDAMP_CUTOFF
            REAL*8     BEG_EPOCH_SEC_TDB
            REAL*8     MID_EPOCH_SEC_TDB
            REAL*8     END_EPOCH_SEC_TDB
            REAL*8     REFERENCE_EPOCH_SEC_TDB
!
            INTEGER*4  PRCEST_MODE
            INTEGER*4  NUTEST_MODE
            INTEGER*4  HFVEST_MODE
            INTEGER*4  EOPEST_MODE
            INTEGER*4  WINDOW_MODE
            INTEGER*4  ADDITIVE_MODE
            INTEGER*4  FCN_MODE
!
            REAL*8     EOP_SEG
            REAL*8     SIGMA_TIE
            REAL*8     SIGMA_TRN
            REAL*8     SIGMA_RAT
            REAL*8     SIGMA_HRM
!C
            REAL*8     START_JD_TDB  ! Julian Date (TDB scale) of the 1-st obs
            REAL*8     TIMEOP_TDB    ! Time interval (TDB) of EOP reference
!                                    ! epoch elapsed from the 1-st observation
!C
            REAL*8     DPSI
            REAL*8     DEPS
!
            INTEGER*4  N_NUT
            INTEGER*4  N_TID
            INTEGER*4  N_ADH
!
            INTEGER*4  WAVE_PRC(M_TRM)
            INTEGER*4  NUMB_PRC
            INTEGER*4  WAVE_NUT(M_TRM,M_NW)
            INTEGER*4  MAIN_NUT(M_NW)
            INTEGER*4  NUMB_NUT(M_NW)
            INTEGER*4  WAVE_TID(M_TRM,M_TID)
            INTEGER*4  MAIN_TID(M_TID)
            INTEGER*4  NUMB_TID(M_TID)
!
            REAL*8     PHAS_NUT(M_ADH)
            REAL*8     PHAS_TID(M_ADH)
            REAL*8     PHAS_ADH(M_ADH)
!
            REAL*8     FREQ_NUT(M_NW)
            REAL*8     FREQ_TID(M_NW)
            REAL*8     FREQ_ADH(M_ADH)
!
            LOGICAL*1  USE_NUT(M_TRM,M_NW)
            LOGICAL*1  MLT_NUT(2,M_NW)
!
            REAL*8     MEAN_PRC(2)
            REAL*8     RATE_PRC(2)
            REAL*8     AMPL_NUT(M_NW,4)
            REAL*8     AMPL_TID(M_NW)
            REAL*8     AMPL_ADH(M_NW)
!
            REAL*8     AVR_MEAN_PRC(2)
            REAL*8     AVR_RATE_PRC(2)
            REAL*8     AVR_AMPL_NUT(M_NW,4)
            REAL*8     AVR_AMPL_TID(M_NW,2)
!
            REAL*8     TIM_EPOCH(M_EOP)
            REAL*8     BACK_FRACTION
            REAL*8     FORW_FRACTION
            REAL*8     COEF(M_TYP,M_NW)
            INTEGER*4  N_EOP
            INTEGER*4  I_SEG
!
            INTEGER*4  N_PRT(M_TRM)
            INTEGER*4  IND_TRM(M_PT)
            INTEGER*4  IREF_PRT(M_PRT,M_TYP)
!
	    INTEGER*4  N_USF
	    INTEGER*4  ACTN_ALL
	    REAL*8     FREQ_USF(M_NW)
	    INTEGER*4  WAVE_USF(M_NW)
	    INTEGER*4  ACTN_USF(M_NW)
!
            INTEGER*4  N_SES
            INTEGER*4  I_SES
            CHARACTER  USNFILE*256
            CHARACTER  ARCFILE*256
            CHARACTER  APRIORI_FILE*256
            CHARACTER  ADHEST_FILE*256
            CHARACTER  ADH_NAME(M_ADH)*16
            CHARACTER  USER_FREQ_FILE*256
	    CHARACTER  AMD_DIR*256
	    INTEGER*4  TIDES_MODE
	    INTEGER*4  TIDES_ARR(8)
!
            INTEGER*4  IUC
            INTEGER*4  LAST_FIELD
        END TYPE  NUT__REC  ! NUT__REC !
!
        CHARACTER  SIN_COS__STL*16, RET_PRG__STL*16, PSI_EPS__STL*16
        PARAMETER  ( SIN_COS__STL     = 'sin_cos         ' )
        PARAMETER  ( RET_PRG__STL     = 'ret_prg         ' )
        PARAMETER  ( PSI_EPS__STL     = 'psi_eps         ' )
        CHARACTER  IAU1976__REF*16, IAU1980__REF*16,  REN2000__REF*16,  &
     &             MHB2000__REF*16, IERS1996__REF*16, WAHR1980__REF*16, & 
     &             MHB2000__REN2000__REF*16, MHB2000_TRANSF__REF*16,    &
     &             MHB2000_ADDON__REF*16, NONE__REF*16
        PARAMETER  ( NONE__REF             = 'none            ' )
        PARAMETER  ( IAU1976__REF          = 'IAU1976         ' )
        PARAMETER  ( IAU1980__REF          = 'IAU1980         ' )
        PARAMETER  ( REN2000__REF          = 'REN2000         ' )
        PARAMETER  ( MHB2000__REF          = 'MHB2000         ' )
        PARAMETER  ( IERS1996__REF         = 'IERS1996        ' )
        PARAMETER  ( WAHR1980__REF         = 'WAHR1980        ' )
        PARAMETER  ( MHB2000_TRANSF__REF   = 'MHB2000_TRANSF  ' )
        PARAMETER  ( MHB2000_ADDON__REF    = 'MHB2000_ADDON   ' )
        PARAMETER  ( MHB2000__REN2000__REF = 'MHB2000__REN2000' )
        CHARACTER  MAS_YR__UNITS*16, MAS__UNITS*16, MAS_MUSEC__UNITS*16, &
     &             PRAD__UNITS*16
        PARAMETER  ( MAS_YR__UNITS    = 'mas_yr          ' )
        PARAMETER  ( MAS__UNITS       = 'mas             ' )
        PARAMETER  ( MAS_MUSEC__UNITS = 'mas_musec       ' )
        PARAMETER  ( PRAD__UNITS      = 'prad            ' )
        INTEGER*4    M_RES
        PARAMETER  ( M_RES = 19 )
!
        TYPE      RES_NUT
            INTEGER*4  FIRST_FIELD
            CHARACTER  CONF_FILE*128
            CHARACTER  INP_PREF*128
            CHARACTER  OUT_PREF*128
            LOGICAL*4  NUT_CALIB
!
            CHARACTER  PRC_REFER*16
            CHARACTER  PRC_STYLE*16
            CHARACTER  PRC_UNITS*16
            CHARACTER  NUT_REFER*16
            CHARACTER  NUT_STYLE*16
            CHARACTER  NUT_UNITS*16
            CHARACTER  HFE_REFER*16
            CHARACTER  HFE_STYLE*16
            CHARACTER  HFE_UNITS*16
            CHARACTER  ADH_REFER*16
            CHARACTER  ADH_STYLE*16
            CHARACTER  ADH_UNITS*16
!
	    LOGICAL*4  FL_NUT_HEO
	    LOGICAL*4  FL_TID_HEO
	    LOGICAL*4  FL_ADH_HEO
!
	    REAL*8     N_SIG	
!
            REAL*8     NPS_VAL(0:M_NW)
            REAL*8     NEC_VAL(0:M_NW)
            REAL*8     NPC_VAL(0:M_NW)
            REAL*8     NES_VAL(0:M_NW)
!
            REAL*8     NPS_SIG(0:M_NW)
            REAL*8     NEC_SIG(0:M_NW)
            REAL*8     NPC_SIG(0:M_NW)
            REAL*8     NES_SIG(0:M_NW)
!
            REAL*8     RIN_VAL(0:M_NW)
            REAL*8     PIN_VAL(0:M_NW)
            REAL*8     ROU_VAL(0:M_NW)
            REAL*8     POU_VAL(0:M_NW)
!
            REAL*8     RIN_SIG(0:M_NW)
            REAL*8     PIN_SIG(0:M_NW)
            REAL*8     ROU_SIG(0:M_NW)
            REAL*8     POU_SIG(0:M_NW)
!
            REAL*8     HFE_VAL(3,2,M_TID)
            REAL*8     HFE_SIG(3,2,M_TID)
            REAL*8     HFE_FRQ(M_TID)
!
            INTEGER*4  IND_TERM(M_PAR)
            INTEGER*4  IND_TYPE(M_PAR)
            INTEGER*4  N_NUT
            INTEGER*4  N_HFE
!
            CHARACTER  HFE_NAM(M_TID)*16
!
            INTEGER*4  STATUS
            INTEGER*4  LAST_FIELD
        END TYPE  RES_NUT  !  RES_NUT  !
!
! << end of include block nut.i  for nutation estimation
!
