      SUBROUTINE NCORT_M ( IDB2, CDBNAM, NCREC )
! ************************************************************************
! *                                                                      *
! *      Auxilary subroutine NCORT_M provides simplified interface to    *
! *   the NCORT. It keeps all parameters needed to NCORT in data         *
! *   structure  NCREC.                                                  *
! *                                                                      *
! *  ###  12-SEP-97   NCORT_M      v1.2  (c)  L. Petrov  17-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'ncrec.i'
      INTEGER*2  IDB2
      CHARACTER  CDBNAM(15)*(*)
!
      TYPE ( NCREC__STRU ) ::  NCREC
!
      CALL NCORT ( NCREC%JSITN, &
     &             NCREC%JSITI, &
     &             NCREC%JCAPPL, &
     &             NUMSTA, &
     &             NCREC%ITT, &
     &             IDB2, &
     &             IDATYP, &
     &             NCREC%ITTB, &
     &             NCREC%ET, &
     &             NCREC%SE, &
     &             NCREC%SS, &
     &             NCREC%OBCAPL, &
     &             NCREC%MCAPL, &
     &             NCREC%JCAVAL, &
     &             NCREC%LATS, &
     &             NCREC%HEIGHTS, &
     &             NCREC%AX_TYPES, &
     &             NCREC%AX_OFFS, &
     &             NCREC%BARO_CALS, &
     &             NCREC%BARO_HEIGHTS, &
     &             NCREC%JCAFFL, &
     &             NCREC%FCAL_NAMES, &
     &             NCREC%NFCAL, &
     &             NCREC%NAMSTA, &
     &             CALCV &
     &           )
      CALL GET_AVG_ATM ( CDBNAM, NCREC%AVG_ATM )
      RETURN
      END  !#!  NCORT_M  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SOCAL_M ( IDB2, NCREC, APP, DERR_RAW, RERR_RAW, DPHER_RAW, &
     &           TAU_CALC, RATE_CALC, COR_TAU, COR_RATE, &
     &           ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X, TAUGR_OBS_S, TAUPH_OBS_X, TAUPH_OBS_S, &
     &           TAUSB_OBS_X, TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S, &
     &           TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X, TAUSB_ERR_S, &
     &           RATE_OBS_X,  RATE_OBS_S,  RATE_ERR_X,  RATE_ERR_S, &
     &           FREQ_GR_X,   FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S, &
     &           FREQ_RATE_X, FREQ_RATE_S, TAU_OC, RATE_OC, TAU_E, RATE_E, &
     &           NOGOOD )
! ************************************************************************
! *                                                                      *
! *      Auxilary subroutine SOCAL_M provides simplified interface to    *
! *   the GET_CALIB. It keeps majority parameters needed to SOCAL in     *
! *   data structure  NCREC. Delays, rates and their formal errors from  *
! *   oborg-area are left untouched.                                     *
! *                                                                      *
! *  ###  12-SEP-1997   SOCAL_M    v2.5  (c) L. Petrov  11-NOV-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INCLUDE   'ncrec.i'
      INTEGER*2  IDB2
      LOGICAL*2  NOGOOD
      REAL*8     APP(2,2), DERR_RAW, RERR_RAW, DPHER_RAW
      TYPE ( NCREC__STRU ) ::  NCREC
      REAL*8     DT_SAVE, RT_SAVE, DERR_SAVE, RERR_SAVE, DPHER_SAVE
      REAL*8     TAU_CALC,    RATE_CALC,     COR_TAU, &
     &           COR_RATE,    ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X, TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X, TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X, TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,  RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,   FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X, FREQ_RATE_S,   TAU_OC,        RATE_OC, &
     &           TAU_E,       RATE_E
!
! --- Correct possibly wrong formal errors
!
      IF ( DERR  .GT. TAU_ERR__BAD  ) DERR  = TAU_ERR__BAD
      IF ( DERR  .LT. TAU_ERR__TINY ) DERR  = TAU_ERR__TINY
      IF ( DPHER .GT. TAU_ERR__BAD  ) DPHER = TAU_ERR__BAD
      IF ( DPHER .LT. TAU_ERR__TINY ) DPHER = TAU_ERR__TINY
!
! --- Preparing data for calibration
!
      TAU_CALC     = DT/1.D6
      RATE_CALC    = RT
      TAUGR_OBS_X  = DOBS/1.D6
      TAUGR_OBS_S  = DOBSXS/1.D6
      TAUPH_OBS_X  = DPH/1.D6
      TAUPH_OBS_S  = DPHXS/1.D6
      TAUSB_OBS_X  = DNB/1.D6
      TAUSB_OBS_S  = DNB_S/1.D6
      TAUGR_ERR_X  = DERR
      TAUGR_ERR_S  = DERRXS
      TAUPH_ERR_X  = DPHER
      TAUPH_ERR_S  = DPHERXS
      TAUSB_ERR_X  = DNBER
      TAUSB_ERR_S  = DNBER_S
      RATE_OBS_X   = ROBS
      RATE_OBS_S   = ROBSXS
      RATE_ERR_X   = RERR
      RATE_ERR_S   = RERRXS
      IF ( .NOT. FL_EQUAL_EFF_FREQ ) THEN
           FREQ_GR_X    = EFFREQ*1.D6
           FREQ_GR_S    = EFFREQ_XS*1.D6
           FREQ_PH_X    = PHEFFREQ*1.D6
           FREQ_PH_S    = PHEFFREQ_XS*1.D6
           FREQ_RATE_X  = REFFREQ*1.D6
           FREQ_RATE_S  = REFFREQ_XS*1.D6
         ELSE
           FREQ_GR_X    = MEAN_EFF_FREQ(1)*1.D6
           FREQ_GR_S    = MEAN_EFF_FREQ(2)*1.D6
           FREQ_PH_X    = MEAN_EFF_FREQ(3)*1.D6
           FREQ_PH_S    = MEAN_EFF_FREQ(4)*1.D6
           FREQ_RATE_X  = MEAN_EFF_FREQ(5)*1.D6
           FREQ_RATE_S  = MEAN_EFF_FREQ(6)*1.D6
      END IF
!
! --- Saving some variables which will be sploiled by GET_CALIB
!
      DT_SAVE    = DT
      RT_SAVE    = RT
      DERR_SAVE  = DERR
      RERR_SAVE  = RERR
      DPHER_SAVE = DPHER
!
! --- Zeroing some variables. They will be modified (calibrated) by
! --- GET_CALIB. As a result they will accumulate all calibrations imposed
! --- by GET_CALIB
!
      DT    = 0.0D0
      RT    = 0.0D0
!
      CALL GET_CALIB ( NCREC%JCAPPL, NCREC%JSITI, NCREC%ITT, NOGOOD, ISITE, &
     &     DT, RT, CALIBS, ICORR, GION, GIONSG, PHION, PHIONS, DERR, RERR, &
     &     DPHER, NCREC%ITTB, NCREC%ET, NCREC%SE, NCREC%SS, CALIBB, CALIBM, &
     &     NCREC%OBCAPL, NCREC%MCAPL, ISITN, ISTAR, VSTARC, AZ, ELEV, ATMPR, &
     &     RELHU, TEMPC, DERR_RAW, RERR_RAW, DPHER_RAW, NCREC%LATS, &
     &     NCREC%HEIGHTS, NCREC%AX_OFFS, NCREC%AX_TYPES, NCREC%BARO_CALS, &
     &     NCREC%BARO_HEIGHTS, APP, NCREC%JCAFFL, &
     &     NCREC%NFCAL, NCREC%FCAL_NAMES, NCREC%NAMSTA, IDB2, &
     &     EFFREQ, PHEFFREQ, REFFREQ, REFFREQ_XS, EFFREQ_XS, PHEFFREQ_XS, &
     &     AXDIF, ISTRN_CHR(ISTAR), SOURCE_WEIGHT_FILE, &
     &     SOURCE_WEIGHTS, NCREC%AVG_ATM, KELDEP_NOISE, ATM_ZENDEL, &
     &     RWT_EL_USE, RWT_SRC_USE, TROP_WZD, AP, FJD, FRACTC, &
     &     %VAL(ADR_TRP), STS_TRP, TRP_USE, VTD_STATUS )
!
! --- Collecting these changes
!
      COR_TAU  = DT/1.D6
      COR_RATE = RT/1.D6
!
! --- Calculation of additive weight corrections. They should be positive
! --- or zero in the case of normal work. However it is possible situation
! --- when this correction appeared to be imaginary. In the case when the
! --- effective frequency is less than 5GHz, GET_CALIB consider input
! --- DERR as a formal error of group delay at the S-band. It recalculte
! --- DERR on the fly and output DERR is formal error of ionosphere
! --- free linear comination of X- and S- band observables + additive
! --- correction. DERR_RAW, DPHER_RAW, RERR_RAW kept up to now values
! --- of formal error of ionosphere free linear combination without
! --- applying baseline-dependent and source dependent quadratical
! --- correction to weithts
!
      IF ( DERR - DERR_SAVE .GT. -1.D-14 ) THEN
           ADDERR_GR_TAU = DSQRT ( DABS ( DERR**2  - DERR_SAVE**2  ) )
           DERR_RAW      = DSQRT ( DERR_SAVE**2    + DERR_RAW**2  )
        ELSE
!
! -------- Special trick to handle S-band situation. Postfix X should not
! -------- embarass -- it means here S-band (It is true, I don't lie!!)
!
           ADDERR_GR_TAU = DSQRT ( DABS ( DERR**2  - DERR_RAW**2  ) )
           TAUGR_ERR_X   = DERR_RAW
      END IF
!
      IF ( DPHER - DPHER_SAVE .GT. -1.D-14 ) THEN
           ADDERR_PH_TAU = DSQRT ( DABS ( DPHER**2 - DPHER_SAVE**2 ) )
           DPHER_RAW     = DSQRT ( DPHER_SAVE**2 + DPHER_RAW**2 )
         ELSE
           ADDERR_PH_TAU = DSQRT ( DABS ( DPHER**2 - DPHER_RAW**2 ) )
           TAUPH_ERR_X   = DPHER_RAW
      END IF
!
      IF ( RERR - RERR_SAVE .GT. -1.D-16 ) THEN
           ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_SAVE**2  ) )
           RERR_RAW    = DSQRT ( RERR_SAVE**2  + RERR_RAW**2  )
         ELSE
           ADDERR_RATE = DSQRT ( DABS ( RERR**2  - RERR_RAW**2   ) )
           RATE_ERR_X  = RERR_RAW
      END IF
!
! --- Restoring modified variables. They are not calibrated, but left in form
! --- in which they were kept in scratch file.
!
      DT    = DT_SAVE
      RT    = RT_SAVE
      DERR  = DERR_SAVE
      RERR  = RERR_SAVE
      DPHER = DPHER_SAVE
!
      RETURN
      END  !#!  SOCAL_M  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ATMPART_M ( IDB2, NCREC )
! ************************************************************************
! *                                                                      *
! *       Auxilary subroutine ATMPART_M provides simplified interface to *
! *   the ATMPART. It keeps all parameters needed to ATMPART in data     *
! *   structure  NCREC.                                                  *
! *                                                                      *
! *  ###  12-SEP-97   ATMPART_M    v1.0  (c)  L. Petrov  12-SEP-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'oborg.i'
      INCLUDE   'ncrec.i'
      INTEGER*2  IDB2
      TYPE ( NCREC__STRU ) ::  NCREC
!
      CALL ATMPART ( NCREC%ITT, ISITE, ISITN, ISTAR, VSTARC, AZ, ELEV, &
     &               ATMPR, RELHU, TEMPC, NCREC%LATS, NCREC%HEIGHTS, &
     &               NCREC%AX_OFFS, NCREC%AX_TYPES, NCREC%BARO_CALS, &
     &               NCREC%BARO_HEIGHTS, IDB2 )
!
      RETURN
      END  !#!  ATMPART_M  #!#
