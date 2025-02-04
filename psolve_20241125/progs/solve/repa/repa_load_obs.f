      SUBROUTINE REPA_LOAD_OBS ( REP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_LOAD loads observations into internal data sructures  *
! *   of REPA.                                                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 01-DEC-2004  REPA_LOAD_OBS  v1.5 (c)  L. Petrov  23-APR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i' 
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
      INCLUDE   'oborg.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'ncrec.i'
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i'
      TYPE     ( REP__TYPE   ) :: REP
      TYPE     ( NCREC__STRU ) :: NCREC
      INTEGER*4  IUER
      INTEGER*2  IDB_I2
      REAL*8     APP(2,2), DERR_RAW, RERR_RAW, DPHER_RAW
      REAL*8     TAU_CALC,      RATE_CALC,     COR_TAU,       COR_RATE, &
     &           ADDERR_GR_TAU, ADDERR_PH_TAU, ADDERR_RATE, &
     &           TAUGR_OBS_X,   TAUGR_OBS_S,   TAUPH_OBS_X,   TAUPH_OBS_S, &
     &           TAUSB_OBS_X,   TAUSB_OBS_S,   TAUGR_ERR_X,   TAUGR_ERR_S, &
     &           TAUPH_ERR_X,   TAUPH_ERR_S,   TAUSB_ERR_X,   TAUSB_ERR_S, &
     &           RATE_OBS_X,    RATE_OBS_S,    RATE_ERR_X,    RATE_ERR_S, &
     &           FREQ_GR_X,     FREQ_GR_S,     FREQ_PH_X,     FREQ_PH_S, &
     &           FREQ_RATE_X,   FREQ_RATE_S,   TAU_OC,        RATE_OC, &
     &           TAU_E,         RATE_E
      REAL*8     FJD_FIRST, FJD_LAST, ERR_SQ, ERR_MIN
      PARAMETER  ( ERR_MIN = 1.D-12  )
      INTEGER*2  NOGOOD
      CHARACTER  STR*128
      INTEGER*4  IOS, IP, J1, J2, J3, J4, J5, IER
      CHARACTER  C_BAS(REPA__M_BAS)*16, C_STA(REPA__M_STA)*8, &
     &           C_SOU(REPA__M_SOU)*8, SOLVE_PS_VIEWER_USE*128
      INTEGER*4  KA_BAS(REPA__M_BAS), KG_BAS(REPA__M_BAS), &
     &           KB_BAS(REPA__M_BAS), KU_BAS(REPA__M_BAS), &
     &           KG_SOU(REPA__M_SOU), KB_SOU(REPA__M_SOU), &
     &           KU_SOU(REPA__M_SOU)
      LOGICAL*4  LEX, FL_USED, FL_GOOD, FL_RECO, FL_REPA_MAP_ELEV
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ, DATYP_INQ
      INTEGER*4, EXTERNAL :: ADD_CLIST, ILEN, I_LEN, LTM_DIF
!
      CALL GETENVAR ( 'SOLVE_PS_VIEWER', SOLVE_PS_VIEWER_USE )
      IF ( ILEN(SOLVE_PS_VIEWER_USE) == 0 ) THEN
           SOLVE_PS_VIEWER_USE = SOLVE_PS_VIEWER
      END IF
!
      CALL GETENVAR ( 'SOLVE_REPA_MAPFUN_INSTEAD_OF_ELEV', STR )
      IF ( STR(1:3) == 'YES' .OR. STR(1:3) == 'yes' ) THEN
           FL_REPA_MAP_ELEV = .TRUE.
         ELSE 
           FL_REPA_MAP_ELEV = .FALSE.
      END IF
!
! --- Deallocate observations and residuals if they were allocated
!
      REP%N_OBS = IDBEND(1) 
      IF ( ASSOCIATED ( REP%OBS ) ) THEN
           DEALLOCATE ( REP%OBS )
      END IF
!
! --- Initialize lists
!
      CALL NOUT ( SIZEOF(REP%LIS), REP%LIS )
!
! --- Initiliaze mapping
!
      IDB_I2 = 1
      CALL FLYBY_MAP_INIT()
      CALL FLYBY_APRIOR()
!
! --- REad NAMFIL file and put results in NCREC object
!
      CALL NCORT_M ( IDB_I2, DBNAME_CH, NCREC )
!
! --- Get JD of the nominal start and nominal stop time of the session
!
      CALL OBSTM ( FJD_FIRST, FJD_LAST )
!
! --- Allocate memory for observations
!
      ALLOCATE ( REP%OBS(REP%N_OBS), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( SIZEOF(REP%OBS(1))*REP%N_OBS, STR )
           CALL ERR_LOG ( 7741, IUER, 'REPA_LOAD_OBS', 'Failure in an '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory' )
           RETURN
      END IF
!
      REP%LIS%L_STA = 0
      REP%LIS%L_BAS = 0
      REP%LIS%L_SOU = 0
!
      CALL ACS_OBSFIL   ( 'O' )
!
! --- Cycle over observations
!
      DO 410 J1=1,REP%N_OBS
!
! ------ Update observation counter
!
         IF ( MOD(J1,REPA__M_PRG) .EQ. 0  .OR.  J1 .EQ. NUMOBS ) THEN
              WRITE ( 6, 110 ) J1, NUMOBS, CHAR(13)
 110          FORMAT ( '  Load observation ',I6,' ( ',I6,' ) ',A$ )
              CALL FLUSH ( 6 )
         END IF
!
! ------ Read the observation from the scratch file
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
!
         IF ( ISITE(1) .LE. 0   .OR.  ISITE(2) .LE. 0 ) THEN
              WRITE ( 6, * ) ' Observation ', J1
              WRITE ( 6, * ) ' isite(1)=',isite(1), &
     &                       ' isite(2)=',isite(2)
              CALL ERR_LOG ( 7742, IUER, 'REPA_LOAD_OBS', 'Error during '// &
     &            'analysis of '//REP%DBNAME_STR//' : some '// &
     &            'scratch files should be updated' )
              RETURN
         END IF
!
         IF ( INDEX ( '123456789', LQUAL_CHR(2:2)    ) .GT. 0  .OR. &
     &        INDEX ( '123456789', LQUALXS_CHR(2:2)  ) .GT. 0  .OR. &
     &        SUPMET .EQ. SUPMET__PRE91                             ) THEN
!
! ----------- Calculation different mapping functions for using them in
! ----------- partials on troposphere delay in zenith direction
!
              CALL ATMPART_M ( IDB_I2, NCREC )
!
! ----------- Making calibration: adding to DT (theoretical time delay)
! ----------- and to RT (theoretical delay rate) some corrections:
!
! ----------- 1) observation dependent contributions where requested;
! ----------- 2) non-flyby calibrations;
! ----------- 3) Apply the selected flyby calibrations:
! ----------- 4) Searching over stations and across the calibration bits
! -----------    in JCAFFL, and apply the calibrations where requested.
! -----------    Signs have been selected in SDBH
! ----------- 5) Add troposphere noise based on average atmosphere delay
! -----------    (roughly elevation dependent)
! ----------- 6) add ionosphere calibration and modify errors;
! ----------- 7) setting flag of goodness of the observation due to
! -----------    ionosphere
! ----------- 8) Apply reweighting constants
!
              CALL SOCAL_M ( IDB_I2, NCREC, APP, DERR_RAW, RERR_RAW,  &
     &                       DPHER_RAW, TAU_CALC, RATE_CALC, COR_TAU, &
     &                       COR_RATE, ADDERR_GR_TAU, ADDERR_PH_TAU,  &
     &                       ADDERR_RATE, TAUGR_OBS_X, TAUGR_OBS_S,   &
     &                       TAUPH_OBS_X, TAUPH_OBS_S, TAUSB_OBS_X,   &
     &                       TAUSB_OBS_S, TAUGR_ERR_X, TAUGR_ERR_S,   &
     &                       TAUPH_ERR_X, TAUPH_ERR_S, TAUSB_ERR_X,   &
     &                       TAUSB_ERR_S, RATE_OBS_X,  RATE_OBS_S,    &
     &                       RATE_ERR_X,  RATE_ERR_S,  FREQ_GR_X,     &
     &                       FREQ_GR_S,   FREQ_PH_X,   FREQ_PH_S,     &
     &                       FREQ_RATE_X, FREQ_RATE_S, TAU_OC, &
     &                       RATE_OC,     TAU_E,       RATE_E, NOGOOD )
         END IF
!
! ------ Setting flags of suppression status
!
         IF ( SUPMET .EQ. SUPMET__META ) THEN
              CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
            ELSE 
              CALL SUPSTAT_SET ( IUNW, IUNWP, LQUAL, LQUALXS, &
     &                           ICORR, GIONSG, PHIONS, IWVBIT1, ISITE, &
     &                           NCREC%JSITI, NCREC%ITT, ISTAR, ELEV, KIONO, &
     &                           SNR, SNR_S, SUPSTAT, UACSUP )
         END IF
!
! ------ Put information about the observation into the OBS record
!
         REP%OBS(J1)%FJD     = FJD
         REP%OBS(J1)%FRACT   = FRACT
         REP%OBS(J1)%TIM_SEC = ((FJD - FJD_FIRST) + FRACT)*86400.0D0
         REP%OBS(J1)%TAU_GR_X      = DOBS*1.D-6
         REP%OBS(J1)%TAU_GR_S      = DOBS_S*1.D-6
         REP%OBS(J1)%TAU_PH_X      = DPH*1.D-6
         REP%OBS(J1)%TAU_PH_S      = DPH_S*1.D-6
         REP%OBS(J1)%TAU_SB_X      = DNB
         REP%OBS(J1)%TAU_SB_S      = DNB_S
         REP%OBS(J1)%TAU_GR_X_ORIG = DOBS_ORIG*1.D-6
         REP%OBS(J1)%TAU_GR_S_ORIG = DOBS_ORIG_S*1.D-6
         REP%OBS(J1)%TAU_PH_X_ORIG = DPH_ORIG*1.D-6
         REP%OBS(J1)%TAU_PH_S_ORIG = DPH_ORIG_S*1.D-6
         REP%OBS(J1)%ERR_GR_X      = DERR
         REP%OBS(J1)%ERR_GR_S      = DERRXS
         REP%OBS(J1)%ERR_SB_X      = DNBER
         REP%OBS(J1)%ERR_SB_S      = DNBER_S
         REP%OBS(J1)%ERR_PH_X      = DPHER
         REP%OBS(J1)%ERR_PH_S      = DPHERXS
         REP%OBS(J1)%SPAMB_GR_X    = FAMB
         REP%OBS(J1)%SPAMB_GR_S    = FAMB_S
         REP%OBS(J1)%SPAMB_PH_X    = PHAMI8*1.D-6
         REP%OBS(J1)%SPAMB_PH_S    = PHAMI8_S*1.D-6
         REP%OBS(J1)%NAMB_GR_X     = NUMAMB
         REP%OBS(J1)%NAMB_GR_S     = NUMAMB_S
         REP%OBS(J1)%NAMB_PH_X     = NPHAM4
         REP%OBS(J1)%NAMB_PH_S     = NPHAM4_S
         REP%OBS(J1)%FRQEFF_GR_X   = EFFREQ*1.D6
         REP%OBS(J1)%FRQEFF_GR_S   = EFFREQ_S*1.D6
         REP%OBS(J1)%FRQEFF_PH_X   = PHEFFREQ*1.D6
         REP%OBS(J1)%FRQEFF_PH_S   = PHEFFREQ_S*1.D6
         REP%OBS(J1)%SCAN_NAME     = SCAN_NAME     
         REP%OBS(J1)%SCAN_PIMA     = FRINGE_ROOT_FINAM
         REP%OBS(J1)%FRINGE_X_FINAM = FRINGE_X_FINAM 
         REP%OBS(J1)%FRINGE_S_FINAM = FRINGE_S_FINAM 
         IF ( .NOT. KBIT ( OPP_STATUS, OPP_SET2__BIT ) ) THEN
!
! ----------- No information about S-band is available
!
              IF (       KBIT ( NCREC%JSITI(ISITE(1)), INT2(4) ) .AND. &
     &             .NOT. KBIT ( NCREC%JSITI(ISITE(1)), INT2(5) ) .AND. &
     &                   KBIT ( NCREC%JSITI(ISITE(2)), INT2(4) ) .AND. &
     &             .NOT. KBIT ( NCREC%JSITI(ISITE(2)), INT2(5) )       ) THEN
!
! ---------------- but ionosphere calibration is applied and available
!
                   REP%OBS(J1)%FRQEFF_GR_S = FREQ__S_DEFAILT
                   IF ( DATYP_INQ ( IDATYP, GRPONL__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, GRPRAT__DTP )      ) THEN
!
! --------------------- Restore TAU_GR_S from GION(1)
!
                        REP%OBS(J1)%FRQEFF_GR_S = FREQ__S_DEFAILT
!
! --------------------- NB: sign before GION(1): GION(1) = - TAU_GROUT_IONO*1.D6
!
                        REP%OBS(J1)%TAU_GR_S = REP%OBS(J1)%TAU_GR_X + &
     &                      GION(1)*1.D-6*( REP%OBS(J1)%FRQEFF_GR_X**2 - &
     &                                      REP%OBS(J1)%FRQEFF_GR_S**2   )/ &
     &                                      REP%OBS(J1)%FRQEFF_GR_S**2
                        ERR_SQ = GIONSG(1)**2*                              &
     &                                 ( REP%OBS(J1)%FRQEFF_GR_X**2 -       &
     &                                   REP%OBS(J1)%FRQEFF_GR_S**2   )**2/ &
     &                                   REP%OBS(J1)%FRQEFF_GR_S**4 -       &
     &                           REP%OBS(J1)%ERR_GR_X**2
                        IF ( ERR_SQ .GT. ERR_MIN**2 ) THEN
                             REP%OBS(J1)%ERR_GR_S = DSQRT ( ERR_SQ )
                           ELSE
                             REP%OBS(J1)%ERR_GR_S = ERR_MIN
                        END IF
                   END IF
!
                   IF ( DATYP_INQ ( IDATYP, PHSONL__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, PHSRAT__DTP )      ) THEN
!
! --------------------- Restore TAU_PH_S from GION(1)
!
                        REP%OBS(J1)%FRQEFF_PH_S = FREQ__S_DEFAILT
!
! --------------------- NB: Sign
!
                        REP%OBS(J1)%TAU_PH_S = REP%OBS(J1)%TAU_PH_X - &
     &                      GION(1)*1.D-6/( REP%OBS(J1)%FRQEFF_PH_X**2 - &
     &                                      REP%OBS(J1)%FRQEFF_PH_S**2   )/ &
     &                                      REP%OBS(J1)%FRQEFF_PH_S**2   
                        ERR_SQ = PHIONS**2*                                 &
     &                                 ( REP%OBS(J1)%FRQEFF_PH_X**2 -       &
     &                                   REP%OBS(J1)%FRQEFF_PH_S**2   )**2/ &
     &                                   REP%OBS(J1)%FRQEFF_PH_S**4 -       &
     &                           REP%OBS(J1)%ERR_PH_X**2
                        IF ( ERR_SQ .GT. ERR_MIN**2 ) THEN
                             REP%OBS(J1)%ERR_PH_S = DSQRT ( ERR_SQ )
                           ELSE
                             REP%OBS(J1)%ERR_PH_S = ERR_MIN
                        END IF
                   END IF
!
                   IF ( DATYP_INQ ( IDATYP, SNBONL__DTP ) .OR. &
     &                  DATYP_INQ ( IDATYP, SNBRAT__DTP )      ) THEN
!
! --------------------- Restore TAU_SB_S from GION(1)
!
                        REP%OBS(J1)%FRQEFF_GR_S = FREQ__S_DEFAILT
                        REP%OBS(J1)%TAU_SB_S = REP%OBS(J1)%TAU_SB_X - &
     &                      GION(1)*1.D-6/( REP%OBS(J1)%FRQEFF_GR_X**2 - &
     &                                      REP%OBS(J1)%FRQEFF_GR_S**2   )/ &
     &                                      REP%OBS(J1)%FRQEFF_GR_S**2   
                        ERR_SQ = GIONSG(1)**2*                              &
     &                                 ( REP%OBS(J1)%FRQEFF_GR_X**2 -       &
     &                                   REP%OBS(J1)%FRQEFF_GR_S**2   )**2/ &
     &                                   REP%OBS(J1)%FRQEFF_GR_S**4 -       &
     &                           REP%OBS(J1)%ERR_GR_X**2
                        IF ( ERR_SQ .GT. ERR_MIN**2 ) THEN
                             REP%OBS(J1)%ERR_SB_S = DSQRT ( ERR_SQ )
                           ELSE
                             REP%OBS(J1)%ERR_SB_S = ERR_MIN
                        END IF
                   END IF
                 ELSE
!
! ---------------- Group inonosphere calibration is either not available
! ---------------- or not applied. Then we set S-band frequency to zero
!
                   REP%OBS(J1)%FRQEFF_GR_S = 0.0D0
                   REP%OBS(J1)%FRQEFF_PH_S = 0.0D0
                   REP%OBS(J1)%SPAMB_GR_S  = 0.0D0
              END IF
         END IF
         REP%OBS(J1)%SNR_X       = SNR
         REP%OBS(J1)%SNR_S       = SNR_S
         IF ( .NOT. FL_REPA_MAP_ELEV ) THEN
              REP%OBS(J1)%EL(1)       = ELEV(1)
              REP%OBS(J1)%EL(2)       = ELEV(2)
            ELSE
!
! ----------- Replace elevations with mapping function. 
! ----------- NB: We muplity by DEG__TO__RAD becase later they
! ----------- will be "transformed" to deg
!
              IF ( ELEV(1) > 3.0D0*DEG__TO__RAD ) THEN
                   REP%OBS(J1)%EL(1) = 1.0D0/DSIN(ELEV(1)) * DEG__TO__RAD
                 ELSE
                   REP%OBS(J1)%EL(1) = 1.0D0/DSIN(3.0D0*DEG__TO__RAD) * DEG__TO__RAD
              END IF
              IF ( ELEV(2) > 3.0D0*DEG__TO__RAD ) THEN
                   REP%OBS(J1)%EL(2) = ELEV(2) * DEG__TO__RAD
                 ELSE 
                   REP%OBS(J1)%EL(2) = 1.0D0/DSIN(3.0D0*DEG__TO__RAD) * DEG__TO__RAD
              END IF
         END IF
         REP%OBS(J1)%AZ(1)       = AZ(1)
         REP%OBS(J1)%AZ(2)       = AZ(2)
         REP%OBS(J1)%FEED_ANG(1) = FEED_ANG(1)
         REP%OBS(J1)%FEED_ANG(2) = FEED_ANG(2)
         REP%OBS(J1)%AIR_TEMP(1) = TEMPC(1) + 273.15D0
         REP%OBS(J1)%AIR_TEMP(2) = TEMPC(2) + 273.15D0
         REP%OBS(J1)%AIR_PRES(1) = ATMPR(1)*100.0D0
         REP%OBS(J1)%AIR_PRES(2) = ATMPR(2)*100.0D0
!
! ------ Load station calibrations
!
         DO 420 J2=1,8
            REP%OBS(J1)%SCAL(J2,1) = CALIBS(1,1,J2)
            REP%OBS(J1)%SCAL(J2,2) = CALIBS(2,1,J2)
 420     CONTINUE
         REP%OBS(J1)%SUPSTAT(1)  = SUPSTAT(1)
         REP%OBS(J1)%SUPSTAT(2)  = SUPSTAT(2)
         REP%OBS(J1)%UACSUP      = UACSUP
         REP%OBS(J1)%AUTO_SUP    = AUTO_SUP
         REP%OBS(J1)%USER_SUP    = USER_SUP
         REP%OBS(J1)%USER_REC    = USER_REC
         REP%OBS(J1)%QUAL_X      = LQUAL_CHR
         REP%OBS(J1)%QUAL_S      = LQUAL_S_CHR
!
! ------ Now updating list of observed sources among all observations
!
         CALL ERR_PASS ( IUER, IER )
         IP = ADD_CLIST ( REPA__M_SOU, REP%LIS%L_SOU, REP%LIS%C_SOU, &
     &                    ISTRN_CHR(ISTAR), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7743, IUER, 'REPA_LOAD_OBS', 'Error in '// &
     &             'adding a source to the list of sources' )
              RETURN
         END IF
         REP%OBS(J1)%IND_SOU = LTM_DIF ( 1, REP%LIS%L_SOU, REP%LIS%C_SOU, &
     &                                   ISTRN_CHR(ISTAR) )
!
! ------ Now updating list of particiapted stations among all observations
!
         CALL ERR_PASS ( IUER, IER )
         IP = ADD_CLIST ( REPA__M_STA, REP%LIS%L_STA, REP%LIS%C_STA, &
     &                         ISITN_CHR(ISITE(1)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7744, IUER, 'REPA_LOAD_OBS', 'Error in '// &
     &             'adding a station to the list of stations' )
              RETURN
         END IF
         REP%OBS(J1)%IND_STA(1) = LTM_DIF ( 1, REP%LIS%L_STA, REP%LIS%C_STA, &
     &                                      ISITN_CHR(ISITE(1)) )
!
         CALL ERR_PASS ( IUER, IER )
         IP = ADD_CLIST ( REPA__M_STA, REP%LIS%L_STA, REP%LIS%C_STA, &
     &                    ISITN_CHR(ISITE(2)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7745, IUER, 'REPA_LOAD_OBS', 'Error in '// &
     &             'adding a station to the list of stations' )
              RETURN
         END IF
         REP%OBS(J1)%IND_STA(2) = LTM_DIF ( 1, REP%LIS%L_STA, REP%LIS%C_STA, &
     &                                      ISITN_CHR(ISITE(2)) )
!
! ------ Now updating list of particiapted baselines among all observations
!
         CALL ERR_PASS ( IUER, IER )
         IP = ADD_CLIST ( REPA__M_BAS, REP%LIS%L_BAS, REP%LIS%C_BAS, &
     &                    ISITN_CHR(ISITE(1))//ISITN_CHR(ISITE(2)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7746, IUER, 'REPA_LOAD_OBS', 'Error in '// &
     &             'adding a baseline to the list of all baselines' )
              RETURN
         END IF
!
         IP = LTM_DIF ( 1, REP%LIS%L_BAS, REP%LIS%C_BAS, &
     &                  ISITN_CHR(ISITE(1))//ISITN_CHR(ISITE(2)) )
!
! ------ Update the source counter of observations at the IP-th baseline 
! ------ according their category
!
         REP%LIS%KA_BAS(IP) = REP%LIS%KA_BAS(IP) + 1
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, USED__SPS )
              FL_RECO = META_SUPR_INQ ( AUTO_SUP, USER_SUP, USER_REC, RECO__SPS )
            ELSE 
              FL_USED = SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS ) 
              FL_RECO = SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS ) 
         END IF
         IF ( FL_USED ) THEN
              REP%LIS%KG_BAS(IP) = REP%LIS%KG_BAS(IP) + 1
              REP%LIS%KG_SOU(REP%OBS(J1)%IND_SOU) = REP%LIS%KG_SOU(REP%OBS(J1)%IND_SOU) + 1
           ELSE IF ( .NOT. FL_USED  .AND.  FL_RECO ) THEN
              REP%LIS%KB_BAS(IP) = REP%LIS%KB_BAS(IP) + 1
              REP%LIS%KB_SOU(REP%OBS(J1)%IND_SOU) = REP%LIS%KB_SOU(REP%OBS(J1)%IND_SOU) + 1
           ELSE
              REP%LIS%KU_BAS(IP) = REP%LIS%KU_BAS(IP) + 1
              REP%LIS%KU_SOU(REP%OBS(J1)%IND_SOU) = REP%LIS%KU_SOU(REP%OBS(J1)%IND_SOU) + 1
         END IF
!!         write ( 6, * ) 'REPA_LOAD_OBS-425 J1= ', J1, ' FLGS= ', FL_USEd, FL_RECO !%%%%
         REP%OBS(J1)%IND_BAS = IP
 410  CONTINUE
      CALL ACS_OBSFIL   ( 'C' )
!
      REP%N_BAS = REP%LIS%L_BAS
      REP%N_STA = REP%LIS%L_STA
      REP%N_SOU = REP%LIS%L_SOU
!
! --- Make unsorted copy of the lists
!
      CALL LIB$MOVC3 ( REP%N_BAS*LEN(REP%LIS%C_BAS(1)), REP%LIS%C_BAS(1), C_BAS )
      CALL LIB$MOVC3 ( REP%N_STA*LEN(REP%LIS%C_STA(1)), REP%LIS%C_STA(1), C_STA )
      CALL LIB$MOVC3 ( REP%N_SOU*LEN(REP%LIS%C_SOU(1)), REP%LIS%C_SOU(1), C_SOU )
!
      CALL COPY_I4   ( REP%N_BAS, REP%LIS%KA_BAS, KA_BAS )
      CALL COPY_I4   ( REP%N_BAS, REP%LIS%KG_BAS, KG_BAS )
      CALL COPY_I4   ( REP%N_BAS, REP%LIS%KB_BAS, KB_BAS )
      CALL COPY_I4   ( REP%N_BAS, REP%LIS%KU_BAS, KU_BAS )
      CALL COPY_I4   ( REP%N_SOU, REP%LIS%KG_SOU, KG_SOU )
      CALL COPY_I4   ( REP%N_SOU, REP%LIS%KB_SOU, KB_SOU )
      CALL COPY_I4   ( REP%N_SOU, REP%LIS%KU_SOU, KU_SOU )
!
! --- Now sort the lists
!
      CALL SORT_CH ( REP%N_BAS, REP%LIS%C_BAS )
      CALL SORT_CH ( REP%N_STA, REP%LIS%C_STA )
      CALL SORT_CH ( REP%N_SOU, REP%LIS%C_SOU )
!
! --- Updatge the counters of the number of observations per baseline, per
! --- category
!
      DO 430 J3=1,REP%N_BAS
         IP = LTM_DIF ( 1, REP%N_BAS, REP%LIS%C_BAS, C_BAS(J3) )
         REP%LIS%KA_BAS(IP) = KA_BAS(J3)
         REP%LIS%KG_BAS(IP) = KG_BAS(J3)
         REP%LIS%KB_BAS(IP) = KB_BAS(J3)
         REP%LIS%KU_BAS(IP) = KU_BAS(J3)
 430  CONTINUE
!
! --- Now make the second run over all observations and update the indexes
! --- of source, station, baseline from the old lists to the indexex against
! --- the new, sorted lists
!
      DO 440 J4=1,REP%N_OBS
         IP = LTM_DIF ( 1, REP%N_BAS, REP%LIS%C_BAS, C_BAS(REP%OBS(J4)%IND_BAS) )
         REP%OBS(J4)%IND_BAS = IP
         IP = LTM_DIF ( 1, REP%N_STA, REP%LIS%C_STA, C_STA(REP%OBS(J4)%IND_STA(1)) )
         REP%OBS(J4)%IND_STA(1) = IP
         IP = LTM_DIF ( 1, REP%N_STA, REP%LIS%C_STA, C_STA(REP%OBS(J4)%IND_STA(2)) )
         REP%OBS(J4)%IND_STA(2) = IP
         IP = LTM_DIF ( 1, REP%N_SOU, REP%LIS%C_SOU, C_SOU(REP%OBS(J4)%IND_SOU) )
         REP%OBS(J4)%IND_SOU = IP
 440  CONTINUE
!
      REP%IND_SOU_SEL      = 0
      REP%IND_SOU_SEL_LAST = 0
      DO 450 J5=1,REP%LIS%L_SOU
         IF ( REP%LIS%C_SOU(J5) == REP%CNF%MARKED_SOURCE ) THEN
              REP%IND_SOU_SEL      = J5
              REP%IND_SOU_SEL_LAST = J5
              REP%LSEL_SOU(J5) = .TRUE.
            ELSE
              REP%LSEL_SOU(J5) = .FALSE.
         END IF
         IP = LTM_DIF ( 1, REP%N_SOU, C_SOU, REP%LIS%C_SOU(J5) )
         REP%LIS%KG_SOU(J5) = KG_SOU(IP)
         REP%LIS%KB_SOU(J5) = KB_SOU(IP)
         REP%LIS%KU_SOU(J5) = KU_SOU(IP)
         IF ( STAR_IND(J5,1) > 0  .OR.  STAR_IND(J5,2) > 0 ) THEN
              REP%LIS%EF_SOU(J5) = .TRUE.
            ELSE
              REP%LIS%EF_SOU(J5) = .FALSE.
         END IF
 450  CONTINUE 
!
      WRITE ( 6, 120 ) NUMOBS
 120  FORMAT ( I6, ' observations loaded            ' )
!
      REP%FRINGE_ROOT_DIR = FRINGE_ROOT_DIR
      CALL INCH ( INT4(EXPSERNO), REP%EXPSERNO_STR )
!
      REP%SOLVE_PS_VIEWER = SOLVE_PS_VIEWER_USE
      IF ( REP%SOLVE_PS_VIEWER(1:1) .NE. '/' ) THEN
           INQUIRE ( FILE=REP%SOLVE_PS_VIEWER, EXIST=LEX ) 
           IF ( .NOT. LEX ) THEN
                REP%SOLVE_PS_VIEWER = PRE_SOL_DIR(1:PRE_SOL_LEN)// &
     &                                REP%SOLVE_PS_VIEWER 
           END IF
      END IF
!
! --- Set the status: LOADED. Amen
!
      REP%STATUS = REPA__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_LOAD_OBS
