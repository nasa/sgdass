      SUBROUTINE MAKE_PSFPLOT ( &
     &           I_TYP, L_OBS, L_BAS, LIS_BAS, C_BAS, BAS_COD, L_SCA, &
     &           TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, UACSUP_PSF, &
     &           AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, PSF_DMAX, &
     &           VAL_CLF, ERR_CLF, PSC_DMAX, L_CLO_ARR, CLO_TIM_ARR, &
     &           CLO_VAL_ARR, CLO_SIG_ARR, &
     &           DIAGI_ARR, TIM_ARR, VAL_ARR, ERR_ARR, DBNAME, PREF_NAME, &
     &           MDLPL_IPAG, MDLPL_FL_CLF, MDLPL_FL_FRAME, SUPMET, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MAKE_PSFPLOT  makes plots of postfit residuals or         *
! *   postfit residuals plus clock function for all baselines using      *
! *   Multi_DiaGI interface. It assumes that the arrays of postfit       *
! *   residuals for all observations of the session as well as formal    *
! *   uncertainties and clock function have been computed already.       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        I_TYP ( INTEGER*4 ) -- Type of the plot. Supported types:     *
! *                            IPSF_TYP ( post fit residuals )           *
! *                            IPSC_TYP ( residuals plus clock function )*
! *        L_OBS ( INTEGER*4 ) -- Total number of observations in the    *
! *                               session.                               *
! *        L_BAS ( INTEGER*4 ) -- Number of selected baselines in the    *
! *                               session.                               *
! *      LIS_BAS ( INTEGER*4 ) -- List of baseline codes for all         *
! *                               baselines produced by NBSTA.           *
! *                               Dimension: L_BAS.                      *
! *        C_BAS ( CHARACTER ) -- Array of baseline names in the format  *
! *                               <station_name1>/<station_name2>        *
! *                               Dimension: L_BAS.                      *
! *      BAS_COD ( INTEGER*4 ) -- Array of baseline codes for each       *
! *                               observation. Dimension: L_OBS.         *
! *        L_SCA ( INTEGER*4 ) -- Nuber of scans in the session.         *
! *      TIM_PSF ( REAL*8    ) -- Array of time tag for each observation *
! *                               elapsed since the first observation of *
! *                               the session. Units: hours.             *
! *                               Dimension: L_OBS.                      *
! *      VAL_PSF ( REAL*8    ) -- Array of postfit residuals.            *
! *                               Units: psec. Dimension: L_OBS.         *
! *      ERR_PSF ( REAL*8    ) -- Array of used formal uncertainties of  *
! *                               observations (with reweight applied).  *
! *                               Units: psec. Dimension: L_OBS.         *
! *     STAT_PSF ( INTEGER*2 ) -- Array of suppression status of each    *
! *                               observation. Dimension: (2,L_OBS).     *
! *   UACSUP_PSF ( INTEGER*2 ) -- Array of user action for suppression   *
! *                               of each observation. Dimension: L_OBS. *
! * AUTO_SUP_PSF ( INTEGER*4 ) -- Post-2005 automatic suppression status.*
! * USER_SUP_PSF ( INTEGER*4 ) -- Post-2005 user action for supression.  *
! * USER_REC_PSF ( INTEGER*4 ) -- Post-2005 user action for resurrecton. *
! *    L_CLO_ARR ( INTEGER*4 ) -- Array of number of points for clock    *
! *                               finction for each station. Dimension:  *
! *                               MBAS.                                  *
! *  CLO_TIM_ARR ( REAL*8    ) -- Array of time tags for clock functions *
! *                               for each baseline.                     *
! *                               Dimension: MAX_CLK*MBAS                *
! *  CLO_VAL_ARR ( REAL*8    ) -- Array of values of clock functions     *
! *                               for each baselines.                    *
! *                               Dimension: MAX_CLK*MBAS                *
! *  CLO_SIG_ARR ( REAL*8    ) -- Array of of formal uncertainties for   *
! *                               clock functions for each baseline.     *
! *                               Dimension: MAX_CLK*MBAS                *
! *     PSF_DMAX ( REAL*8    ) -- Maximal in module value of postfit     *
! *                               residuals among used observations.     *
! *                               Units: psec.                           *
! *      VAL_CLF ( REAL*8    ) -- Array of values of clock function for  *
! *                               each observation. Clock function is    *
! *                               a difference of values of linear       *
! *                               spline for the 2-nd station of the     *
! *                               observation and the 1-st station.      *
! *                               clock breaks and global clock          *
! *                               polynomials are not taken into account.*
! *                               Units: psec. Dimension: L_OBS.         *
! *      ERR_CLF ( REAL*8    ) -- Array of formal uncertainties of clock *
! *                               function. NB: correlation between      *
! *                               coefficients of linear spline between  *
! *                               two stations are ignored.              *
! *                               Units: psec. Dimension: L_OBS.         *
! *     PSC_DMAX ( REAL*8    ) -- Maximal in module value of postfit     *
! *                               residuals plus clock function among    *
! *                               used observations. Units: psec.        *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! *       SUPMET ( INTEGER*2 ) -- Supprssion method code.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      TIM_ARR ( REAL*8    ) -- Array of arguments for plotting for    *
! *                               each observation, each baseline, each  *
! *                               data type. Units: hours.               *
! *                               Dimension: (L_SCA,L_BAS,5).            *
! *                               TIM_ARR keeps 5 arrays for each        *
! *                               baseline:                              *
! *                            1 -- time tag for post fit residuals      *
! *                                 for good observations;               *
! *                            2 -- time tag for post fit residuals      *
! *                                 for bad observations;                *
! *                            3 -- time tag for residuals plus clock    *
! *                                 function for good observations;      *
! *                            4 -- time tag for residuals plus clock    *
! *                                 function for good observations;      *
! *                            5 -- time tag for clock function for all  *
! *                                 (good and bad) observations at the   *
! *                                 baseline.                            *
! *                            "good" observation means used in the      *
! *                                   solution;                          *
! *                            "bad"  observation means not used in the  *
! *                                   solution but recoverable;          *
! *      VAL_ARR ( REAL*8    ) -- Array of values for plotting for       *
! *                               each observation, each baseline, each  *
! *                               data type. Units: picoseconds.         *
! *                               Dimension: (L_SCA,L_BAS,5).            *
! *      ERR_ARR ( REAL*8    ) -- Array of formal uncertainties of       *
! *                               plotting values for each observation,  *
! *                               each baseline, each data type.         *
! *                               Units: picoseconds.                    *
! *                               Dimension: (L_SCA,L_BAS,5).            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    DIAGI_ARR ( RECORD    ) -- Array of DIAGI data structures which   *
! *                               keeps parameters of each individual    *
! *                               plot. Dimension: (L_BAS,2)             *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
! *   MDLPL_IPAG ( INTEGER*4 ) -- A Page number which will be displayed  *
! *                               first.                                 *
! * MDLPL_FL_CLF ( LOGICAL*4 ) -- If TRUE and the plot type is ITYP_PSC  *
! *                               then the clock function will be        *
! *                               displayed, otherwise not.              *
! * MDLPL_FL_FRAME (LOGICAL*4) -- Flag. If .TRUE. then the plotting      *
! *                               frame is "global" -- the same for all  *
! *                               stations. If .FALSE. then the          *
! *                               individual frame: min, max is set for  *
! *                               each station.                          *
! *         IUER ( INTEGER*4, OPT ) -- Universal error handler.          *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ###  26-JUL-1999  MAKE_PSFPLOT  v2.5 (c) L. Petrov  06-APR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'diagi_local.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4    MSTA, MPTS, M_BAS, M_BUT
      PARAMETER  ( MSTA = MAX_ARC_STA )
      PARAMETER  ( MPTS = MAX_PTS     )
      PARAMETER  ( M_BAS = MAX_ARC_BSL )
      PARAMETER  ( M_BUT = 8           )
      INTEGER*4  I_TYP, L_OBS, L_BAS, L_SCA, L_CLO_ARR(M_BAS), IUER
      REAL*8     TIM_PSF(L_OBS), VAL_PSF(L_OBS), ERR_PSF(L_OBS), PSF_DMAX, &
     &           VAL_CLF(L_OBS), ERR_CLF(L_OBS), PSC_DMAX
      INTEGER*2  STAT_PSF(2,L_OBS), UACSUP_PSF(L_OBS), SUPMET
      INTEGER*4  AUTO_SUP_PSF(MAX_OBS), USER_SUP_PSF(MAX_OBS), &
     &           USER_REC_PSF(MAX_OBS)
      INTEGER*4  BAS_COD(L_OBS), LIS_BAS(L_BAS), MDLPL_IPAG
      REAL*8     TIM_ARR(L_SCA,L_BAS,5), VAL_ARR(L_SCA,L_BAS,5), &
     &           ERR_ARR(L_SCA,L_BAS,5)
      REAL*8     CLO_TIM_ARR(MAX_CLK,M_BAS), CLO_VAL_ARR(MAX_CLK,M_BAS), &
     &           CLO_SIG_ARR(MAX_CLK,M_BAS)
      LOGICAL*4  MDLPL_FL_CLF, MDLPL_FL_FRAME, FL_DSBS, FL_USED, FL_RECO
      CHARACTER  C_BAS(L_BAS)*(*), DBNAME*(*), PREF_NAME*(*)
      TYPE ( DIAGI_STRU ) ::  DIAGI_ARR(L_BAS,2)
!
      INTEGER*4  NP_USED(M_BAS), NP_BAD(M_BAS), NP_ALL(M_BAS)
      REAL*8     TIME_SPAN, TIME_LAST, TIME_FIRST
      REAL*8     YPS_ADJ, YPC_ADJ, X_ADJ
      INTEGER*4  ICL_GOOD,  ICL_CLO,   ICL_BAD
      DATA       ICL_GOOD,  ICL_CLO,   &
     &         ICL_BAD/ ICL1__DEF, ICL2__DEF, ICL3__DEF /
      PARAMETER  ( YPS_ADJ = 1.61D0 ) ! Adjustmet for PSF-type plot
      PARAMETER  ( YPC_ADJ = 1.08D0 ) ! Adjustmet for PSC-type plot
      PARAMETER  ( X_ADJ   = 0.02 ) ! horizontal adjustment
      CHARACTER  TITS(M_BAS)*17, COMMON_TITLE*80, BUT_NAME(M_BUT)*80, &
     &           BUT_LET(M_BUT)*3, PREF_USE*128, STR*80, STR1*80, USE_TITLE*128
      CHARACTER  CS_BAS(M_BAS)*17
      INTEGER*4  J1, J2, J3, J4, J5, ICODE, L_BUT, IEL, IXREF_BAS(M_BAS), &
     &           L_PAG, L_PLT, NC, NR, IPL, KPL, IB_PLT, IE_PLT, K_PLT, &
     &           IARD, IAR_GP, IAR_BP, IAR_GC, IAR_BC, IAR_CF, IER
      INTEGER*4  LEN_DIAGI
      REAL*8     PS_ACC(M_BAS), WW_ACC(M_BAS), WRMS(M_BAS)
      REAL*8     PSF_MIN(M_BAS), PSF_MAX(M_BAS), VALMIN, VALMAX
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IFIND_PL, LTM_DIF
!
! --- Setting some constant parameters
!
      IAR_GP = 1 ! Index of an array with "good postfit residuals"
      IAR_BP = 2 ! Index of an array with "bad postfit residuals"
      IAR_GC = 3 ! Index of an array with "good residuals + clocks"
      IAR_BC = 4 ! Index of an array with "bad residuals + clocks"
      IAR_CF = 5 ! Index of an array with "clock function"
!
      IF ( I_TYP .EQ. IPSF_TYP ) THEN
           IARD = 1 ! DIAGI array with PSF data type
         ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
           IARD = 2 ! DIAGI array with PSC data type
      END IF
      LEN_DIAGI = LOC(DIAGI_ARR(1,1)%STATUS) - &
     &            LOC(DIAGI_ARR(1,1)%IFIRST_FIELD) + 4
 910  CONTINUE
!
! --- Initialization of observation counters and accumulators
!
      CALL NOUT_I4 ( L_BAS, NP_USED )
      CALL NOUT_I4 ( L_BAS, NP_BAD  )
      CALL NOUT_I4 ( L_BAS, NP_ALL  )
      CALL NOUT_R8 ( L_BAS, PS_ACC  )
      CALL NOUT_R8 ( L_BAS, WW_ACC  )
!
! --- Create a list CS_BAS whcih keeps baseline names sorted by alphabet
!
      CALL LIB$MOVC3 ( 17*L_BAS, C_BAS, CS_BAS )
      CALL SORT_CH ( L_BAS, CS_BAS )
!
! --- Build a cross reference table from unsorted array of te baselines to
! --- a sorted array.
!
      DO 410 J1=1,L_BAS
         IXREF_BAS(J1) = LTM_DIF ( 1, L_BAS, C_BAS, CS_BAS(J1) )
 410  CONTINUE
!
      TIME_FIRST = -1.D15
      DO 420 J2=1,L_OBS
!
! ------ Bypass an observation made at the deselected baseline
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_DSBS = BTEST ( AUTO_SUP_PSF(J2), INT4(DSBS__SPS) )
              FL_USED = META_SUPR_INQ ( AUTO_SUP_PSF(J2), USER_SUP_PSF(J2), &
     &                                  USER_REC_PSF(J2), USED__SPS ) 
              FL_RECO = META_SUPR_INQ ( AUTO_SUP_PSF(J2), USER_SUP_PSF(J2), &
     &                                  USER_REC_PSF(J2), RECO__SPS ) 
            ELSE 
              FL_DSBS = SUPR_INQ ( STAT_PSF(1,J2), UACSUP_PSF(J2), DSBS__SPS ) 
              FL_USED = SUPR_INQ ( STAT_PSF(1,J2), UACSUP_PSF(J2), USED__SPS ) 
              FL_RECO = SUPR_INQ ( STAT_PSF(1,J2), UACSUP_PSF(J2), RECO__SPS ) 
         END IF
!
         IF ( FL_DSBS ) GOTO 420
!
! ------ IPL -- an index of the baseline code of the j2-th observation in the
! ------ array of baseline codes BAS_COD
!
         IPL = IFIND_PL ( L_BAS, LIS_BAS, BAS_COD(J2) )
         NP_ALL(IPL) = NP_ALL(IPL) + 1
!
! ------ Test: whether the observation was in solution
!
         IF ( FL_USED ) THEN
!
! ----------- It was a good observation. Fill arrays
!
              NP_USED(IPL) = NP_USED(IPL) + 1
              TIM_ARR(NP_USED(IPL),IPL,IAR_GP) = TIM_PSF(J2)
              VAL_ARR(NP_USED(IPL),IPL,IAR_GP) = VAL_PSF(J2)
              ERR_ARR(NP_USED(IPL),IPL,IAR_GP) = ERR_PSF(J2)
              TIM_ARR(NP_USED(IPL),IPL,IAR_GC) = TIM_PSF(J2)
              VAL_ARR(NP_USED(IPL),IPL,IAR_GC) = VAL_PSF(J2) + VAL_CLF(J2)
!              ERR_ARR(NP_USED(IPL),IPL,IAR_GC) = DSQRT ( ERR_PSF(J2)**2 +
!     #                                                   ERR_CLF(J2)**2   )
              ERR_ARR(NP_USED(IPL),IPL,IAR_GC) = ERR_PSF(J2)
!
! ----------- Update accumulators for computation of baseline-dependent wrms
!
              PS_ACC(IPL) = PS_ACC(IPL) + VAL_PSF(J2)**2/ERR_PSF(J2)**2
              WW_ACC(IPL) = WW_ACC(IPL) + 1.D0/ERR_PSF(J2)**2
!
! ----------- Update register with minimal and maximal value
!
              IF ( I_TYP .EQ. IPSF_TYP ) THEN
                   VALMAX = VAL_PSF(J2) + ERR_PSF(J2)
                   VALMIN = VAL_PSF(J2) - ERR_PSF(J2)
                 ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
                   VALMAX = VAL_PSF(J2) + VAL_CLF(J2) + ERR_PSF(J2)
                   VALMIN = VAL_PSF(J2) + VAL_CLF(J2) - ERR_PSF(J2)
              END IF
!
              IF ( NP_USED(IPL) .EQ. 1  .AND.  NP_BAD(IPL) .EQ. 0 ) THEN
                   PSF_MAX(IPL) = VALMAX
                   PSF_MIN(IPL) = VALMIN
                 ELSE
                   IF ( VALMAX .GT. PSF_MAX(IPL) ) PSF_MAX(IPL) = VALMAX
                   IF ( VALMIN .LT. PSF_MIN(IPL) ) PSF_MIN(IPL) = VALMIN
              END IF
           ELSE
             IF ( FL_RECO ) THEN
!
! --------------- It was a bad observation(not used in the solution) but still
! --------------- recoverable
!
                  NP_BAD(IPL) = NP_BAD(IPL) + 1
                  TIM_ARR(NP_BAD(IPL),IPL,IAR_BP) = TIM_PSF(J2)
                  VAL_ARR(NP_BAD(IPL),IPL,IAR_BP) = VAL_PSF(J2)
                  ERR_ARR(NP_BAD(IPL),IPL,IAR_BP) = ERR_PSF(J2)
                  TIM_ARR(NP_BAD(IPL),IPL,IAR_BC) = TIM_PSF(J2)
                  VAL_ARR(NP_BAD(IPL),IPL,IAR_BC) = VAL_PSF(J2) + VAL_CLF(J2)
                  ERR_ARR(NP_BAD(IPL),IPL,IAR_BC) = ERR_PSF(J2)
!
! --------------- Update registr with minimal and maximal value
!
                  IF ( I_TYP .EQ. IPSF_TYP ) THEN
                       VALMAX = VAL_PSF(J2) + ERR_PSF(J2)
                       VALMIN = VAL_PSF(J2) - ERR_PSF(J2)
                    ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
                       VALMAX = VAL_PSF(J2) + VAL_CLF(J2) + ERR_PSF(J2)
                       VALMIN = VAL_PSF(J2) + VAL_CLF(J2) - ERR_PSF(J2)
                  END IF
!
                  IF ( NP_BAD(IPL) .EQ. 1  .AND.  NP_USED(IPL) .EQ. 0 ) THEN
                       PSF_MAX(IPL) = VALMAX
                       PSF_MIN(IPL) = VALMIN
                     ELSE
                       IF ( VALMAX .GT. PSF_MAX(IPL) ) PSF_MAX(IPL) = VALMAX
                       IF ( VALMIN .LT. PSF_MIN(IPL) ) PSF_MIN(IPL) = VALMIN
                  END IF
             END IF
         END IF
!
! ------ Put values of clock function
!
         TIM_ARR(NP_ALL(IPL),IPL,IAR_CF) = TIM_PSF(J2)
         VAL_ARR(NP_ALL(IPL),IPL,IAR_CF) = VAL_CLF(J2)
         ERR_ARR(NP_ALL(IPL),IPL,IAR_CF) = ERR_CLF(J2)
!
         IF ( TIME_FIRST == -1.D15 ) TIME_FIRST = TIM_PSF(J2)
         TIME_LAST  = TIM_PSF(J2)
 420  CONTINUE
!
! --- Set a common title
!
      CALL CLRCH ( COMMON_TITLE )
      IF ( I_TYP .EQ. IPSF_TYP ) THEN
           COMMON_TITLE = DBNAME(1:I_LEN(DBNAME))//' Post-fit residuals'
         ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
           COMMON_TITLE = DBNAME(1:I_LEN(DBNAME))//' Residuals + clock function'
      END IF
!
      TIME_SPAN = TIME_LAST - TIME_FIRST
!
! --- Fill array of DiaGI data structures
!
 920  CONTINUE
      DO 430 J3=1,L_BAS
!
! ------ KPL -- is an index oth baseline in sorted array. This trick allows to
! ------ locate plots sorted by names of the baselines. Cunning?
!
         KPL = IXREF_BAS(J3)
!
         CALL NOUT  ( LEN_DIAGI, DIAGI_ARR(J3,IARD) )
         CALL CLRCH ( DIAGI_ARR(J3,IARD)%ZAG        )
         CALL CLRCH ( DIAGI_ARR(J3,IARD)%NAME       )
         CALL CLRCH ( DIAGI_ARR(J3,IARD)%ARG_UNITS  )
!
! ------ Determine the size of the screen for DiaGI
!
         CALL CLRCH ( STR )
         CALL GETENVAR ( 'DIAGI_SCREEN', STR )
         CALL TRAN   ( 11, STR, STR )
         DIAGI_ARR(J3,IARD)%IDEV = IXS__DEF
         IF ( STR(1:5) .EQ. 'TINY' ) THEN
               DIAGI_ARR(J3,IARD)%IDEV = 1
             ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
               DIAGI_ARR(J3,IARD)%IDEV = 2
             ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
               DIAGI_ARR(J3,IARD)%IDEV = 3
             ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
               DIAGI_ARR(J3,IARD)%IDEV = 4
             ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
               DIAGI_ARR(J3,IARD)%IDEV = 5
         END IF
!
         IF ( I_TYP .EQ. IPSF_TYP ) THEN
!
! ----------- Plot type: "Post fit residuals"
!
              DIAGI_ARR(J3,IARD)%NCLR      = 2
!
! ----------- Function: good post fit residuals
!
              DIAGI_ARR(J3,IARD)%NPOI(1)   = NP_USED(KPL)
              DIAGI_ARR(J3,IARD)%ADR_X8(1) = LOC(TIM_ARR(1,KPL,IAR_GP))
              DIAGI_ARR(J3,IARD)%ADR_Y8(1) = LOC(VAL_ARR(1,KPL,IAR_GP))
              DIAGI_ARR(J3,IARD)%ADR_E8(1) = LOC(ERR_ARR(1,KPL,IAR_GP))
              DIAGI_ARR(J3,IARD)%LER(1)    = .TRUE.
              DIAGI_ARR(J3,IARD)%ICOL(1)   = ICL_GOOD
              DIAGI_ARR(J3,IARD)%IBST(1)   = 2
              DIAGI_ARR(J3,IARD)%ILST(1)   = 1
              DIAGI_ARR(J3,IARD)%IOST(1)   = 1
              DIAGI_ARR(J3,IARD)%IPST(1)   = 5
              DIAGI_ARR(J3,IARD)%IWST(1)   = 2
!
! ----------- Function: bad post fit residuals
!
              DIAGI_ARR(J3,IARD)%NPOI(2)   = NP_BAD(KPL)
              DIAGI_ARR(J3,IARD)%ADR_X8(2) = LOC(TIM_ARR(1,KPL,IAR_BP))
              DIAGI_ARR(J3,IARD)%ADR_Y8(2) = LOC(VAL_ARR(1,KPL,IAR_BP))
              DIAGI_ARR(J3,IARD)%ADR_E8(2) = LOC(ERR_ARR(1,KPL,IAR_BP))
              DIAGI_ARR(J3,IARD)%LER(2)    = .TRUE.
              DIAGI_ARR(J3,IARD)%ICOL(2)   = ICL_BAD
              DIAGI_ARR(J3,IARD)%IBST(2)   = 2
              DIAGI_ARR(J3,IARD)%ILST(2)   = 1
              DIAGI_ARR(J3,IARD)%IOST(2)   = 1
              DIAGI_ARR(J3,IARD)%IPST(2)   = 5
              DIAGI_ARR(J3,IARD)%IWST(2)   = 2
!
              DIAGI_ARR(J3,IARD)%ICLR      = 2
!
              IF ( MDLPL_FL_FRAME ) THEN
                   DIAGI_ARR(J3,IARD)%YMIN = -PSF_DMAX*YPS_ADJ
                   DIAGI_ARR(J3,IARD)%YMAX =  PSF_DMAX*YPS_ADJ
                 ELSE
                   DIAGI_ARR(J3,IARD)%YMIN =  PSF_MIN(KPL)*YPS_ADJ
                   DIAGI_ARR(J3,IARD)%YMAX =  PSF_MAX(KPL)*YPS_ADJ
              END IF
            ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
!
! ----------- Plot type: "residuals plus clock function"
!
              DIAGI_ARR(J3,IARD)%NCLR      = 3
!
              IF ( MDLPL_FL_CLF ) THEN
!                   DIAGI_ARR(J3,IARD).NPOI(1) = NP_ALL(KPL)
                   DIAGI_ARR(J3,IARD)%NPOI(1) = L_CLO_ARR(KPL)
                 ELSE
                   DIAGI_ARR(J3,IARD)%NPOI(1) = 0
              END IF
!
! ----------- Function: clock function
!
              DIAGI_ARR(J3,IARD)%ADR_X8(1) = LOC(CLO_TIM_ARR(1,KPL))
              DIAGI_ARR(J3,IARD)%ADR_Y8(1) = LOC(CLO_VAL_ARR(1,KPL))
              DIAGI_ARR(J3,IARD)%ADR_E8(1) = LOC(CLO_SIG_ARR(1,KPL))
              DIAGI_ARR(J3,IARD)%LER(1)    = .TRUE.
              DIAGI_ARR(J3,IARD)%ICOL(1)   = ICL_CLO
              DIAGI_ARR(J3,IARD)%IBST(1)   = 4
              DIAGI_ARR(J3,IARD)%ILST(1)   = 2
              DIAGI_ARR(J3,IARD)%IOST(2)   = 1
              DIAGI_ARR(J3,IARD)%IPST(1)   = 1
              DIAGI_ARR(J3,IARD)%IWST(1)   = 1
!
! ----------- Function: good post fit residuals
!
              DIAGI_ARR(J3,IARD)%NPOI(2)   = NP_USED(KPL)
              DIAGI_ARR(J3,IARD)%ADR_X8(2) = LOC(TIM_ARR(1,KPL,IAR_GC))
              DIAGI_ARR(J3,IARD)%ADR_Y8(2) = LOC(VAL_ARR(1,KPL,IAR_GC))
              DIAGI_ARR(J3,IARD)%ADR_E8(2) = LOC(ERR_ARR(1,KPL,IAR_GC))
              DIAGI_ARR(J3,IARD)%LER(2)    = .TRUE.
              DIAGI_ARR(J3,IARD)%ICOL(2)   = ICL_GOOD
              DIAGI_ARR(J3,IARD)%IBST(2)   = 2
              DIAGI_ARR(J3,IARD)%ILST(2)   = 1
              DIAGI_ARR(J3,IARD)%IOST(2)   = 1
              DIAGI_ARR(J3,IARD)%IPST(2)   = 5
              DIAGI_ARR(J3,IARD)%IWST(2)   = 2
!
! ----------- Function: bad post fit residuals
!
              DIAGI_ARR(J3,IARD)%NPOI(3)   = NP_BAD(KPL)
              DIAGI_ARR(J3,IARD)%ADR_X8(3) = LOC(TIM_ARR(1,KPL,IAR_BC))
              DIAGI_ARR(J3,IARD)%ADR_Y8(3) = LOC(VAL_ARR(1,KPL,IAR_BC))
              DIAGI_ARR(J3,IARD)%ADR_E8(3) = LOC(ERR_ARR(1,KPL,IAR_BC))
              DIAGI_ARR(J3,IARD)%LER(3)    = .TRUE.
              DIAGI_ARR(J3,IARD)%ICOL(3)   = ICL_BAD
              DIAGI_ARR(J3,IARD)%IBST(3)   = 2
              DIAGI_ARR(J3,IARD)%ILST(3)   = 1
              DIAGI_ARR(J3,IARD)%IOST(3)   = 1
              DIAGI_ARR(J3,IARD)%IPST(3)   = 5
              DIAGI_ARR(J3,IARD)%IWST(3)   = 2
!
              DIAGI_ARR(J3,IARD)%ICLR      = 2
!
              IF ( MDLPL_FL_FRAME ) THEN
                   DIAGI_ARR(J3,IARD)%YMIN = -PSC_DMAX*YPC_ADJ
                   DIAGI_ARR(J3,IARD)%YMAX =  PSC_DMAX*YPC_ADJ
                 ELSE
                   DIAGI_ARR(J3,IARD)%YMIN =  PSF_MIN(KPL)*YPC_ADJ
                   DIAGI_ARR(J3,IARD)%YMAX =  PSF_MAX(KPL)*YPC_ADJ
              END IF
         END IF
!
         DIAGI_ARR(J3,IARD)%XMIN      = TIME_FIRST - X_ADJ*TIME_SPAN
         DIAGI_ARR(J3,IARD)%XMAX      = TIME_LAST  + X_ADJ*TIME_SPAN
!
! ------ Compute a baseline-dependent wrms
!
         IF ( WW_ACC(KPL) .GT. 1.D-30 ) THEN
              WRMS(KPL) = DSQRT ( PS_ACC(KPL)/WW_ACC(KPL) )
           ELSE
              WRMS(KPL) = 0.0
         END IF
!
! ------ Build a string with plot title
!
         CALL CLRCH ( STR )
         IEL = I_LEN(C_BAS(KPL)(1:8))
         IF ( WRMS(KPL) .LT. 1000.0 ) THEN
              WRITE ( UNIT=STR(1:5), FMT='(0PF5.1)' ) WRMS(KPL)
              CALL CHASHL ( STR(1:5) )
              IF ( I_TYP .EQ. IPSF_TYP ) THEN
                   DIAGI_ARR(J3,IARD)%ZAG = DBNAME(1:I_LEN(DBNAME))//' '// &
     &                          C_BAS(KPL)(1:IEL)//'/'//C_BAS(KPL)(10:17)// &
     &                          ' Postfit residuals wrms='//STR(1:5)//' ps'
                ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
                   DIAGI_ARR(J3,IARD)%ZAG = DBNAME(1:I_LEN(DBNAME))//' '// &
     &                          C_BAS(KPL)(1:IEL)//'/'//C_BAS(KPL)(10:17)// &
     &                          ' Residuals + clock wrms='//STR(1:5)//' ps'
              END IF
            ELSE
              WRITE ( UNIT=STR(1:12), FMT='(1PF12.5)' ) WRMS(KPL)*1.D12
              CALL CHASHL ( STR(1:12) )
              DIAGI_ARR(J3,IARD)%ZAG = DBNAME(1:I_LEN(DBNAME))//' '// &
     &                               C_BAS(KPL)(1:IEL)//'/'//C_BAS(KPL)(10:17)// &
     &                               ' wrms='//STR(1:12)//' sec'
         END IF
!
! ------ Form a name of the plot to be used for hardcopy
!
         DIAGI_ARR(J3,IARD)%NAME = C_BAS(KPL)(1:IEL)//'.'//C_BAS(KPL)(10:17)
!
         DO 440 J4=1,I_LEN(DIAGI_ARR(J3,IARD)%NAME)
            IF ( DIAGI_ARR(J3,IARD)%NAME(J4:J4) .EQ. ' ' ) THEN
                 DIAGI_ARR(J3,IARD)%NAME(J4:J4) = '_'
            END IF
 440     CONTINUE
!
! ------ Transform it to letters of lower registr
!
         CALL TRAN ( 12, DIAGI_ARR(J3,IARD)%NAME, DIAGI_ARR(J3,IARD)%NAME )
!
! ------ ... and add a prefix
!
         IF ( I_TYP .EQ. IPSF_TYP ) THEN
              DIAGI_ARR(J3,IARD)%NAME = PREF_NAME(1:I_LEN(PREF_NAME))//'psf_'// &
     &                                  DIAGI_ARR(J3,IARD)%NAME
           ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
              DIAGI_ARR(J3,IARD)%NAME = PREF_NAME(1:I_LEN(PREF_NAME))//'psc_'// &
     &                                  DIAGI_ARR(J3,IARD)%NAME
         END IF
!
         DIAGI_ARR(J3,IARD)%ARG_UNITS = 'Time (hours)'
         DIAGI_ARR(J3,IARD)%ITRM      = 0
         DIAGI_ARR(J3,IARD)%IBATCH    = 0
         DIAGI_ARR(J3,IARD)%STATUS    = DIA__DEF
!
         TITS(J3) = C_BAS(KPL)(1:IEL)//'/'//C_BAS(KPL)(10:17)
 430  CONTINUE
!
! --- Compute: how many rows (NR) and how many columns (NC) plot should have;
! --- L_PLT -- total number of plots at one page
! --- L_PAG -- number of pages
!
      IF ( L_BAS .LE. 9 ) THEN
           L_PAG = 1
           L_PLT = L_BAS
           NC = 3
           NR = 3
        ELSE IF ( L_BAS .LE. 16 ) THEN
           L_PAG = 1
           L_PLT = L_BAS
           NC = 4
           NR = 4
        ELSE
           L_PLT = 25
           L_PAG = L_BAS/L_PLT + MIN(1,MOD(L_BAS,L_PLT))
           NC = 5
           NR = 5
      END IF
!
! --- Adjusting the number of the current page
!
      IF ( MDLPL_IPAG .LE. 0     ) MDLPL_IPAG = 1
      IF ( MDLPL_IPAG .GT. L_PAG ) MDLPL_IPAG = L_PAG
!
! --- Initialize array of buttins
!
      DO 450 J5=1,M_BUT
         CALL CLRCH ( BUT_NAME(J5) )
         CALL CLRCH ( BUT_LET(J5)  )
 450  CONTINUE
!
! --- Set button names and letter codes
!
      BUT_NAME(1) = 'Change directory | for Web plot'
      BUT_LET(1)  = 'Ccc'
      L_BUT = 1
!
      IF ( I_TYP .EQ. IPSC_TYP  .AND.  MDLPL_FL_CLF ) THEN
           L_BUT = L_BUT + 1
           BUT_NAME(L_BUT) = 'Clock function | OFF'
           BUT_LET(L_BUT)  = 'Fff'
         ELSE IF ( I_TYP .EQ. IPSC_TYP  .AND.  .NOT. MDLPL_FL_CLF ) THEN
           L_BUT = L_BUT + 1
           BUT_NAME(L_BUT) = 'Clock function | ON'
           BUT_LET(L_BUT)  = 'Ooo'
      END IF
!
      IF ( L_PAG .GT. 1 ) THEN
           L_BUT = L_BUT + 1
           BUT_NAME(L_BUT) = 'Next page'
           BUT_LET(L_BUT)  = 'Nn'//CHAR(221)
!
           L_BUT = L_BUT + 1
           BUT_NAME(L_BUT) = 'Previous page'
           BUT_LET(L_BUT)  = 'Pp'//CHAR(220)
      END IF
!
      L_BUT = L_BUT + 1
      IF ( MDLPL_FL_FRAME ) THEN
           BUT_NAME(L_BUT) = 'Set local | frame'
           BUT_LET(L_BUT)  = 'Lll'
         ELSE
           BUT_NAME(L_BUT) = 'Set global | frame'
           BUT_LET(L_BUT)  = 'Ggg'
      END IF
!
      L_BUT = L_BUT + 1
      BUT_NAME(L_BUT) = 'HELP'
      BUT_LET(L_BUT)  = 'Hhh'
!
      L_BUT = L_BUT + 1
      BUT_NAME(L_BUT) = 'EXIT'
      BUT_LET(L_BUT)  = 'Xxx'
!
 930  CONTINUE
!
! ------ Compute IB_PLT -- number of the first individual plot to be displayed
! ------                   at the current page (page number MDLPL_IPAG)
! ------ and     K_PLT  -- total number of plots to be displayed at the
! ------                   current page
!
         IB_PLT = (MDLPL_IPAG-1)*L_PLT + 1
         IE_PLT =  MDLPL_IPAG*L_PLT
         IF ( IE_PLT .GT. L_BAS ) IE_PLT = L_BAS
         K_PLT = IE_PLT-IB_PLT+1
!
! ------ Update used title and used prefix name for hardcopy names
!
         CALL CLRCH ( USE_TITLE )
         CALL CLRCH ( PREF_USE )
         IF ( I_TYP .EQ. IPSF_TYP ) THEN
              PREF_USE = PREF_NAME(1:I_LEN(PREF_NAME))//'psf_'
           ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
              PREF_USE = PREF_NAME(1:I_LEN(PREF_NAME))//'psc_'
         END IF
!
         IF ( L_PAG .EQ. 1 ) THEN
              USE_TITLE = COMMON_TITLE(1:I_LEN(COMMON_TITLE))
            ELSE
              CALL CLRCH ( STR  )
              CALL CLRCH ( STR1 )
              CALL INCH  ( MDLPL_IPAG, STR )
              CALL INCH  ( L_PAG, STR1 )
              USE_TITLE = COMMON_TITLE(1:I_LEN(COMMON_TITLE))//' '// &
     &                    STR(1:I_LEN(STR))//'('//STR1(1:I_LEN(STR1))//')'
              PREF_USE  = PREF_USE(1:I_LEN(PREF_USE))//STR(1:I_LEN(STR))//'_'
         ENDIF
!
! ------ Uh! At least the most important: call Multi_DiaGI
!
         ICODE = 0
         CALL ERR_PASS ( IUER, IER )
         CALL MULTI_DIAGI ( USE_TITLE(1:I_LEN(USE_TITLE)), &
     &                      K_PLT, NC, NR, TITS(IB_PLT), &
     &                      M_BUT, BUT_NAME, BUT_LET, &
     &                      PREF_USE, DIAGI_ARR(IB_PLT,IARD), ICODE, IER )
!
         IF ( IER .NE. 0 ) THEN
!
! ----------- Error was detected
!
              CALL PGENDQ() ! Iconify window
              IF ( IER .EQ. 4197  .OR.  IER .EQ. 4199  .OR.  IER .EQ. 4200) THEN
                   CALL ERR_LOG ( 9341, -1, 'MAKE_PSFPLOT', 'Error in '// &
     &                 'attempt to make a hardcopy. Try to change a Web '// &
     &                 'directory' )
                   CALL HIT_CONT ( %VAL(0), %VAL(0) )
                   GOTO 910
                 ELSE
                   CALL ERR_LOG ( 9342, IUER, 'MAKE_PSFPLOT', 'Error in '// &
     &                           'MULTI_DIAGI' )
                   RETURN
              END IF
         END IF
!
! ------ Parse a returned command code
!
         IF ( ICODE .GT. 0 ) THEN
              IF ( BUT_LET(ICODE)(1:1) .EQ. 'C' ) THEN
                   CALL PGENDQ() ! Iconify window
!
! ---------------- Change Web_dir command
!
                   CALL CHANGE_WEBDIR ( PREF_NAME )
                   GOTO 930
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'O' ) THEN
!
! ---------------- Toggle MDLPL_FL_CLF
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_FL_CLF = .NOT. MDLPL_FL_CLF
                   GOTO 920
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'F' ) THEN
!
! ---------------- Toggle MDLPL_FL_CLF
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_FL_CLF = .NOT. MDLPL_FL_CLF
                   GOTO 920
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'N' ) THEN
!
! ---------------- Next page
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_IPAG = MDLPL_IPAG + 1
                   IF ( MDLPL_IPAG .GT. L_PAG ) MDLPL_IPAG = 1
                   GOTO 930
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'P' ) THEN
!
! ---------------- Previous page
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_IPAG = MDLPL_IPAG - 1
                   IF ( MDLPL_IPAG .LT. 1 ) MDLPL_IPAG = L_PAG
                   GOTO 930
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'L' ) THEN
!
! ---------------- Set local frame
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_FL_FRAME = .FALSE.
                   GOTO 910
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'G' ) THEN
!
! ---------------- Set global frame
!
                   CALL PGENDQ() ! Iconify window
                   MDLPL_FL_FRAME = .TRUE.
                   GOTO 910
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'H' ) THEN
!
! ---------------- Help command
!
                   CALL ERR_PASS  ( IUER, IER )
                   IF ( I_TYP .EQ. IPSF_TYP ) THEN
                        CALL MDLPL_HLP ( MDLPL_PLUS_HLP_PSF, IER )
                     ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
                        CALL MDLPL_HLP ( MDLPL_PLUS_HLP_PSC, IER )
                   END IF
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 9343, IUER, 'MAKE_PSFPLOT', 'Error '// &
     &                      'in attempt to get on-line help' )
                        RETURN
                   END IF
                   CALL PGENDQ() ! Iconify window
                   GOTO 930
                ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'X' ) THEN
                   CALL PGENDQ() ! Iconify window
!
! ---------------- Exit command
!
                   CALL ERR_LOG ( 0, IUER )
                   RETURN
              END IF
            ELSE
              CALL PGENDQ() ! Iconify window
!
! ----------- Exit
!
              CALL ERR_LOG ( 0, IUER )
              RETURN
         END IF
      GOTO 930
!
      END  !#!  MAKE_PSFPLOT  #!#
