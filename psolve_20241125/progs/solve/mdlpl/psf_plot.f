      SUBROUTINE PSF_PLOT ( DBNAME, PREF_NAME, F_READ, I_TYP, &
     &           TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, UACSUP_PSF, &
     &           AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, PSF_DMAX, &
     &           VAL_CLF, ERR_CLF, PSC_DMAX, &
     &           L_OBS, BAS_COD, L_BAS, LIS_BAS, C_BAS, &
     &           MEM_LEN_PSF, MEM_ADR_PSF, &
     &           ADR_DIAGI_ARR, ADR_TIM, ADR_VAL, ADR_ERR, &
     &           L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, CLO_SIG_ARR, &
     &           MDLPL_IPS_PAG, MDLPL_IPC_PAG, MDLPL_FL_CLF, &
     &           MDLPL_FL_FRAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PSF_PLOT  in according with value of F_READ reads or not  *
! *   reads residuals file, the file with adjustments to segmented       *
! *   clock function and allocate dynamic memory for plots. Then it      *
! *   makes plots of postfit residuals or postfit residuals plus clock   *
! *   function for all baselines using Multi_DiaGI interface.            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! *        I_TYP ( INTEGER*4 ) -- Type of the plot. Supported types:     *
! *                            IPSF_TYP ( post fit residuals )           *
! *                            IPSC_TYP ( residuals plus clock function )*
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
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
! * PSF_DMAX_PSF ( REAL*8    ) -- Maximal in module value of postfit     *
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
! *        L_OBS ( INTEGER*4 ) -- Total number of observations in the    *
! *                               session.                               *
! *      BAS_COD ( INTEGER*4 ) -- Array of baseline codes for each       *
! *                               observation. Dimension: L_OBS.         *
! *        L_BAS ( INTEGER*4 ) -- Number of selected baselines in the    *
! *                               session.                               *
! *      LIS_BAS ( INTEGER*4 ) -- List of baseline codes for all         *
! *                               baselines produced by NBSTA.           *
! *                               Dimension: L_BAS.                      *
! *        C_BAS ( CHARACTER ) -- Array of baseline names in the format  *
! *                               <station_name1>/<station_name2>        *
! *                               Dimension: L_BAS.                      *
! *  MEM_LEN_PSF ( INTEGER*4 ) -- Amount of dynamic memory allocated for *
! *                               PSF_PLOT (in bytes).                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
! *       F_READ ( LOGICAL*4 ) -- Flag. TRUE means that residual file    *
! *                               and clock adjustments file have been   *
! *                               already read and dynamic memory is     *
! *                               allocated. FALSE means that these      *
! *                               actions have to be done.               *
! *  MEM_ADR_PSF ( INTEGER*8 ) -- Address of the first byte of a pool of *
! *                               dynamic memory used by PSF_PLOT.       *
! * ADR_DIAGI_ARR ( RECORD   ) -- Address of DIAGI_ARR.                  *
! *      ADR_TIM ( INTEGER*8 ) -- Address of arrays of arguments of plots*
! *      ADR_VAL ( INTEGER*8 ) -- Address of arrays of values of plots.  *
! *      ADR_ERR ( INTEGER*8 ) -- Address of arrays of sigmas of plots.  *
! *    L_CLO_ARR ( INTEGER*8 ) -- Array of number of points for clock    *
! *                               finction for each station. Dimension:  *
! *                               MBAS.                                  *
! *  CLO_VAL_ARR ( REAL*8    ) -- Array of time tags for clock functions *
! *                               for each baseline.                     *
! *                               Dimension: MAX_CLK*MBAS                *
! *  CLO_VAL_ARR ( REAL*8    ) -- Array of values of clock functions     *
! *                               for each baselines.                    *
! *                               Dimension: MAX_CLK*MBAS                *
! *  CLO_SIG_ARR ( REAL*8    ) -- Array of of formal uncertainties for   *
! *                               clock functions for each baseline.     *
! *                               Dimension: MAX_CLK*MBAS                *
! * MDLPL_IPS_PAG ( INTEGER*4) -- Number of the page to be displayed     *
! *                               (for IPSF_TYP plot type).              *
! * MDLPL_IPC_PAG ( INTEGER*4) -- Number of the page to be displayed     *
! *                               (for IPSC_TYP plot type).              *
! * MDLPL_FL_CLF ( LOGICAL*4 ) -- Flag of plotting clock function.       *
! *                               If TRUE then clock function is         *
! *                               displayed (for IPSC_TYP plot type      *
! *                               only).                                 *
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
! *  ###  26-JUL-99    PSF_PLOT    v1.4  (c) L. Petrov  08-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  M_BAS, MSTA, MPTS
      PARAMETER  ( M_BAS = MAX_ARC_BSL )
      PARAMETER  ( MSTA = MAX_ARC_STA )
      PARAMETER  ( MPTS = MAX_PTS     )
      CHARACTER  DBNAME*(*), PREF_NAME*(*)
      REAL*8     TIM_PSF(MAX_OBS), VAL_PSF(MAX_OBS), ERR_PSF(MAX_OBS), &
     &           VAL_CLF(MAX_OBS), ERR_CLF(MAX_OBS), PSF_DMAX, PSC_DMAX
      INTEGER*2  STAT_PSF(2,MAX_OBS), UACSUP_PSF(MAX_OBS)
      INTEGER*4  AUTO_SUP_PSF(MAX_OBS), USER_SUP_PSF(MAX_OBS), &
     &           USER_REC_PSF(MAX_OBS)
      INTEGER*4  I_TYP, L_OBS, L_BAS, BAS_COD(MAX_OBS), LIS_BAS(M_BAS), &
     &           MDLPL_IPS_PAG, MDLPL_IPC_PAG
      CHARACTER  C_BAS(M_BAS)*17, STR*80
      LOGICAL*4  F_READ, MDLPL_FL_CLF, MDLPL_FL_FRAME
      INTEGER*4  IUER
!
      INTEGER*4  MDLPL_IPAG, IER
      INTEGER*8  LEN_DIAGI
      INTEGER*8        LEN_DIAGI_ARR, MEM_LEN_PSF, LEN_TIM, LEN_VAL, LEN_ERR
      ADDRESS__TYPE :: ADR_DIAGI_ARR, MEM_ADR_PSF, ADR_TIM, ADR_VAL, ADR_ERR
      INTEGER*4  L_CLO_ARR(M_BAS)
      REAL*8     CLO_TIM_ARR(MAX_CLK,M_BAS), CLO_VAL_ARR(MAX_CLK,M_BAS), &
     &           CLO_SIG_ARR(MAX_CLK,M_BAS)
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  I_LEN
!
      IF ( .NOT. F_READ ) THEN
!
! -------- Data were not read. Get post fit residuals, clock function and other
! -------- things
!
           CALL ERR_PASS ( IUER, IER )
           CALL PSF_GET  ( L_OBS, TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, &
     &          UACSUP_PSF, AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, &
     &          VAL_CLF, ERR_CLF, BAS_COD, L_BAS, LIS_BAS, C_BAS, &
     &          L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, CLO_SIG_ARR, &
     &          PSF_DMAX, PSC_DMAX, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9331, IUER, 'PSF_PLOT', 'Error in attempt '// &
     &              'to read postfit residuals from residual file' )
                RETURN
           END IF
           F_READ = .TRUE.
!
! -------- Compute amount dynamic memory to be grabbed
!
           LEN_DIAGI     = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
           LEN_DIAGI_ARR = LEN_DIAGI*L_BAS*2
           LEN_TIM       = 8*NUMSCA*L_BAS*5
           LEN_VAL       = 8*NUMSCA*L_BAS*5
           LEN_ERR       = 8*NUMSCA*L_BAS*5
!
! -------- Grab dynamic memory
!
           CALL ERR_PASS ( IUER, IER )
           CALL GRAB_MEM ( IER, MEM_LEN_PSF,   MEM_ADR_PSF,   4, &
     &                          LEN_DIAGI_ARR, ADR_DIAGI_ARR, &
     &                          LEN_TIM,       ADR_TIM, &
     &                          LEN_VAL,       ADR_VAL, &
     &                          LEN_ERR,       ADR_ERR           )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL IINCH   ( MEM_LEN_PSF, STR )
                CALL ERR_LOG ( 9332, IUER, 'MDLPL_PLUS', 'Error in attempt '// &
     &               'to allocate '//STR(1:I_LEN(STR))// &
     &               ' bytes of dynamic memory' )
                RETURN
           END IF
!
! -------- Clearing obtained segment of memory
!
           CALL NOUT ( MEM_LEN_PSF, %VAL(MEM_ADR_PSF) )
      END IF
!
! --- Reduce two variables to one
!
      IF ( I_TYP .EQ. IPSF_TYP ) THEN
           MDLPL_IPAG = MDLPL_IPS_PAG
         ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
           MDLPL_IPAG = MDLPL_IPC_PAG
      END IF
!
! --- Call a routine which actually makes a plot of postfit residuals and
! --- postfit residuals with clock function
!
      CALL ERR_PASS ( IUER, IER )
      CALL MAKE_PSFPLOT ( I_TYP, L_OBS, L_BAS, LIS_BAS, C_BAS, BAS_COD, &
     &     NUMSCA, TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, UACSUP_PSF, &
     &     AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, PSF_DMAX, &
     &     VAL_CLF, ERR_CLF, PSC_DMAX, L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, &
     &     CLO_SIG_ARR, %VAL(ADR_DIAGI_ARR), %VAL(ADR_TIM), %VAL(ADR_VAL), &
     &     %VAL(ADR_ERR), DBNAME, PREF_NAME, MDLPL_IPAG, MDLPL_FL_CLF, &
     &     MDLPL_FL_FRAME, SUPMET, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9333, IUER, 'PSF_PLOT', 'Error in attempt '// &
     &         'to make a plot of post fit residuals' )
           RETURN
      END IF
!
      IF ( I_TYP .EQ. IPSF_TYP ) THEN
           MDLPL_IPS_PAG = MDLPL_IPAG
         ELSE IF ( I_TYP .EQ. IPSC_TYP ) THEN
           MDLPL_IPC_PAG = MDLPL_IPAG
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PSF_PLOT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PSF_GET ( L_OBS, TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, &
     &           UACSUP_PSF, AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, &
     &           VAL_CLF, ERR_CLF, BAS_COD, L_BAS, LIS_BAS, C_BAS, &
     &           L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, CLO_SIG_ARR, &
     &           PSF_DMAX, PSC_DMAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PSF_GET  reads residual file and file with adjustments    *
! *   to segmented clock function.                                       *
! *                                                                      *
! *   It buils                                                           *
! *                                                                      *
! *   a) Array with time tags, value and formal uncertainty of post      *
! *      fit residuals.                                                  *
! *   b) Array of values of clock function. Clock function is called a   *
! *      contribution to time delay due to adjustments of segmented      *
! *      clocks.                                                         *
! *   c) Array of status of observations, array user action for          *
! *      suppression, array of baseline codes.                           *
! *   d) Lists of baseline codes and baseline names.                     *
! *   e) maximal in module value of post fit residual and postfit        *
! *      residual plus clock function.                                   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *        L_OBS ( INTEGER*4 ) -- Total number of observations in the    *
! *                               session.                               *
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
! *      VAL_CLF ( REAL*8    ) -- Array of values of clock function for  *
! *                               each observation. Clock function is    *
! *                               a difference of values of linear       *
! *                               spline for the 2-nd station of the     *
! *                               observation and the 1-st station.      *
! *                               Global clock polynomials are not taken *
! *                               into account. Units: psec.             *
! *                               Dimension: L_OBS.                      *
! *      ERR_CLF ( REAL*8    ) -- Array of formal uncertainties of clock *
! *                               function. NB: correlation between      *
! *                               coefficients of linear spline between  *
! *                               two stations are ignored.              *
! *                               Units: psec. Dimension: L_OBS.         *
! *      BAS_COD ( INTEGER*4 ) -- Array of baseline codes for each       *
! *                               observation. Dimension: L_OBS.         *
! *        L_BAS ( INTEGER*4 ) -- Number of selected baselines in the    *
! *                               session.                               *
! *      LIS_BAS ( INTEGER*4 ) -- List of baseline codes for all         *
! *                               baselines produced by NBSTA.           *
! *                               Dimension: L_BAS.                      *
! *        C_BAS ( CHARACTER ) -- Array of baseline names in the format  *
! *                               <station_name1>/<station_name2>        *
! *                               Dimension: L_BAS.                      *
! *    L_CLO_ARR ( INTEGER*4 ) -- Array of number of points for clock    *
! *                               finction for each station. Dimension:  *
! *                               MBAS.                                  *
! *  CLO_VAL_ARR ( REAL*8    ) -- Array of time tags for clock functions *
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
! *     PSC_DMAX ( REAL*8    ) -- Maximal in module value of postfit     *
! *                               residuals plus clock function among    *
! *                               used observations. Units: psec.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ###  26-JUL-99     PSF_GET    v2.1 (c)  L. Petrov  25-JUL-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'resfl.i'
      INCLUDE    'precm.i'
      INCLUDE    'prfil.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  M_BAS
      PARAMETER  ( M_BAS = MAX_ARC_BSL )
      REAL*8     TIM_PSF(MAX_OBS), VAL_PSF(MAX_OBS), ERR_PSF(MAX_OBS), &
     &           VAL_CLF(MAX_OBS), ERR_CLF(MAX_OBS), PSF_DMAX, PSC_DMAX
      INTEGER*2  STAT_PSF(2,MAX_OBS), UACSUP_PSF(MAX_OBS)
      INTEGER*4  BAS_COD(MAX_OBS), L_OBS, L_BAS, LIS_BAS(M_BAS), &
     &           L_CLO_ARR(M_BAS), IUER
      INTEGER*4  AUTO_SUP_PSF(MAX_OBS), USER_SUP_PSF(MAX_OBS), &
     &           USER_REC_PSF(MAX_OBS)
      REAL*8     CLO_TIM_ARR(MAX_CLK,M_BAS), CLO_VAL_ARR(MAX_CLK,M_BAS), &
     &           CLO_SIG_ARR(MAX_CLK,M_BAS)
      CHARACTER  C_BAS(M_BAS)*17
!
      INTEGER*4    MSTA, MSTR, MBUF, MPTS
      PARAMETER  ( MSTA = MAX_ARC_STA )
      PARAMETER  ( MSTR = 16384       )
      PARAMETER  ( MBUF = 512         )
      PARAMETER  ( MPTS = MAX_PTS     )
      INTEGER*2  LDBNAM(5,15), IDBV(15), NUMDB
      REAL*8     RFJD_FIRST, RFRCT_FIRST
      INTEGER*4  IDBE(15), J1, J2, J3, J4, J5, ISTA1, ISTA2, IDBGN, IDEND, &
     &           LC_BAS, L_CLO(MSTA), LBUF_CLO, IER
      REAL*8     TIM_CLO(MPTS,MSTA), VAL_CLO(MPTS,MSTA), SIG_CLO(MPTS,MSTA), &
     &           MODU_ARR(MPTS), MODC_ARR(MPTS), TIM2(MAX_CLK*2)
      CHARACTER  BAS_NAME*17, FINAM_CLO*255, BUF_CLO(MBUF)*(MSTR)
      REAL*8     VAL1, VAL2, SIG1, SIG2
      REAL*8     EPS_SEC
      PARAMETER  ( EPS_SEC = 0.1 ) ! min time between epochs
      LOGICAL*4  FL_USED
      INTEGER*4  NSTBA
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- Learn the total number of observations in the database
!
      IDBGN = 1
      CALL OPENNAMFIL()
      CALL DBPOX ( NUMDB, LDBNAM, IDBV, IDBE )
      IDEND = IDBE(1)
!
! --- Set name of clock file
!
      CALL CLRCH ( FINAM_CLO )
      FINAM_CLO = PRE_SCR_DIR(:PRE_SD_LEN)//'CLOB'//PRE_LETRS
!
! --- Reading file for segmented clocks
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM_CLO, MBUF, BUF_CLO, LBUF_CLO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9351, IUER, 'PSF_GET', 'Error during reading '// &
     &         'segmented clock file '//FINAM_CLO )
           RETURN
      END IF
!
! --- Cycle over stations. Extract information from buffer with adjustments
! --- to station-dependent clock funciton.
!
      DO 410 J1=1,NUMSTA
         CALL ERR_PASS ( IUER, IER )
         CALL GETSTA_PTS ( MPTS, L_CLO(J1), ISITN_CHR(J1), ICLB_TYP, LBUF_CLO, &
     &                     BUF_CLO, TIM_CLO(1,J1), VAL_CLO(1,J1), &
     &                     SIG_CLO(1,J1), MODU_ARR, MODC_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9352, IUER, 'PSF_GET', 'Error during '// &
     &            'parsing segmented clock file '//FINAM_CLO )
              RETURN
         END IF
!
         IF ( L_CLO(J1) .LT. 2 ) THEN
!
! ----------- Clock were not estimated for the J1-th station. Set zeroes
!
              L_CLO(J1)     =  2
              TIM_CLO(1,J1) =  0.0
              TIM_CLO(2,J1) = 24.0
              VAL_CLO(1,J1) =  0.0
              VAL_CLO(2,J1) =  0.0
              SIG_CLO(1,J1) =  0.0
              SIG_CLO(2,J1) =  0.0
         END IF
 410  CONTINUE
!
! --- Initialization
!
      PSF_DMAX = -1.1
      PSC_DMAX = -1.1
      LC_BAS = 0
      L_BAS  = 0
      L_OBS  = 0
!
      CALL ACS_RESFIL ( 'O'   )
!
! --- Cycle over all observations of th database
!
      DO 420 J2=IDBGN, IDEND ! Running over the observations!
!
! ------ Read record with residual for the J2-th observation
!
         CALL USE_RESFIL ( J2, 'R' )
         L_OBS = L_OBS + 1
         IF ( J2 .EQ. 1 ) THEN
!
! ----------- Store the data of the first observation
!
              RFJD_FIRST  = RFJD
              RFRCT_FIRST = RFRCT
         END IF
!
! ------ Compute TIM_PSF -- time tag (time elapsed from the first observation
! ------ expressed in hours)
!
         TIM_PSF(J2) = ( (RFJD - RFJD_FIRST) + (RFRCT - RFRCT_FIRST) )*24.0D0
         VAL_PSF(J2) = RDOC*1.D3
         ERR_PSF(J2) = RDERR*1.D3
         STAT_PSF(1,J2) = SUPSTAT_RES(1)
         STAT_PSF(2,J2) = SUPSTAT_RES(2)
         UACSUP_PSF(J2) = UACSUP_RES
         AUTO_SUP_PSF(J2) = AUTO_SUP_RES
         USER_SUP_PSF(J2) = USER_SUP_RES
         USER_REC_PSF(J2) = USER_REC_RES
!
! ------ Compute BAS_COD -- baseline code
!
         BAS_COD(J2) = NSTBA ( INT4(IRSITE(1)), INT4(IRSITE(2)) )
!
! ------ Now time came to compute baseline-dependent clock function.
!
! ------ Now find value and APPROXIMATE formal uncertainty of clock function
! ------ by linear interploation between two nodes of a linear spline
!
         CALL MDLPL_SPLINE_GET ( L_CLO(IRSITE(1)), TIM_CLO(1,IRSITE(1)), &
     &                           VAL_CLO(1,IRSITE(1)), SIG_CLO(1,IRSITE(1)), &
     &                           TIM_PSF(J2), VAL1, SIG1 )
         CALL MDLPL_SPLINE_GET ( L_CLO(IRSITE(2)), TIM_CLO(1,IRSITE(2)), &
     &                           VAL_CLO(1,IRSITE(2)), SIG_CLO(1,IRSITE(2)), &
     &                           TIM_PSF(J2), VAL2, SIG2 )
         VAL_CLF(J2) = VAL2 - VAL1
         ERR_CLF(J2) = DSQRT ( SIG2**2 + SIG1**2 )
!
! ------ Bypass the observation made at the deselected baseline
! ------ for further comutations
!
         IF ( SUPMET == SUPMET__META ) THEN
              IF ( BTEST ( AUTO_SUP_PSF(J2), INT4(DSBS__SPS) ) ) GOTO 420
              FL_USED = META_SUPR_INQ ( AUTO_SUP_PSF(J2), USER_SUP_PSF(J2), &
     &                                  USER_REC_PSF(J2), USED__SPS )
            ELSE
              IF ( SUPR_INQ ( STAT_PSF(1,J2), UACSUP_PSF(J2), DSBS__SPS ) ) GOTO 420
              FL_USED =  SUPR_INQ ( STAT_PSF(1,J2), UACSUP_PSF(J2), USED__SPS )
         END IF
!
! ------ Try to update maximal in module postfit residual (and postfit
! ------ residuals plus clock function) among used observations
!
         IF ( PSF_DMAX .LE. -1.0 ) THEN
              IF ( FL_USED ) THEN
                   PSF_DMAX = DABS(VAL_PSF(J2))
                   PSC_DMAX = DABS(VAL_PSF(J2) + VAL_CLF(J2))
              END IF
            ELSE
              IF ( FL_USED ) THEN
                   IF ( DABS(VAL_PSF(J2)) .GT. PSF_DMAX ) THEN
                        PSF_DMAX = DABS(VAL_PSF(J2))
                   END IF
                   IF ( DABS(VAL_PSF(J2)+VAL_CLF(J2)) .GT. PSC_DMAX ) THEN
                        PSC_DMAX = DABS(VAL_PSF(J2)+VAL_CLF(J2))
                   END IF
              END IF
         END IF
!
! ------ Add the baseline code to the list of baseline codes if it is the
! ------ first used observation at the baseline
!
         CALL ADD_LIS   ( M_BAS, L_BAS,  LIS_BAS, BAS_COD(J2), -3 )
!
! ------ Create a baseline name and add it to the list of baseline namses
!
         BAS_NAME = ISITN_CHR(IRSITE(1))//'/'//ISITN_CHR(IRSITE(2))
         CALL ADD_CLIST ( M_BAS, LC_BAS, C_BAS, BAS_NAME, -3 )
 420  CONTINUE
!
! --- Close residual file
!
      CALL ACS_RESFIL ( 'C' )
!
! --- Now compute values of clock spline at the nodes. Tricky point: some
! --- arrays of clock function estiames may contain clock breaks and therefore
! --- clock function for different stations may have different nodes.
! --- We should take it into account
!
      DO 430 J3=1,L_BAS
         CALL NBAST ( LIS_BAS(J3), ISTA1, ISTA2 )
!
! ------ form array of time epoch for both stations of the baselione: first
! ------ time tags of the first station, then time tag of the second station
!
         CALL COPY_R8 ( L_CLO(ISTA1), TIM_CLO(1,ISTA1), TIM2(1) )
         CALL COPY_R8 ( L_CLO(ISTA2), TIM_CLO(1,ISTA2), TIM2(L_CLO(ISTA1)+1) )
!
! ------ ... sort it
!
         CALL SORT_R8 ( L_CLO(ISTA1)+L_CLO(ISTA2), TIM2 )
!
! ------ The build and array of time epochs by excluding very near points
!
         L_CLO_ARR(J3) = 1
         CLO_TIM_ARR(L_CLO_ARR(J3),J3) = TIM2(1)
!
         DO 440 J4=1,L_CLO(ISTA1)+L_CLO(ISTA2)
            IF ( ( TIM2(J4) - CLO_TIM_ARR(L_CLO_ARR(J3),J3) ) .GT. &
     &           EPS_SEC/3600.D0 ) THEN
!
                 L_CLO_ARR(J3) = L_CLO_ARR(J3) + 1
                 CLO_TIM_ARR(L_CLO_ARR(J3),J3) = TIM2(J4)
            END IF
 440     CONTINUE
!
! ------ Well. We have dont it. Now compute array of baselin-dependent clock
! ------ function with formal uncertainties
!
         DO 450 J5=1,L_CLO_ARR(J3)
!
! --------- Now find value and APPROXIMATE value of clock function by linear
! --------- interploation between two nodes of a linear spline
!
            CALL MDLPL_SPLINE_GET ( L_CLO(ISTA1), TIM_CLO(1,ISTA1), &
     &                              VAL_CLO(1,ISTA1), SIG_CLO(1,ISTA1), &
     &                              CLO_TIM_ARR(J5,J3), VAL1, SIG1 )
            CALL MDLPL_SPLINE_GET ( L_CLO(ISTA2), TIM_CLO(1,ISTA2), &
     &                              VAL_CLO(1,ISTA2), SIG_CLO(1,ISTA2), &
     &                              CLO_TIM_ARR(J5,J3), VAL2, SIG2 )
            CLO_VAL_ARR(J5,J3) = VAL2 - VAL1
            CLO_SIG_ARR(J5,J3) = DSQRT ( SIG2**2 + SIG1**2 )
 450     CONTINUE
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PSF_GET  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MDLPL_SPLINE_GET ( L_SPL, TIM_SPL, VAL_SPL, SIG_SPL, &
     &                              TIM, VAL, SIG )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_SPLINE_GET  computes value of linear spline and     *
! *   its formal uncertainty for an arbitrary point within the interval  *
! *   of interpolation.                                                  *
! *   NB: formal uncertainty IS NOT CORRECT since estimates of linear    *
! *   spline are considered as uncorrelated. Thus, this uncertainty      *
! *   SHOULD NOT be used for anything but plotting.                      *
! *                                                                      *
! * ### 01-FEB-2000 MDLPL_SPLINE_GET v1.1 (c) L. Petrov 05-FEB-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  L_SPL
      REAL*8     TIM_SPL(L_SPL), VAL_SPL(L_SPL), SIG_SPL(L_SPL)
      REAL*8     TIM, VAL, SIG, TIM_EPS
      PARAMETER  ( TIM_EPS = 1.0D-6 )
      INTEGER*4  IPLB, IPLE
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: FLIN8
!
! --- First find node in the array of spline epoch:
! ------
! ------       |----*----------|
! ------     IPLB  OBS       IPLE
! ------
! ------ Routine IXMN8_S does this business
!
      IPLB = IXMN8 ( L_SPL, TIM_SPL, TIM )
      IF ( IPLB .GE. L_SPL ) IPLB = L_SPL - 1
      IF ( IPLB .EQ. -1 ) THEN
           IPLB = 1
           IPLE = 2
         ELSE IF ( IPLB .EQ. -2 ) THEN
           IPLB = L_SPL-1
           IPLE = L_SPL
         ELSE
           IPLE = IPLB+1
      END IF
      IF ( TIM_SPL(IPLB+1) - TIM_SPL(IPLB) > TIM_EPS ) THEN
!
! --------- Now find value of clock function by linear interploation between
! --------- two nodes of a linear spline
!
            VAL = FLIN8 ( TIM, 2, TIM_SPL(IPLB), VAL_SPL(IPLB), 1 )
!
! --------- Compute APPROXIMATE formal uncertainty of the value of clock function
! --------- We neglect correlation between the estimates of linear spline parameteres
! --------- It is in general incorrectm but better than nothing
!
            SIG = FLIN8 ( TIM, 2, TIM_SPL(IPLB), SIG_SPL(IPLB), 1 )
         ELSE
            VAL = VAL_SPL(IPLB)
            SIG = SIG_SPL(IPLB)
      END IF
!
      RETURN
      END  !#!  MDLPL_SPLINE_GET  #!#
