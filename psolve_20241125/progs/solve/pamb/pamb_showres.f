      SUBROUTINE PAMB_SHOWRES ( ADD_MES, N_OBS, DBOBJ, OBSSCA, &
     &                          OBSBAS, PAMBI, RES, RST, IPLSTA_LAST, &
     &                          IPLFD_STA, IBL_LAST, ISR, ISS_SOU, ITRI_LAST, &
     &                          RES_U, RST_U, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_SHOWERS  prepares the plot of different types of     *
! *   baseline-dependent or station-dependent residuals and plots them   *
! *   on X-window using advanced DiaGI interface. Baselines, stations,   *
! *   sources to be plotted are selected by mouse using various screen   *
! *   forms.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  VER_PAMB ( CHARACTER ) -- String with PAMB-identifier and number    *
! *                            of the current version.                   *
! *   ADD_MES ( INTEGER*4 ) -- Additional message which will be plotted  *
! *                            at the banner of the plot.                *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about      *
! *                            the session.                              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            postfit residuals.                        *
! * IPLSTA_LAST ( INTEGER*4 ) -- Index in the list DBOBJ.LIS_STA of the  *
! *                            station which has been selected at the    *
! *                            previous call of SELSTA or 0 if this call *
! *                            of SELSTA is the first call.              *
! * IPLFD_STA ( INTEGER*4 ) -- Index in the list DBOBJ.LIS_STA of the    *
! *                            fiducial station for station dependent    *
! *                            plots.                                    *
! *       ISR ( INTEGER*4 ) -- Source selection.                         *
! *                            ISR = 0 -- means to use all sources.      *
! *                            ISR > 0 -- means to use observation of    *
! *                                       the source with index ISR in   *
! *                                       the list DBOBJ.C_SOU           *
! *                            ISR < 0 -- means to use observation of    *
! *                                       -ISR sources from the list     *
! *                                       ISS_SOU.                       *
! *  IBL_LAST ( INTEGER*4 ) -- Index of the baseline which has been      *
! *                            selected at the previous call of SELBAS.  *
! * ITRI_LAST ( INTEGER*4 ) -- Index of the triangle which has been      *
! *                            selected at the previous call of          *
! *                            SELTRI or 0 if this call of               *
! *                            SELTRI is the first call.                 *
! *   ISS_SOU ( INTEGER*4 ) -- The list of sources to be used for        *
! *                            plotting. It is ignored when ISR =<0 .    *
! *                            Contains the list of indices in the list  *
! *                            of names DBOBJ.C_SOU                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     RES_U ( RECORD    ) -- Array of data structures keeping          *
! *                            information about the quantities which    *
! *                            have been plotted.                        *
! *     RST_U ( RECORD    ) -- Data structure keeping the statistics of  *
! *                            the quantiles which have been plotted.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  10-NOV-97  PAMB_SHOWERS  v5.5  (c)  L. Petrov 08-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbc3.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
      INCLUDE   'pamb.i'
      INCLUDE   'diagi_local.i'
      INCLUDE   'diagi.i'
      INTEGER*4  N_OBS, IBL_LAST, ISR, ISS_SOU(MO_SOU), ITRI_LAST(3), IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(*)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS), RES_U(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST, RST_U
      TYPE ( DIAGI_STRU  ) ::  DIAGI_S
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      TYPE ( APA__STRU ) ::  APA(N_OBS)
      INTEGER*4  J1, J2, J3, J4, NB, IBL, NOBS_IBL, IUL, NZ, LUN, IPL, K1, &
     &           NBT, IP, IPT, ITMP, ISTAR, ISTAR_SCA, IPL_STA, IPLSTA_LAST, &
     &           IPLFD_STA, IER
      INTEGER*4  M_NPT, M_ISS
      PARAMETER  ( M_NPT = 8192, M_ISS = 12 )
      REAL*8     TC_ARR(M_NPT,M_ISS), XC_ARR(M_NPT,M_ISS), EC_ARR(M_NPT,M_ISS)
      INTEGER*4  I_NPT(M_ISS), L_OBS, LIS_OBS(MO_BAS)
      REAL*8     TIM(MO_OBS), VAL(MO_OBS), SIG(MO_OBS), &
     &           WEI(MO_OBS), FILL, TIME_SPAN, PAMB_SP, OBS_1, OBS_2, OBS_3, &
     &           ARG_UP(2), VAL_UP(2), ARG_DOWN(2), VAL_DOWN(2), ARF_VAL, &
     &           ARF_SIG
      REAL*8     DR, SH, AV, WRMS_U, CLS, CLS_OLD, ER_CLS, TIM_LAST, &
     &           ECLS_X, ECLS_S
      REAL*8     AX_PX,  AX_GX,  AX_GS
      REAL*8     AS_PS,  AS_GX,  AS_GS
      REAL*8     F2_GX, F2_GS, F2_PX, F2_PS, B1P
      REAL*8     Q_GR, Q_GR_ERR, Q_PH, Q_PH_ERR
      CHARACTER  ADD_MES*(*), STR*80, STR1*80, MES*80, APA_DIR*255, FINAM*255, &
     &           UNITS*8
      PARAMETER  ( FILL     = 0.02  )
      INTEGER*4  N_TRI, NPT, ITRI(3), JTRI(3), JTRI_SEL(3), NBAS_TRI(3), &
     &           NOBS_TRI(3), N_SCA_OLD, ISG_TRI(3), IND_TAU(2), IND_FRE(2), &
     &           ISG_COR(2), IAMB, ICLS_X, ICLS_S, ICOND, LEN_DIAGI, IPAR
      LOGICAL*4  LSEL_BAS(MO_BAS), LSEL_STA(MO_STA), LP, LSTA_TYPE, LBAS_TYPE, &
     &           LTRI_TYPE, LSCA_TYPE, LPHS_TYPE, F_ONEBAND, LSUP_STA(MO_STA)
      INTEGER*2  IDATYP_OLD
      LOGICAL*4  FL_USED, FL_RECO, FL_URPH
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: IFIND_PL, NSTBA, I_LEN
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ, SELTRI, DATYP_INQ
!
! --- Clear DIAGI_S object
!
      LEN_DIAGI = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( LEN_DIAGI, DIAGI_S )
!
! --- Set the screen size
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'DIAGI_SCREEN', STR )
      CALL TRAN   ( 11, STR, STR )
      DIAGI_S%IDEV = IXS__DEF
      IF ( STR(1:4) .EQ. 'TINY' ) THEN
           DIAGI_S%IDEV = 1
         ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
           DIAGI_S%IDEV = 2
         ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
           DIAGI_S%IDEV = 3
         ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
           DIAGI_S%IDEV = 4
         ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
           DIAGI_S%IDEV = 5
      END IF 
!
! --- Determine: whether we should apply ionosphere free linear combination
! --- of group/phase delay or we should apply one-band difference phase-group.
! --- It depends on what is the solution type.
!
      IF ( DATYP_INQ ( DBOBJ%IDATYP, GX__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, GS__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, PX__DTP) .OR. &
     &     DATYP_INQ ( DBOBJ%IDATYP, PS__DTP)      ) THEN
!
           F_ONEBAND = .TRUE.
        ELSE
           F_ONEBAND = .FALSE.
      END IF
!
! --- I. Initialization: set the attribute "not selected" for all baselines
!     ~~~~~~~~~~~~~~~~~
!
      DO 410 J1=1,MO_BAS
         LSEL_BAS(J1) = .FALSE.
         IF ( J1 .EQ. IBL_LAST ) LSEL_BAS(J1) = .TRUE.
 410  CONTINUE
      IPL_STA = IPLSTA_LAST
      LSTA_TYPE = .FALSE.
      LBAS_TYPE = .FALSE.
      LTRI_TYPE = .FALSE.
      LSCA_TYPE = .FALSE.
!
      IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S1p' .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S2p' .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S3p' .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S4p'       ) THEN
           LSTA_TYPE = .TRUE.
           LSCA_TYPE = .TRUE.
        ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Ggg'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Ppg'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'B1p'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'B2p'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Ppp'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PGi'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PPi'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PDi'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pra'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'GPd'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP'               ) THEN
!
           LBAS_TYPE = .TRUE.
        ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Gdc'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pdc'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pgc'  .OR. &
     &            PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc'       ) THEN
           LTRI_TYPE = .TRUE.
      END IF
!C
      LPHS_TYPE = .FALSE.
      IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Ppg'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Ppp'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PPi'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PDi'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pra'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'GPd'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'PDd'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pgc'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pdc'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S1p'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S2p'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S3p'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S4p'  .OR. &
     &     PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc'       ) THEN
!
           LPHS_TYPE = .TRUE.
      END IF
!
      IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP' ) THEN
!
! -------- Get name of AOB directory
!
           CALL CLRCH   ( APA_DIR )
           CALL GETENVAR  ( 'AOB_DIR', APA_DIR )
!
! -------- Create the name of APA file
!
           CALL CATLG_FNAME ( DBOBJ%NAME(1:10), 0, APA_DIR, '.APA', FINAM )
!
! -------- Openning APA file
!
           CALL ERR_PASS  ( IUER, IER )
           CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6241, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &              'attempt to open APA file '//FINAM )
                RETURN
           END IF
!
! -------- Reading APA file
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, 4, N_OBS, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6242, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &              'reading the first record of APA file '//FINAM )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, LEN_APA*DBOBJ%L_OBS, APA, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6243, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &              'reading the last record of APA file '//FINAM )
                RETURN
           END IF
!
! -------- Closing APA file
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN, IER )
      END IF
!
! --- II. Preparing data for plotting
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Cycle over all observation and producing observables and their sigmas
! --- for plotting. Not to spoil old stuff we copy it to array RES_U
!
      DO 420 J2=1,DBOBJ%L_OBS
!
! ------ Check: should we plot this observation or it suits to suppression
! ------ criterion
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J2)%AUTO_SUP, &
     &                                  OBSBAS(J2)%USER_SUP, &
     &                                  OBSBAS(J2)%USER_REC, &
     &                                  USED__SPS )
              FL_RECO = META_SUPR_INQ ( OBSBAS(J2)%AUTO_SUP, &
     &                                  OBSBAS(J2)%USER_SUP, &
     &                                  OBSBAS(J2)%USER_REC, &
     &                                  RECO__SPS )
              FL_URPH = BTEST ( OBSBAS(J2)%AUTO_SUP, INT4(WPAS__SPS) )
            ELSE
              FL_USED = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), OBSBAS(J2)%UACSUP, &
     &                             USED__SPS )
              FL_RECO = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), OBSBAS(J2)%UACSUP, &
     &                             RECO__SPS )
              FL_URPH = SUPR_INQ ( OBSBAS(J2)%SUPSTAT(1), OBSBAS(J2)%UACSUP, &
     &                             URPH__SPS ) 
         END IF
         IF ( PAMB_PSL_TYPE .EQ. PSL__ALL ) THEN
!
! ----------- Plot all
!
              CONTINUE
            ELSE IF ( PAMB_PSL_TYPE .EQ. PSL__GDL ) THEN
!
! ----------- Plot only good for group delay solution types observatioins
!
              IDATYP_OLD = IDATYP
              IDATYP     = G_GXS__DTP
              IF ( .NOT. FL_USED ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 420
              END IF
              IDATYP = IDATYP_OLD
            ELSE IF ( PAMB_PSL_TYPE .EQ. PSL__PDL ) THEN
!
! ----------- Plot only good for phase delay solution types observatioins
!
              IDATYP_OLD = IDATYP
              IDATYP     = P_PXS__DTP
              IF ( .NOT. FL_USED ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 420
              END IF
              IDATYP = IDATYP_OLD
         END IF
         IF ( LPHS_TYPE ) THEN
!
! ----------- Look: maybe this observation is unrecoverable for phase delay
! ----------- solutions?
!
              IF ( FL_URPH ) GOTO 420
            ELSE
!
! ----------- Look: maybe this observation is unrecoverable for group delay
! ----------- solutions?
!
              IDATYP_OLD = IDATYP
              IDATYP     = G_GXS__DTP
              IF ( .NOT. FL_RECO ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 420
              END IF
              IDATYP = IDATYP_OLD
         END IF
!
! ------ Zeroing data structire for gathering statistical infromation
!
         CALL LIB$MOVC3 ( ML_RES, RES(J2), RES_U(J2) )
!
         F2_GX = OBSBAS(J2)%FREQ_IONO_GR**2
         F2_GS = OBSBAS(J2)%FREQ_IONO_GR_OPP**2
         F2_PX = OBSBAS(J2)%FREQ_IONO_PH**2
         F2_PS = OBSBAS(J2)%FREQ_IONO_PH_OPP**2
!
! ------ Calculate estimates of ionosphere content
!
         Q_GR = -( OBSBAS(J2)%TAUGR_OBS - OBSBAS(J2)%TAUGR_OBS_OPP )* &
     &             OBSBAS(J2)%FREQ_IONO_GR**2 * OBSBAS(J2)%FREQ_IONO_GR_OPP**2/ &
     &           ( OBSBAS(J2)%FREQ_IONO_GR**2 - OBSBAS(J2)%FREQ_IONO_GR_OPP**2 )
         Q_GR_ERR = DSQRT ( OBSBAS(J2)%TAUGR_ERR**2 + &
     &                      OBSBAS(J2)%TAUGR_ERR_OPP**2 )* &
     &             OBSBAS(J2)%FREQ_IONO_GR**2 * OBSBAS(J2)%FREQ_IONO_GR_OPP**2/ &
     &           ( OBSBAS(J2)%FREQ_IONO_GR**2 - OBSBAS(J2)%FREQ_IONO_GR_OPP**2 )
!
! ------ Calculate phase-delay ionosphere content and its error
!
         Q_PH = ( OBSBAS(J2)%TAUPH_OBS - OBSBAS(J2)%TAUPH_OBS_OPP ) * &
     &            OBSBAS(J2)%FREQ_IONO_PH**2 * OBSBAS(J2)%FREQ_IONO_PH_OPP**2/ &
     &          ( OBSBAS(J2)%FREQ_IONO_PH**2 - OBSBAS(J2)%FREQ_IONO_PH_OPP**2 )
         Q_PH_ERR = DSQRT ( OBSBAS(J2)%TAUPH_ERR**2 + &
     &                      OBSBAS(J2)%TAUPH_ERR_OPP**2 ) * &
     &         OBSBAS(J2)%FREQ_IONO_PH**2 * OBSBAS(J2)%FREQ_IONO_PH_OPP**2/ &
     &       ( OBSBAS(J2)%FREQ_IONO_PH**2 - OBSBAS(J2)%FREQ_IONO_PH_OPP**2 )
!
         IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ggg' ) THEN
!
! --------- Get residuals from the previous solution. Nothing more.
!
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__XBAND           ) THEN
!
! ----------- Print phase delay residuals for X-band using ionosphere
! ----------- correction derived from group delays
! ----------- and residuals from the previous solution
!
              RES_U(J2)%PSF_DEL =      PAMBI(J2)%RES_PX_GXS
              RES_U(J2)%WEI_DEL = 1.D0/PAMBI(J2)%ERR_PX_GXS
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GX__DTP ) ) THEN
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2)%TAUGR_ERR**2  + &
     &                                       OBSBAS(J2)%TAUPH_ERR**2  + &
     &                                       OBSBAS(J2)%TAUGR_ERR_COR**2 )
                 ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, GS__DTP ) ) THEN
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2)%TAUGR_ERR**2 + &
     &                                       OBSBAS(J2)%TAUPH_ERR_OPP**2 + &
     &                                       OBSBAS(J2)%TAUGR_ERR_COR**2 )
              END IF
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__SBAND           ) THEN
!
! ----------- Get phase delay residuals for S-band using ionosphere
! ----------- correction derived from group delays
! ----------- and residuals from the previous solution
!
              RES_U(J2)%PSF_DEL =      PAMBI(J2)%RES_PS_GXS
              RES_U(J2)%WEI_DEL = 1.D0/PAMBI(J2)%ERR_PS_GXS
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GX__DTP ) ) THEN
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2)%TAUGR_ERR**2  + &
     &                                       OBSBAS(J2)%TAUPH_ERR**2  + &
     &                                       OBSBAS(J2)%TAUGR_ERR_COR**2 )
                 ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, GS__DTP ) ) THEN
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT( OBSBAS(J2)%TAUGR_ERR_OPP**2 + &
     &                                       OBSBAS(J2)%TAUPH_ERR_OPP**2 + &
     &                                       OBSBAS(J2)%TAUGR_ERR_COR**2 )
              END IF
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B1p' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__XBAND           ) THEN
              RES_U(J2)%PSF_DEL=F2_PX/(F2_PX + F2_GS)*OBSBAS(J2)%TAUPH_OBS     + &
     &                          F2_GS/(F2_PX + F2_GS)*OBSBAS(J2)%TAUGR_OBS_OPP - &
     &                          PAMBI(J2)%TAU_CA
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &                  ( F2_PX/(F2_PX + F2_GS)*OBSBAS(J2)%TAUPH_ERR     )**2 + &
     &                  ( F2_GS/(F2_PX + F2_GS)*OBSBAS(J2)%TAUGR_ERR_OPP )**2 &
     &                                       )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B2p' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__SBAND           ) THEN
!
              RES_U(J2)%PSF_DEL = OBSBAS(J2)%TAUPH_OBS_OPP                  - &
     &                  F2_PX*F2_GS/(F2_PS*(F2_PX + F2_GS)) * &
     &                  ( OBSBAS(J2)%TAUPH_OBS - OBSBAS(J2)%TAUGR_OBS_OPP ) - &
     &                  PAMBI(J2)%TAU_CA
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &           OBSBAS(J2)%TAUPH_ERR_OPP**2                                   + &
     &     ( F2_PX*F2_GS/(F2_PS*(F2_PX + F2_GS))*OBSBAS(J2)%TAUPH_ERR     )**2 + &
     &     ( F2_PX*F2_GS/(F2_PS*(F2_PX + F2_GS))*OBSBAS(J2)%TAUGR_ERR_OPP )**2 &
     &                                       )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pra' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! ----------- Get "raw" phase delay ambiguity for the X-band
!
              IF ( F_ONEBAND ) THEN
!
! ---------------- One band mode
!
                   RES_U(J2)%PSF_DEL = OBSBAS(J2)%TAUPH_OBS - &
     &                                 OBSBAS(J2)%TAUGR_OBS
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &                                              OBSBAS(J2)%TAUPH_ERR**2 + &
     &                                              OBSBAS(J2)%TAUGR_ERR**2   )
                 ELSE
!
! ---------------- Ionsphere free linear combination ion two-band mode
!
                   AX_PX = 1.0D0
                   AX_GX = -(F2_GX*(F2_GS + F2_PX)) / (F2_PX*(F2_GX - F2_GS))
                   AX_GS =  (F2_GS*(F2_GX + F2_PX)) / (F2_PX*(F2_GX - F2_GS))
!
                   RES_U(J2)%PSF_DEL = ( AX_PX * OBSBAS(J2)%TAUPH_OBS     + &
     &                                   AX_GX * OBSBAS(J2)%TAUGR_OBS     + &
     &                                   AX_GS * OBSBAS(J2)%TAUGR_OBS_OPP &
     &                                 )
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &                                (AX_PX*OBSBAS(J2)%TAUPH_ERR)**2     + &
     &                                (AX_GX*OBSBAS(J2)%TAUGR_ERR)**2     + &
     &                                (AX_GS*OBSBAS(J2)%TAUGR_ERR_OPP)**2   )
              END IF
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pra' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! ----------- Get "raw" phase delay ambiguity for the X-band
!
              IF ( F_ONEBAND ) THEN
!
! ---------------- One-band mode
!
                   RES_U(J2)%PSF_DEL = OBSBAS(J2)%TAUPH_OBS_OPP - &
     &                                 OBSBAS(J2)%TAUGR_OBS_OPP
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &                                            OBSBAS(J2)%TAUPH_ERR_OPP**2 + &
     &                                            OBSBAS(J2)%TAUGR_ERR_OPP**2  )
                 ELSE
!
! ---------------- Two-band mode
!
                   AS_PS =  1.0D0
                   AS_GX = -(F2_GX*(F2_GS + F2_PS)) / (F2_PS*(F2_GX - F2_GS))
                   AS_GS =  (F2_GS*(F2_GX + F2_PS)) / (F2_PS*(F2_GX - F2_GS))
!
                   RES_U(J2)%PSF_DEL = ( AS_PS * OBSBAS(J2)%TAUPH_OBS_OPP + &
     &                                   AS_GX * OBSBAS(J2)%TAUGR_OBS     + &
     &                                  AS_GS * OBSBAS(J2)%TAUGR_OBS_OPP &
     &                                 )
                   RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( &
     &                                 (AS_PS*OBSBAS(J2)%TAUPH_ERR_OPP)**2 + &
     &                                 (AS_GX*OBSBAS(J2)%TAUGR_ERR)**2     + &
     &                                 (AS_GS*OBSBAS(J2)%TAUGR_ERR_OPP)**2   )
              END IF
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppp' ) THEN
!
! ----------- Phase delay ionosphere free linear combination
!
              RES_U(J2)%PSF_DEL =      PAMBI(J2)%RES_P_PXS
              RES_U(J2)%WEI_DEL = 1.D0/PAMBI(J2)%ERR_P_PXS
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PGi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! ----------- Phase delay ionosphere correction for the X-band derived from
! ----------- group delays
!
              RES_U(J2)%PSF_DEL = Q_GR/OBSBAS(J2)%FREQ_IONO_GR**2
              RES_U(J2)%WEI_DEL = 1.D0/( Q_GR_ERR/OBSBAS(J2)%FREQ_IONO_GR**2 )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PGi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! ----------- Phase delay ionosphere correction for the S-band derived from
! ----------- group delays
!
              RES_U(J2)%PSF_DEL = Q_GR/OBSBAS(J2)%FREQ_IONO_GR_OPP**2
              RES_U(J2)%WEI_DEL = 1.D0/ &
     &                               ( Q_GR_ERR/OBSBAS(J2)%FREQ_IONO_GR_OPP**2 )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PPi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! ----------- Phase delay ionosphere correction for the X-band derived from
! ----------- phase delays
!
              RES_U(J2)%PSF_DEL = ( OBSBAS(J2)%TAUPH_OBS - &
     &                              OBSBAS(J2)%TAUPH_OBS_OPP )* &
     &                              F2_PS/(F2_PX-F2_PS)
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( ( OBSBAS(J2)%TAUPH_ERR**2 + &
     &                                           OBSBAS(J2)%TAUPH_ERR_OPP**2 )* &
     &                                 F2_PS/(F2_PX-F2_PS) )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         res_u(j2).psf_del = ( obsbas(j2).tauph_obs -                     ! %%
!     #                         obsbas(j2).tauph_obs_opp )                 ! %%
!         res_u(j2).wei_del = 1.d0/dsqrt ( obsbas(j2).tauph_err**2 +       ! %%
!     #                                    obsbas(j2).tauph_err_opp**2 )   ! %%
!         res_u(j2).psf_del = ( obsbas(j2).tauph_obs -                      ! %%
!     #                         obsbas(j2).taugr_obs )                      ! %%
!         res_u(j2).wei_del = 1.d0/dsqrt ( obsbas(j2).tauph_err**2 +        ! %%
!     #                                    obsbas(j2).taugr_err**2   )      ! %%
!         res_u(j2).psf_del = ( obsbas(j2).taugr_obs -                      ! %%
!     #                         obsbas(j2).taugr_obs_opp )                  ! %%
!         res_u(j2).wei_del = 1.d0/dsqrt ( obsbas(j2).taugr_err**2 +        ! %%
!     #                                    obsbas(j2).taugr_err_opp**2  )   ! %%
!         res_u(j2).psf_del = ( obsbas(j2).tauph_obs_opp -                  ! %%
!     #                         obsbas(j2).taugr_obs )                      ! %%
!         res_u(j2).wei_del = 1.d0/dsqrt ( obsbas(j2).tauph_err_opp**2 +    ! %%
!     #                                    obsbas(j2).taugr_err**2       )  ! %%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PPi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! ----------- Phase delay ionosphere correction for the S-band derived from
! ----------- phase delays
!
              RES_U(J2)%PSF_DEL = ( OBSBAS(J2)%TAUPH_OBS - &
     &                              OBSBAS(J2)%TAUPH_OBS_OPP)* &
     &                              F2_PX/(F2_PX-F2_PS)
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( ( OBSBAS(J2)%TAUPH_ERR**2 + &
     &                                           OBSBAS(J2)%TAUPH_ERR_OPP**2 )* &
     &                              F2_PX/(F2_PX-F2_PS) )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PDi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! ----------- Difference of phase delay ionosphere correction for the X-band
! ----------- derived from group delays and phase delays
!
              RES_U(J2)%PSF_DEL = ( Q_GR/OBSBAS(J2)%FREQ_IONO_PH**2 - &
     &                              Q_PH/OBSBAS(J2)%FREQ_IONO_PH**2 )
              RES_U(J2)%WEI_DEL = 1.D0/( DSQRT ( Q_GR_ERR**2 + Q_PH_ERR**2 )/ &
     &                                   OBSBAS(J2)%FREQ_IONO_PH**2 )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'PDi' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! ----------- Difference of phase delay ionosphere correction for the S-band
! ----------- derived from group delays and phase delays
!
              RES_U(J2)%PSF_DEL = ( Q_GR/OBSBAS(J2)%FREQ_IONO_PH_OPP**2 - &
     &                              Q_PH/OBSBAS(J2)%FREQ_IONO_PH_OPP**2 )
              RES_U(J2)%WEI_DEL = 1.D0/( DSQRT ( Q_GR_ERR**2 + Q_PH_ERR**2 )/ &
     &                                          OBSBAS(J2)%FREQ_IONO_PH_OPP**2 )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! ----------- Get exsternal data from AOB for the X-band
!
              CALL CHIN ( PTP_ABR(PAMB_PLOT_TYPE)(3:3), IP )
              IF ( IP .LT. 1     ) IP = 1
              IF ( IP .GT. M_APA ) IP = M_APA
              RES_U(J2)%PSF_DEL = APA(J2)%X_PARAM(IP)
              RES_U(J2)%WEI_DEL = 1.D0
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP' .AND. &
     &        PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! ----------- Get exsternal data from AOB for the S-band
!
              CALL CHIN ( PTP_ABR(PAMB_PLOT_TYPE)(3:3), IP )
              IF ( IP .LT. 1     ) IP = 1
              IF ( IP .GT. M_APA ) IP = M_APA
              RES_U(J2)%PSF_DEL = APA(J2)%S_PARAM(IP)
              RES_U(J2)%WEI_DEL = 1.D0
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'GPd' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__XBAND           ) THEN
!
              RES_U(J2)%PSF_DEL = OBSBAS(J2)%TAUGR_OBS - OBSBAS(J2)%TAUPH_OBS
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2)%TAUGR_ERR**2 + &
     &                                         OBSBAS(J2)%TAUPH_ERR**2   )
!!            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'GPd' .AND.
!!     #                PAMB_PLOT_BAND .EQ. PAMB__SBAND           ) THEN
!!              RES_U(J2).PSF_DEL = OBSBAS(J2).TAUGR_OBS     -
!!     #                            OBSBAS(J2).TAUGR_OBS_OPP
!!              RES_U(J2).WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2).TAUGR_ERR**2     +
!!     #                                         OBSBAS(J2).TAUGR_ERR_OPP**2   )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'GPd' .AND. &
     &                PAMB_PLOT_BAND .EQ. PAMB__SBAND           ) THEN
!
! ----------- Group minus phase delay difference without taking into account
! ----------- ionosphere
!
              RES_U(J2)%PSF_DEL = OBSBAS(J2)%TAUGR_OBS_OPP - &
     &                            OBSBAS(J2)%TAUPH_OBS_OPP
              RES_U(J2)%WEI_DEL = 1.D0/DSQRT ( OBSBAS(J2)%TAUGR_ERR_OPP**2 + &
     &                                         OBSBAS(J2)%TAUPH_ERR_OPP**2   )
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Gdc' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pdc' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pgc' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S1p' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S2p' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S3p' ) THEN
              CONTINUE
            ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'S4p' ) THEN
              CONTINUE
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( PAMB_PLOT_TYPE, STR )
              CALL ERR_LOG ( 6244, IUER, 'PAMB_SHOWRES', 'Unsupported '// &
     &            'plotting type: '//STR(1:I_LEN(STR))//' band='// &
     &             BAND_STR(PAMB_PLOT_BAND) )
              RETURN
         END IF
 420  CONTINUE
!
 910  CONTINUE
      IF ( LSTA_TYPE ) THEN
           MES  = 'Select the station. '//ADD_MES
!
! -------- IVa. Station selection for plotting
!
           DO 510 K1=1,MO_STA
              LSEL_STA(K1) = .FALSE.
              IPL = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, IPLSTA_LAST )
              IF ( K1 .EQ. IPL ) LSEL_STA(K1) = .TRUE.
!
! ----------- Set suppression status for selection of the K1-th station.
! ----------- Station which were not used in solution will be suppressed from
! ----------- selection in menu.
!
              IF ( IFIND_PL ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%LIS_STA(K1) ) &
     &             .LE. 0 ) THEN
                   LSUP_STA(K1) = .TRUE.
                ELSE
                  LSUP_STA(K1) = .FALSE.
                  IF ( K1 .EQ. IPL ) IPLSTA_LAST = -1
              END IF
 510       CONTINUE
!
! -------- Launch screen form for station selection
!
           CALL ERR_PASS ( IUER, IER )
           CALL SELSTA   ( 2, MES, DBOBJ, LSUP_STA, LSEL_STA, IPL_STA, &
     &                     IPLSTA_LAST, IPLFD_STA, IER )
           IF ( IPL_STA .LE. 0  ) GOTO 810
           IPLSTA_LAST = IPL_STA
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6245, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &                        'station selection' )
                RETURN
           END IF
        ELSE IF ( LBAS_TYPE ) THEN
           MES  = 'Select the baseline. '//ADD_MES
!
! -------- IVb. Baseline selection for plotting
!
           CALL ERR_PASS   ( IUER, IER )
           CALL SELBAS     ( MES, DBOBJ, LSEL_BAS, IBL_LAST, IBL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6246, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &                        'baseline selection' )
                RETURN
           END IF
           IF ( IBL .LE. 0 ) GOTO 810
           IBL_LAST = IBL
           LSEL_BAS(IBL) = .TRUE.
!
! -------- Calculation of statistics over the new "residuals" to be plotted
!
           CALL ERR_PASS ( IUER, IER )
           CALL RESID_ST ( .FALSE., .FALSE., 0.D0, 0.D0, 0.D0, 0, N_OBS, DBOBJ, &
     &                     OBSSCA, OBSBAS, RES_U, RST_U, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6247, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &              'calculation statistics of residuals for database '// &
     &               DBOBJ%NAME )
                RETURN
           END IF
        ELSE IF ( LTRI_TYPE ) THEN
!
! -------- IVc. Triangle selection for plotting
!
           CALL ERR_PASS    ( IUER, IER )
           LP = SELTRI ( 'Triangle select', DBOBJ, ITRI_LAST, ITRI, IER )
           IF ( .NOT. LP ) GOTO 810
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6248, IUER, 'PAMB_SHOWRES', 'Error during '// &
     &                        'triangle selection' )
                RETURN
           END IF
           ITRI_LAST(1) = ITRI(1)
           ITRI_LAST(2) = ITRI(2)
           ITRI_LAST(3) = ITRI(3)
!
! -------- Important!
! -------- Array ITRI keeps indices of the station in the list of USED stations
! -------- Array JTRI keeps indices of the station in the list of PARTICIPATED
! -------- stations
!
           JTRI(1) = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                       DBOBJ%UIS_STA( ITRI(1) ) )
           JTRI(2) = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                       DBOBJ%UIS_STA( ITRI(2) ) )
           JTRI(3) = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, &
     &                                       DBOBJ%UIS_STA( ITRI(3) ) )
           CALL COPY_I4 ( 3, JTRI, JTRI_SEL ) 
      END IF
!
! --- V. Calculation of the arrays: argument of the function (TIM), its values
! --- (VAL) and sigma (SIG) for plotting
!
      NOBS_IBL = 0
      N_TRI = 0
      N_SCA_OLD = -99
      NPT   = 0
      L_OBS = 0
      CALL NOUT_I4 ( M_ISS, I_NPT )
      CALL PRCH ( CHAR(10)//CHAR(13)//'Computing...   '//CHAR(1) )
      DO 430 J3=1,DBOBJ%L_OBS
         NB = NSTBA ( INT4(OBSBAS(J3)%ISITE(1)), INT4(OBSBAS(J3)%ISITE(2)) )
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( OBSBAS(J3)%AUTO_SUP, &
     &                                  OBSBAS(J3)%USER_SUP, &
     &                                  OBSBAS(J3)%USER_REC, &
     &                                  USED__SPS )
              FL_RECO = META_SUPR_INQ ( OBSBAS(J3)%AUTO_SUP, &
     &                                  OBSBAS(J3)%USER_SUP, &
     &                                  OBSBAS(J3)%USER_REC, &
     &                                  RECO__SPS )
              FL_URPH = BTEST ( OBSBAS(J3)%AUTO_SUP, INT4(WPAS__SPS) )
            ELSE
              FL_USED = SUPR_INQ ( OBSBAS(J3)%SUPSTAT(1), OBSBAS(J3)%UACSUP, &
     &                             USED__SPS )
              FL_RECO = SUPR_INQ ( OBSBAS(J3)%SUPSTAT(1), OBSBAS(J3)%UACSUP, &
     &                             RECO__SPS )
              FL_URPH = SUPR_INQ ( OBSBAS(J3)%SUPSTAT(1), OBSBAS(J3)%UACSUP, &
     &                             URPH__SPS ) 
         END IF
!
         IF ( PAMB_PSL_TYPE .EQ. PSL__ALL ) THEN
!
! ----------- Plot all
!
              CONTINUE
            ELSE IF ( PAMB_PSL_TYPE .EQ. PSL__GDL ) THEN
!
! ----------- Plot only observations which are good for phase delay solution
! ----------- types
!
              IDATYP_OLD = IDATYP
              IDATYP     = G_GXS__DTP
              IF ( .NOT. FL_USED ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 430
              END IF
              IDATYP = IDATYP_OLD
            ELSE IF ( PAMB_PSL_TYPE .EQ. PSL__PDL ) THEN
!
! ----------- Plot only observations which are good for phase delay solution
! ----------- types
!
              IDATYP_OLD = IDATYP
              IDATYP     = P_PXS__DTP
              IF ( .NOT. FL_USED ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 430
              END IF
              IDATYP = IDATYP_OLD
         END IF
         IF ( LPHS_TYPE ) THEN
!
! ----------- Look: maybe this observation is unrecoverable for phase delay
! ----------- solutions?
!
              IF ( FL_URPH ) GOTO 430
            ELSE
!
! ----------- Look: maybe this observation is unrecoverable for group delay
! ----------- solutions?
!
              IDATYP_OLD = IDATYP
              IDATYP     = G_GXS__DTP
              IF ( .NOT. FL_RECO ) THEN
                   IDATYP = IDATYP_OLD
                   GOTO 430
              END IF
              IDATYP = IDATYP_OLD
         END IF
!
         ITMP  = INT4(OBSBAS(J3)%IND_SCA)
         ISTAR = INT4(OBSSCA(ITMP)%ISTAR)
!
! ------ Deselection for sources
!
         IF ( ISR .EQ. 0 ) THEN
!
! ----------- All-source mode
!
              CONTINUE
            ELSE IF ( ISR .GT. 0 ) THEN
!
! ----------- One-source mode: check is this source the selected one?
!
              IF ( ISR .NE. ISTAR ) GOTO 430
            ELSE IF ( ISR .LT. 0 ) THEN
!
! ----------- One-source mode: check is this source in the list of selceted
! ----------- sources?
!
              IF ( IFIND_PL ( -ISR, ISS_SOU, ISTAR ) .LE. 0 ) GOTO 430
         END IF
!
         IF ( LBAS_TYPE                                            .AND. &
     &        IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, NB ) .EQ. IBL       ) THEN
!
              NOBS_IBL = NOBS_IBL + 1
!
! ----------- Transforming time arguments to hours
!
              TIM(NOBS_IBL) = RES_U(J3)%TT*24.0
              IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg' .OR. &
     &             PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B1p' .OR. &
     &             PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B2p' .OR. &
     &             PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pra' .OR. &
     &             PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppp'       ) THEN
!
! --------------- Representation residuals in turns of phase
!
                  IF ( PAMB_PLOT_BAND .EQ. 1 ) THEN
!
! ------------------- ... For X-band
!
                      IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B1p' ) THEN
                           F2_GX = OBSBAS(J3)%FREQ_IONO_GR**2
                           F2_GS = OBSBAS(J3)%FREQ_IONO_GR_OPP**2
                           F2_PX = OBSBAS(J3)%FREQ_IONO_PH**2
                           F2_PS = OBSBAS(J3)%FREQ_IONO_PH_OPP**2
!
                           B1P = F2_PX/(F2_PX + F2_GS)
                           VAL(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH * &
     &                                              (RES_U(J3)%PSF_DEL/B1P)
                           SIG(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH / &
     &                                              (RES_U(J3)%WEI_DEL/B1P)
                         ELSE
                           VAL(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH * &
     &                                               RES_U(J3)%PSF_DEL
                           SIG(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH / &
     &                                               RES_U(J3)%WEI_DEL
                      END IF
                      WEI(NOBS_IBL) =1.D0/SIG(NOBS_IBL)
                    ELSE
!
! ------------------- For S-band
!
                   VAL(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH_OPP *RES_U(J3)%PSF_DEL
                   SIG(NOBS_IBL) =OBSBAS(J3)%FREQ_OBSV_PH_OPP /RES_U(J3)%WEI_DEL
                   WEI(NOBS_IBL) =1.D0/SIG(NOBS_IBL)
                  END IF
                ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP' ) THEN
!
! --------------- APA: no transformation
!
                  VAL(NOBS_IBL) = RES_U(J3)%PSF_DEL
                  SIG(NOBS_IBL) = 1.D0
                  WEI(NOBS_IBL) = 1.D0
                ELSE
!
! --------------- For other cases -- in psecs
!
                  VAL(NOBS_IBL) = RES_U(J3)%PSF_DEL*1.D12
                  SIG(NOBS_IBL) = 1.D12 /RES_U(J3)%WEI_DEL
                  WEI(NOBS_IBL) = RES_U(J3)%WEI_DEL /1.D12
               END IF ! ptp_abr...
!
! ------------ Find a "color" slot in TC_ARR/XC_ARR/EC_ACC arrays
!
               IPT = 0
               IF ( ISR .EQ. 0 ) THEN
                    IPT = 1
                  ELSE IF ( ISR .GT. 0 ) THEN
                    IF ( ISR .EQ. ISTAR ) THEN
                         IPT = 1
                    END IF
                  ELSE IF ( ISR .LT. 0 ) THEN
                    IPT = IFIND_PL ( -ISR, ISS_SOU, ISTAR )
                END IF
!
! ------------- Putting new point to the slot if we have found where
!
                IF ( IPT .GT. 0 ) THEN
                     I_NPT(IPT) = I_NPT(IPT) + 1
                     TC_ARR(I_NPT(IPT),IPT) = TIM(NOBS_IBL)
                     XC_ARR(I_NPT(IPT),IPT) = VAL(NOBS_IBL)
                     EC_ARR(I_NPT(IPT),IPT) = SIG(NOBS_IBL)
                     NPT = NPT + 1
                END IF
            ELSE IF ( LTRI_TYPE ) THEN
              IF ( INT4(OBSBAS(J3)%IND_SCA) .NE. N_SCA_OLD  .OR. &
     &             J3 .EQ. DBOBJ%L_OBS                       ) THEN
                 IF ( N_TRI .EQ. 3 ) THEN
                    CALL SIGN_TRICLS ( NBAS_TRI, DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                   ISG_TRI, ISG_COR, IND_FRE, IND_TAU, -3 )
                    IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Gdc'  .AND. &
     &                    PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! -------------------- Group delay closure for X-band
!
                       CLS = ISG_TRI(1)*OBSBAS(NOBS_TRI(1))%TAUGR_OBS + &
     &                       ISG_TRI(2)*OBSBAS(NOBS_TRI(2))%TAUGR_OBS + &
     &                       ISG_TRI(3)*OBSBAS(NOBS_TRI(3))%TAUGR_OBS
!
! -------------------- Fixing differences in epochs
!
                       CLS = CLS + &
     &                    ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                    ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
                       ER_CLS = DSQRT ( OBSBAS(NOBS_TRI(1))%TAUGR_ERR**2 + &
     &                                  OBSBAS(NOBS_TRI(2))%TAUGR_ERR**2 + &
     &                                  OBSBAS(NOBS_TRI(3))%TAUGR_ERR**2   )
                    ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Gdc'  .AND. &
     &                    PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! -------------------- Group delay closure for S-band
!
                       CLS = ISG_TRI(1)*OBSBAS(NOBS_TRI(1))%TAUGR_OBS_OPP + &
     &                       ISG_TRI(2)*OBSBAS(NOBS_TRI(2))%TAUGR_OBS_OPP + &
     &                       ISG_TRI(3)*OBSBAS(NOBS_TRI(3))%TAUGR_OBS_OPP
!
! -------------------- Fixing differences in epochs
!
                       CLS = CLS + &
     &                    ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS_OPP * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                    ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS_OPP * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
                       ER_CLS = DSQRT ( OBSBAS(NOBS_TRI(1))%TAUGR_ERR_OPP**2 + &
     &                                  OBSBAS(NOBS_TRI(2))%TAUGR_ERR_OPP**2 + &
     &                                  OBSBAS(NOBS_TRI(3))%TAUGR_ERR_OPP**2   )
                    ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pdc' .OR. &
     &                        PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc' .OR. &
     &                        PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc'     ) THEN
!
! -------------------- Computation of the phase delay misclosure
!
                       IF ( PAMB_PLOT_BAND .EQ. PAMB__XBAND  ) THEN
!
! ------------------------- Getting phase delay ambiguity spacings and
! ------------------------- observables for X-band.
!
                            PAMB_SP = 1.D0/OBSBAS(NOBS_TRI(1))%FREQ_OBSV_PH
                            OBS_1  = OBSBAS(NOBS_TRI(1))%TAUPH_OBS
                            OBS_2  = OBSBAS(NOBS_TRI(2))%TAUPH_OBS
                            OBS_3  = OBSBAS(NOBS_TRI(3))%TAUPH_OBS
                         ELSE IF ( PAMB_PLOT_BAND .EQ. PAMB__SBAND  ) THEN
!
! ------------------------- The same for the S-band
!
                            PAMB_SP = 1.D0/OBSBAS(NOBS_TRI(1))%FREQ_OBSV_PH_OPP
                            OBS_1  = OBSBAS(NOBS_TRI(1))%TAUPH_OBS_OPP
                            OBS_2  = OBSBAS(NOBS_TRI(2))%TAUPH_OBS_OPP
                            OBS_3  = OBSBAS(NOBS_TRI(3))%TAUPH_OBS_OPP
                       END IF
!
! -------------------- Producing closure
!
                       CLS = ISG_TRI(1)*OBS_1 + &
     &                       ISG_TRI(2)*OBS_2 + &
     &                       ISG_TRI(3)*OBS_3
!
! -------------------- Fixing differences in epochs
!
                       IF ( PAMB_PLOT_BAND .EQ. PAMB__XBAND  ) THEN
!
                            CLS = CLS + &
     &                       ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS * &
     &                                  OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                       ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS * &
     &                                  OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
                          ELSE IF ( PAMB_PLOT_BAND .EQ. PAMB__SBAND  ) THEN
!
                            CLS = CLS + &
     &                    ISG_COR(1)*OBSBAS(NOBS_TRI(IND_FRE(1)))%RATE_OBS_OPP * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(1)))%TAU_C + &
     &                    ISG_COR(2)*OBSBAS(NOBS_TRI(IND_FRE(2)))%RATE_OBS_OPP * &
     &                               OBSBAS(NOBS_TRI(IND_TAU(2)))%TAU_C
                       END IF
                       CLS_OLD = CLS
                       IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc'  .OR. &
     &                      PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc'       ) THEN
!
! ------------------------- Fixing possible phase ambiguity jumps
!
                            IAMB = NINT ( CLS/PAMB_SP )
                            CLS  = CLS - IAMB*PAMB_SP
                            IF ( CLS .GT. 0.5*PAMB_SP ) THEN
                                 CLS = CLS - PAMB_SP
                            END IF
                            IF ( CLS .LT. -0.5*PAMB_SP ) THEN
                                 CLS = CLS + PAMB_SP
                            END IF
                       END IF
!
                       IF ( PAMB_PLOT_BAND .EQ. PAMB__XBAND ) THEN
                            ER_CLS = DSQRT ( OBSBAS(NOBS_TRI(1))%TAUPH_ERR**2 + &
     &                                       OBSBAS(NOBS_TRI(2))%TAUPH_ERR**2 + &
     &                                       OBSBAS(NOBS_TRI(3))%TAUPH_ERR**2  )
                          ELSE IF ( PAMB_PLOT_BAND .EQ. PAMB__SBAND ) THEN
                            ER_CLS=DSQRT(OBSBAS(NOBS_TRI(1))%TAUPH_ERR_OPP**2 + &
     &                                   OBSBAS(NOBS_TRI(2))%TAUPH_ERR_OPP**2 + &
     &                                   OBSBAS(NOBS_TRI(3))%TAUPH_ERR_OPP**2  )
                       END IF
!
                       IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Amc' ) THEN
!
! ------------------------- Calculation ambiguity misclosure and its sigma
! ------------------------- (in phase turns). We multuply result by 1.d-12
! ------------------------- because it will be multipied by 1.D12 below.
!
                            CALL ERR_PASS ( IUER, IER )
                            CALL AMB_CLS ( DBOBJ, OBSBAS, NOBS_TRI, ICLS_X, &
     &                                     ICLS_S, ECLS_X, ECLS_S, IER )
!
                            IF ( PAMB_PLOT_BAND .EQ. PAMB__XBAND ) THEN
                                 CLS    = ICLS_X *1.D-12
                                 ER_CLS = ECLS_X *1.D-12
                               ELSE IF ( PAMB_PLOT_BAND .EQ. PAMB__SBAND ) THEN
                                 CLS    = ICLS_S *1.D-12
                                 ER_CLS = ECLS_S *1.D-12
                            END IF
                         ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Phc' )THEN
                            CLS    = CLS   / PAMB_SP *1.D-12
                            ER_CLS = ER_CLS/ PAMB_SP *1.D-12
                       END IF
                    ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pgc'  .AND. &
     &                    PAMB_PLOT_BAND .EQ. PAMB__XBAND               ) THEN
!
! -------------------- Closure of Pgx o-c for X-band
!
                       PAMB_SP = 1.D0/OBSBAS(NOBS_TRI(1))%FREQ_OBSV_PH
                       CLS = ISG_TRI(1)*PAMBI(NOBS_TRI(1))%RES_PX_GXS + &
     &                       ISG_TRI(2)*PAMBI(NOBS_TRI(2))%RES_PX_GXS + &
     &                       ISG_TRI(3)*PAMBI(NOBS_TRI(3))%RES_PX_GXS
                       ER_CLS = DSQRT ( PAMBI(NOBS_TRI(1))%ERR_PX_GXS**2 + &
     &                                  PAMBI(NOBS_TRI(2))%ERR_PX_GXS**2 + &
     &                                  PAMBI(NOBS_TRI(3))%ERR_PX_GXS**2 )
                       CLS    = CLS    /PAMB_SP *1.D-12
                       ER_CLS = ER_CLS /PAMB_SP *1.D-12
                    ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:3) .EQ. 'Pgc'  .AND. &
     &                    PAMB_PLOT_BAND .EQ. PAMB__SBAND               ) THEN
!
! -------------------- Closure of Pgx o-c for S-band
!
                       PAMB_SP = 1.D0/OBSBAS(NOBS_TRI(1))%FREQ_OBSV_PH_OPP
                       CLS = ISG_TRI(1)*PAMBI(NOBS_TRI(1))%RES_PS_GXS + &
     &                       ISG_TRI(2)*PAMBI(NOBS_TRI(2))%RES_PS_GXS + &
     &                       ISG_TRI(3)*PAMBI(NOBS_TRI(3))%RES_PS_GXS
                       ER_CLS = DSQRT ( PAMBI(NOBS_TRI(1))%ERR_PS_GXS**2 + &
     &                                  PAMBI(NOBS_TRI(2))%ERR_PS_GXS**2 + &
     &                                  PAMBI(NOBS_TRI(3))%ERR_PS_GXS**2 )
                       CLS    = CLS    /PAMB_SP *1.D-12
                       ER_CLS = ER_CLS /PAMB_SP *1.D-12
                    END IF  !  ptp_abr(pamb_plot_type)
!
! ----------------- Find the "color" slot in TC_ARR/XC_ARR/EC_ACC arrays
!
                    ITMP      = INT4 ( OBSBAS(NOBS_TRI(1))%IND_SCA )
                    ISTAR_SCA = INT4 ( OBSSCA(ITMP)%ISTAR )
                    IPT = 0
                    IF ( ISR .EQ. 0 ) THEN
                         IPT = 1
                       ELSE IF ( ISR .GT. 0 ) THEN
                         IF ( ISR .EQ. ISTAR_SCA ) THEN
                              IPT = 1
                         END IF
                       ELSE IF ( ISR .LT. 0 ) THEN
                         IPT = IFIND_PL ( -ISR, ISS_SOU, ISTAR_SCA )
                    END IF
!
! ----------------- Putting new point to the slot if we have found where
!
                    IF ( IPT .GT. 0 ) THEN
                         I_NPT(IPT) = I_NPT(IPT) + 1
                         TC_ARR(I_NPT(IPT),IPT) = RES_U(NOBS_TRI(1))%TT*24.0
                         XC_ARR(I_NPT(IPT),IPT) = CLS*1.D12
                         EC_ARR(I_NPT(IPT),IPT) = ER_CLS*1.D12
!
                         NPT = NPT + 1
                         TIM(NPT) = TC_ARR(I_NPT(IPT),IPT)
                         VAL(NPT) = XC_ARR(I_NPT(IPT),IPT)
                         SIG(NPT) = EC_ARR(I_NPT(IPT),IPT)
                         WEI(NPT) = 1.D0/SIG(NPT)
                    END IF
                 END IF ! n_tri=3
                 N_TRI = 0
              END IF  ! new scan
!
              IF ( IFIND_PL ( 3, JTRI, INT4(OBSBAS(J3)%ISITE(1)) ) .GT.0 &
     &                     .AND. &
     &             IFIND_PL ( 3, JTRI, INT4(OBSBAS(J3)%ISITE(2)) ) .GT.0 &
     &                     .AND. &
     &             N_TRI .LT. 3 ) THEN
!
                   N_TRI = N_TRI + 1
                   NBAS_TRI(N_TRI) = IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, &
     &                                  NSTBA ( INT4(OBSBAS(J3)%ISITE(1)), &
     &                                          INT4(OBSBAS(J3)%ISITE(2)) ) )
                   NOBS_TRI(N_TRI) = J3
              END IF
              N_SCA_OLD = INT4(OBSBAS(J3)%IND_SCA)
            ELSE IF ( LSCA_TYPE ) THEN
!
! ----------- Scan type of data
!
              IF ( INT4(OBSBAS(J3)%IND_SCA) .GT. N_SCA_OLD  .OR. &
     &             J3 .EQ. DBOBJ%L_OBS                            ) THEN
                   IF ( L_OBS .GT. 0 ) THEN
                        IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S1p' ) THEN
                             IPAR =  1
                           ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S2p' ) THEN
                             IPAR =  2
                           ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S3p' ) THEN
                             IPAR =  3
                           ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S4p' ) THEN
                             IPAR =  4
                           ELSE
                             IPAR = -1
                        END IF
!
! --------------------- Calculation of station dependnent difference phase minus
! --------------------- group delays for the station IPL_STA granted IPLFD_STA
! --------------------- as a fiducial station
!
                        CALL ERR_PASS ( IUER, IER )
                        CALL SHOW_ARF ( IPAR, L_OBS, LIS_OBS, PAMB_PLOT_BAND, &
     &                       DBOBJ, OBSBAS, OBSSCA, PAMBI, IPLFD_STA, IPL_STA, &
     &                       ARF_VAL, ARF_SIG, ICOND, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( N_SCA_OLD, STR )
                             CALL ERR_LOG ( 6249, IUER, 'PAMB_SHOWRES', &
     &                           'Error in SHOW_ARF for scan '//STR )
                             RETURN
                        END IF
                      ELSE
                        ICOND = -1
                   END IF
!
                   IF ( ICOND .EQ. 1 ) THEN
                        ITMP      = INT4 ( OBSBAS(LIS_OBS(1))%IND_SCA )
                        ISTAR_SCA = INT4 ( OBSSCA(ITMP)%ISTAR )
!
! --------------------- Find the "color" slot in TC_ARR/XC_ARR/EC_ACC arrays
!
                        IPT = 0
                        IF ( ISR .EQ. 0 ) THEN
                             IPT = 1
                           ELSE IF ( ISR .GT. 0 ) THEN
                             IF ( ISR .EQ. ISTAR ) THEN
                                  IPT = 1
                             END IF
                           ELSE IF ( ISR .LT. 0 ) THEN
                             IPT = IFIND_PL ( -ISR, ISS_SOU, ISTAR )
                        END IF
!
! --------------------- Putting a new point to the slot if we have found where
!
                        IF ( IPT .GT. 0 ) THEN
                             I_NPT(IPT) = I_NPT(IPT) + 1
                             IF ( I_NPT(IPT) .GT. M_NPT ) THEN
                                  CALL CLRCH ( STR )
                                  CALL INCH  ( M_NPT, STR )
                                  CALL ERR_LOG ( 6250, IUER, 'PAMB_SHOWRES', &
     &                                'Number of points excceded the limit '// &
     &                                 STR )
                                  RETURN
                             END IF
                             TC_ARR(I_NPT(IPT),IPT) = RES_U(LIS_OBS(1))%TT*24.0
                             XC_ARR(I_NPT(IPT),IPT) = ARF_VAL
                             EC_ARR(I_NPT(IPT),IPT) = ARF_SIG
!
                             NPT = NPT + 1
                             TIM(NPT) = TC_ARR(I_NPT(IPT),IPT)
                             VAL(NPT) = XC_ARR(I_NPT(IPT),IPT)
                             SIG(NPT) = EC_ARR(I_NPT(IPT),IPT)
                             WEI(NPT) = 1.D0/SIG(NPT)
                        END IF ! ipt
                   END IF ! icond
                   N_SCA_OLD = INT4(OBSBAS(J3)%IND_SCA)
                   L_OBS = 0
                 ELSE
                   L_OBS = L_OBS + 1
                   LIS_OBS(L_OBS) = J3
              END IF
         END IF ! lbas_type
 430  CONTINUE
      CALL PRCH ( '  done '//CHAR(10)//CHAR(13) )
!
      IF ( LBAS_TYPE ) THEN
!
! -------- Calculation of the parameters of the linear trend for residuals to be
! -------- plotted
!
           CALL REGRW8    ( NOBS_IBL, TIM, VAL, WEI, %VAL(0), DR, SH, -3 )
!
! -------- Calculation of wrms of the residuals with respect to the linear trend
!
           CALL DISP_WTR8 ( NOBS_IBL, TIM, VAL, WEI, DR, SH, %VAL(0), WRMS_U, &
     &                      NZ, -3 )
         ELSE IF ( LSCA_TYPE  .AND.  NPT .GT. 2 ) THEN
           CALL REGRW8    ( NPT, TIM, VAL, WEI, %VAL(0), DR, SH, -3 )
           CALL DISP_WTR8 ( NPT, TIM, VAL, WEI, DR, SH, %VAL(0), WRMS_U, &
     &                      NZ, -3 )
      END IF
!
! --- VI. Filling fields of the data structure DiaGi for plotting
!
      CALL CLRCH  ( DIAGI_S%ZAG )
      IF ( LBAS_TYPE ) THEN
           IF ( NOBS_IBL .LT. 2  ) THEN
                WRITE ( 6, * ) ' NOBS_IBL = ',NOBS_IBL,' for ',DBOBJ%C_BAS(IBL)
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                GOTO 910
           END IF
!
           CALL CLRCH  ( STR )
           CALL INCH   ( NOBS_IBL, STR )
           CALL CLRCH  ( STR1 )
!
! -------- Calcilation the index of the seleceted baseline in the
! -------- list of used baselines
!
           IUL = IFIND_PL ( DBOBJ%U_BAS, DBOBJ%UIS_BAS, DBOBJ%LIS_BAS(IBL) )
           IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B1p' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B2p' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pra' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppp'       ) THEN
!
                WRITE ( UNIT=STR1, FMT='(F8.3)' ) WRMS_U
             ELSE
                WRITE ( UNIT=STR1, FMT='(F8.1)' ) WRMS_U
           END IF
           CALL CHASHL ( STR1 )
           IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
           DIAGI_S%ZAG = DBOBJ%NAME//' '//DBOBJ%C_BAS(IBL)//' '// &
     &                 PTP_ABR(PAMB_PLOT_TYPE)//'('// &
     &                 BAND_ABR(PAMB_PLOT_BAND)//') '// &
     &                 ADD_MES(1:I_LEN(ADD_MES))//' wrms='// &
     &                 STR1(1:I_LEN(STR1))
           IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B1p' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'B2p' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pra' .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppp'       ) THEN
!
                DIAGI_S%ZAG = DIAGI_S%ZAG(1:I_LEN(DIAGI_S%ZAG))//' turns'
              ELSE IF ( PTP_ABR(PAMB_PLOT_TYPE)(1:2) .EQ. 'AP' ) THEN
                DIAGI_S%ZAG = DIAGI_S%ZAG(1:I_LEN(DIAGI_S%ZAG))//' units'
              ELSE
                DIAGI_S%ZAG = DIAGI_S%ZAG(1:I_LEN(DIAGI_S%ZAG))//' psec'
           END IF
         ELSE IF ( LTRI_TYPE ) THEN
            IF ( NPT .LT. 2 ) THEN
                 WRITE ( 6, * ) ' JTRI = ', JTRI
                 WRITE ( 6, * ) ' NPT = ',NPT,' for ', &
     &                            DBOBJ%C_STA(JTRI_SEL(1))//'/'// &
     &                            DBOBJ%C_STA(JTRI_SEL(2))//'/'// &
     &                            DBOBJ%C_STA(JTRI_SEL(3))
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
                 GOTO 910
            END IF
!
            CALL CLRCH ( UNITS )
            IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Phc' .OR. &
     &           PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pgc' .OR. &
     &           PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Amc'      ) THEN
!
                 UNITS = 'turns'
               ELSE
                 UNITS = 'psec'
            END IF
!
            DIAGI_S%ZAG = DBOBJ%NAME(1:I_LEN(DBOBJ%NAME))//' '// &
     &                    DBOBJ%C_STA(JTRI(1))//'/'// &
     &                    DBOBJ%C_STA(JTRI(2))//'/'// &
     &                    DBOBJ%C_STA(JTRI(3))//' ('// &
     &                    BAND_STR(PAMB_PLOT_BAND)//') "'// &
     &                    PTP_ABR(PAMB_PLOT_TYPE)//'" '//UNITS(1:I_LEN(UNITS))
         ELSE IF ( LSTA_TYPE ) THEN
            IF ( NPT .LT. 2 ) THEN
                 WRITE ( 6, * ) ' NPT = ',NPT,' for ',DBOBJ%C_STA(IPL_STA)//'('// &
     &                                        DBOBJ%C_STA(IPLFD_STA)//')'
                 CALL HIT_CONT ( %VAL(0), %VAL(0) )
                 GOTO 910
            END IF
            WRITE ( UNIT=STR1, FMT='(F8.3)' ) WRMS_U
            CALL CHASHL ( STR1 )
            IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
!
            CALL CLRCH ( UNITS )
            IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S1p'  .OR. &
     &           PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S2p'  .OR. &
     &           PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S3p'  .OR. &
     &           PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S4p'       ) THEN
                 UNITS = 'turns'
            END IF
!
! --------- Making a title
!
            DIAGI_S%ZAG = DBOBJ%NAME(1:I_LEN(DBOBJ%NAME))//' '// &
     &                    DBOBJ%C_STA(IPL_STA)//'('// &
     &                    DBOBJ%C_STA(IPLFD_STA)//') '// &
     &                    BAND_STR(PAMB_PLOT_BAND)//' "'// &
     &                    PTP_ABR(PAMB_PLOT_TYPE)//'" wrms='// &
     &                    STR1(1:I_LEN(STR1))//' '//UNITS(1:I_LEN(UNITS))
      END IF
!
      IF ( ISR .GE. 0 ) THEN
           DIAGI_S%ICLR      = 1
           DIAGI_S%NCLR      = 1
           IF ( LBAS_TYPE ) THEN
                DIAGI_S%NPOI(1)   = NOBS_IBL
                TIM_LAST          = TIM(NOBS_IBL)
              ELSE IF ( LTRI_TYPE .OR. LSCA_TYPE ) THEN
                DIAGI_S%NPOI(1)   = NPT
                TIM_LAST          = TIM(NPT)
           END IF
!
! -------- Setting values of data structure DIAGI for plotting
!
           DIAGI_S%ADR_X8(1) = LOC(TIM)
           DIAGI_S%ADR_Y8(1) = LOC(VAL)
           DIAGI_S%ADR_E8(1) = LOC(SIG)
           DIAGI_S%LER (1)   = .TRUE.
           DIAGI_S%ICOL(1)   = 1
!
           DIAGI_S%ICOL(1) = 1
           DIAGI_S%IOST(1) = 1
           DIAGI_S%IPST(1) = 5
           DIAGI_S%IWST(1) = 1
           IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S1p'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S2p'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S3p'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'S4p'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Pgc'       ) THEN
                DIAGI_S%IBST(1) = 2
              ELSE
                DIAGI_S%IBST(1) = 0
           END IF
           DIAGI_S%ILST(1) = 1
!
           TIME_SPAN         = ( TIM_LAST - TIM(1) )
           DIAGI_S%XMIN      = TIM(1)     - FILL*TIME_SPAN
           DIAGI_S%XMAX      = TIM_LAST   + FILL*TIME_SPAN
!
           DIAGI_S%YMIN      =  1.0  ! To force Diagi to calculate
           DIAGI_S%YMAX      = -1.0  ! boundaries anew
           DIAGI_S%ITRM      = 0
           DIAGI_S%IBATCH    = 0
           DIAGI_S%STATUS    = DIA__DEF
!
           IF ( PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppg'  .OR. &
     &          PTP_ABR(PAMB_PLOT_TYPE) .EQ. 'Ppp'       ) THEN
!
! ------------- Setting two black lines for showing min and max boundary for
! ------------- phase ( 1/2 of phase turn )
!
                ARG_DOWN(1) = DIAGI_S%XMIN - FILL*TIME_SPAN
                ARG_DOWN(2) = DIAGI_S%XMAX + FILL*TIME_SPAN
                VAL_DOWN(1) = -0.5
                VAL_DOWN(2) = -0.5
!
                ARG_UP(1) = DIAGI_S%XMIN - FILL*TIME_SPAN
                ARG_UP(2) = DIAGI_S%XMAX + FILL*TIME_SPAN
                VAL_UP(1) = 0.5
                VAL_UP(2) = 0.5
!
                DIAGI_S%NCLR      = 3
!
                DIAGI_S%NPOI(2)   = 2
                DIAGI_S%ADR_X8(2) = LOC(ARG_DOWN)
                DIAGI_S%ADR_Y8(2) = LOC(VAL_DOWN)
                DIAGI_S%ADR_E8(2) = 0
                DIAGI_S%LER (2)   = .FALSE.
                DIAGI_S%ICOL(2)   = 13
                DIAGI_S%IPST(2)   = 1
                DIAGI_S%IPST(2)   = 1
                DIAGI_S%IWST(2)   = 1
                DIAGI_S%IBST(2)   = 0
                DIAGI_S%ILST(2)   = 2
!
                DIAGI_S%NPOI(3)   = 2
                DIAGI_S%ADR_X8(3) = LOC(ARG_UP)
                DIAGI_S%ADR_Y8(3) = LOC(VAL_UP)
                DIAGI_S%ADR_E8(3) = 0
                DIAGI_S%LER (3)   = .FALSE.
                DIAGI_S%ICOL(3)   = 13
                DIAGI_S%IOST(3)   = 1
                DIAGI_S%IPST(3)   = 1
                DIAGI_S%IWST(3)   = 1
                DIAGI_S%IBST(3)   = 0
                DIAGI_S%ILST(3)   = 2
           END IF
         ELSE IF ( ISR .LT. 0 ) THEN
!
           DIAGI_S%ICLR = 1
           DIAGI_S%NCLR = -ISR
           DO 440 J4=1,-ISR
              DIAGI_S%NPOI(J4)   = I_NPT(J4)
              DIAGI_S%ADR_X8(J4) = LOC(TC_ARR(1,J4))
              DIAGI_S%ADR_Y8(J4) = LOC(XC_ARR(1,J4))
              DIAGI_S%ADR_E8(J4) = LOC(EC_ARR(1,J4))
              DIAGI_S%LER (J4)   = .TRUE.
              DIAGI_S%ICOL(J4)   = J4
              DIAGI_S%IOST(J4)   = 1
              DIAGI_S%IPST(J4)   = 5
              DIAGI_S%IWST(J4)   = 1
              DIAGI_S%IBST(J4)   = 2
              DIAGI_S%ILST(J4)   = 2
 440       CONTINUE
!
           IF ( LBAS_TYPE ) THEN
                TIM_LAST     = TIM(NOBS_IBL)
              ELSE IF ( LTRI_TYPE .OR. LSTA_TYPE ) THEN
                TIM_LAST     = TIM(NPT)
           END IF
           TIME_SPAN         = TIM_LAST - TIM(1)
           DIAGI_S%XMIN      = TIM(1)   - FILL*TIME_SPAN
           DIAGI_S%XMAX      = TIM_LAST + FILL*TIME_SPAN
!
           DIAGI_S%YMIN      =  1.0  ! To force Diagi to calculate
           DIAGI_S%YMAX      = -1.0  ! boundaries anew
           DIAGI_S%ITRM      = 0
           DIAGI_S%IBATCH    = 0
!
           DIAGI_S%STATUS    = DIA__DEF
        END IF
        CALL CLRCH ( DIAGI_S%NAME )
!
! ----- VII. Calling the main routine of DiaGI
!
        CALL ERR_PASS ( IUER, IER )
        CALL DIAGI    ( DIAGI_S, IER )
        GOTO 910
!
 810  CONTINUE
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  PAMB_SHOWRES  #!#
