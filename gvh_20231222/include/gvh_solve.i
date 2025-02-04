!
      INTEGER*4    M_ACM
      PARAMETER  ( M_ACM = 4 ) ! Max number of station for a priori clock model
!
      TYPE      CAL__INFO__TYPE
	  INTEGER*4  CLASS       ! station/baseline
	  INTEGER*4  MODE        ! Delay/phs-grp-rat-band
	  INTEGER*4  RESERVED(2)
      END  TYPE CAL__INFO__TYPE
      INTEGER*4  CAL__DEL, CAL__MOD
      PARAMETER  ( CAL__DEL = 901 ) ! Delay calibration
      PARAMETER  ( CAL__MOD = 902 ) ! Mode  calibration
!
! --- Constants specifying suppression codes
!
      INTEGER*2     BQCX__SPS, BQCS__SPS, NOFX__SPS, NOFS__SPS, &
     &              CUEL__SPS, DSBS__SPS, DSSO__SPS, BWVR__SPS, &
     &              BPRN__SPS, GION__SPS, GIO1__SPS, GIO2__SPS, &
     &              GIO3__SPS, GIO4__SPS, PION__SPS, PIO1__SPS, &
     &              PIO2__SPS, PIO3__SPS, PIO4__SPS, CION__SPS, &
     &              XAMB__SPS, SAMB__SPS, WPAS__SPS, IUNW__SPS, &
     &              SET1__SPS, SET2__SPS, GOOD__SPS, CBAD__SPS, &
     &              UNRC__SPS, DECM__SPS, IOUS__SPS, INIT__SPS, &
     &              LSNR__SPS
      PARAMETER  (  BQCX__SPS =  1 ) ! Bad quality code for X-band
      PARAMETER  (  BQCS__SPS =  2 ) ! Bad quality code for S-band
      PARAMETER  (  NOFX__SPS =  3 ) ! No fringes for X-band
      PARAMETER  (  NOFS__SPS =  4 ) ! No fringes for S-band
      PARAMETER  (  CUEL__SPS =  5 ) ! Observation made below cut off limit
      PARAMETER  (  DSBS__SPS =  6 ) ! Observation at deselected baseline
      PARAMETER  (  DSSO__SPS =  7 ) ! Observation of deselected source
      PARAMETER  (  BWVR__SPS =  8 ) ! Bad WVR mask
      PARAMETER  (  BPRN__SPS =  9 ) ! No parangle correction available
      PARAMETER  (  GION__SPS = 10 ) ! GION calibration is not available
      PARAMETER  (  GIO1__SPS = 11 ) ! GION calibration is bad
      PARAMETER  (  GIO2__SPS = 12 ) ! GION calibration is bad
      PARAMETER  (  GIO3__SPS = 13 ) ! GION calibration is bad
      PARAMETER  (  GIO4__SPS = 14 ) ! GION calibration is bad
      PARAMETER  (  PION__SPS = 15 ) ! PION calibration is not available
      PARAMETER  (  PIO1__SPS = 16 ) ! PION calibration is bad
      PARAMETER  (  PIO2__SPS = 17 ) ! PION calibration is bad
      PARAMETER  (  PIO3__SPS = 18 ) ! PION calibration is bad
      PARAMETER  (  PIO4__SPS = 18 ) ! PION calibration is bad ! Overlap with PIO3__SPS
      PARAMETER  (  LSNR__SPS = 19 ) ! Low SNR
      PARAMETER  (  CION__SPS = 20 ) ! CION calibration is not available *
      PARAMETER  (  XAMB__SPS = 21 ) ! X-band phase ambiguity not resolved
      PARAMETER  (  SAMB__SPS = 22 ) ! S-band phase ambiguity not resolved
      PARAMETER  (  IUNW__SPS = 23 ) ! IUNW code is not zero
      PARAMETER  (  WPAS__SPS = 24 ) ! Wrong phase delay ambiguity spacings
      PARAMETER  (  IOUS__SPS = 25 ) ! Ionospheric calibration is used if available
      PARAMETER  (  DECM__SPS = 26 ) ! Decimation is set on
      PARAMETER  (  INIT__SPS = 27 ) ! The bit filed has been initialized
      PARAMETER  (  SET1__SPS = 28 ) ! Circumstnaces bits are set up
      PARAMETER  (  SET2__SPS = 29 ) ! Usage satatus bits are set up
      PARAMETER  (  GOOD__SPS = 30 ) ! Observation is marked as good
      PARAMETER  (  CBAD__SPS = 31 ) ! Observation is marked as conditionally
!                                    ! bad, but may become recoverable
      PARAMETER  (  UNRC__SPS = 32 ) ! Observation is marked as bad and
!                                    ! may never become recoverable
!
! --- Unquire codes of suppression status
!
      INTEGER*2     USED__SPS, RECO__SPS, URPH__SPS, MAXC__SPS
      PARAMETER  (  USED__SPS  = 129 ) ! Is observation used in solution?
      PARAMETER  (  RECO__SPS  = 130 ) ! Is observation recoverable?
      PARAMETER  (  URPH__SPS  = 131 ) ! Is observation unrecoverable for phase
! delay solution types?
      PARAMETER  (  MAXC__SPS  =  32 ) ! Total number of used bits in SPS-code
!
! --- Constants specifying user action for suppression
!
      INTEGER*2     GSUP__UAS, GOVV__UAS, PSUP__UAS, POVV__UAS, &
     &              INIT__UAS
      PARAMETER  (  GSUP__UAS = 1  ) ! Suppress good observation (group)
      PARAMETER  (  GOVV__UAS = 2  ) ! Restore bad observation (group)
      PARAMETER  (  PSUP__UAS = 3  ) ! Suppress good observation (phase)
      PARAMETER  (  POVV__UAS = 4  ) ! Restore bad observation (phase)
      PARAMETER  (  INIT__UAS =16  ) ! Flag: UACSUP has been initialized
!
      INTEGER*4 MAX_STA, MAX_BSL, MAX_SRC
      INTEGER*4 MAX_ARC_STA, MAX_ARC_SRC, MAX_ARC_BSL
      PARAMETER ( MAX_STA=512,       & ! max total number of station
     &            MAX_BSL=8192,      & ! max number of baselines (should be
     &            MAX_SRC=32000,     & ! max total number of sources
     &            MAX_ARC_STA=32,    & ! max number of station at one session
     &            MAX_ARC_SRC=1024,  & ! max number of sources at one session
     &            MAX_ARC_BSL=(MAX_ARC_STA*(MAX_ARC_STA-1))/2 &
     &          )
!
! --- Constants defining various types of solution
!
      INTEGER*2     GRPRAT__DTP, PHSRAT__DTP,  SNBRAT__DTP, &
     &              GRPONL__DTP, PHSONL__DTP,  SNBONL__DTP, &
     &              RATONL__DTP,  G_GXS__DTP,  PX_GXS__DTP, &
     &              PS_GXS__DTP,  PX_GX__DTP,   PX_GS__DTP, &
     &               PS_GX__DTP,  PS_GS__DTP,   P_PXS__DTP, &
     &               DELAY__DTP,   RATE__DTP,   GROUP__DTP, &
     &               PHASE__DTP,  MIXED__DTP,   SINGL__DTP, &
     &                  GX__DTP,     GS__DTP,      PX__DTP, &
     &                  PS__DTP,  SNG_X__DTP,   SNG_S__DTP, &
     &               XBAND__DTP,  SBAND__DTP,    COMB__DTP, &
     &               IOCAL__DTP,  FIRST__DTP,    LAST__DTP, &
     &                DUAL__DTP
      CHARACTER*21  GRPRAT__DTC, PHSRAT__DTC, SNBRAT__DTC, &
     &              GRPONL__DTC, PHSONL__DTC, SNBONL__DTC, &
     &              RATONL__DTC,  G_GXS__DTC, PX_GXS__DTC, &
     &              PS_GXS__DTC,  PX_GX__DTC,  PX_GS__DTC, &
     &                  GX__DTC,     GS__DTC,     PX__DTC, &
     &                  PS__DTC,  SNG_X__DTC,  SNG_S__DTC, &
     &               PS_GX__DTC,  PS_GS__DTC,  P_PXS__DTC, &
     &                DUAL__DTC
!
      PARAMETER ( GRPRAT__DTP = 0, GRPRAT__DTC='Group delay & rate  ' )
      PARAMETER ( PHSRAT__DTP = 1, PHSRAT__DTC='Phase delay & rate  ' )
      PARAMETER ( SNBRAT__DTP = 2, SNBRAT__DTC='N.Band delay & rate ' )
      PARAMETER ( GRPONL__DTP = 3, GRPONL__DTC='Group delay only    ' )
      PARAMETER ( PHSONL__DTP = 4, PHSONL__DTC='Phase delay only    ' )
      PARAMETER ( SNBONL__DTP = 5, SNBONL__DTC='N.Band delay only   ' )
      PARAMETER ( RATONL__DTP = 6, RATONL__DTC='Rate only           ' )
      PARAMETER (  G_GXS__DTP = 7,  G_GXS__DTC='G-Gxs combination   ' )
      PARAMETER ( PX_GXS__DTP = 8, PX_GXS__DTC='Px-Gxs combination  ' )
      PARAMETER ( PS_GXS__DTP = 9, PS_GXS__DTC='Ps-Gxs combination  ' )
      PARAMETER (  PX_GX__DTP = 10, PX_GX__DTC='Px-Gx combination   ' )
      PARAMETER (  PX_GS__DTP = 11, PX_GS__DTC='Px-Gs combination   ' )
      PARAMETER (  PS_GX__DTP = 12, PS_GX__DTC='Ps-Gx combination   ' )
      PARAMETER (  PS_GS__DTP = 13, PS_GS__DTC='Ps-Gs combination   ' )
      PARAMETER (  P_PXS__DTP = 14, P_PXS__DTC='P-Pxs combination   ' )
      PARAMETER (     GX__DTP = 15,    GX__DTC='Group delay X-band  ' )
      PARAMETER (     GS__DTP = 16,    GS__DTC='Group delay S-band  ' )
      PARAMETER (     PX__DTP = 17,    PX__DTC='Phase delay X-band  ' )
      PARAMETER (     PS__DTP = 18,    PS__DTC='Phase delay S-band  ' )
      PARAMETER (  SNG_X__DTP = 19, SNG_X__DTC='Single band X-band  ' )
      PARAMETER (  SNG_S__DTP = 20, SNG_S__DTC='Single band S-band  ' )
      PARAMETER (   DUAL__DTP = 21,  DUAL__DTC='Dual band gr. delays' )
      PARAMETER (  DELAY__DTP = 101                                   )
      PARAMETER (   RATE__DTP = 102                                   )
      PARAMETER (  GROUP__DTP = 103                                   )
      PARAMETER (  PHASE__DTP = 104                                   )
      PARAMETER (  MIXED__DTP = 105                                   )
      PARAMETER (  SINGL__DTP = 106                                   )
      PARAMETER (  XBAND__DTP = 201                                   )
      PARAMETER (  SBAND__DTP = 202                                   )
      PARAMETER (   COMB__DTP = 203                                   )
      PARAMETER (  IOCAL__DTP = 204                                   )
      PARAMETER (  FIRST__DTP = GRPRAT__DTP, LAST__DTP = DUAL__DTP    )
!
      INTEGER*4    UNDF__FTP, PHS_GRP_HOPS__FTP, PHS_GRP_DTEC_HOPS__FTP, &
     &             PHS_GRP_PIMA__FTP, PHS_GRP_DTEC_PIMA__FTP, PHS_GRP_ACL_PIMA__FTP
      PARAMETER  ( UNDF__FTP              =   0 )
      PARAMETER  ( PHS_GRP_HOPS__FTP      =   1 )
      PARAMETER  ( PHS_GRP_DTEC_HOPS__FTP =   2 )
      PARAMETER  ( PHS_GRP_PIMA__FTP      = 101 )
      PARAMETER  ( PHS_GRP_DTEC_PIMA__FTP = 102 )
      PARAMETER  ( PHS_GRP_ACL_PIMA__FTP  = 103 )
!
!
! --- Constants specifying suppression strategy codes
!
      INTEGER*2    SUPMET__PRE98, SUPMET__PRE91, &
     &             SUPMET__COMB1, SUPMET__SNGBA, &
     &             SUPMET__META,  SUPMET__UND,   &
     &             SUPMET__DEF,   SUPMET__FIRST, SUPMET__LAST
      PARAMETER  ( SUPMET__PRE98 = 501 ) ! pre-98 method of observ. suppression
      PARAMETER  ( SUPMET__PRE91 = 502 ) ! pre-91 method of observ. suppression
      PARAMETER  ( SUPMET__COMB1 = 503 ) ! combination method of suppression
      PARAMETER  ( SUPMET__SNGBA = 504 ) ! single band method of suppression
      PARAMETER  ( SUPMET__META  = 505 ) ! advanced Meta-Solve strategey
      PARAMETER  ( SUPMET__UND   = -1  ) ! undefined method
      PARAMETER  ( SUPMET__DEF   = SUPMET__PRE98 ) ! Default suppression method
      PARAMETER  ( SUPMET__FIRST = SUPMET__PRE98 ) ! First available supmet_xxx
      PARAMETER  ( SUPMET__LAST  = SUPMET__META ) ! Last  supported supmet_xxx
