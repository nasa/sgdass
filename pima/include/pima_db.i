!
! >>>>> pima_db.i   2009.08.18_21:40:45  v 1.0 --  2009.09.28_22:59:48
!
        INTEGER*4  M2G__FR1, M2G__FR2, M2G__CL1, M2G__SL1, SLV__MAX_SOLTYP
        PARAMETER  ( M2G__FR1 = 1 )
        PARAMETER  ( M2G__FR2 = 2 )
        PARAMETER  ( M2G__CL1 = 3 )
        PARAMETER  ( M2G__SL1 = 4 )
        PARAMETER  ( SLV__MAX_SOLTYP = 32 )
        INTEGER*2  DATYP__DEF, SUPMET__DEF
        PARAMETER  ( DATYP__DEF  =  15 )
        PARAMETER  ( SUPMET__DEF = 505 )
        REAL*8     ATM_CNS__DEF, CLO_CNS__DEF, BSCL_CNS__DEF
        PARAMETER  ( ATM_CNS__DEF  = 1.111111D-14 )
        PARAMETER  ( CLO_CNS__DEF  = 2.D-14      )
        PARAMETER  ( BSCL_CNS__DEF = 5.D-6       )
!
        INTEGER*4     GRPRAT__DTP, PHSRAT__DTP,  SNBRAT__DTP, &
     &                GRPONL__DTP, PHSONL__DTP,  SNBONL__DTP, &
     &                RATONL__DTP,  G_GXS__DTP,  PX_GXS__DTP, &
     &                PS_GXS__DTP,  PX_GX__DTP,   PX_GS__DTP, &
     &                 PS_GX__DTP,  PS_GS__DTP,   P_PXS__DTP, &
     &                 DELAY__DTP,   RATE__DTP,   GROUP__DTP, &
     &                 PHASE__DTP,  MIXED__DTP,   SINGL__DTP, &
     &                    GX__DTP,     GS__DTP,      PX__DTP, &
     &                    PS__DTP,  SNG_X__DTP,   SNG_S__DTP
	INTEGER*4  FIRST__DTP, LAST__DTP
!
        PARAMETER ( GRPRAT__DTP = 0   )
        PARAMETER ( PHSRAT__DTP = 1   )
        PARAMETER ( SNBRAT__DTP = 2   )
        PARAMETER ( GRPONL__DTP = 3   )
        PARAMETER ( PHSONL__DTP = 4   )
        PARAMETER ( SNBONL__DTP = 5   )
        PARAMETER ( RATONL__DTP = 6   )
        PARAMETER (  G_GXS__DTP = 7   )
        PARAMETER ( PX_GXS__DTP = 8   )
        PARAMETER ( PS_GXS__DTP = 9   )
        PARAMETER (  PX_GX__DTP = 10  )
        PARAMETER (  PX_GS__DTP = 11  )
        PARAMETER (  PS_GX__DTP = 12  )
        PARAMETER (  PS_GS__DTP = 13  )
        PARAMETER (  P_PXS__DTP = 14  )
        PARAMETER (     GX__DTP = 15  )
        PARAMETER (     GS__DTP = 16  )
        PARAMETER (     PX__DTP = 17  )
        PARAMETER (     PS__DTP = 18  )
        PARAMETER (  SNG_X__DTP = 19  )
        PARAMETER (  SNG_S__DTP = 20  )
        PARAMETER (  FIRST__DTP = GRPRAT__DTP, LAST__DTP = SNG_S__DTP   )
!
        REAL*8     ATM_PRES_DEF, AIR_TEMP_DEF, REL_HUMID_DEF, CABLE_DEL_DEF
        REAL*8     ATM_PRES_MIN, AIR_TEMP_MIN, REL_HUMID_MIN, CABLE_DEL_MIN
        REAL*8     ATM_PRES_MAX, AIR_TEMP_MAX, REL_HUMID_MAX, CABLE_DEL_MAX
        PARAMETER  ( ATM_PRES_DEF  =   -99900.0D0 )
        PARAMETER  ( AIR_TEMP_DEF  =     -999.0D0 )
        PARAMETER  ( REL_HUMID_DEF =     -999.0D0 )
        PARAMETER  ( CABLE_DEL_DEF =        0.0D0 )
!@        PARAMETER  ( AIR_TEMP_DEF  =    288.5D0 )
!@        PARAMETER  ( ATM_PRES_DEF  = 101300.0D0 )
!@        PARAMETER  ( REL_HUMID_DEF =   -999.0D0 )
!
        PARAMETER  ( ATM_PRES_MIN  =   40000.0D0  )
        PARAMETER  ( AIR_TEMP_MIN  =     150.0D0  )
        PARAMETER  ( REL_HUMID_MIN =       0.0D0  )
        PARAMETER  ( CABLE_DEL_MIN =      -1.0D-7 )
!
        PARAMETER  ( ATM_PRES_MAX  =  120000.0D0  )
        PARAMETER  ( AIR_TEMP_MAX  =     400.0D0  )
        PARAMETER  ( REL_HUMID_MAX =       1.0D0  )
        PARAMETER  ( CABLE_DEL_MAX =       1.0D-7 )
!
        INTEGER*4    OBS__2BN, DET__2BN, AVL__2BN, ION__2BN
        PARAMETER  ( OBS__2BN = 0 ) ! Second band was observed
        PARAMETER  ( DET__2BN = 1 ) ! Second band was detected
        PARAMETER  ( AVL__2BN = 2 ) ! Second band information is available
        PARAMETER  ( ION__2BN = 3 ) ! Ionospheric contribution from the 2nd band
!
! ----- Constants specifying suppression codes
!
        INTEGER*4     BQCX__SPS, BQCS__SPS, NOFX__SPS, NOFS__SPS, &
     &                CUEL__SPS, DSBS__SPS, DSSO__SPS, BWVR__SPS, &
     &                BPRN__SPS, GION__SPS, GIO1__SPS, GIO2__SPS, &
     &                GIO3__SPS, GIO4__SPS, PION__SPS, PIO1__SPS, &
     &                PIO2__SPS, PIO3__SPS, PIO4__SPS, CION__SPS, &
     &                XAMB__SPS, SAMB__SPS, WPAS__SPS, IUNW__SPS, &
     &                SET1__SPS, SET2__SPS, GOOD__SPS, CBAD__SPS, &
     &                UNRC__SPS, DECM__SPS, IOUS__SPS, INIT__SPS
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
        PARAMETER  (  PIO4__SPS = 19 ) ! PION calibration is bad
        PARAMETER  (  CION__SPS = 20 ) ! CION calibration is not available *
        PARAMETER  (  XAMB__SPS = 21 ) ! X-band phase ambiguity not resolved
        PARAMETER  (  SAMB__SPS = 22 ) ! S-band phase ambiguity not resolved
        PARAMETER  (  IUNW__SPS = 23 ) ! IUNW code is not zero
        PARAMETER  (  WPAS__SPS = 24 ) ! Wrong phase delay ambiguity spacings
        PARAMETER  (  IOUS__SPS = 25 ) ! Ionospheric calibration is used if available
        PARAMETER  (  DECM__SPS = 26 ) ! Decimation is set on
        PARAMETER  (  INIT__SPS = 27 ) ! The bit filed has beein initialized
        PARAMETER  (  SET1__SPS = 28 ) ! Circumstnaces bits are set up
        PARAMETER  (  SET2__SPS = 29 ) ! Usage satatus bits are set up
        PARAMETER  (  GOOD__SPS = 30 ) ! Observation is marked as good
        PARAMETER  (  CBAD__SPS = 31 ) ! Observation is marked as conditionally
!                                    ! bad, but may become recoverable
        PARAMETER  (  UNRC__SPS = 32 ) ! Observation is marked as bad and
	INTEGER*4     INIT__UAS
        PARAMETER  (  INIT__UAS = 16  ) ! Flag: UACSUP has been initialized
!
        INTEGER*4    M__CAL
        PARAMETER  ( M__CAL = 64 ) !  maximal number of calibrations
        INTEGER*4  CAL__DEL, CAL__MOD
        PARAMETER  ( CAL__DEL = 901 ) ! Delay calibration
        PARAMETER  ( CAL__MOD = 902 ) ! Mode  calibration
	INTEGER*4  SOLVE__MAX_ARC_STA
	PARAMETER  ( SOLVE__MAX_ARC_STA = 32 ) ! For compatibility with Solve
!
        TYPE      CAL__INFO__TYPE
	    INTEGER*4  CLASS       ! station/baseline
	    INTEGER*4  MODE        ! Delay/phs-grp-rat-band
	    INTEGER*4  RESERVED(2)
        END  TYPE CAL__INFO__TYPE
!
        TYPE      CAL__TYPE
	     CHARACTER  NAME*8
	     TYPE  ( CAL__INFO__TYPE ) :: INFO
	     INTEGER*4  APPLIED(SOLVE__MAX_ARC_STA)
        END  TYPE CAL__TYPE
