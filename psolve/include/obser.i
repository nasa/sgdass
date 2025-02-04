!
! >>>>> INCLUDE-BLOCK with descriptions of data structures used for iterative
!       solutions. It keeps reduced set of parameters of the session.
!
!       include block solve.i SHOULD BE declared before!
!
!  pet  2012.03.12  Increased  ML_DBOBJ  from  47260 to 59548
!                   Increased  ML_RST    from  28440 to 42776
!                   Increased  ML_CHI    from  44292 to 62724
!  pet  2020.09.30  Increased  ML_DBOBJ, ML_HLD, ML_CHI, ML_RST
!  pet  2022.02.14  Added TEC_APR and DEL_BIAS_UL
!
!       obser.i  12-SEP-1997  v2.31  (c)  L. Petrov  --  2022.02.14_15:42:58
!
      INTEGER*4    MO_SOU, MO_STA, MO_BAS, MO_OBS, MO_SCA, MO_TRI
      PARAMETER  ( MO_SOU = MAX_ARC_SRC )
      PARAMETER  ( MO_STA = MAX_ARC_STA )
      PARAMETER  ( MO_BAS = (MO_STA*(MO_STA-1))/2 )
      PARAMETER  ( MO_TRI = ((MO_STA-1)*(MO_STA-2))/2 )
      PARAMETER  ( MO_OBS = MAX_OBS )
      PARAMETER  ( MO_SCA = 8192    )
      INTEGER*4    ELIM__MBS
      PARAMETER  ( ELIM__MBS = 8 ) ! min number of observations at one baseline
!                                  ! which accpetable for using
!                                  ! baseline-dependent statistics
      INTEGER*4    ML_SCA, ML_STA, ML_BAS, ML_HLD, ML_RES, ML_RST, ML_DBOBJ, &
     &             ML_CHI
      INTEGER*4    DBOBJ__UNF, DBOBJ__DON
      INTEGER*4    E__UNF, E__SOU, E__STA1, E__STA2, E__BAS
      PARAMETER  ( DBOBJ__UNF =  -1 )
      PARAMETER  ( DBOBJ__DON = 901 )
      PARAMETER  (     E__UNF = 801 ) ! Type "Undefined"
      PARAMETER  (     E__SOU = 802 ) ! Type "Source"
      PARAMETER  (     E__STA1= 803 ) ! Type "Station_1"
      PARAMETER  (     E__STA2= 804 ) ! Type "Station_2"
      PARAMETER  (     E__BAS = 805 ) ! Type "Baseline"
!C
      TYPE      SCA_O__STRU
          REAL*8      FJD    !! FIRST !!
          REAL*8      FRACTC
          REAL*8      FRACT
          REAL*8      DERTAU_BAS(3)
          INTEGER*2   FILLER_(3)
          INTEGER*2   ISTAR  !! LAST  !!
      END TYPE  SCA_O__STRU   !  SCA_O__STRU    !
      PARAMETER   ( ML_SCA = 56 )
!
      TYPE      STA_O__STRU
          REAL*8      ELEV   !! FIRST
          REAL*8      AZIM
          REAL*8      DERTAU_TRO
          REAL*8      DERTAU_TRO_GRAD(2)
          REAL*8      DERTAU_AXOF
          REAL*8      TEMPC
          REAL*8      ATMPR
          REAL*8      RELHU
          INTEGER*2   FILLER_1(3)
          INTEGER*2   IND_SCA  !! LAST
      END TYPE  STA_O__STRU   !  STA_O__STRU    !
      PARAMETER       ( ML_STA = 80 )
!
      TYPE      BAS_O__STRU
          REAL*8      TAU_C    !! FIRST
          REAL*8      FRE_C
!
          REAL*8      TAUGR_OBS
          REAL*8      TAUGR_ERR
          REAL*8      TAUSB_OBS
          REAL*8      TAUSB_ERR
          REAL*8      TAUPH_OBS
          REAL*8      TAUPH_ERR
          REAL*8      RATE_OBS
          REAL*8      FREQ_IONO_GR
          REAL*8      FREQ_IONO_PH
          REAL*8      FREQ_OBSV_PH
!
          REAL*8      TAUGR_OBS_OPP
          REAL*8      TAUGR_ERR_OPP
          REAL*8      TAUSB_OBS_OPP
          REAL*8      TAUSB_ERR_OPP
          REAL*8      TAUPH_OBS_OPP
          REAL*8      TAUPH_ERR_OPP
          REAL*8      RATE_OBS_OPP
          REAL*8      FREQ_IONO_GR_OPP
          REAL*8      FREQ_IONO_PH_OPP
          REAL*8      FREQ_OBSV_PH_OPP
!
          REAL*8      DERTAU_SOU(2)
          REAL*8      DERTAU_EOP(5)
          REAL*8      DERTAU_GAM
          REAL*8      DERTAU_PRE
!
          REAL*8      TAU_COR
          REAL*8      FRE_COR
          REAL*8      TAUGR_ERR_COR
          REAL*8      TAUSB_ERR_COR
          REAL*8      TAUPH_ERR_COR
	  REAL*8      GIONSG(2)
	  REAL*8      TEC_APR(2)
	  REAL*8      DTEC_ADJ
	  REAL*8      DTEC_ERR
	  REAL*8      DEL_BIAS_UL
!
          CHARACTER*2 LQUAL_CHR
          CHARACTER*2 LQUAL_CHR_OPP
!
          INTEGER*2   ISITE(2)
          INTEGER*2   IUNW
          INTEGER*2   IUNWP
          INTEGER*2   ICORR
          INTEGER*2   SUPSTAT(2)
          INTEGER*2   DTEC_FLG
          INTEGER*2   UACSUP
          INTEGER*4   AUTO_SUP
          INTEGER*4   USER_SUP
          INTEGER*4   USER_REC
          INTEGER*2   FILLER(1)
          INTEGER*2   IND_SCA  !! LAST
      END TYPE  BAS_O__STRU   !  BAS_O__STRU    /
      PARAMETER       ( ML_BAS = 384 )
!
      TYPE      HLD_O__STRU
          INTEGER*4   FIRST_FIELD
          INTEGER*4   SCA
          INTEGER*4   STA(MO_STA)
          INTEGER*4   OBS
          REAL*8      WEIGR_BAS  ( MO_BAS ) ! ( 1/SIGMA )
          REAL*8      WEIPH_BAS  ( MO_BAS ) ! ( 1/SIGMA )
          INTEGER*4   LAST_FIELD
      END TYPE  HLD_O__STRU   !  HLD_O__STRU    /
      PARAMETER       ( ML_HLD = 523284 )
!
      TYPE      RES_O__STRU
          REAL*8      TT           !  time elapsed from the first scan
          REAL*8      PSF_DEL      !  post fit residual for time delay
          REAL*8      OC_DEL       !  sec
          REAL*8      WEI_DEL      !  (1/SIGMA)
          REAL*8      AMB_SP       !  sec
          REAL*8      AMBION_SP    !  sec
          INTEGER*4   NUMAMB_USED  !  used ambiguity
          INTEGER*4   NUMAMB_NEW   !  new ambiguitiy
          INTEGER*4   NUMAMB_S_USED  !  used ambiguity for the S-band
          INTEGER*4   NUMAMB_S_NEW   !  new ambiguitiy for the S-band
!
          INTEGER*4   NUMAMB_GR_USED    !  used group ambiguity
          INTEGER*4   NUMAMB_GR_NEW     !  new  group ambiguitiy
          INTEGER*4   NUMAMB_GR_S_USED  !  used group ambiguity for the S-band
          INTEGER*4   NUMAMB_GR_S_NEW   !  new  group ambiguitiy for the S-band
!
          INTEGER*2   CLO_SEG  !  Index of the clock      segment
          INTEGER*2   ATM_SEG  !  Index of the atmosphere segment
          INTEGER*2   EOP_SEG  !  Index of the EOP        segment
!
          INTEGER*2   CLO_SEG_LAST    !  Index of the last clock  segment
          INTEGER*2   ATM_SEG_LAST    !  Index of the last atmos. segment
          INTEGER*2   EOP_SEG_LAST    !  Index of the last EOP    segment
          INTEGER*2   CURR_CSG        !  Index of the current common segment
!
          LOGICAL*1   PROT            !  Protection flag
          CHARACTER   END_MARKER_RES*1
      END TYPE  RES_O__STRU   !  RES_O__STRU    /
      PARAMETER       ( ML_RES = 96 )
!
      INTEGER*4    RST__INIT, RST__STADONE, RST__MAXUP, RST__STUPD
      PARAMETER  ( RST__INIT    = 701 ) ! Status: initialized
      PARAMETER  ( RST__STADONE = 702 ) ! status: staisitcs done
      PARAMETER  ( RST__MAXUP   = 703 ) ! Code: bypass statistics
      PARAMETER  ( RST__STUPD   = 704 ) ! Code: updates statistics
!
      TYPE      RST_O__STRU
          INTEGER*4   FIRST_FIELD
          INTEGER*4   L_OBS       ! Total number of observations
          INTEGER*4   U_OBS       ! Number of used observations
          INTEGER*4   R_OBS       ! Number of restorable observations
          INTEGER*4   U_BAS       ! Number of used baselines
          INTEGER*4   U_SOU       ! Number of used sources
!
          REAL*8      WW_ACC         ! Global weight accumulator
          REAL*8      WB_ACC(MO_BAS) ! Baseline weight accumulator
          REAL*8      WS_ACC(MO_SOU) ! Source weight accumulator
!
          REAL*8      WRMS_G      ! WRMS among all used observations
          REAL*8       RMS_G      !  RMS among all used observations
          REAL*8      WDPR_G      ! square root from weighted dispersion of
!                                 ! post-fit residuals among all used observat.
          REAL*8      WRMS_B(MO_BAS) ! Baseline dependent WRMS among all used
!                                    ! observations of the certain baseline
          REAL*8      WDPR_B(MO_BAS) ! Baseline dependent WDPR among all used
!                                    ! observations of the certain baseline
          INTEGER*4   KU_BAS(MO_BAS) ! Number of used observations at the
!                                    ! certain baseline
          REAL*8      WRMS_S(MO_SOU) ! Source dependent WRMS among all used
!                                    ! observations of the certain source
          REAL*8      WDPR_S(MO_SOU) ! Source dependent WDPR among all used
!                                    ! observations of the certain source
          INTEGER*4   KU_SOU(MO_SOU) ! Number of used observations of the
!                                    ! certain source
          REAL*8      WNPR_MXOA_G ! max globally scaled normalized postfit for
!                                 ! outliers among all observations
          INTEGER*4   INDX_MXOA_G !   Index of observations yielded this postfit
          REAL*8      WNPR_MXOU_G ! max globally scaled normalized postfit for
!                                 ! outliers among used observations
          INTEGER*4   INDX_MXOU_G !   Index of observations yielded this postfit
          REAL*8      WNPR_MXOA_B ! max baselinely scaled normalized postfit
!                                 ! for outliers among all observations
          INTEGER*4   INDX_MXOA_B !   Index of observations yielded this postfit
          REAL*8      WNPR_MXOU_B ! max baselinely scaled normalized postfit
!                                 ! for outliers among used observations
          INTEGER*4   INDX_MXOU_B !   Index of observations yielded this postfit
!
          REAL*8      WNPR_MXOP_G ! max postfit for used observations
          INTEGER*4   INDX_MXOP_G !   Index of observations yielded this postfit
!
          REAL*8      WNPR_MINN_G ! min globally scaled normalized postfit
!                                 ! for recoverable not used observations
          INTEGER*4   INDX_MINN_G !   Index of observations yielded this postfit
          INTEGER*4   NAMC_MINN_G !   Ambiguity correction for this obs.
          REAL*8      AMBS_MINN_G !   Ambiguity spasing for this observation
          REAL*8      WNPR_MINN_B ! min baselinely scaled normalized postfit
!                                 ! for recoverable not used observations
          INTEGER*4   INDX_MINN_B !   Index of observations yielded this postfit
          INTEGER*4   NAMC_MINN_B !   Ambiguity correction for this obs.
          REAL*8      AMBS_MINN_B !   Ambiguity spasing for this observation
          REAL*8      WNPR_MINP_G ! min postfit for recoverable not used
!                                 ! observations
          INTEGER*4   INDX_MINP_G !   Index of observations yielded this postfit
          INTEGER*4   NAMC_MINP_G !   Ambiguity correction for this obs.
          REAL*8      AMBS_MINP_G !   Ambiguity spasing for this observation
          INTEGER*4   COUNTER     ! counter of the operations
          INTEGER*4   LAST_FIELD
      END TYPE  RST_O__STRU   !  RST_O__STRU    /
      PARAMETER       ( ML_RST = 942808 )
!
      TYPE      DBOBJ_O__STRU
          INTEGER*4   FIRST_FIELD
          CHARACTER   NAME*16
          INTEGER*4   MARKER_1
!
          INTEGER*4   L_OBS  ! Total number of observations
          INTEGER*4   U_OBS  ! Number of used observations
          INTEGER*4   R_OBS  ! Number of good, but rejected by user observations
          INTEGER*4   CG_OBS ! Number of conditionally good observations
          INTEGER*4   L_SCA  ! Number of scans
!
          INTEGER*4   L_SOU
          INTEGER*4   LIS_SOU(MO_SOU)
          INTEGER*4   KL_SOU (MO_SOU)
          INTEGER*4   U_SOU
          INTEGER*4   UIS_SOU(MO_SOU)
          INTEGER*4   KU_SOU (MO_SOU)
          CHARACTER   C_SOU  (MO_SOU)*8
!
          INTEGER*4   L_STA
          INTEGER*4   LIS_STA(MO_STA)
          INTEGER*4   KL_STA (MO_STA)
          INTEGER*4   U_STA
          INTEGER*4   UIS_STA(MO_STA)
          INTEGER*4   KU_STA (MO_STA)
          CHARACTER   C_STA  (MO_STA)*8
!
          INTEGER*4   L_BAS
          INTEGER*4   LIS_BAS(MO_BAS)
          INTEGER*4   KL_BAS (MO_BAS)
          INTEGER*4   U_BAS
          INTEGER*4   UIS_BAS(MO_BAS)
          INTEGER*4   KU_BAS (MO_BAS)
          CHARACTER   C_BAS  (MO_BAS)*17
!
          INTEGER*4   U_TRI
          INTEGER*4   UIS_TRI(3,MO_TRI)
          CHARACTER   C_TRI  (MO_TRI)*26
!
          INTEGER*4   MARKER_2
!
          REAL*8      FJD_F
          REAL*8      UTC_F
          REAL*8      TCT_F
!
          REAL*8      FJD_L
          REAL*8      UTC_L
          REAL*8      TCT_L
!
          REAL*8      SES_SPAN
          LOGICAL*4   F_AMB
          LOGICAL*4   F_ION
          LOGICAL*4   F_AMB_CHANGED
!
          INTEGER*2   IDATYP
          INTEGER*2   FILLER_1(3)
          INTEGER*4   STATUS
          INTEGER*4   LAST_FIELD
      END TYPE  DBOBJ_O__STRU   ! DBOBJ_O__STRU /
      PARAMETER  ( ML_DBOBJ = 2338636 )
!
      TYPE      CHIACC__STRU
          INTEGER*4   FIRST_FIELD
          REAL*8      CHI_GLO
          REAL*8      CHIMAT_GLO
          REAL*8      CHIMA4_GLO
          REAL*8      WEI2_GLO
          REAL*8      WEIGR_GLO
          REAL*8      WEIPH_GLO
          INTEGER*4   NEQU_GLO
!
          REAL*8      CHI_BAS    ( MO_BAS )
          REAL*8      CHIMAT_BAS ( MO_BAS )
          REAL*8      CHIMA4_BAS ( MO_BAS )
          REAL*8      WEI2_BAS   ( MO_BAS )
          REAL*8      WEIGR_BAS  ( MO_BAS )
          REAL*8      WEIPH_BAS  ( MO_BAS )
          INTEGER*4   NEQU_BAS   ( MO_BAS )
!
          REAL*8      CHI_SOU    ( MO_SOU )
          REAL*8      CHIMAT_SOU ( MO_SOU )
          REAL*8      CHIMA4_SOU ( MO_SOU )
          REAL*8      WEI2_SOU   ( MO_SOU )
          INTEGER*4   NEQU_SOU   ( MO_SOU )
          INTEGER*4   LAST_FIELD
      END TYPE  CHIACC__STRU   ! CHIACC__STRU !
      PARAMETER ( ML_CHI = 1734212 )
!
! <<<<< end of INCLUDE-BLOCK  obser.i
!
