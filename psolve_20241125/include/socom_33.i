!@This is the start of file SOCOM (33 block socom)
!
! Last modified 21-JUL-2000 16:41:16
!
! IF YOU CHANGE THIS FILE YOU MUST ALSO UPDATE:
!   1.  Q_SOCOM
!   2.  COPIES OF CGM_COM.F IN THE CUTIL, TRSTA AND RTSRC DIRECTORIES
!       (THE TRSTA AND RTSRC COPIES ARE THE SAME).
!   3.  trans/trans.f - to pick up the positions of constraint_bits,
!       parameters IPOS1, IPOS2 etc.
!   4. ../sdbh/srset.f - Initialize any added variables there.
!   5.  ALL superfiles !
!
       INTEGER*2   IFREE_LEN_33
       INTEGER*2   SRC_BIT_WORDS_33, MAX_SRC_33, MAX_GRAD_33
       INTEGER*4   JSOCOM_BLOCKS_33, JSOCOM_WORDS_33
       PARAMETER  ( MAX_SRC_33  = 768 )
       PARAMETER  ( MAX_GRAD_33 =  64 )
       PARAMETER  ( SRC_BIT_WORDS_33 = (MAX_SRC_33+WORD_BITS-1)/WORD_BITS )
       PARAMETER  ( JSOCOM_BLOCKS_33 = 33, &
     &              JSOCOM_WORDS_33  = JSOCOM_BLOCKS_33*BLOCK_WORDS )
!
       PARAMETER ( IFREE_LEN_33         =  39 ) ! 2-bytes words
!CC
       REAL*8 &
     &        PI_33, &
     &        FJDCL_33(MAX_CLK), &
     &        TATM_33(MAX_ATM), &
     &        ELMIN_33, &
     &        VLIGHT_33, &
     &        TROT_33(MAX_ROT), &
     &        WRMS_33(3), &
     &        ROTAP_33(MAX_ROT,4), &
     &        UT1INB_33(3), &
     &        WOBINB_33(3), &
     &        CALCV_33, &
     &        ATMOS_INTERVAL_33, &
     &        CLOCK_INTERVAL_33, &
     &        ROT_INTERVAL_33(2), &
     &        TROT_A1_33, &
     &        UT1PTB_33(MAX_EROT_VALUES), &
     &        WOBXXB_33(MAX_EROT_VALUES), &
     &        WOBYYB_33(MAX_EROT_VALUES)
!
       REAL*8 &
     &        FCNPER_33, &
     &        SACNST_33(MAX_ARC_STA), &
     &        SCCNST_33(MAX_ARC_STA), &
     &        ELVCUT_33(MAX_ARC_STA), &
     &        EOPCONS_33(3), &
     &        EOPRCONS_33(3), &
     &        SEOCNST_33(2), &
     &        PWCCNST_33, &
     &        NUTCONS_33(2), &
     &        TGRAD_33(MAX_GRAD_33), &
     &        GRAD_INTERVAL_33, &
     &        GRADCONS_33(2)
!
       LOGICAL*2 &
     &        LOGBCL_33, &
     &        BMODE_CL_33, &
     &        BMODE_AT_33, &
     &        CLK_BRK_STAT_33, &
     &        FLYBY_WARNING_33, &
     &        SITE_DEP_CONST_33, &
     &        SIMULATION_TEST_33, &
     &        SITE_DEP_EL_CUT_33, &
     &        SHORT_UT1_IN_33, &
     &        SOL_AVAIL_33, &
     &        OLD_CLOCKS_33, &
     &        OLD_ATMS_33, &
     &        SKIP_EOP_OFF_33, &
     &        CGM_TYPE_33
!
       CHARACTER*50 USER_PRO_33
       CHARACTER*68 USER_BUF_33
       CHARACTER*4  SCR_FIL_ORIGIN_33
!
       INTEGER*2 &
     &        NUMSTR_33, NUMSTA_33,  NPOLD_33, &
     &        ICLOCK_33(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        IDNWT_33,  IPRES_33,  IRNCD_33(2),  ITDGLB_33, &
     &        NPARAM_33,  IDATYP_33, NROT_33,  NSOURC_33, &
     &        NSPARM_33(MAX_ARC_STA),  NUMATM_33(MAX_ARC_STA), &
     &        NUMGRAD_33(MAX_ARC_STA), IATSTR_33(MAX_ARC_STA), &
     &        ICLMAX_33,  NUMCLK_33(MAX_ARC_STA), &
     &        ICLSTR_33(MAX_ARC_STA),  IPSTP_33, &
     &        LNUT_33(3),  LPREC_33, &
     &        LTIDE_33(STA_BIT_WORDS,3), LREL_33, &
     &        LROT_33(ROT_BIT_WORDS,3),  LATM_33(ATM_BIT_WORDS,3), &
     &        LCLK_33(MAX_CLK),  LSTAR_33(SRC_BIT_WORDS_33,2), &
     &        LAXOF_33(STA_BIT_WORDS), &
     &        LSITEC_33(STA_BIT_WORDS,3), &
     &        ISRSEL_33(SRC_BIT_WORDS_33), &
     &        IUEN_33,  ICLSTA_33(ARC_STA_BIT_WORDS,MAX_CLK), &
     &        NFLEPS_33,  FLEPS_33(14), &
     &        NFLPSI_33, FLPSI_33(14), &
     &        IDPNUT_33(7),  NDPNUT_33, &
     &        LSITEV_33(STA_BIT_WORDS,3), IARCSOC_33, &
     &        NSLAST_33, IDBSEL_33, &
     &        NDB_33,  IDCSEL_33, &
     &        IBLSEL_G_33(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        IBLSEL_P_33(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        CONSTRAINT_BITS_33, &
     &        INDL_33, WVMASK_33(MAX_ARC_STA),  BM_REF_CL_33, &
     &        NROT_A1_33(2), EOP_STYLE_33(2), &
     &        EOPA1_CHOICE_33(2), IEOPL_33, &
     &        NUMSTAX_33, INTERPOLATION_UT1_33, &
     &        INTERPOLATION_PM_33, BGROUND_33, &
     &        LPROP_33(SRC_BIT_WORDS_33,2),  TOTSTA_33, &
     &        LGRAD_33(2),  INIT_INTERACTIVE_33, &
     &        CLOCK_REF_BITS_33(2),  IDBEST_33, &
     &        OPP_STATUS_33,  PAMB_STATUS_33, &
     &        SUPMET_33, IFREE_SOCOM_33(IFREE_LEN_33), &
     &        SOCOM_LAST_I2_33
!
      CHARACTER*1 UT1_RS_33, &
     &            UT1_RS_FLYBY_33
!
      REAL*8 &
     &        NUTPSI_DIF_33, &
     &        NUTEPS_DIF_33
!
      INTEGER*4 &
     &        NUMOBS_33, &
     &        IDBEND_33(MAX_DBS), &
     &        NUMSCA_33
      INTEGER*2 &
     &        STABIT_G_33(2), &
     &        STABIT_P_33(2)
!
      INTEGER*2 ISOCOM_33 ( JSOCOM_WORDS_33 )
!
      EQUIVALENCE ( ISOCOM_33, PI_33 )
!
!  common
!
       COMMON / SOCOM / &
!
!  real*8
!
     &        PI_33,   FJDCL_33,  TATM_33, ELMIN_33,  VLIGHT_33, &
     &        TROT_33, WRMS_33,   ROTAP_33,  UT1INB_33, &
     &        WOBINB_33,  UT1_RS_33,  UT1_RS_FLYBY_33, &
     &        CALCV_33,  ATMOS_INTERVAL_33,  CLOCK_INTERVAL_33, &
     &        FCNPER_33,  SACNST_33, &
     &        SCCNST_33,  ELVCUT_33, EOPCONS_33, &
!
!  logical*2
!
     &        LOGBCL_33,  BMODE_CL_33, &
     &        BMODE_AT_33, CLK_BRK_STAT_33, &
     &        FLYBY_WARNING_33,  SITE_DEP_CONST_33, &
     &        SIMULATION_TEST_33,  SITE_DEP_EL_CUT_33, &
     &        SHORT_UT1_IN_33, &
!
!  integer*2
!
     &        NUMSTR_33,  NUMSTA_33, &
!
!  integer*4
!
     &        NUMOBS_33, &
!
!  integer*2
!
     &        NPOLD_33,   ICLOCK_33, &
     &        IDNWT_33,   IPRES_33, &
     &        IRNCD_33,   ITDGLB_33, &
     &        NPARAM_33,  IDATYP_33, &
     &        NROT_33,    NSOURC_33, &
     &        NSPARM_33,  NUMATM_33, &
     &        IATSTR_33,  ICLMAX_33, &
     &        NUMCLK_33,  ICLSTR_33, &
     &        IPSTP_33,   LNUT_33, &
     &        LPREC_33,   LTIDE_33, &
     &        LREL_33,    LROT_33, &
     &        LATM_33,    LCLK_33, &
     &        LSTAR_33,   LAXOF_33, &
     &        LSITEC_33,  ISRSEL_33, &
     &        IUEN_33,    ICLSTA_33, &
     &        NFLEPS_33,  FLEPS_33, &
     &        NFLPSI_33,  FLPSI_33, &
     &        IDPNUT_33,  NDPNUT_33, &
     &        LSITEV_33,  IARCSOC_33, &
     &        NSLAST_33, &
!
!  integer*4
!
     &        IDBEND_33, &
!
!  integer*2
!
     &        IDBSEL_33,    NDB_33, &
     &        IDCSEL_33,    IBLSEL_G_33, &
     &        CONSTRAINT_BITS_33,   INDL_33, &
     &        WVMASK_33,    BM_REF_CL_33, &
!
!    Additions
!
     &        EOP_STYLE_33,   EOPA1_CHOICE_33,  NROT_A1_33,  ROT_INTERVAL_33, &
     &        TROT_A1_33,     SEOCNST_33,       IEOPL_33,       NUMSTAX_33, &
     &        SOL_AVAIL_33,   INTERPOLATION_UT1_33, &
     &        INTERPOLATION_PM_33,  BGROUND_33, &
     &        OLD_CLOCKS_33,   OLD_ATMS_33,     EOPRCONS_33,    UT1PTB_33, &
     &        WOBXXB_33,       WOBYYB_33,       LPROP_33, &
     &        PWCCNST_33,      TOTSTA_33,       SKIP_EOP_OFF_33, &
     &        USER_PRO_33,     USER_BUF_33,     NUTCONS_33, &
     &        SCR_FIL_ORIGIN_33, GRAD_INTERVAL_33, &
     &        TGRAD_33,        GRADCONS_33, &
     &        NUMGRAD_33,      LGRAD_33, &
     &        CLOCK_REF_BITS_33,  INIT_INTERACTIVE_33, &
     &        IDBEST_33,       NUMSCA_33,  STABIT_G_33, &
     &        STABIT_P_33,     IBLSEL_P_33,     CGM_TYPE_33, &
     &        OPP_STATUS_33,   PAMB_STATUS_33,  SUPMET_33, &
     &        NUTPSI_DIF_33, &
     &        NUTEPS_DIF_33, &  ! real*8
     &        IFREE_SOCOM_33, &
     &        SOCOM_LAST_I2_33
