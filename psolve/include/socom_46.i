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
       INTEGER*2   IFREE_LEN_46
       INTEGER*2   SRC_BIT_WORDS_46, MAX_SRC_46
       INTEGER*4   JSOCOM_BLOCKS_46, JSOCOM_WORDS_46
       PARAMETER  ( MAX_SRC_46  = 4096 )
       PARAMETER  ( SRC_BIT_WORDS_46 = (MAX_SRC_46+WORD_BITS-1)/WORD_BITS )
       PARAMETER  ( JSOCOM_BLOCKS_46 = 46, &
     &              JSOCOM_WORDS_46  = JSOCOM_BLOCKS_46*BLOCK_WORDS )
!
       PARAMETER ( IFREE_LEN_46         =  39 ) ! 2-bytes words
!CC
       REAL*8 &
     &        PI_VAR_46, &
     &        FJDCL_46(MAX_CLK), &
     &        TATM_46(MAX_ATM), &
     &        ELMIN_46, &
     &        VLIGHT_46, &
     &        TROT_46(MAX_ROT), &
     &        WRMS_46(3), &
     &        ROTAP_46(MAX_ROT,4), &
     &        UT1INB_46(3), &
     &        WOBINB_46(3), &
     &        CALCV_46, &
     &        ATMOS_INTERVAL_46, &
     &        CLOCK_INTERVAL_46, &
     &        ROT_INTERVAL_46(2), &
     &        TROT_A1_46, &
     &        UT1PTB_46(MAX_EROT_VALUES), &
     &        WOBXXB_46(MAX_EROT_VALUES), &
     &        WOBYYB_46(MAX_EROT_VALUES)
!
       REAL*8 &
     &        FCNPER_46, &
     &        SACNST_46(MAX_ARC_STA), &
     &        SCCNST_46(MAX_ARC_STA), &
     &        ELVCUT_46(MAX_ARC_STA), &
     &        EOPCONS_46(3), &
     &        EOPRCONS_46(3), &
     &        SEOCNST_46(2), &
     &        PWCCNST_46, &
     &        NUTCONS_46(2), &
     &        TGRAD_46(MAX_GRAD), &
     &        GRAD_INTERVAL_46, &
     &        GRADCONS_46(2), &
     &        NUTPSI_DIF_46, &
     &        NUTEPS_DIF_46, &
     &        NUTPSI_AVE_46, &
     &        NUTEPS_AVE_46, &
     &        CHISQR_46(3),  &
     &        UTC_M_TAI_46
!
       LOGICAL*2 &
     &        LOGBCL_46, &
     &        BMODE_CL_46, &
     &        BMODE_AT_46, &
     &        CLK_BRK_STAT_46, &
     &        FLYBY_WARNING_46, &
     &        SITE_DEP_CONST_46, &
     &        SIMULATION_TEST_46, &
     &        SITE_DEP_EL_CUT_46, &
     &        SHORT_UT1_IN_46, &
     &        SOL_AVAIL_46, &
     &        OLD_CLOCKS_46, &
     &        OLD_ATMS_46, &
     &        SKIP_EOP_OFF_46, &
     &        CGM_TYPE_46
!
       CHARACTER*50 USER_PRO_46
       CHARACTER*68 USER_BUF_46
       CHARACTER*4  SCR_FIL_ORIGIN_46
       CHARACTER  EXP_DESC_46*80
       CHARACTER  EXP_CODE_46*8
       CHARACTER  PI_NAME_46*80
       CHARACTER  CORRELATOR_NAME_46*32
       CHARACTER  CORRTYPE_46*8
       CHARACTER  REC_MODE_46*80
!
       CHARACTER  UT1_RS_46*1
       CHARACTER  UT1_RS_FLYBY_46*1
       CHARACTER  EOP_TS_CALC_46*8
       CHARACTER  EOP_TS_MODF_46*8
       CHARACTER  DBNAME_CH_46*10
       CHARACTER  ENV_FINAM_46*128
!
       INTEGER*2 &
     &        NUMSTR_46, NUMSTA_46,  NPOLD_46, &
     &        ICLOCK_46(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        IDNWT_46,  IPRES_46,  IRNCD_46(2),  ITDGLB_46, &
     &        NPARAM_46,  IDATYP_46, NROT_46,  NSOURC_46, &
     &        NSPARM_46(MAX_ARC_STA),  NUMATM_46(MAX_ARC_STA), &
     &        NUMGRAD_46(MAX_ARC_STA), IATSTR_46(MAX_ARC_STA), &
     &        ICLMAX_46,  NUMCLK_46(MAX_ARC_STA), &
     &        ICLSTR_46(MAX_ARC_STA),  IPSTP_46, &
     &        LNUT_46(3),  LPREC_46, &
     &        LTIDE_46(STA_BIT_WORDS,3), LREL_46, &
     &        LROT_46(ROT_BIT_WORDS,3),  LATM_46(ATM_BIT_WORDS,3), &
     &        LCLK_46(MAX_CLK),  LSTAR_46(SRC_BIT_WORDS_46,2), &
     &        LAXOF_46(STA_BIT_WORDS), &
     &        LSITEC_46(STA_BIT_WORDS,3), &
     &        ISRSEL_46(SRC_BIT_WORDS_46), &
     &        IUEN_46,  ICLSTA_46(ARC_STA_BIT_WORDS,MAX_CLK), &
     &        NFLEPS_46,  FLEPS_46(14), &
     &        NFLPSI_46, FLPSI_46(14), &
     &        IDPNUT_46(7),  NDPNUT_46, &
     &        LSITEV_46(STA_BIT_WORDS,3), IARCSOC_46, &
     &        NSLAST_46, IDBSEL_46, &
     &        NDB_46,  IDCSEL_46, &
     &        IBLSEL_G_46(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        IBLSEL_P_46(ARC_STA_BIT_WORDS,MAX_ARC_STA), &
     &        CONSTRAINT_BITS_46, &
     &        INDL_46, WVMASK_46(MAX_ARC_STA),  BM_REF_CL_46, &
     &        NROT_A1_46(2), EOP_STYLE_46(2), &
     &        EOPA1_CHOICE_46(2), IEOPL_46, &
     &        NUMSTAX_46, INTERPOLATION_UT1_46, &
     &        INTERPOLATION_PM_46, BGROUND_46, &
     &        LPROP_46(SRC_BIT_WORDS_46,2),  TOTSTA_46, &
     &        LGRAD_46(2),  INIT_INTERACTIVE_46, &
     &        CLOCK_REF_BITS_46(2),  IDBEST_46, &
     &        OPP_STATUS_46,  PAMB_STATUS_46, &
     &        SUPMET_46, IFREE_SOCOM_46(IFREE_LEN_46), &
     &        SOCOM_LAST_I2_46
!
      INTEGER*4 &
     &        NUMOBS_46, &
     &        IDBEND_46(MAX_DBS), &
     &        NUMSCA_46
      INTEGER*2 &
     &        STABIT_G_46(2), &
     &        STABIT_P_46(2)
!
      INTEGER*2 ISOCOM_46 ( JSOCOM_WORDS_46 )
!
      EQUIVALENCE ( ISOCOM_46, PI_VAR_46 )
!
!  common
!
       COMMON / SOCOM / &
!       REAL*8 &
     &            PI_VAR_46, &
     &            FJDCL_46, &
     &            TATM_46, &
     &            ELMIN_46, &
     &            VLIGHT_46, &
     &            TROT_46, &
     &            WRMS_46, &
     &            ROTAP_46, &
     &            UT1INB_46, &
     &            WOBINB_46, &
     &            CALCV_46, &
     &            ATMOS_INTERVAL_46, &
     &            CLOCK_INTERVAL_46, &
     &            ROT_INTERVAL_46, &
     &            TROT_A1_46, &
     &            UT1PTB_46, &
     &            WOBXXB_46, &
     &            WOBYYB_46, &
     &            FCNPER_46, &
     &            SACNST_46, &
     &            SCCNST_46, &
     &            ELVCUT_46, &
     &            EOPCONS_46, &
     &            EOPRCONS_46, &
     &            SEOCNST_46, &
     &            PWCCNST_46, &
     &            NUTCONS_46, &
     &            TGRAD_46, &
     &            GRAD_INTERVAL_46, &
     &            GRADCONS_46, &
     &            NUTPSI_DIF_46, &
     &            NUTEPS_DIF_46, &
     &            NUTPSI_AVE_46, &
     &            NUTEPS_AVE_46, &
     &            CHISQR_46,  &
     &            UTC_M_TAI_46
