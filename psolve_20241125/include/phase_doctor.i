!
! >>>>> INCLUDE-BLOCK with description of data structures used by
!       Phase_doctor
!
!       Phase_doctor.i 30-JUL-99  v 1.00 Leonid Petrov  10-DEC-2001 17:17:09
!
        CHARACTER  PHD__LABEL*20, PHD__DATE*10
        PARAMETER   ( PHD__LABEL = 'Phase Doctor v 1.00 ' )
        PARAMETER   ( PHD__DATE  = '2001.12.10'           )
        REAL*8        PHD__VER_MIN
        PARAMETER   ( PHD__VER_MIN = 1.00 ) ! Mininal version number with
!                                           ! compatible data strucutres
!
! ----- Limits specifications
!
      INTEGER*4    MAX_SCA, MAX_OBS, MAX_LOG, MLEN_LOG, &
     &             MAX_STA, MAX_BAS, MAX_CHA, M_BAN, M_CNT, MAX_OTL, MAX_BRK
      PARAMETER  ( MAX_SCA  =    1024 )
      PARAMETER  ( MAX_OBS  = 64*1024 )
      PARAMETER  ( MAX_LOG  = 64*1024 ) ! max number of lines in a log file
      PARAMETER  ( MLEN_LOG = 128     ) ! max length of the line in log file
      PARAMETER  ( MAX_STA  = 24      )
      PARAMETER  ( MAX_BAS  = 512     )
      PARAMETER  ( MAX_CHA  = 16      )
      PARAMETER  ( M_BAN    = 2       )
      PARAMETER  ( M_CNT    = 5       ) ! Number of contributions
      PARAMETER  ( MAX_OTL  = 32      ) ! max number of outliers per stat/chan
      PARAMETER  ( MAX_BRK  = 12      ) ! maximal number of phase breaks per s/c
!
      INTEGER*4  MPR_SPC
      PARAMETER  ( MPR_SPC = 5 )
!
      TYPE      PD_SES_STRU
          REAL*8     FJD_B
          REAL*8     UTC_B
          REAL*8     DURA_SEC
          REAL*8     FRQ_SKY(MAX_CHA)
          REAL*8     FRQ_LO(MAX_CHA)
          REAL*8     FRQ_IF(MAX_CHA)
          REAL*8     FRE_BAN(M_BAN)
!
          INTEGER*4  SPUR_STAT(MPR_SPC,MAX_CHA,MAX_STA)
          REAL*8     SPUR_AMP(MPR_SPC,MAX_CHA,MAX_STA)
          REAL*8     SPUR_AMP_SIG(MPR_SPC,MAX_CHA,MAX_STA)
          REAL*8     SPUR_PHS(MPR_SPC,MAX_CHA,MAX_STA)
          REAL*8     PCAMP_LEV(MAX_CHA,MAX_STA)
          REAL*8     PCAMP_LEV_SIG(MAX_CHA,MAX_STA)
          REAL*8     PC_NOISE(MAX_CHA,MAX_STA)
          REAL*8     PC_MIN(MAX_CHA,MAX_STA)
          REAL*8     PC_MAX(MAX_CHA,MAX_STA)
          INTEGER*4  PC_NPOI(MAX_CHA,MAX_STA)
          INTEGER*4  SPUR_PLOT_STAT
!
          REAL*8     OFFPHA_VAL(MAX_CHA,MAX_STA)
          REAL*8     OFFPHA_SIG(MAX_CHA,MAX_STA)
!
          REAL*8     TIM_BRK(MAX_BRK,MAX_CHA,MAX_STA)
          REAL*8     VAL_BRK(MAX_BRK,MAX_CHA,MAX_STA)
          REAL*8     TIM_OTL(MAX_OTL,MAX_STA)
!
          INTEGER*4  L_BRK(MAX_CHA,MAX_STA)
          INTEGER*4  L_OTL(MAX_STA)
!
          INTEGER*4  YEAR_B
          INTEGER*4  DOY_B
          INTEGER*4  L_SCA
          INTEGER*4  L_OBS
          INTEGER*4  L_STA
          INTEGER*4  L_BAS
          INTEGER*4  L_CHA
          INTEGER*4  LX_CHA
          INTEGER*4  LS_CHA
          INTEGER*4  L_BAN
          INTEGER*4  MCHAN_BAN(2)
          INTEGER*4  MOBS_BAS
          INTEGER*4  CAB_SIGN(MAX_STA)
          INTEGER*4  LIS_BAS(MAX_BAS)
          CHARACTER  CLIS_BAS(MAX_BAS)*17
          CHARACTER  EXPNAME*10
          CHARACTER  DATE*28
          CHARACTER  SCAN_ID(MAX_SCA)*28
          CHARACTER  STANAME(MAX_STA)*8
          INTEGER*4  BBC_TO_FREQ(MAX_CHA,MAX_STA)
          INTEGER*4  FREQ_TO_BBC(MAX_CHA,MAX_STA)
          INTEGER*4  FREQ_TO_BAN(MAX_CHA)
          INTEGER*4  LOGFILE(MAX_STA)
          INTEGER*4  SPURADJ_STAT(MAX_CHA,MAX_STA)
          INTEGER*4  CHAN_STAT(MAX_CHA,MAX_STA)
!
          INTEGER*4  QLIM
          INTEGER*4  OFF_STA
          BYTE       SPUR_EST(MPR_SPC,MAX_CHA,MAX_STA)
          BYTE       SPUR_APL(MPR_SPC,MAX_CHA,MAX_STA)
          BYTE       OFFPHA_EST(MAX_CHA,MAX_STA)
!
          BYTE       OFFSETS_STATUS
          BYTE       SPURS_STATUS
!
          INTEGER*4  LAST_FIELD
      END TYPE  PD_SES_STRU  !  PD_SES_STRU !
!
      TYPE      PD_STA_STRU
          REAL*8     FJD
          REAL*8     UTC
          REAL*8     ELEV
          REAL*8     AZIM
          REAL*8     PARANG
          REAL*8     CABDEL
          REAL*8     TSYS(MAX_CHA)
          REAL*8     PHA_PHC(MAX_CHA)
          REAL*8     AMP_PHC(MAX_CHA)
          INTEGER*4  IAMB_PHC(MAX_CHA)
          BYTE       USE_FRQ(MAX_CHA)
          BYTE       USE_STM(MAX_CHA)
          INTEGER*4  LAST_FIELD
      END TYPE  PD_STA_STRU  !  PD_STA_STRU  !
!
      TYPE      PD_BAS_STRU
          REAL*8     TAUGR(2)
          REAL*8     PHASE(2)
          REAL*8     RATE(2)
          REAL*8     COHER_AMP(2)
          REAL*8     TAUGR_ERR(2)
          REAL*8     SNR(2)
          REAL*8     RES_PHA(MAX_CHA)
          REAL*8     AMP_CHA(MAX_CHA)
          REAL*8     SMP_CHA(MAX_CHA)
          REAL*8     SAMPLE_RATE
	  INTEGER*4  BITS_SAMPLE
          INTEGER*4  SCAN_IND
          INTEGER*4  BASCODE
          INTEGER*4  NOAP(MAX_CHA)
          CHARACTER  QCODE(2)*1
          BYTE       REF_FREQ(2)
          BYTE       USE_FRQ(MAX_CHA)
          INTEGER*4  LAST_FIELD
      END TYPE  PD_BAS_STRU  !  PD_BAS_STRU  !
!
      INTEGER*4   MTYP__PHD, MARR__PHD
      INTEGER*4   RST_TIM__PHD, ZST_TIM__PHD, ZST_ELE__PHD, &
     &            RCA_TIM__PHD, RCP_TIM__PHD, ACP_TIM__PHD, &
     &            RCA_RCP__PHD, NCA_ACP__PHD, CPC_TIM__PHD, &
     &            BPC_TIM__PHD, PCS_ADJ__PHD, PCS_RES__PHD, &
     &            PCR_TIM__PHD, PCR_PHS__PHD, PCF_TIM__PHD, &
     &            RSX_PHS__PHD, RSS_PHS__PHD, RCX_PHS__PHD, &
     &            RCS_PHS__PHD, GDX_SPR__PHD, GDS_SPR__PHD, &
     &            PDX_SPR__PHD, PDS_SPR__PHD, WRI_MOD__PHD, &
     &            WRI_DBS__PHD, XBN_CNT__PHD, SBN_CNT__PHD
!
      PARAMETER ( RST_TIM__PHD =  1 )
      PARAMETER ( ZST_TIM__PHD =  2 )
      PARAMETER ( ZST_ELE__PHD =  3 )
      PARAMETER ( RCA_TIM__PHD =  4 )
      PARAMETER ( RCP_TIM__PHD =  5 )
      PARAMETER ( ACP_TIM__PHD =  6 )
      PARAMETER ( RCA_RCP__PHD =  7 )
      PARAMETER ( NCA_ACP__PHD =  8 )
      PARAMETER ( CPC_TIM__PHD =  9 )
      PARAMETER ( BPC_TIM__PHD = 10 )
      PARAMETER ( PCS_ADJ__PHD = 11 )
      PARAMETER ( PCS_RES__PHD = 12 )
      PARAMETER ( PCR_TIM__PHD = 13 )
      PARAMETER ( PCR_PHS__PHD = 14 )
      PARAMETER ( PCF_TIM__PHD = 15 )
      PARAMETER ( MTYP__PHD    = 15 )
      PARAMETER ( MARR__PHD    =  3 )
      PARAMETER ( RSX_PHS__PHD = 21 )
      PARAMETER ( RSS_PHS__PHD = 22 )
      PARAMETER ( RCX_PHS__PHD = 23 )
      PARAMETER ( RCS_PHS__PHD = 24 )
      PARAMETER ( GDX_SPR__PHD = 25 )
      PARAMETER ( GDS_SPR__PHD = 26 )
      PARAMETER ( PDX_SPR__PHD = 27 )
      PARAMETER ( PDS_SPR__PHD = 28 )
      PARAMETER ( WRI_MOD__PHD = 101 )
      PARAMETER ( WRI_DBS__PHD = 102 )
      PARAMETER ( XBN_CNT__PHD = 201 )
      PARAMETER ( SBN_CNT__PHD = 202 )
!
      BYTE        USED__PHD
      PARAMETER ( USED__PHD    =  1 )
!
      INTEGER*4    MFREC__PHD
      PARAMETER  ( MFREC__PHD  = 6 )
      INTEGER*4    IVRB__MIN, IVRB__MAX, IVRB__DEF
      PARAMETER  ( IVRB__MIN = 0,  IVRB__MAX = 4,  IVRB__DEF = 1 )
      CHARACTER    CVRB_RANGE*6
      PARAMETER  ( CVRB_RANGE = '[0, 4]' )
      INTEGER*4    ARA1__PHD, ARA2__PHD, ARA3__PHD, &
     &             MAR1__PHD, MAR2__PHD, MAR3__PHD
      PARAMETER  ( ARA1__PHD = 801 )
      PARAMETER  ( ARA2__PHD = 802 )
      PARAMETER  ( ARA3__PHD = 803 )
      PARAMETER  ( MAR1__PHD = 901 )
      PARAMETER  ( MAR2__PHD = 902 )
      PARAMETER  ( MAR3__PHD = 903 )
!
      INTEGER*4    UNDF__PHD, ADJ__PHD, RES__PHD, &
     &             EST__PHD,  APL__PHD, NAP__PHD, RJC__PHD
      PARAMETER  ( UNDF__PHD = 0   )
      PARAMETER  (  ADJ__PHD = 601 )
      PARAMETER  (  RES__PHD = 602 )
      PARAMETER  (  EST__PHD = 63  )
      PARAMETER  (  APL__PHD = 64  )
      PARAMETER  (  NAP__PHD = 65  )
      PARAMETER  (  RJC__PHD = 255 )
      INTEGER*4    X__BAND, S__BAND
      PARAMETER  ( X__BAND = 1 )
      PARAMETER  ( S__BAND = 2 )
      CHARACTER  NAME__BAN(2)*6
      DATA       NAME__BAN  / &
     &                        'X-band', &
     &                        'S-band' &
     &                      /
      INTEGER*4  SIN__SPR, COS__SPR, LCO__SPR
      PARAMETER  ( SIN__SPR = -1 )
      PARAMETER  ( COS__SPR = -2 )
      PARAMETER  ( LCO__SPR = -3 )
!
      INTEGER*4  GDX_CNT__PHD, PDX_CNT__PHD, RTX_CNT__PHD, &
     &           GDS_CNT__PHD, PDS_CNT__PHD, RTS_CNT__PHD
      PARAMETER  ( GDX_CNT__PHD = 1 ) ! Group delay at X-band
      PARAMETER  ( PDX_CNT__PHD = 2 ) ! Phase delay at X-band
      PARAMETER  ( RTX_CNT__PHD = 3 ) ! Delay rate  at X-band
      PARAMETER  ( GDS_CNT__PHD = 4 ) ! Group delay at S-band
      PARAMETER  ( PDS_CNT__PHD = 5 ) ! Phase delay at S-band
      PARAMETER  ( RTS_CNT__PHD = 6 ) ! Delay rate  at S-band
!
      CHARACTER    HELP01__PHD*19, HELP02__PHD*19
      PARAMETER  ( HELP01__PHD = 'phase_doctor_01.hlp' )
      PARAMETER  ( HELP02__PHD = 'phase_doctor_02.hlp' )
!
      CHARACTER  CODE_SPC(MPR_SPC)*2
      DATA       CODE_SPC &
     &           / &
     &             'A:', &
     &             'B:', &
     &             'C:', &
     &             'D:', &
     &             'L:' &
     &           /
      INTEGER*4  A__SPUR, B__SPUR, C__SPUR, D__SPUR, AMP__COMP, L__COMP, &
     &           FIRST__SPUR, LAST__SPUR
      PARAMETER  ( A__SPUR = 1   )
      PARAMETER  ( B__SPUR = 2   )
      PARAMETER  ( C__SPUR = 3   )
      PARAMETER  ( D__SPUR = 4   )
      PARAMETER  ( FIRST__SPUR = A__SPUR )
      PARAMETER  ( LAST__SPUR  = D__SPUR )
      PARAMETER  ( L__COMP   = 5   )
      PARAMETER  ( AMP__COMP = 101 )
!
! <<<<< end of INCLUDE-BLOCK  phase_doctor.i
!
