!@This is the start of file &flyby.i
!
!   Last update: 2006.02.08_17:29:51
!
!  Modifications:
!
!  pet 08-JAN-99  -- created
!  pet 16-APR-99  -- added stuf from intrp_eopmod, intrp_eovr
!  pet 19-APR-99  -- added METEO_WARN array
!  pet 21-APR-99  -- added arrays from ../cutil/calcalc.f and
!                                      ../cutil/partcalc.f
!  pet 1999.10.15 -- added new arrays ECC_OLD, ECC_NEW
!  pet 2000.09.25 -- added new arrays MGR_NORTH, MGR_EAST
!  pet 2001.01.11 -- added new array  MONU_NAME_NEW
!  pet 2001.01.12 -- added variable METRIC_NEW
!  pet 2001.04.16 -- added variable AP_OLD
!  pet 2002.12.17 -- added variables FJDCT_BEG_PSV, TIM_PSV, VALLIN_PSV,
!                                    VALSPL_PSV, COESPL_PSV,
!                                    FL_PSV_LIN, FL_PSV_SPL
!  pet 2005.03.15 -- added variables FJDCT_BEG_BSP, TIM_BSP, VALSPL_BSP, 
!                                    COESPL_BSP, XYZ_BSP, STAUSE_BSP
!  pet 2006.02.08 -- added variable  FL_CAL_SAVE   
!
      INTEGER*2   MAX_C, MAX_F
      PARAMETER ( MAX_C = MAX_EROT_VALUES      )
      PARAMETER ( MAX_F = MAX_FLYBY_EOP_VALUES )
!
      REAL*8     DELUT1, DELX, DELY, T_OLD
      REAL*8     UT1B,  WOBXB, WOBYB, UT1V, WOBXV, WOBYV, DUT, DLOD, DOMEGA
      REAL*8     DUDOT, DXDOT, DYDOT
!
      REAL*8     UC_YP1, UC_YPN,UC_XTABLE(MAX_C), UC_Y2(MAX_C)
      REAL*8     XC_YP1, XC_YPN, XC_XTABLE(MAX_C), XC_Y2(MAX_C)
      REAL*8     YC_YP1, YC_YPN, YC_XTABLE(MAX_C), YC_Y2(MAX_C)
!
      REAL*8     UF_YP1, UF_YPN, UF_XTABLE(MAX_F), UF_Y2(MAX_F)
      REAL*8     XF_YP1, XF_YPN, XF_XTABLE(MAX_F), XF_Y2(MAX_F)
      REAL*8     YF_YP1, YF_YPN, YF_XTABLE(MAX_F), YF_Y2(MAX_F)
!
      LOGICAL*2  UC_INITIALIZED, UF_INITIALIZED
      LOGICAL*2  XC_INITIALIZED, XF_INITIALIZED
      LOGICAL*2  YC_INITIALIZED, YF_INITIALIZED
!
      REAL*8  UT1_YP1, UT1_YPN, UT1_Y2(MAX_C), UT1_TABLE(MAX_C)
      REAL*8   XW_YP1,  XW_YPN,  XW_Y2(MAX_C),  XW_TABLE(MAX_C)
      REAL*8   YW_YP1,  YW_YPN,  YW_Y2(MAX_C),  YW_TABLE(MAX_C)
!
      LOGICAL*2 UT1_INITIALIZED
      LOGICAL*2 XW_INITIALIZED
      LOGICAL*2 YW_INITIALIZED
!
      REAL*8    UT1_EPOCHS(MAX_C), UT1_Z2(MAX_C), UT1_ZP1, UT1_ZPN
      REAL*8     WX_EPOCHS(MAX_C),  WX_Z2(MAX_C),  WX_ZP1,  WX_ZPN
      REAL*8     WY_EPOCHS(MAX_C),  WY_Z2(MAX_C),  WY_ZP1,  WY_ZPN
      REAL*8    UD1, UD2, UD3, XD1, XD2, XD3, YD1, YD2, YD3
!
      REAL*8     AZ_OLD(MAX_ARC_STA),       ELEV_OLD(MAX_ARC_STA), &
     &           ELDOT_OLD(MAX_ARC_STA),    ATMPR_OLD(MAX_ARC_STA), &
     &           RELHU_OLD(MAX_ARC_STA),    TEMPC_OLD(MAX_ARC_STA), &
     &           WET_OLD(MAX_ARC_STA,2), &
     &           CFADRY_OLD(MAX_ARC_STA,2), CFAWET_OLD(MAX_ARC_STA,2), &
     &           TIME_CAL_OLD(MAX_ARC_STA), TIME_PRT_OLD(MAX_ARC_STA), &
     &           AP_OLD(MAX_ARC_STA,2)
      INTEGER*2  ISTAR_CAL_OLD(MAX_ARC_STA), ISTAR_PRT_OLD(MAX_ARC_STA)
      REAL*8     ECC_OLD(3,MAX_ARC_STA), ECC_NEW(3,MAX_ARC_STA)
      REAL*8     MGR_NORTH(MAX_ARC_STA), MGR_EAST(MAX_ARC_STA)
      INTEGER*4  SCA_LAST(MAX_ARC_STA)
!
      LOGICAL*2  UT1_ZINITIALIZED
      LOGICAL*2  WX_ZINITIALIZED
      LOGICAL*2  WY_ZINITIALIZED
      LOGICAL*2  SRC_SUBSTITUTE_INIT, SIT_SUBSTITUTE_INIT, VEL_SUBSTITUTE_INIT
!
      LOGICAL*2  METEO_WARN(MAX_ARC_STA), &
     &           CAL_INIT(MAX_ARC_STA),   PRT_INIT(MAX_ARC_STA)
      LOGICAL*4  FL_PSV_LIN, FL_PSV_SPL
!
      INTEGER*2  N_UT1, N_WOB, IERR4, I4, MAX_F_SAVED
!
      INTEGER*2  NSTAR_KEEP, NSIT_KEEP, NVEL_KEEP
      CHARACTER  LSONAM_KEEP(MAX_SRC)*8, LSITNM_KEEP(MAX_STA)*8, &
     &           LVELNM_KEEP(MAX_STA)*8
      REAL*8     SUBRD_KEEP(2,MAX_SRC), SITCO_KEEP(3,MAX_STA), &
     &           VELCO_KEEP(3,MAX_STA), TIME0X_KEEP
!
      INTEGER*2  IO
      CHARACTER  MONU_NAME_NEW(MAX_ARC_STA)*10, METRIC_NEW*8
!
      REAL*8     FJDCT_BEG_PSV, TIM_PSV(M__PSV), &
     &           VALLIN_PSV(M__PSV,3,MAX_ARC_STA), &
     &           VALSPL_PSV(M__PSV,3,MAX_ARC_STA), &
     &           COESPL_PSV(M__PSV,3,MAX_ARC_STA), &
     &           XYZ_PSV(3,MAX_ARC_STA), TEM_AVR(MAX_ARC_STA)
!
      REAL*8     FJDCT_BEG_BSP, TIM_BSP(M__BSP_INT),   &
     &           VALSPL_BSP(M__BSP_INT,3,MAX_ARC_STA), &
     &           COESPL_BSP(M__BSP_INT,3,MAX_ARC_STA), &
     &           XYZ_BSP(3,MAX_ARC_STA)
      LOGICAL*1  STAUSE_BSP(MAX_ARC_STA)
      LOGICAL*1  FL_CAL_SAVE   
!CCCC
      COMMON   / FLYBY_COMMON / &
     &           DELUT1, DELX, DELY, T_OLD, &
     &           UT1B,  WOBXB, WOBYB, UT1V, WOBXV, WOBYV, DUT, DLOD, DOMEGA, &
     &           DUDOT, DXDOT, DYDOT, &
     &           UC_YP1, UC_YPN, UC_XTABLE, UC_Y2, &
     &           XC_YP1, XC_YPN, XC_XTABLE, XC_Y2, &
     &           YC_YP1, YC_YPN, YC_XTABLE, YC_Y2, &
!
     &           UF_YP1, UF_YPN, UF_XTABLE, UF_Y2, &
     &           XF_YP1, XF_YPN, XF_XTABLE, XF_Y2, &
     &           YF_YP1, YF_YPN, YF_XTABLE, YF_Y2, &
!
     &           UT1_YP1, UT1_YPN, UT1_Y2, UT1_TABLE, &
     &           XW_YP1,  XW_YPN,  XW_Y2,  XW_TABLE, &
     &           YW_YP1,  YW_YPN,  YW_Y2,  YW_TABLE, &
!
     &           UT1_EPOCHS, UT1_Z2, UT1_ZP1, UT1_ZPN, &
     &           WX_EPOCHS,  WX_Z2,   WX_ZP1,  WX_ZPN, &
     &           WY_EPOCHS,  WY_Z2,   WY_ZP1,  WY_ZPN, &
     &           UD1, UD2, UD3, &
     &           XD1, XD2, XD3, &
     &           YD1, YD2, YD3, &
!
     &           ECC_OLD,   ECC_NEW, &
     &           MGR_NORTH, MGR_EAST, &
!
     &           AZ_OLD, ELEV_OLD, ELDOT_OLD, ATMPR_OLD, RELHU_OLD, &
     &           TEMPC_OLD, CFADRY_OLD, CFAWET_OLD, WET_OLD, &
     &           TIME_CAL_OLD, TIME_PRT_OLD, AP_OLD, &
!
     &           ISTAR_CAL_OLD, ISTAR_PRT_OLD, &
!
     &           UC_INITIALIZED, UF_INITIALIZED, &
     &           XC_INITIALIZED, XF_INITIALIZED, &
     &           YC_INITIALIZED, YF_INITIALIZED, &
!
     &           UT1_INITIALIZED,  XW_INITIALIZED,  YW_INITIALIZED, &
     &           UT1_ZINITIALIZED, WX_ZINITIALIZED, WY_ZINITIALIZED, &
!
     &           METEO_WARN, CAL_INIT, PRT_INIT, &
!
     &           IO, N_UT1, N_WOB, IERR4, I4, MAX_F_SAVED, &
!
     &           MONU_NAME_NEW, METRIC_NEW, &
     &           FJDCT_BEG_PSV, TIM_PSV, VALLIN_PSV, VALSPL_PSV, COESPL_PSV, &
     &           XYZ_PSV, &
     &           FJDCT_BEG_BSP, TIM_BSP, VALSPL_BSP, &
     &           COESPL_BSP, XYZ_BSP, STAUSE_BSP,    &
     &           SCA_LAST, &
     &           FL_PSV_LIN, FL_PSV_SPL, &
     &           SRC_SUBSTITUTE_INIT, NSTAR_KEEP, LSONAM_KEEP, SUBRD_KEEP, &
     &           SIT_SUBSTITUTE_INIT, NSIT_KEEP,  LSITNM_KEEP, SITCO_KEEP, &
     &           VEL_SUBSTITUTE_INIT, NVEL_KEEP,  LVELNM_KEEP, VELCO_KEEP, &
     &           TIME0X_KEEP, FL_CAL_SAVE, TEM_AVR
!
!CCCC <<<< flyby.i
