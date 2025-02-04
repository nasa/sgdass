      SUBROUTINE MALO_INTRP_PWTUV ( HEB_OH, HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V, &
     &                              MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_INTRP_PWTUV
! *                                                                      *
! * ## 04-NOV-2014  MALO_INTRP_PWTUV  v1.0 (c) L. Petrov 04-NOV-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INTEGER*4  NTIM, LP_LEV, IUER
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_OH, HEB_D, HEB_T, HEB_Q, HEB_U, HEB_V
      REAL*4     HEI_R4(MALO__MHEI), LAT_VAL, LON_VAL, ARGS(4)
      INTEGER*8  MEL
      INTEGER*4  M_LEV
      PARAMETER  ( M_LEV = MALO__MHEI )
      REAL*8     EPS, PRES_HIGH, HEI_MIN, HEI_MAX, HEI_MIN_INT, HEI_MAX_INT
      PARAMETER  ( PRES_HIGH    = 25000.0D0 )
      PARAMETER  ( HEI_MIN      =  -500.0D0 )
      PARAMETER  ( HEI_MAX      = 12700.0D0 )
      PARAMETER  ( HEI_MIN_INT  = -1000.0D0 )
      PARAMETER  ( HEI_MAX_INT  = 40000.0D0 )
      PARAMETER  ( EPS = 1.D-5 )
      REAL*8       SPD__U1_GMAO72, SPD__U2_GMAO72, SPD__U3_GMAO72 
      PARAMETER  ( SPD__U1_GMAO72 =    20.25319 )
      PARAMETER  ( SPD__U2_GMAO72 =  1200.00000 )
      PARAMETER  ( SPD__U3_GMAO72 =  -169.30782 )
      REAL*8       SPD__R, SPD__MA, SPD__H2O
      PARAMETER  ( SPD__R        = 8.314472D0   ) ! CIPM-2007
      PARAMETER  ( SPD__MA       = 0.02896546D0 ) ! CIPM-2007
      PARAMETER  ( SPD__H2O      = 0.01801528D0 ) ! CIPM-2007
      REAL*8       MD, MW, W_LEV_MIN
      PARAMETER  ( MD    = MA__MAPL  )
      PARAMETER  ( MW    = H2O__MAPL )
      PARAMETER  ( W_LEV_MIN = 0.01 )
      REAL*8     T(M_LEV), H(M_LEV), H_ARR(M_LEV), &
     &           P_ARR(M_LEV), P_SPL(M_LEV), P_LOG(M_LEV), &
     &           W_ARR(M_LEV), W_SPL(M_LEV), W_LOG(M_LEV), &
     &           T_ARR(M_LEV), T_SPL(M_LEV), T_LEV(M_LEV), &
     &           W_LEV(M_LEV), LON_TST, LAT_TST
      CHARACTER  STR*128, STR1*128, TEST_STR*8
      INTEGER*4  L_LON, L_LAT, IND, K_LEV, L_LEV, N_LEV, DIMS(4), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           INDS(4), IND_1ST, IND_LAST, IT_LAT, IT_LON, &
     &           L_TIM, IP, IS, IER
      REAL*8     GE, G, PHI, T_1ST, T_RATE, TEMP
      REAL*8     HARR(2), TARR(2), MIN_VAL, MAX_VAL
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IXMN8, IXMN4, IXMN4_S
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4, VAL_4D_BSPL4
      REAL*8,    EXTERNAL :: ISPL8, FSPL8, PW_TO_RELH
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      TEST_STR = 'timer'  ! Supported values: none, timer, diff, pw, pres, temp, intrp, dewt
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_INTRP_PWTUV  !#!#
