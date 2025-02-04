      FUNCTION   SPD_REF_CIDDOR96_W532 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_CIDDOR96_W532
! *                                                                      *
! * # 16-SEP-2013 SPD_REF_CIDDOR96_W532 v1.1 (c) L. Petrov 24-NOV-2017 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_CIDDOR96_W532 
      REAL*8     P, PW, TEM
      REAL*8     ZM
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM+SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM+SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM+SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM+SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      SPD_REF_CIDDOR96_W532 = SPD__CIDDOR_W532_ST *P /TEM * ZM + &
     &                        SPD__CIDDOR_W532_SW *PW/TEM * ZM
!
      RETURN
      END  FUNCTION   SPD_REF_CIDDOR96_W532  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_REF_CIDDOR96_W532_WATER ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_CIDDOR96_W532_WATER
! *                                                                      *
! * # 16-SEP-2013 SPD_REF_CIDDOR96_W532 v1.0 (c) L. Petrov 25-NOV-2017 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_CIDDOR96_W532_WATER
      REAL*8     P, PW, TEM
      REAL*8     ZM
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM+SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM+SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM+SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM+SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      SPD_REF_CIDDOR96_W532_WATER = SPD__CIDDOR_W532_SW *PW/TEM * ZM
!
      RETURN
      END  FUNCTION   SPD_REF_CIDDOR96_W532_WATER  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_REF_CIDDOR96_W1064 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_CIDDOR96_W1064 
! *                                                                      *
! * # 16-SEP-2013 SPD_REF_CIDDOR96_W1064 v1.1 (c) L. Petrov 24-NOV-2017 #*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_CIDDOR96_W1064
      REAL*8     P, PW, TEM
      REAL*8     ZM
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM+SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM+SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM+SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM+SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      SPD_REF_CIDDOR96_W1064 = SPD__CIDDOR_W1064_ST *P /TEM * ZM + &
     &                         SPD__CIDDOR_W1064_SW *PW/TEM * ZM
!
      RETURN
      END  FUNCTION   SPD_REF_CIDDOR96_W1064  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_REF_RUEGER02 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_RUEGER02 
! *                                                                      *
! * ### 05-FEB-2014  SPD_REF_RUEGER02 v1.0 (c) L. Petrov 05-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_RUEGER02 
      REAL*8     P, PW, TEM
!
      SPD_REF_RUEGER02 = SPD__RUEGER_K1R*(P - PW)/TEM + &
     &                   SPD__RUEGER_K2R*PW/TEM + &
     &                   SPD__RUEGER_K3R*PW/TEM**2
!
      RETURN
      END  FUNCTION   SPD_REF_RUEGER02  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_REF_APARICIO11 ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_APARICIO11 
! *                                                                      *
! * ### 06-FEB-2014 SPD_REF_APARICIO11 v1.0 (c) L. Petrov 06-FEB-2014 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_APARICIO11 
      REAL*8     P, PW, TEM
      REAL*8     ZM, Q, RHO_D, RHO_TOT, RHO_W
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM-SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM-SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM-SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM-SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      RHO_TOT = (SPD__MD/SPD__R*P/TEM - (SPD__MD - SPD__MW)/SPD__R*PW/TEM )*ZM
      Q = SPD__MW/SPD__MD*PW/(P - (1.0D0 - SPD__MW/SPD__MD)*PW)
      RHO_W = Q*RHO_TOT
      RHO_D = RHO_TOT - RHO_W
      SPD_REF_APARICIO11 = (SPD__APARICIO_N1D + SPD__APARICIO_N2D/TEM)*RHO_D + &
     &                     (SPD__APARICIO_N1W + SPD__APARICIO_N2W/TEM)*RHO_W   
      SPD_REF_APARICIO11 = SPD_REF_APARICIO11*(1.0D0 + SPD_REF_APARICIO11/6.0D0)
!
      RETURN
      END  FUNCTION   SPD_REF_APARICIO11  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   SPD_REF_APARICIO11_WATER ( P, PW, TEM )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_REF_APARICIO11_WATER
! *                                                                      *
! * # 06-FEB-2014 SPD_REF_APARICIO11_WATER v1.0 (c) Petrov 18-FEB-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      REAL*8     SPD_REF_APARICIO11_WATER
      REAL*8     P, PW, TEM
      REAL*8     ZM, Q, RHO_D, RHO_TOT, RHO_W
!
      ZM = 1.D0/ &
     &           ( 1.0D0 &
     &             - P/TEM*         (SPD__COMP_A0 + SPD__COMP_A1*(TEM-SPD__ABS_TEMP) +  &
     &                                              SPD__COMP_A2*(TEM-SPD__ABS_TEMP)**2 ) &
     &             + PW/TEM*        (SPD__COMP_B0 + SPD__COMP_B1*(TEM-SPD__ABS_TEMP)) &
     &             + PW**2/(P*TEM)* (SPD__COMP_C0 + SPD__COMP_C1*(TEM-SPD__ABS_TEMP)) &
     &             + P**2/TEM**2*    SPD__COMP_D0 &
     &             + PW**2/TEM**2*   SPD__COMP_E0 &
     &           )
      RHO_TOT = (SPD__MD/SPD__R*P/TEM - (SPD__MD - SPD__MW)/SPD__R*PW/TEM )*ZM
      Q = SPD__MW/SPD__MD*PW/(P - (1.0D0 - SPD__MW/SPD__MD)*PW)
      RHO_W = Q*RHO_TOT
      RHO_D = RHO_TOT - RHO_W
      SPD_REF_APARICIO11_WATER = (SPD__APARICIO_N1W + SPD__APARICIO_N2W/TEM)*RHO_W   
!
      RETURN
      END  FUNCTION   SPD_REF_APARICIO11_WATER  !#!#
