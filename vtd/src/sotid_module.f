      MODULE SOTID_MODULE
      INCLUDE   'sotid_type.i'
      INTERFACE
!
      SUBROUTINE SOTID_SET ( GEN_LOVE, MODEL_2D, ORDER_2D, ZERO_FREQ_LOVE, &
     &                       MODEL_3D, TIDCNF, IUER )
         INCLUDE   'sotid_type.i'
         TYPE ( TIDCNF__STRU ) ::  TIDCNF
         INTEGER*4  GEN_LOVE, MODEL_2D, ORDER_2D, ZERO_FREQ_LOVE, MODEL_3D, IUER
      END SUBROUTINE SOTID_SET
!
      SUBROUTINE SOTID_PRE ( L_STA, C_STA, R_CFS, TIDCNF, STATID, IUER )
         INCLUDE   'sotid_type.i'
         INTEGER*4  L_STA, IUER
         CHARACTER  C_STA(L_STA)*(SOTID__NAM_LEN)
         REAL*8     R_CFS(3,SOTID__MAX_STA)
         TYPE ( STATID__STRU ) ::  STATID(L_STA)
         TYPE ( TIDCNF__STRU ) ::  TIDCNF
      END SUBROUTINE SOTID_PRE
!
      SUBROUTINE SOTID_TIM ( MJD, TAI, UT1_M_TAI, TIDCNF, TIMTID, IUER )
         INCLUDE   'sotid_type.i'
         INTEGER*4   MJD, IUER
         REAL*8      TAI, UT1_M_TAI
         TYPE ( TIDCNF__STRU ) ::  TIDCNF
         TYPE ( TIMTID__STRU ) ::  TIMTID
      END SUBROUTINE SOTID_TIM
!
      SUBROUTINE SOTID_DSP ( TIDCNF, TIMTID, STATID, D_REN )
         INCLUDE   'sotid_type.i'
         REAL*8     D_REN(3)
         TYPE ( TIDCNF__STRU ) ::  TIDCNF
         TYPE ( TIMTID__STRU ) ::  TIMTID
         TYPE ( STATID__STRU ) ::  STATID
      END SUBROUTINE SOTID_DSP
!
      FUNCTION   SOTID_INQ ( REQUEST, TIDCNF, OUT_STR, OUT_STR_LEN, IUER )
         INCLUDE   'sotid_type.i'
         INTEGER*4  SOTID_INQ
         TYPE ( TIDCNF__STRU ) ::  TIDCNF
         INTEGER*4  REQUEST, OUT_STR_LEN, IUER
         CHARACTER  OUT_STR*(*)
      END  FUNCTION  SOTID_INQ 
!
      END INTERFACE
      END MODULE SOTID_MODULE
