MODULE NERS
CONTAINS
      SUBROUTINE NERS_INIT ( CONFIG_FILE, NERS, TIME_TAI_START, TIME_TAI_STOP, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      REAL*8     TIME_TAI_START, TIME_TAI_STOP
      INTEGER*4  IUER
      END  SUBROUTINE NERS_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_LOAD ( NERS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER
      END  SUBROUTINE NERS_LOAD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_EOP ( NERS, TIME_TAI, CPAR, M_PAR, L_PAR, PARS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER 
      CHARACTER  CPAR*(*)
      REAL*8     TIME_TAI, PARS(M_PAR)
      END  SUBROUTINE NERS_GET_EOP  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_SERIES ( NERS, TIME_TAI_START, TIME_TAI_END, TIME_STEP, &
     &                             CPAR, M_PAR, M_SER, NS, TIM, SER, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, M_SER, NS, IUER
      REAL*8     TIM(M_SER), SER(M_SER,M_PAR)
      REAL*8     TIME_TAI_START, TIME_TAI_END, TIME_STEP
      REAL*8     TAI_BEG, TAI_END, UTC_CUR
      CHARACTER  CPAR*(*)
      END  SUBROUTINE NERS_GET_SERIES  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_INQ ( NERS, REQ, M_PAR, L_PAR, PARS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER 
      CHARACTER  REQ*(*)
      REAL*8     PARS(M_PAR)
      END  SUBROUTINE NERS_INQ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_UTCMTAI ( NERS, UTC, UTC_M_TAI, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER 
      REAL*8     UTC, UTC_M_TAI
      END  SUBROUTINE NERS_GET_UTCMTAI !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_QUIT ( CODE, NERS )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  CODE 
      END  SUBROUTINE NERS_QUIT  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_AZEL_COMP ( NERS, TIM_TAI, COO_TRS, RA, DEC, &
     &                            REFR_MODE, AZ, EL, AZ_RATE, EL_RATE, IUER )
      IMPLICIT   NONE
      INCLUDE   'ners.i'
      REAL*8     TIM_TAI, COO_TRS(3), RA, DEC, AZ, EL, AZ_RATE, EL_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      TYPE     ( NERS__TYPE ) :: NERS
      END  SUBROUTINE NERS_AZEL_COMP !#!#  
!
! ------------------------------------------------------------------------
!
      FUNCTION   TIM_TO_DATE ( TIM, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     TIM
      CHARACTER  TIM_TO_DATE*23
      INTEGER*4  IUER
      END  FUNCTION  TIM_TO_DATE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_INIT_ ( CONFIG_FILE, NERS, TIME_TAI_START, TIME_TAI_STOP, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      REAL*8     TIME_TAI_START, TIME_TAI_STOP
      INTEGER*4  IUER
      END  SUBROUTINE NERS_INIT_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_LOAD_ ( NERS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER
      END  SUBROUTINE NERS_LOAD_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_EOP_ ( NERS, TIME_TAI, CPAR, M_PAR, L_PAR, PARS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER 
      CHARACTER  CPAR*(*)
      REAL*8     TIME_TAI, PARS(M_PAR)
      END  SUBROUTINE NERS_GET_EOP_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_SERIES_ ( NERS, TIME_TAI_START, TIME_TAI_END, TIME_STEP, &
     &                              CPAR, M_PAR, M_SER, NS, TIM, SER, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, M_SER, NS, IUER
      REAL*8     TIM(M_SER), SER(M_SER,M_PAR)
      REAL*8     TIME_TAI_START, TIME_TAI_END, TIME_STEP
      REAL*8     TAI_BEG, TAI_END, UTC_CUR
      CHARACTER  CPAR*(*)
      END  SUBROUTINE NERS_GET_SERIES_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_INQ_ ( NERS, REQ, M_PAR, L_PAR, PARS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER 
      CHARACTER  REQ*(*)
      REAL*8     PARS(M_PAR)
      END  SUBROUTINE NERS_INQ_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_UTCMTAI_ ( NERS, UTC, UTC_M_TAI, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER 
      REAL*8     UTC, UTC_M_TAI
      END  SUBROUTINE NERS_GET_UTCMTAI_ !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_QUIT_ ( CODE, NERS )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  CODE 
      END  SUBROUTINE NERS_QUIT_  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_AZEL_COMP_ ( NERS, TIM_TAI, COO_TRS, RA, DEC, &
     &                            REFR_MODE, AZ, EL, AZ_RATE, EL_RATE, IUER )
      IMPLICIT   NONE
      INCLUDE   'ners.i'
      REAL*8     TIM_TAI, COO_TRS(3), RA, DEC, AZ, EL, AZ_RATE, EL_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      TYPE     ( NERS__TYPE ) :: NERS
      END  SUBROUTINE NERS_AZEL_COMP_ !#!#  
!
! ------------------------------------------------------------------------
!
      FUNCTION   TIM_TO_DATE_ ( TIM, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     TIM
      CHARACTER  TIM_TO_DATE_*23
      INTEGER*4  IUER
      END  FUNCTION  TIM_TO_DATE_  !#!#
!
END MODULE NERS
