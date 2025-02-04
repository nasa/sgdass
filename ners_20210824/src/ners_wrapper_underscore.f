      SUBROUTINE NERS_INIT_ ( CONFIG_FILE, NERS, TIME_TAI_START, TIME_TAI_STOP, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      CHARACTER  CONFIG_FILE*(*)
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      REAL*8     TIME_TAI_START, TIME_TAI_STOP
      INTEGER*4  IUER
      CALL NERS_INIT ( CONFIG_FILE, NERS, TIME_TAI_START, TIME_TAI_STOP, IUER )
      RETURN
      END  SUBROUTINE NERS_INIT_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_VERSION_ ( INQ, ANS )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      CHARACTER  INQ, ANS
      CALL NERS_VERSION ( INQ, ANS )
      RETURN
      END  SUBROUTINE NERS_VERSION_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_LOAD_ ( NERS, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER
      CALL NERS_LOAD ( NERS, IUER )
      RETURN
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
      CALL NERS_GET_EOP ( NERS, TIME_TAI, CPAR, L_PAR, PARS, IUER )
      RETURN
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
      CALL NERS_GET_SERIES ( NERS, TIME_TAI_START, TIME_TAI_END, TIME_STEP, &
     &                       CPAR, M_PAR, M_SER, NS, TIM, SER, IUER )
      RETURN
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
      CALL NERS_INQ ( NERS, REQ, M_PAR, L_PAR, PARS, IUER )
      RETURN
      END  SUBROUTINE NERS_INQ_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_AZELHA_COMP_ ( NERS, TIM_TAI, COO_TRS, RA, DEC, &
     &                               REFR_MODE, AZ, EL, HA, &
     &                               AZ_RATE, EL_RATE, HA_RATE, IUER )
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      REAL*8     TIM_TAI, COO_TRS(3), RA, DEC, AZ, EL, HA, &
     &           AZ_RATE, EL_RATE, HA_RATE
      CHARACTER  REFR_MODE*(*)
      INTEGER*4  IUER 
      TYPE     ( NERS__TYPE ) :: NERS
      CALL NERS_AZELHA_COMP ( NERS, TIM_TAI, COO_TRS, RA, DEC, &
     &                        REFR_MODE, AZ, EL, HA, &
     &                        AZ_RATE, EL_RATE, HA_RATE, IUER )
      RETURN
      END  SUBROUTINE NERS_AZELHA_COMP_  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_GET_UTCMTAI_ ( NERS, UTC, UTC_M_TAI, IUER )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  IUER 
      REAL*8     UTC, UTC_M_TAI
      CALL NERS_GET_UTCMTAI ( NERS, UTC, UTC_M_TAI, IUER )
      RETURN
      END  SUBROUTINE NERS_GET_UTCMTAI_ !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NERS_QUIT_ ( CODE, NERS )
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__FCS_TYPE ) :: NERS
      INTEGER*4  CODE 
      CALL NERS_QUIT ( CODE, NERS )
      RETURN
      END  SUBROUTINE NERS_QUIT_  !#!#  
!
! ------------------------------------------------------------------------
!
      FUNCTION   TIM_TO_DATE_ ( TIM, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      REAL*8     TIM
      CHARACTER  TIM_TO_DATE_*23
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
      INTEGER*4  IUER
      TIM_TO_DATE_ = TIM_TO_DATE ( TIM, IUER )
      RETURN 
      END  FUNCTION  TIM_TO_DATE_  !#!  
