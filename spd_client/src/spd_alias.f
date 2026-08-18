      SUBROUTINE GET_OPA_TATM_SPECTRUM_ ( PAR_STR, SPD_DEL, MJD, TAI, EL, AZ, &
     &                                    N_FRQ, FRQ_ARR, VAL_ARR, IUER )
      IMPLICIT   NONE 
      CHARACTER  PAR_STR*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  MJD, N_FRQ, IUER
      REAL*8     TAI, EL, AZ, FRQ_ARR(N_FRQ), VAL_ARR(N_FRQ)
      CALL GET_OPA_TATM_SPECTRUM ( PAR_STR, SPD_DEL, MJD, TAI, EL, AZ, &
     &                                   N_FRQ, FRQ_ARR, VAL_ARR, IUER )
      RETURN
      END  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_OPA_TATM_TIPPING_ ( PAR_STR, SPD_DEL, MJD, TAI, FREQ, AZ, &
     &                                   N_EL, EL_ARR, VAL_ARR, IUER )
      IMPLICIT   NONE 
      CHARACTER  PAR_STR*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  MJD, N_EL, IUER
      REAL*8     TAI, FREQ, AZ, EL_ARR(N_EL), VAL_ARR(N_EL)
      CALL GET_OPA_TATM_TIPPING ( PAR_STR, SPD_DEL, MJD, TAI, FREQ, AZ, &
     &                                  N_EL, EL_ARR, VAL_ARR, IUER )
      RETURN
      END  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_INIT_ ( SPD_CLI_CONF, SPD_CLI, IUER )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  SPD_CLI_CONF*(*)
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      INTEGER*4  IUER
      CALL SPD_CLI_INIT ( SPD_CLI_CONF, SPD_CLI, IUER )
      RETURN
      END  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_GET_2PD_ ( SPD_CLI, N_DEL, SPD_2P, IVRB, IUER )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      CHARACTER  SPD_CLI_CONF*128
      INTEGER*4  N_DEL, IVRB, IUER
      TYPE     ( SPD_CLI__TYPE  ) :: SPD_CLI
      TYPE     ( SPD_2P__TYPE   ) :: SPD_2P(N_DEL)
      CALL SPD_CLI_GET_2PD ( SPD_CLI, N_DEL, SPD_2P, IVRB, IUER )
      RETURN
      END  !#!#  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLI_QUIT_ ( SPD_CLI )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_CLI__TYPE ) :: SPD_CLI
      CALL SPD_CLI_QUIT ( SPD_CLI )
      RETURN
      END  !#!# 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_CLIENT_VERSION_ ( INQ, ANS )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER  INQ*(*), ANS*(*)
      CALL SPD_CLIENT_VERSION ( INQ, ANS )
      RETURN
      END  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_DEL_INIT_ ( SPD_DEL, IUER )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  IUER
      CALL SPD_DEL_INIT ( SPD_DEL, IUER )
      RETURN
      END  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_DEL_QUIT_ ( SPD_DEL, IUER )
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  IUER
      CALL SPD_DEL_QUIT ( SPD_DEL, IUER )
      RETURN
      END  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_LOAD_MF_ ( L_STA, SPD, LAYER_HEIGHT, LAYER_FWHM, IUER )
      INCLUDE   'spd.i'
      INTEGER*4  L_STA, IUER 
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      REAL*8     LAYER_HEIGHT, LAYER_FWHM
      CALL SPD_LOAD_MF ( L_STA, SPD, LAYER_HEIGHT, LAYER_FWHM, IUER )
      RETURN
      END  SUBROUTINE SPD_LOAD_MF_ !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_LOAD_BSPD_ ( L_DIR, C_DIR, SPD_BIAS_FILE, L_STA, C_STA, &
     &                            MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                            SPD_DEL, IUER )
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL(L_STA)
      INTEGER*4  L_DIR, L_STA, MJD_BEG, MJD_END, IUER
      CHARACTER  C_STA(L_STA)*(*), C_DIR(L_DIR)*(*), SPD_BIAS_FILE*(*)
      REAL*8     TAI_BEG, TAI_END
      CALL SPD_LOAD_BSPD ( L_DIR, C_DIR, SPD_BIAS_FILE, L_STA, C_STA, &
     &                     MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                     SPD_DEL, IUER )
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_INTRP_DELAY_ ( STA_NAM, L_STA, SPD_DEL, ELEV, AZIM, &
     &                              ELEV_RATE, AZIM_RATE, MJD_OBS, TAI_OBS, &
     &                              SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                              SPD_RATE_DER_ZEN, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  L_STA, MJD_OBS, IUER
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL(L_STA)
      CHARACTER  STA_NAM*(*)
      CHARACTER  STR*128, STR1*128, STR2*128
      REAL*8     ELEV, AZIM, ELEV_RATE, AZIM_RATE, TAI_OBS, SPD_DELAY, &
     &           SPD_RATE, SPD_DELAY_DER_ZEN, SPD_RATE_DER_ZEN
      CALL SPD_INTRP_DELAY ( STA_NAM, L_STA, SPD_DEL, ELEV, AZIM, &
     &                       ELEV_RATE, AZIM_RATE, MJD_OBS, TAI_OBS, &
     &                       SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                       SPD_RATE_DER_ZEN, IUER )
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_RES_INTRP_ ( SPD_DIR, N_STA, C_STA, MJD_BEG, TAI_BEG, &
     &                            MJD_END, TAI_END, SPD_DEL, MODE_STR, IVRB, &
     &                            IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  N_STA, MJD_BEG, MJD_END, IVRB, IUER 
      CHARACTER  SPD_DIR*(*), C_STA(N_STA)*(*), MODE_STR*(*)
      TYPE       ( SPD_DEL__TYPE ) :: SPD_DEL(N_STA)
      REAL*8     TAI_BEG, TAI_END
      CALL SPD_RES_INTRP ( SPD_DIR, N_STA, C_STA, MJD_BEG, TAI_BEG, &
     &                     MJD_END, TAI_END, SPD_DEL, MODE_STR, IVRB, &
     &                     IUER )
      RETURN
      END  !#!  
