      SUBROUTINE SPD_LOAD_BSPD_ ( L_DIR, C_DIR, SPD_BIAS_FILE, L_STA, C_STA, &
     &                            MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                            SPD, IUER )
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      INTEGER*4  L_DIR, L_STA, MJD_BEG, MJD_END, IUER
      CHARACTER  C_STA(L_STA)*(*), C_DIR(L_DIR)*(*), SPD_BIAS_FILE*(*)
      REAL*8     TAI_BEG, TAI_END
      CALL SPD_LOAD_BSPD ( L_DIR, C_DIR, SPD_BIAS_FILE, L_STA, C_STA, &
     &                     MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                     SPD, IUER )
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_INTRP_DELAY_ ( STA_NAM, L_STA, SPD, ELEV, AZIM, &
     &                              ELEV_RATE, AZIM_RATE, MJD_OBS, TAI_OBS, &
     &                              SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                              SPD_RATE_DER_ZEN, IUER )
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  L_STA, MJD_OBS, IUER
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      CHARACTER  STA_NAM*(*)
      CHARACTER  STR*128, STR1*128, STR2*128
      REAL*8     ELEV, AZIM, ELEV_RATE, AZIM_RATE, TAI_OBS, SPD_DELAY, &
     &           SPD_RATE, SPD_DELAY_DER_ZEN, SPD_RATE_DER_ZEN
      CALL SPD_INTRP_DELAY ( STA_NAM, L_STA, SPD, ELEV, AZIM, &
     &                       ELEV_RATE, AZIM_RATE, MJD_OBS, TAI_OBS, &
     &                       SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                       SPD_RATE_DER_ZEN, IUER )
      RETURN
      END  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPD_INIT_ ( SPD, IUER )
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  IUER
      CALL SPD_INIT ( SPD, IUER )
      RETURN
      END  SUBROUTINE SPD_INIT_  !#!  
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
