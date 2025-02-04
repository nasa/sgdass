      FUNCTION   PIMA_MEAN_FREQ ( PIM, IND_OBS )
! ************************************************************************
! *                                                                      *
! *   Function PIMA_MEAN_FREQ returns weighted mean frequency for a      *
! *   given observation.                                                 *
! *                                                                      *
! *  ### 02-APR-2020  PIMA_MEAN_FREQ  v1.0 (c) L. Petrov 02-APR-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      REAL*8     PIMA_MEAN_FREQ
      INTEGER*4  IND_OBS
      REAL*8     WW_ACC
      INTEGER*1  MASK_CHN
      INTEGER*4  J1, J2
!
      PIMA_MEAN_FREQ = 0.0D0
      WW_ACC         = 0.0D0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         DO 420 J2=1,PIM%NCHN
            IF ( PIM%BANDPASS_MASK_STYLE .NE. PIMA__NO ) THEN
!
! -------------- Apply bandpass mask for computing WEI_PNT
!
                 MASK_CHN = PIM%BANDPASS_MASK(J2,J1,PIM%OBS(IND_OBS)%STA_IND(1),PIMA__MASK_FRNG) * &
     &                      PIM%BANDPASS_MASK(J2,J1,PIM%OBS(IND_OBS)%STA_IND(2),PIMA__MASK_FRNG)
               ELSE
                 MASK_CHN = 1
            END IF 
            PIMA_MEAN_FREQ = PIMA_MEAN_FREQ + MASK_CHN*PIM%FREQ_ARR(J2,J1,PIM%CONF%FRQ_GRP)
            WW_ACC = WW_ACC + MASK_CHN
 420     CONTINUE 
 410  CONTINUE 
      IF ( WW_ACC > 0 ) THEN
           PIMA_MEAN_FREQ = PIMA_MEAN_FREQ/WW_ACC
         ELSE
           PIMA_MEAN_FREQ = PIM%FREQ_ARR(J2,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)
      END IF
!
      RETURN
      END  FUNCTION   PIMA_MEAN_FREQ   !#!  
