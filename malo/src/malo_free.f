      SUBROUTINE MALO_FREE ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_FREE
! *                                                                      *
! *  ### 12-OCT-2012    MALO_FREE  v1.4 (c)  L. Petrov  17-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i' 
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      INTEGER*4  IER
!
      IF ( ASSOCIATED ( MALO%LAT  ) ) DEALLOCATE ( MALO%LAT )
      IF ( ASSOCIATED ( MALO%LON  ) ) DEALLOCATE ( MALO%LON )
      IF ( ASSOCIATED ( MALO%MODC ) ) DEALLOCATE ( MALO%MODC )
      IF ( ASSOCIATED ( MALO%MJD_ARR ) ) DEALLOCATE ( MALO%MJD_ARR )
      IF ( ASSOCIATED ( MALO%TAI_ARR ) ) DEALLOCATE ( MALO%TAI_ARR )
      IF ( ASSOCIATED ( MALO%SPR  ) ) DEALLOCATE ( MALO%SPR )
      IF ( ASSOCIATED ( MALO%SPH  ) ) DEALLOCATE ( MALO%SPH )
      IF ( ASSOCIATED ( MALO%LSM  ) ) DEALLOCATE ( MALO%LSM )
      IF ( ASSOCIATED ( MALO%LOVE ) ) DEALLOCATE ( MALO%LOVE )
      IF ( ASSOCIATED ( MALO%STA  ) ) DEALLOCATE ( MALO%STA )
      IF ( ASSOCIATED ( MALO%TIM  ) ) DEALLOCATE ( MALO%TIM )
      IF ( ASSOCIATED ( MALO%LEV  ) ) DEALLOCATE ( MALO%LEV )
      IF ( ASSOCIATED ( MALO%PPWTEM_4D) ) DEALLOCATE ( MALO%PPWTEM_4D )
      MALO%SPR_STATUS = MALO__UNDF
      MALO%SPH_STATUS = MALO__UNDF
      MALO%LSM_STATUS = MALO__UNDF
      MALO%STA_STATUS = MALO__UNDF
      MALO%MDC_STATUS = MALO__UNDF
      MALO%PPWTEM_STATUS = MALO__UNDF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_FREE  !#!#
