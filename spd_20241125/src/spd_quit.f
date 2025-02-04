      SUBROUTINE SPD_4D_QUIT ( SPD_4D )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_4D_QUIT 
! *                                                                      *
! * ### 24-JAN-2015    SPD_4D_QUIT    v1.0 (c) L. Petrov 24-JAN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_4D__TYPE ) :: SPD_4D
      CHARACTER  STR*128
      INTEGER*4  NLON, NLAT, IUER
      INTEGER*4  IER
!
      IF ( ASSOCIATED ( SPD_4D%LON ) ) DEALLOCATE ( SPD_4D%LON  )
      IF ( ASSOCIATED ( SPD_4D%LAT ) ) DEALLOCATE ( SPD_4D%LAT  )
      IF ( ASSOCIATED ( SPD_4D%LEV ) ) DEALLOCATE ( SPD_4D%LEV  )
      IF ( ASSOCIATED ( SPD_4D%TIM ) ) DEALLOCATE ( SPD_4D%TIM  )
      IF ( ASSOCIATED ( SPD_4D%REFR) ) DEALLOCATE ( SPD_4D%REFR )
      SPD_4D%DIMS   = 0
      SPD_4D%STATUS = SPD__UNDF
!
      RETURN
      END  SUBROUTINE   SPD_4D_QUIT  !#!#
