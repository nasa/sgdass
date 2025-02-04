      SUBROUTINE VTD_GAL_ABR ( VTD, ISTA1, ISTA2, ISOU, TAU_ABR, RATE_ABR, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_GAL_ABR computes galactic aberration -- position       *
! *   offset with respect to the source reference epoch due to           *
! *   acceleration towards the Galactic Center. Galactic aberration      *
! *   is not accounted for galactic objects with known parallaxes.       *
! *                                                                      *
! *   ACC__GAL = (VEL_sun)**2/Gal_dist                                   *
! *                                                                      *
! *   According to  M. Reid et al. (2014), 10.1088/0004-637X/783/2/130,  *
! *                                                                      *
! *   VEL_sun  = 255.2 km/s = 2.552D5 m/s.                               *
! *   Gal_dist = 8.34 kpc = 8.34D3 * 1.49597870700.D11 * 2.06264806D5 =  *
! *              2.573D+20 m.                                            *
! *                                                                      *
! *   ACC__GAL = 2.552D5**2/2.573D+20 = 2.531D-10 m/s**2                 *
! *                                                                      *
! *   ACC_rate = ACC_gal/c/Year_sec/206264.806 = 0.000 005 49 arcsec/yr  *
! *                                                                      *
! *  The following parameter was used in 2007-2024: ACC__GAL = 1.845D-10 *
! *                                                                      *
! *  ### 01-NOV-2007  VTD_GAL_ABR  v1.2 (c)  L. Petrov  21-MAY-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      REAL*8     TAU_ABR, RATE_ABR
      INTEGER*4  ISTA1, ISTA2, ISOU, IUER
!
      REAL*8   S__GAL(3), ACC__GAL
!
! --- Coordinates of galactic centrum 
! --- Declination:    -28_56_00.0
! --- Rigth ascesion:  17_45_36.6
!
      DATA       S__GAL            &
     &         /                   &
     &             -0.054915097D0, &
     &             -0.873458639D0, & 
     &             -0.483791626D0  & 
     &         /
!
! --- Acceleration of the Solar system due to galactic rotation
!
      PARAMETER  ( ACC__GAL = 2.531D-10 ) ! m/s^2
      REAL*8     ABE(3), S_VEC(3), B_CRS(3), V_CRS(3), DELTA_TIM
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IF ( VTD%SOU(ISOU)%PRLX > 1.0D0-20 ) THEN
           TAU_ABR  = 0.0D0
           RATE_ABR = 0.0D0
           CALL ERR_LOG ( 0, IUER )
      END IF
!
      DELTA_TIM = (VTD%MOM%MJD - VTD%SOU(ISOU)%MJD_REF)*86400.0 + &
     &            (VTD%MOM%TAI - VTD%SOU(ISOU)%TAI_REF)
      S_VEC = VTD%SOU(ISOU)%S_CRS
!
! --- Get the baseline vector in CRS
!
      CALL SUB_VV_V ( 3, VTD%STA(ISTA1)%COO_CRS, VTD%STA(ISTA2)%COO_CRS, B_CRS )
      CALL SUB_VV_V ( 3, VTD%STA(ISTA1)%VEL_CRS, VTD%STA(ISTA2)%VEL_CRS, V_CRS )
!
! --- Compute the normalized aberration vector. It differs by
! --- the constant of the galactic acceleration
!
      CALL ADDC_VV ( 3, DELTA_TIM/VTD__C, S__GAL, &
     &                 -DP_VV_V(3, S__GAL, S_VEC)*DELTA_TIM/VTD__C, S_VEC, &
     &                  ABE )
      TAU_ABR  = ACC__GAL * DP_VV_V ( 3, ABE, B_CRS )/VTD__C
      RATE_ABR = ACC__GAL * DP_VV_V ( 3, ABE, V_CRS )/VTD__C
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_GAL_ABR !#!#
