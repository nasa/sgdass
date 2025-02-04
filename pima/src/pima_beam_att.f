      FUNCTION   PIMA_BEAM_ATT ( PIM, FREQ, IND_SOU, IND_STA )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_BEAM_ATT computes the coefficients in a range [0, 1]  *
! *   of antenna primary beam attenuation for the off-beam pointing.     *
! *                                                                      *
! * ### 20-SEP-2013  PIMA_BEAM_ATT  v2.0 (c)  L. Petrov  03-APR-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE     ) :: PIM
      REAL*8     PIMA_BEAM_ATT 
      REAL*8     FREQ
      INTEGER*4  IND_SOU, IND_STA
      REAL*8     PIMA__ATT_MIN, ARG_MIN
      PARAMETER  ( PIMA__ATT_MIN = 0.03D0 )
      PARAMETER  ( ARG_MIN = 1.D-5  )
      REAL*8     COEF_ATT_EXP(3)
      REAL*8     ARG
      REAL*8     INTRINSIC, BESSEL_J1
!
! --- These coefficeients were taken for VLA antenna for 8.4 GHz from
! --- http://www.aips.nrao.edu/cgi-bin/ZXHLP2.PL?PBCOR
!
      DATA       COEF_ATT_EXP &
     &           / &
     &            -2.46951D-17, & ! s^2 m^{-2}
     &             2.23575D-34, & ! s^4 m^{-4}
     &            -7.43697D-52  & ! s^6 m^{-6}
     &           /
      REAL*8     DIST
      REAL*8,    EXTERNAL :: ARC_LEN_AD 
!
! --- Get  the distance between pointing direction and apriori coordinates
!
      DIST = ARC_LEN_AD ( PIM%SOU(IND_SOU)%ALPHA_INP, PIM%SOU(IND_SOU)%DELTA_INP, &
     &                    PIM%SOU(IND_SOU)%ALPHA,     PIM%SOU(IND_SOU)%DELTA      )
!
!@      PIMA_BEAM_ATT =   1.0D0                                                    &
!@     &                + COEF_ATT_EXP(1)*(FREQ*PIM%STA(IND_STA)%ANT_DIAM*DIST)**2 &
!@     &                + COEF_ATT_EXP(2)*(FREQ*PIM%STA(IND_STA)%ANT_DIAM*DIST)**4 &
!@     &                + COEF_ATT_EXP(3)*(FREQ*PIM%STA(IND_STA)%ANT_DIAM*DIST)**6 
!@      IF ( PIMA_BEAM_ATT < PIMA__ATT_MIN ) THEN
!@           PIMA_BEAM_ATT = PIMA__ATT_MIN 
!@      END IF
!
      ARG = PI__NUM* FREQ*PIM%STA(IND_STA)%ANT_DIAM*DIST/VTD__C
      IF ( ARG > ARG_MIN ) THEN
           PIMA_BEAM_ATT = ( 2.0D0 * BESSEL_J1( ARG )/ARG )**2
         ELSE
           PIMA_BEAM_ATT = 1.0D0
      END IF
      RETURN
      END  FUNCTION   PIMA_BEAM_ATT  !#!  
