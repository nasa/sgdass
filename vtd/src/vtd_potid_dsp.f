      SUBROUTINE VTD_POTID_DSP ( VTD, ISTA, PTD_REN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_POTID_DSP computes the vector of site displacements    *
! *   caused by pole tide. In order to comply with recommendations of    *
! *   IERS Conventions 1996 and 2003 an option of specifying the secular *
! *   model of polar motion is supported. It should be noted that        *
! *   correct model of secular pole motion for computation displacements *
! *   caused by pole tide is zero. Non-zero model advocated by Harald    *
! *   Schuh and published in IERS Conventions IS ERRONEOUS!!             *
! *                                                                      *
! *   Love numbers for pole tide are taken according to the specified    *
! *   model for the annual frequency.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    ISTA ( INTEGER*4 ) -- Index of the station in the internal list   *
! *                          of stations stored in the VTD object.       *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * PTD_REN ( REAL*8    ) -- Vector of site displacement caused by       *
! *                          pole tide in local topocentric reference    *
! *                          system: Up, East, North. Units: meters.     *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 29-JAN-2004   VTD_POTID_DSP  v2.0 (c) L. Petrov 26-FEB-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  ISTA, IUER
      REAL*8     PTD_REN(3)
      REAL*8     ANN_FREQ
      PARAMETER  ( ANN_FREQ = PI2/(86400.0D0*365.25D0) )
      REAL*8     LOVE_VEC(9), LOVE_H2, LOVE_L2, RN_TRS(3), RD, TIM_YEAR
      REAL*8     XPL_USE, YPL_USE, XMOD_POL, YMOD_POL, TAI_SEC
      INTEGER*4  IER
!
      IF ( VTD%STA(ISTA)%RAD < 0.9*VTD__REA ) THEN
!
! -------- Do not need to copmute pole tide for the geocenter or the orbiting station
!
           PTD_REN = 0.0D0
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Get Love numbers for the annual frequency
!
      CALL ERR_PASS ( IUER, IER )
      CALL SOTID_GET_LOVE ( VTD%TIDCNF_PTD, 2, 0, ANN_FREQ, LOVE_VEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2421, IUER, 'VTD_POTID_DSP', 'Error in an '// &
     &         'attempt to compute Love number for annual frequency. '// &
     &         'no love, alas!' )
           RETURN 
      END IF
      LOVE_H2 = LOVE_VEC(1)
      LOVE_L2 = LOVE_VEC(5)
!
! --- Compute normalized vector of site position in TRS: RN_TRS
!
      CALL COPY_R8  ( 3, VTD%STA(ISTA)%BEG_TRS, RN_TRS )
      CALL NORM_VEC ( 3, RN_TRS, RD )
!
! --- Compute "effective pole coordinates" by subtracting a linear model
! --- of secular polar motion
! --- (Strictly speaking it is wrong. Zero values for mean pole and pole
! ---  drift are recommended)
!
      TAI_SEC = (VTD%MOM%MJD - J2000__MJD)*86400.0D0 - 43200.0D0 + VTD%MOM%TAI
      TIM_YEAR = 365.25D0*86400.0D0
      IF ( VTD%CONF%MEAN_POLE_MODEL == VTD__NONE ) THEN
           XMOD_POL = 0.0D0
           YMOD_POL = 0.0D0
         ELSE IF ( VTD%CONF%MEAN_POLE_MODEL == VTD__MPL_IERS2010 ) THEN
           IF ( VTD%MOM%MJD .GE. VTD__REF_MJD_2010 ) THEN
                XMOD_POL =   VTD__XMPOL_2010B(0)*ARCSEC__TO__RAD                        &
     &                     + VTD__XMPOL_2010B(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR
                YMOD_POL =   VTD__YMPOL_2010B(0)*ARCSEC__TO__RAD                        &
     &                     + VTD__YMPOL_2010B(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR
             ELSE
                XMOD_POL =   VTD__XMPOL_2010A(0)*ARCSEC__TO__RAD                        &
     &                     + VTD__XMPOL_2010A(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR       &
     &                     + VTD__XMPOL_2010A(2)*ARCSEC__TO__RAD*(TAI_SEC/TIM_YEAR)**2  &
     &                     + VTD__XMPOL_2010A(3)*ARCSEC__TO__RAD*(TAI_SEC/TIM_YEAR)**3
                YMOD_POL =   VTD__YMPOL_2010A(0)*ARCSEC__TO__RAD                        &
     &                     + VTD__YMPOL_2010A(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR       &
     &                     + VTD__YMPOL_2010A(2)*ARCSEC__TO__RAD*(TAI_SEC/TIM_YEAR)**2  &
     &                     + VTD__YMPOL_2010A(3)*ARCSEC__TO__RAD*(TAI_SEC/TIM_YEAR)**3
           END IF
         ELSE IF ( VTD%CONF%MEAN_POLE_MODEL == VTD__MPL_IERS2022 ) THEN
                XMOD_POL =   VTD__XMPOL_2022(0)*ARCSEC__TO__RAD                     &
     &                     + VTD__XMPOL_2022(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR
                YMOD_POL =   VTD__XMPOL_2022(0)*ARCSEC__TO__RAD                     &
     &                     + VTD__XMPOL_2022(1)*ARCSEC__TO__RAD*TAI_SEC/TIM_YEAR
      END IF
      XPL_USE = VTD%MOM%XPL - XMOD_POL
      YPL_USE = VTD%MOM%YPL - YMOD_POL
!
! --- Compute displacement caused by pole tide in REN system
!
      IF ( VTD%STA(ISTA)%GAC_ELL < 8.0D0 ) THEN
           WRITE ( 6, 110 ) VTD%STA(ISTA)%IVS_NAME, VTD%STA(ISTA)%COO_TRS(1:3,1), &
     &                      VTD%STA(ISTA)%GAC_ELL
 110       FORMAT ( 'IVS_name: ',A, ' Coo: ', 3(F13.4, 1X), ' Grav.acc: ', 1PD12.5 )
           CALL ERR_LOG ( 2421, IUER, 'VTD_POTID_DSP', 'Wrong gravity acceleration '// &
     &         'for station '//VTD%STA(ISTA)%IVS_NAME//' please check its '// &
     &         'a priori coordinates' )
           RETURN
      END IF
      PTD_REN(1) = -OM__EAR**2*VTD%STA(ISTA)%RAD**2/VTD%STA(ISTA)%GAC_ELL* &
     &             LOVE_H2*RN_TRS(3)* &
     &             (RN_TRS(1)*XPL_USE - RN_TRS(2)*YPL_USE)
      PTD_REN(2) =  OM__EAR**2*VTD%STA(ISTA)%RAD**2/VTD%STA(ISTA)%GAC_ELL* &
     &             LOVE_L2*RN_TRS(3)/DSQRT(1.0D0 - RN_TRS(3)**2)* &
     &             (RN_TRS(1)*YPL_USE + RN_TRS(2)*XPL_USE)
      PTD_REN(3) = -OM__EAR**2*VTD%STA(ISTA)%RAD**2/VTD%STA(ISTA)%GAC_ELL* &
     &             LOVE_L2*(1.0D0 - 2.0D0*RN_TRS(3)**2)/ &
     &                      DSQRT(1.0D0 - RN_TRS(3)**2)* &
     &             (RN_TRS(1)*XPL_USE - RN_TRS(2)*YPL_USE)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_POTID_DSP  !#!#
