      SUBROUTINE SPD_3D_ZEN ( NH, XI, RFR_VAL, SPL_RFR_VAL, DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_3D_ZEN  computes path delays throuth the neutral      *
! *   atmosphere in zenith direction by integrating the refractivity     *
! *   field along the straight path. The refractivity profile is         *
! *   represented thtough coefficients of epxansion over the B-spline    *
! *   basis.                                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *          NH ( INTEGER*4  ) -- The number of knots of the B-spline    *
! *                               expansion.                             *
! *          XI ( INTEGER*4  ) -- The array of the geometric heights     *
! *                               above the ground of B-spline knots.    *
! *                               Dimension: NH. Units: meters.          *
! *     RFR_VAL ( REAL*8     ) -- The array of the refractivity minus 1  *
! *                               as a function of XI. Dimension: NH.    *
! *                               Units: dimensionless. NB: One should   *
! *                               be added to REFR_VAL in order to get   *
! *                               the true refractivity.                 *
! * SPL_RFR_VAL ( REAL*8     ) -- The array of the coefficients of the   *
! *                               B-spline of the of the 3rd order that  *
! *                               interpolates RFR_VAL.                  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *        DEL  ( REAL*8     ) -- Excess path delay through the          *
! *                               atmosphere relative to the the case    *
! *                               if the wave between the emitter and    *
! *                               received were propagated in vacuum.    *
! *                               Units: seconds.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *                                                                      *
! *  ### 29-NOV-2008   SPD_3D_ZEN   v1.0 (c) L. Petrov  30-NOV-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'astro_constants.i'
      INTEGER*4  NH, IUER
      REAL*8     XI(NH), RFR_VAL(NH), SPL_RFR_VAL(NH), DEL
      REAL*8       VTD__C
      PARAMETER  ( VTD__C   = 299792458.0D0 )
      REAL*8,    EXTERNAL :: ISPL8 
      INTEGER*4  J1, IER
!
! --- Compute zenith path delay by integrating along the strait line
!
      CALL ERR_PASS ( IUER, IER )
      DEL = ISPL8 ( XI(NH), NH, XI, RFR_VAL, 1, NH, SPL_RFR_VAL, IER )/VTD__C
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5521, IUER, 'SPD_3D_ZEN', 'Error in an '// &
     &         'attempt to compute the integral of '// &
     &         'path delay in zenith direction using '// &
     &         'coefficients of B-spline' )
           RETURN 
      END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_ZEN  !#!  
