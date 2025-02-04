      FUNCTION   VTD_CORR_AMP ( BX, BY, FREQ, MAP, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  VTD_CORR_AMP  computes the amplitude of the              *
! *   interferometric visibility function at a given baseline for        *
! *   a source with a given map in the form of a set of delta functions. *
! *   The coordinates of the projection of the baseline to X-direction   *
! *   ad Y-direction are given. The X-direction is determined as a unit  *
! *   vector from the reference point of the map to the norther          *
! *   celestial pole. Y-direction is defined as a unit vector which is   *
! *   a vector product of the unit vector to the source direction and    *
! *   the X-direction vector. X and Y directions are the unit vectors    *
! *   which are in the plane tangential to the source direction.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *                                                                      *
! *   BX   ( REAL*8     ) -- Projection of the baseline vector to the    *
! *                          X-direction. Units: meters.                 *
! *   BY   ( REAL*8     ) -- Projection of the baseline vector to the    *
! *                          Y-direction. Units: meters.                 *
! *   FREQ ( REAL*8     ) -- Observing frequency in Hz.                  *
! *   MAP  ( SOUMAP__TYPE ) -- Derived object with information about     *
! *                            image.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  <VTD_CORR_AMP>  ( REAL*8     ) -- Amplitude of the interferometric  *
! *                                    visibility function.              *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 19-MAR-2007   VTD_CORR_AMP   v1.0 (c) L. Petrov  19-MAR-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      REAL*8     BX, BY, FREQ
      TYPE     ( SOUMAP__TYPE ) :: MAP
      REAL*8     VTD_CORR_AMP
      INTEGER*4  IUER
      REAL*8     DRE, DIM, XI, GS, BP, VTD__C
      REAL*8     FLUX_MIN_SHARE
!@      PARAMETER  ( FLUX_MIN_SHARE = 0.05D0 )
      PARAMETER  ( FLUX_MIN_SHARE = 0.00005D0 )
      PARAMETER  ( VTD__C = 299792458.0D0 )
      INTEGER*4  J1
!
      VTD_CORR_AMP = 0.0D0
      IF ( MAP%STATUS_CC .NE. SMP__LOAD ) THEN
           CALL ERR_LOG ( 2641, IUER, 'VTD_CORR_AMP', 'Trap of internal '// &
     &         'control: clean components were not loaded for image '// &
     &          MAP%FINAM )
           RETURN 
      END IF
!
      DRE = 0.0D0
      DIM = 0.0D0
!
!!      do 410 j1=6,map%num_cc
      DO 410 J1=1,MAP%NUM_CC
         BP = ( BX*MAP%COOR_CC(1,J1) + BY*MAP%COOR_CC(2,J1) )
         GS = 1.0D0
         DRE = DRE + GS* MAP%FLUX_CC(J1) * DCOS ( PI2*FREQ/VTD__C * BP )
         DIM = DIM + GS* MAP%FLUX_CC(J1) * DSIN ( PI2*FREQ/VTD__C * BP )
 410  CONTINUE 
      VTD_CORR_AMP = DSQRT  ( DRE**2 + DIM**2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   VTD_CORR_AMP  !#!#
