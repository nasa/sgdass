      SUBROUTINE SPD_GAUSSIAN_LAYER_MF ( HL, WL, EL_MIN, EL_MAX, M_DEG, &
     &                                   LN, EL_ARG_ARR, MF_BSPL_ARR,   &
     &                                   MF_ARG_ARR, EL_BSPL_ARR, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_GAUSSIAN_LAYER_MF computes the B-spline coefficients   *
! *   of the mapping function for a case when the atmosphere density is  *
! *   considered as a Gaussian layer at height HL with full width half   *
! *   maximum ( FWHM ) WL. The mapping function is defined in a range of *
! *   elevations [EL_MIN, EL_MAX]. The mapping function is computed      *
! *   under assumptions:                                                 *
! *     1) the Earth is spherical;                                       *
! *     2) the residual Earth's atmosphere is uniform over longitude     *
! *        and latitude.                                                 *
! *                                                                      *
! *   NB: arrays MF_BSPL_ARR and EL_BSPL_ARR have non-standard dimension.*
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     HL ( REAL*8    ) -- The height of the layer center.              *
! *                         Units: meters.                               *
! *     WL ( REAL*8    ) -- The full width half maximum layer thickness. *
! *                         Units: meters.                               *
! * EL_MIN ( REAL*8    ) -- Minimum elevation angle. Units: rad.         *
! * EL_MAX ( REAL*8    ) -- Maximum elevation angle. Units: rad.         *
! *  M_DEG ( INTEGER*4 ) -- Degree of the B-spline expansion.            *
! *                         Recommended value: 3.                        *
! *     LN ( INTEGER*4 ) -- The number of knots of B-spline expansion    *
! *                         of the mapping function.                     *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * EL_ARG_ARR ( REAL*8    ) -- Array of elevations as arguments for     *
! *                             mapping function. Dimension: LN.         *
! *                             Units: rad.                              *
! * MF_BSPL_ARR ( REAL*8   ) -- Array of B-spline coefficients of        *
! *                             mapping function expansion over the      *
! *                             B-spline basis with argument EL_ARG_ARR. *
! *                             Dimension: [1-M_DEG:LN].                 *
! *                             Units: dimensionless.                    *
! * MF_ARG_ARR ( REAL*8    ) -- Array of values of mapping function as   *
! *                             arguments for the inverse mapping        *
! *                             mapping function. Dimension: LN.         *
! *                             Units: dimensionless.                    *
! * EL_BSPL_ARR ( REAL*8   ) -- Array of B-spline coefficients of        *
! *                             the inverse mapping function expansion   *
! *                             over the  B-spline basis with argument   *
! *                             M_ARG_ARR. Dimension: [1-M_DEG:LN].      *
! *                             Units: rad.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * # 16-AUG-2014 SPD_GAUSSIAN_LAYER_MF v1.0 (c) L. Petrov 16-AUG-2014 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_DEG, LN, IUER 
      INCLUDE   'astro_constants.i'
      REAL*8     HL, WL
      REAL*8     EL_MIN, EL_MAX, EL_ARG_ARR(LN), MF_BSPL_ARR(1-M_DEG:LN), &
     &           MF_ARG_ARR(LN), EL_BSPL_ARR(1-M_DEG:LN)
      REAL*8     MAP_MIN, MAP_MAX, MAP_VAL, EL_VAL
      INTEGER*4  MP
      PARAMETER  ( MP = 256 )
      REAL*8     REA, EPS
      PARAMETER  ( REA = 6378137.0D0 )
      PARAMETER  ( EPS = 1.D-5       )
      REAL*8     H_MIN, H_MAX, X_MIN, X_MAX, Y_MIN, Y_MAX, &
     &           H, XL, YL, WF, ZEN_INT 
      REAL*8     X(MP), Y(MP), FU(1-M_DEG:MP), FD(1-M_DEG:MP)
      INTEGER*4  J1, J2, J3, IND, IER
      REAL*8,    EXTERNAL :: SPL_INT
!
! --- The length of the integration interval selected in such a way,
! --- the function in exponent is less by module than EPS
!
      WF = DSQRT ( -DLOG(EPS)/(4.0D0*DLOG(2.0D0)) )
!
! --- [H_MIN, H_MAX] -- integration interval
! --- [X_MIN, X_MAX] -- the same, but in different units (scaled by 1/REA)
!
      H_MIN = MAX ( 0.0D0, HL - WL*WF )
      H_MAX =              HL + WL*WF
      IF ( H_MIN < 0.0 ) H_MIN = 0.0D0
      X_MIN = H_MIN/REA
      X_MAX = H_MAX/REA
      XL    = HL/REA
!
! --- Compute the an expression under the integral for the density twoards vertical direction
!
      DO 410 J1=1,MP
         H      = H_MIN + (J1-1)*(H_MAX - H_MIN)/(MP-1)
         X(J1)  = H/REA
         FD(J1) = DEXP ( -(REA/WL)**2*4.0D0*DLOG(2.0D0) * (X(J1) - XL)**2 ) 
 410  CONTINUE 
!
! --- Expand that expression over B-spline basis of the M_DEG th degree
!
      CALL ERR_PASS ( IUER, IER )
      CALL BSPL_1D_CMP ( M_DEG, 0, MP, X, FD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2421, IUER, 'SPD_GAUSSIAN_LAYER_MF', 'Error in '// &
     &         'an attempt to expand mapping function over the '// &
     &         'B-spline basis' )
           RETURN 
      END IF
!
! --- Compute the integral of density over the zenith direction
!
      ZEN_INT = SPL_INT ( MP, X, M_DEG, FD, X(MP) )
!
      DO 420 J2=1,LN
         IF ( J2 == 1 ) THEN
              EL_VAL = EL_MIN
            ELSE IF ( J2 == LN ) THEN
              EL_VAL = EL_MAX
            ELSE 
!
! ----------- We use Chebyshev alternance for defining the knot sequence
!
              EL_VAL = EL_MAX - (1.0D0 + DCOS( (P2I*(2*J2-3))/(LN-2) ))*(EL_MAX - EL_MIN)/2.0D0
         END IF
!
! ------ The interval of integration along slant direction at elevation EL_VAL.
! ------ Units are scaled by 1/REA factor
!
         Y_MIN = DSQRT ( DSIN(EL_VAL)**2 + 2.D0*X_MIN + X_MIN**2) - DSIN(EL_VAL)
         Y_MAX = DSQRT ( DSIN(EL_VAL)**2 + 2.D0*X_MAX + X_MAX**2) - DSIN(EL_VAL)
!
! ------ Compute the function under integral
!
         DO 430 J3=1,MP
            Y(J3) = Y_MIN + (J3-1)*(Y_MAX - Y_MIN)/(MP-1)
            FU(J3) = DEXP ( -(REA/WL)**2*4.0D0*DLOG(2.0D0)* &
     &                       ( DSQRT(1.D0 + 2.0D0*DSIN(EL_VAL)*Y(J3) &
     &                               + Y(J3)**2) - (1.0D0 + XL) )**2 &
     &                    )
 430     CONTINUE 
!
! ------ Expand that function over the B-spline basis of the M_DEG th degree
!
         CALL ERR_PASS ( IUER, IER )
         CALL BSPL_1D_CMP ( M_DEG, 0, MP, Y, FU, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2422, IUER, 'SPD_GAUSSIAN_LAYER_MF', 'Error '// &
     &            'in an attempt to expand mapping function over the '// &
     &            'B-spline basis' )
              RETURN 
         END IF
!
! ------ ... and finall ycompute the mapping function by dividing 
! ------ density integral along the slant direction to the density
! ------ integral along the zenith direction
!
         MF_BSPL_ARR(J2) = SPL_INT ( MP, Y, M_DEG, FU, Y(MP) )/ ZEN_INT
         EL_ARG_ARR(J2)  = EL_VAL
!
         MF_ARG_ARR(LN+1-J2)  = MF_BSPL_ARR(J2) 
         EL_BSPL_ARR(LN+1-J2) = EL_VAL
 420  CONTINUE 
!
! --- Expand mapping function into B-spline basis
!
      CALL ERR_PASS    ( IUER, IER )
      CALL BSPL_1D_CMP ( M_DEG, 0, LN, EL_ARG_ARR, MF_BSPL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2423, IUER, 'SPD_GAUSSIAN_LAYER_MF', 'Error '// &
     &         'in an attempt to expand mapping function over the '// &
     &         'B-spline basis' )
           RETURN 
      END IF
!
! --- Expand inverse mapping function into B-spline basis
!
      CALL ERR_PASS    ( IUER, IER )
      CALL BSPL_1D_CMP ( M_DEG, 0, LN, MF_ARG_ARR, EL_BSPL_ARR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2424, IUER, 'SPD_GAUSSIAN_LAYER_MF', 'Error '// &
     &         'in an attempt to expand an inverse of the mapping '// &
     &         'function over the B-spline basis' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_GAUSSIAN_LAYER_MF  !#!#
