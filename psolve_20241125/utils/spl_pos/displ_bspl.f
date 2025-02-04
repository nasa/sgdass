      SUBROUTINE DISPL_BSPL ( FL_SPLINE_ONLY, L_NOD, NOD_ARR, L_DEG, ARG_REF, &
     &                        VEL_SCL, EST_VEC, COV_MAT, ARG, VAL, SIG )
! ************************************************************************
! *                                                                      *
! *   Routine DISPL_BSPL computes site displament and its standard       *
! *   deviation for the moment of time ARG modeled with the set of       *
! *   coefficients of expansion with the B-spline basis, global          *
! *   displacement and global velocity.                                  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FL_SPLINE_ONLY ( LOGICAL*4 ) -- If .FALSE. the site evolution model  *
! *                                 will consists of both spline,        *
! *                                 global position and velocity.        *
! *                                 If .TRUE., the site evolution model  *
! *                                 will consist of only spline.         *
! *   L_NOD ( INTEGER*4 ) -- The total number of knots.                  *
! * NOD_ARR ( REAL*8    ) -- The array of knot time epochs in seconds    *
! *                          from J2000.0 epoch. Dimension:              *
! *                          1-M_SPD:L_NOD.                              *       
! *   L_DEG ( INTEGER*4 ) -- The degree of B-spline basis.               *
! * ARG_REF ( REAL*8    ) -- The reference epoch for gloval site         *
! *                          position in seconds from J2000.0            *
! * VEL_SCL ( REAL*8    ) -- The scaling factor which has to be applied  *
! *                          to estimates of global site velocity.       *
! * EST_VEC ( REAL*8    ) -- The vector of estimates.                    *
! *                          Dimension: 1-M__SPD:L_NOD+1,3.              *
! *                          Meaning: B-spline coefficients occupy       *
! *                          section 1-L_DEG:L_NOD-1, L_NOD occupies     *
! *                          global site adjustments; L_NOD+1 occupies   *
! *                          global velocities.                          *
! * COV_MAT ( REAL*8    ) -- Covariance matrix of estimates of EST_VEC   *
! *                          vector. Input in the crust-fixed coordinate *
! *                          system; output in the topocentric reference *
! *                          system. The covariance matrix is in packed  *
! *                          upper triangular representation. Dimension  *
! *                          of each raw/column runs 1-M__SPD:L_NOD+1.   *
! *     ARG ( REAL*8    ) -- Time argment of the poech to which site     *
! *                          displacements are to be computed.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     VAL ( REAL*8    ) -- Vector of site displacements. Dimesion: 3.  *
! *                          Units: meters.                              *
! *     SIG ( REAL*8    ) -- Vector of standard deviations of site       *
! *                          displacements. Dimension: 3. Units: meters. *
! *                                                                      *
! *  ### 03-MAR-2005    DISPL_BSPL   v2.0 (c) L. Petrov 31-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INTEGER*4  L_NOD, L_DEG
      LOGICAL*4  FL_SPLINE_ONLY, FL_UEN
      REAL*8     NOD_ARR(1-M__SPD:L_NOD), EST_VEC(3,1-M__SPD:M__SPN), &
     &           COV_MAT(*), ARG_REF, VEL_SCL, ARG, VAL(3), SIG(3)
      REAL*8,    ALLOCATABLE :: EQU_VEC(:,:), USE_EST(:,:), TMP_VEC(:), &
     &           ROT_MAT(:,:), TMP_MAT(:,:),  USE_COV(:)
      INTEGER*4  INOD, KNOT, L_EQU, IEQU, IC, J1, J2, J3, J4, IER
      INTEGER*4, ALLOCATABLE :: IND_VEC(:,:), IND_MAT(:,:)
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: ATAN_CS, BSPL_VAL, DP_VV_V
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
! --- Compute the total number of parameter in one equation L_EQU
!
      IF ( FL_SPLINE_ONLY ) THEN
           L_EQU = L_DEG + 1
         ELSE 
           L_EQU = L_DEG + 3
      ENDIF
!
! --- Allocate memory for temporary arrays
!
      ALLOCATE ( TMP_VEC(L_EQU) )
      ALLOCATE ( EQU_VEC(L_EQU,3) )
      ALLOCATE ( USE_EST(L_EQU,3) )
      ALLOCATE ( USE_COV( (3*L_EQU*(3*L_EQU+1))/2) )
      ALLOCATE ( IND_VEC(L_EQU,3) )
      ALLOCATE ( IND_MAT(L_EQU,3) )
      CALL NOUT_R8 ( L_EQU, EQU_VEC )
!
! --- Find the index of the pivot element
!
      INOD = IXMN8 ( L_NOD, NOD_ARR(1), ARG )
      IF ( INOD .GT. 0 ) THEN
!
! -------- Cycle over components: X, Y, Z
!
           DO 410 J1=1,3
              IEQU = 0
!
! ----------- Build equation of dependence of site coordinate on coefficients of
! ----------- B-spline, global position and global linear velocity
!
              DO 420 J2=-L_DEG,0
                 KNOT = INOD + J2
                 IEQU = IEQU + 1
                 IND_VEC(IEQU,J1) =  KNOT
                 IND_MAT(IEQU,J1) = (IND_VEC(IEQU,J1)+M__SPD-1)*3 + J1
                 EQU_VEC(IEQU,J1) = BSPL_VAL ( L_NOD, NOD_ARR(1), L_DEG, KNOT, ARG )
 420          CONTINUE 
!
              IF ( FL_SPLINE_ONLY ) THEN
                   CONTINUE 
                 ELSE 
                   IND_VEC(L_EQU-1,J1) = L_NOD
                   IND_MAT(L_EQU-1,J1) = (IND_VEC(L_EQU-1,J1)+M__SPD-1)*3 + J1
                   EQU_VEC(L_EQU-1,J1) = 1.0D0
!
                   IND_VEC(L_EQU,J1)   = L_NOD+1
                   IND_MAT(L_EQU,J1)   = (IND_VEC(L_EQU,J1)+M__SPD-1)*3 + J1
                   EQU_VEC(L_EQU,J1)   = (ARG - ARG_REF)*VEL_SCL
              END IF
!
! ----------- Collect vector of coefficients and the covariance matrix
!
              DO 430 J3=1,L_EQU
                 USE_EST(J3,J1) = EST_VEC(J1,IND_VEC(J3,J1))
                 DO 440 J4=J3,L_EQU
                    USE_COV( LOCS(J3,J4) ) = COV_MAT( LOCS(IND_MAT(J3,J1),IND_MAT(J4,J1)) )
 440             CONTINUE 
 430          CONTINUE 
!
! ----------- Compute the position and its formal uncertainty
!
              VAL(J1) = DP_VV_V ( L_EQU, EQU_VEC(1,J1), USE_EST(1,J1) )
              CALL MUL_MV_SV_V  ( L_EQU, USE_COV, L_EQU, EQU_VEC(1,J1), &
     &                            L_EQU, TMP_VEC, -3 )
              SIG(J1) = DSQRT ( DP_VV_V ( L_EQU, EQU_VEC(1,J1), TMP_VEC )  )
 410       CONTINUE 
      END IF
!
! --- Free memory for temporary arrays
!
      DEALLOCATE ( USE_EST )
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( TMP_VEC )
      DEALLOCATE ( USE_COV )
      DEALLOCATE ( IND_VEC )
      DEALLOCATE ( IND_MAT )
!
      RETURN
      END  SUBROUTINE  DISPL_BSPL 
