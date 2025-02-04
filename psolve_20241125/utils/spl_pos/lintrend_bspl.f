      SUBROUTINE LINTREND_BSPL ( L_NOD, NOD_ARR, L_DEG, ARG_REF, VEL_SCL, &
     &                           EST_VEC, COV_MAT, ARG, VAL, SIG )
! ************************************************************************
! *                                                                      *
! *   Routine LINTREND_BSPL 
! *                                                                      *
! *  ### 03-MAR-2005  LINTREND_BSPL  v1.0 (c) L. Petrov 11-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INTEGER*4  L_NOD, L_DEG
      LOGICAL*4  FL_UEN
      REAL*8     NOD_ARR(1-M__SPD:L_NOD), EST_VEC(3,1-M__SPD:M__SPN), &
     &           COV_MAT(*), ARG_REF, VEL_SCL, ARG, VAL(3), SIG(3)
      REAL*8,    ALLOCATABLE :: EQU_VEC(:,:), USE_EST(:,:), TMP_VEC(:), &
     &           ROT_MAT(:,:), TMP_MAT(:,:),  USE_COV(:)
      INTEGER*4  INOD, KNOT, L_EQU, L_PAR, IEQU, IC, J1, J2, J3, J4, IER
      INTEGER*4, ALLOCATABLE :: IND_VEC(:,:), IND_MAT(:,:)
      INTEGER*4, EXTERNAL :: IXMN8
      REAL*8,    EXTERNAL :: ATAN_CS, BSPL_VAL, DP_VV_V
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      L_EQU = L_DEG + 3
      L_PAR = 3*M__SPD + 3*(L_NOD + 1)
      ALLOCATE ( TMP_VEC(L_EQU) )
      ALLOCATE ( EQU_VEC(L_EQU,3) )
      ALLOCATE ( USE_EST(L_EQU,3) )
      ALLOCATE ( USE_COV( (3*L_EQU*(3*L_EQU+1))/2) )
      ALLOCATE ( IND_VEC(L_EQU,3) )
      ALLOCATE ( IND_MAT(L_EQU,3) )
      CALL NOUT_R8 ( L_EQU, EQU_VEC )
!
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
! ----------- Build equation of dependence of site coordinate in coefficients of
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
              IND_VEC(L_EQU-1,J1) = L_NOD
              IND_MAT(L_EQU-1,J1) = (IND_VEC(L_EQU-1,J1)+M__SPD-1)*3 + J1
              EQU_VEC(L_EQU-1,J1) = 1.0D0
!
              IND_VEC(L_EQU,J1)   = L_NOD+1
              IND_MAT(L_EQU,J1)   = (IND_VEC(L_EQU,J1)+M__SPD-1)*3 + J1
              EQU_VEC(L_EQU,J1)   = (ARG - ARG_REF)*VEL_SCL
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
      DEALLOCATE ( USE_EST )
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( TMP_VEC )
      DEALLOCATE ( USE_COV )
      DEALLOCATE ( IND_VEC )
      DEALLOCATE ( IND_MAT )
!
      RETURN
      END  SUBROUTINE  LINTREND_BSPL 
