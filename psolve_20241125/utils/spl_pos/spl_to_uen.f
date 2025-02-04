      SUBROUTINE SPL_TO_UEN ( L_NOD, L_DEG, COO_TRS, EST_VEC, COV_MAT )
! ************************************************************************
! *                                                                      *
! *   Auxiliary Routine SPL_TO_UEN transforms the estimates of the       *
! *   coefficients site displacements expansion with the B-spline basis  *
! *   as well as adjustment to global position and velocity and their    *
! *   covariance matrix from the crust-fixed X-Y-Z coordinate system to  *
! *   the topocentric Up-East-North coordinate system.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_NOD ( INTEGER*4 ) -- The total number of knots.                  *
! *   L_DEG ( INTEGER*4 ) -- The degree of the B-spline.                 *    
! * COO_TRS ( REAL*8    ) -- Vector of site coordinates in the           *
! *                          crust-fixed coordinate system.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * EST_VEC ( REAL*8    ) -- Vector of estimates. Input in the           *
! *                          crust-fixed coordinate system; output in    *
! *                          the topocentric reference system.           *
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
! *                                                                      *
! *  ### 11-MAR-2005  SPL_TO_UEN   v1.0 (c)  L. Petrov  11-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INTEGER*4  L_NOD, L_DEG
      REAL*8     COO_TRS(3), EST_VEC(*), COV_MAT(*)
      REAL*8,    ALLOCATABLE :: COV_NEW(:), EST_NEW(:)
      REAL*8,    ALLOCATABLE :: ROT_MAT(:,:), TMP_MAT(:,:)
      REAL*8     LONG, PP, LAT_GCN, LAT_GDT, CF, SF, CL, SL, RAD, MU, &
     &           UEN_TO_TRS(3,3), TRS_TO_UEN(3,3)
      INTEGER*4  L_PAR, IC, J1, J2, J3, IER
      REAL*8,    EXTERNAL :: ATAN_CS
!
      L_PAR = 3*M__SPD + 3*(L_NOD + 1)
!
      ALLOCATE ( ROT_MAT( L_PAR, L_PAR ) )
      ALLOCATE ( TMP_MAT( L_PAR, L_PAR ) )
      ALLOCATE ( EST_NEW( L_PAR ) )
      ALLOCATE ( COV_NEW( (L_PAR*(L_PAR+1))/2 ) )
!
      CALL NOUT_R8 ( (L_PAR*(L_PAR+1))/2, COV_NEW )
      LONG = ATAN_CS ( COO_TRS(1), COO_TRS(2) )
      IF ( LONG < 0.0D0 ) LONG = PI2 + LONG
      PP  = DSQRT ( COO_TRS(1)**2 + COO_TRS(2)**2 )
      LAT_GCN = DATAN( COO_TRS(3)/PP )
!
! --- Computation of geodetic latitude
!
      RAD = DSQRT ( COO_TRS(1)**2 + COO_TRS(2)**2 + COO_TRS(3)**2 )
      MU = DATAN ( COO_TRS(3)/PP * &
     &             ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/RAD  ) )
!
      LAT_GDT = DATAN( ( (1.D0 - VTD__FE)*COO_TRS(3) + &
     &                 VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                ( (1.D0 - VTD__FE)* &
     &                ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
! --- Compute the matrix of transformation from UEN to TRS
!
      CF = DCOS(LAT_GDT)
      SF = DSIN(LAT_GDT)
      CL = DCOS(LONG)
      SL = DSIN(LONG)
!
      UEN_TO_TRS(1,1) = CF*CL
      UEN_TO_TRS(2,1) = CF*SL
      UEN_TO_TRS(3,1) = SF
!
      UEN_TO_TRS(1,2) = -SL
      UEN_TO_TRS(2,2) =  CL
      UEN_TO_TRS(3,2) =  0.D0
!
      UEN_TO_TRS(1,3) = -SF*CL
      UEN_TO_TRS(2,3) = -SF*SL
      UEN_TO_TRS(3,3) =  CF
!
! --- Transpose the matrix and get the matrix of transfromation from TRS to
! --- UEN -- just that what we need
!
      CALL TM83 ( UEN_TO_TRS, TRS_TO_UEN )
!
      CALL NOUT_R8 ( L_PAR*L_PAR, ROT_MAT ) 
!
! --- Build the rotation matrix for EST_VEC vector. This matrix consist of
! --- 3x3 diagonal blocks for rtoation of each triad of EST_VEC
!
      IC = 0
      DO 410 J1=1-M__SPD,L_NOD+1
         DO 420 J2=1,3
            DO 430 J3=1,3
               ROT_MAT(IC+J3,IC+J2) = TRS_TO_UEN(J3,J2)
 430        CONTINUE 
 420     CONTINUE 
         IC = IC + 3
 410  CONTINUE 
!
! --- Applying transfromation to the covariance matrix ...
!
      IER = -1
      CALL MUL_MM_ST_I ( L_PAR, COV_MAT, L_PAR, L_PAR, ROT_MAT, &
     &                   L_PAR, L_PAR, TMP_MAT, IER )
      IER = -1
      CALL MUL_MM_II_S ( L_PAR, L_PAR, ROT_MAT, L_PAR, L_PAR, TMP_MAT, &
     &                   L_PAR, COV_NEW, IER )
!
! --- ... and the covariance vector
!
      IER = -1
      CALL MUL_MV_IV_V ( L_PAR, L_PAR, ROT_MAT, L_PAR, EST_VEC, &
     &                   L_PAR, EST_NEW, IER )
!
      CALL COPY_R8 ( (L_PAR*(L_PAR+1))/2, COV_NEW, COV_MAT )
      CALL COPY_R8 ( L_PAR,               EST_NEW, EST_VEC )
!
      DEALLOCATE ( ROT_MAT )
      DEALLOCATE ( TMP_MAT )
      DEALLOCATE ( EST_NEW )
      DEALLOCATE ( COV_NEW )
!
      RETURN
      END  SUBROUTINE  SPL_TO_UEN 
