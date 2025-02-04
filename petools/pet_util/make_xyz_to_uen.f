      SUBROUTINE MAKE_XYZ_TO_UEN  ( COO, XYZ_TO_UEN )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MAKE_XYZ_TO_UEN  makes a matrix which transforms       *
! *   vectors from the global XYZ (Crust Fixed Reference system or CFS)  *
! *   reference system to the local UEN (Up, East, North) reference      *
! *   system.                                                            *
! *                                                                      *
! * ### 15-JUL-1999  MAKE_XYZ_TO_UEN  v2.0 (c) L. Petrov 01-NOV-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     COO(3), XYZ_TO_UEN(3,3)
      REAL*8     ZORT(3), UORT(3), EORT(3), NORT(3), RD
      REAL*8     PHI_GCN, PHI_GDT, LAMBDA, H_ELL, G_ACC, CF, SF, CL, SL
      LOGICAL*4  FLAG__RAD
      FLAG__RAD = .FALSE.
!
      IF ( FLAG__RAD ) THEN
           ZORT(1) = 0.D0
           ZORT(2) = 0.D0
           ZORT(3) = 1.D0
!
! -------- Compute orts: u-ort (up), e-ort (east) and n-ort (north)
!
           CALL COPY_V   ( 3, COO, UORT )
           CALL NORM_VEC ( 3, UORT,  RD )
!
           CALL VM83     ( ZORT, UORT, EORT )
           CALL NORM_VEC ( 3, EORT, RD )
!
           CALL VM83     ( UORT, EORT, NORT )
           CALL NORM_VEC ( 3, NORT, RD )
!
           XYZ_TO_UEN(1,1) = UORT(1)
           XYZ_TO_UEN(1,2) = UORT(2)
           XYZ_TO_UEN(1,3) = UORT(3)
!
           XYZ_TO_UEN(2,1) = EORT(1)
           XYZ_TO_UEN(2,2) = EORT(2)
           XYZ_TO_UEN(2,3) = EORT(3)
!
           XYZ_TO_UEN(3,1) = NORT(1)
           XYZ_TO_UEN(3,2) = NORT(2)
           XYZ_TO_UEN(3,3) = NORT(3)
         ELSE 
           CALL REF_ELL ( 0, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
!
           CF = DCOS(PHI_GDT)
           SF = DSIN(PHI_GDT)
           CL = DCOS(LAMBDA)
           SL = DSIN(LAMBDA)
!
           XYZ_TO_UEN(1,1) =  CF*CL
           XYZ_TO_UEN(1,2) =  CF*SL
           XYZ_TO_UEN(1,3) =  SF
!
           XYZ_TO_UEN(2,1) = -SL
           XYZ_TO_UEN(2,2) =  CL
           XYZ_TO_UEN(2,3) =  0.D0
!
           XYZ_TO_UEN(3,1) = -SF*CL
           XYZ_TO_UEN(3,2) = -SF*SL
           XYZ_TO_UEN(3,3) =  CF
      END IF
!
      RETURN
      END  !#!  MAKE_XYZ_TO_UEN  #!#
