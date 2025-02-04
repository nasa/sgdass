      SUBROUTINE MAKE_UEN_TO_XYZ  ( COO, UEN_TO_XYZ )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MAKE_UEN_TO_XYZ  makes a matrix which transforms       *
! *   vectors from local UEN (Up, East, North) reference system to a     *
! *   global XYZ (Crust Fixed Reference system or CFS).                  *
! *                                                                      *
! * ### 15-JUL-1999  MAKE_UEN_TO_XYZ  v2.0 (c) L. Petrov 01-NOV-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     COO(3), UEN_TO_XYZ(3,3)
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
! -------- Compute orts: U-ort (up), E-ort (east) and N-ort (north)
!
           CALL COPY_V   ( 3, COO, UORT )
           CALL NORM_VEC ( 3, UORT, RD )
!
           CALL VM83     ( ZORT, UORT, EORT )
           CALL NORM_VEC ( 3, EORT, RD )
!
           CALL VM83     ( UORT, EORT, NORT )
           CALL NORM_VEC ( 3, NORT, RD )
!
           UEN_TO_XYZ(1,1) = UORT(1)
           UEN_TO_XYZ(2,1) = UORT(2)
           UEN_TO_XYZ(3,1) = UORT(3)
!
           UEN_TO_XYZ(1,2) = EORT(1)
           UEN_TO_XYZ(2,2) = EORT(2)
           UEN_TO_XYZ(3,2) = EORT(3)
!
           UEN_TO_XYZ(1,3) = NORT(1)
           UEN_TO_XYZ(2,3) = NORT(2)
           UEN_TO_XYZ(3,3) = NORT(3)
         ELSE 
           CALL REF_ELL ( 0, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
!
           CF = DCOS(PHI_GDT)
           SF = DSIN(PHI_GDT)
           CL = DCOS(LAMBDA)
           SL = DSIN(LAMBDA)
!
           UEN_TO_XYZ(1,1) =  CF*CL
           UEN_TO_XYZ(2,1) =  CF*SL
           UEN_TO_XYZ(3,1) =  SF
!
           UEN_TO_XYZ(1,2) = -SL
           UEN_TO_XYZ(2,2) =  CL
           UEN_TO_XYZ(3,2) =  0.D0
!
           UEN_TO_XYZ(1,3) = -SF*CL
           UEN_TO_XYZ(2,3) = -SF*SL
           UEN_TO_XYZ(3,3) =  CF
      END IF
!
      RETURN
      END  !#!  MAKE_UEN_TO_XYZ  #!#
