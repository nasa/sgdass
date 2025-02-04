      SUBROUTINE GTLHV ( AP1E, AP2E, VDIF, T, SPAR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTLHV PROGRAM SPECIFICATION
!
! 1.1 Given the a priori site vectors AP1 and AP2 this routine will
!     define the corrected baseline vector in the LHV system and will
!     generate the partial derivative of L,H,V with respect to
!     X,Y,Z stations 1 and 2.
!
! 1.2 REFERENCES:
!
! 2.  GTLHV INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 AP1E(3),AP2E(3),VDIF(3)
!
! AP1E - The a priori station 1 position
! AP2E - The a priori station 2 position
! VDIF - The baseline vector after adjustment in the conventional
!        XYZ coordinate system
!
! 2.3 OUTPUT Variables:
!
      REAL*8 T(3),SPAR(6,11)
!
! SPAR - In the last three rows of this matrix are stored the
!        partial derivatives of L,V,H with respect to the
!        conventional X,Y,Z of the two stations
! T - The LHV baseline vector
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: bwork
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,K
      REAL*8    VTEMP(3), VL(3), VH(3), VV(3), AP2(3), AP1(3), DOTP, BAS_SQU
      INTEGER*2  INT2_ARG
!
! 4.  HISTORY
!   WHO  WHEN    WHAT
!   JWR  830920  Created
!   PET  990404  Improved comments
!   PET  2001.06.29  Fixed a bug: the previous versoin tryied to devide on zero
!                    in the case of xzero baseline
!
! 5.  GTLHV PROGRAM STRUCTURE
!
      BAS_SQU = 0.0D0
      DO I=1,3
         AP1(I)=AP1E(I)
         AP2(I)=AP2E(I)
         BAS_SQU = BAS_SQU + ( AP1(I) - AP2(I) )**2
      ENDDO
!
      IF ( BAS_SQU .LT. 1.D-8 ) THEN
!
! -------- Null baseline case
!
           CALL NOUT_R8 ( 3, VDIF )
           CALL NOUT_R8 ( 6, SPAR(1,9) )
           CALL NOUT_R8 ( 6, SPAR(1,10) )
           CALL NOUT_R8 ( 6, SPAR(1,11) )
           RETURN
      END IF
!
! --- Compute the unit vector in the direction of the length, horizontal
! --- and vertical according to the Trask system.
!
! --- Compute the length direction unit vector in the XYZ system.
!
      CALL VECSB ( AP2, AP1, VTEMP )
      CALL VUNIT ( VTEMP, VL )
!
! --- Compute the horizontal direction unit vector in the XYZ system.
!
      CALL CROSP ( AP2, AP1, VTEMP )
      CALL VUNIT ( VTEMP, VH )
!
! --- Compute the vertical direction unit vector in the XYZ system.
!
      CALL CROSP ( VL, VH, VV )
!
! --- Now compute the baseline components in the Trask system by using
! --- the dot product.
!
      T(1) = DOTP ( VDIF, VL )
      T(2) = DOTP ( VDIF, VH )
      T(3) = DOTP ( VDIF, VV )
!
! --- Now compute the partial derivatives of LVH with respect to XYZ at
! --- stations 1 and 2.
!
      SPAR(1,9)  = -VL(1)
      SPAR(2,9)  = -VL(2)
      SPAR(3,9)  = -VL(3)
!
      SPAR(1,10) = -VH(1)
      SPAR(2,10) = -VH(2)
      SPAR(3,10) = -VH(3)
!
      SPAR(1,11) = -VV(1)
      SPAR(2,11) = -VV(2)
      SPAR(3,11) = -VV(3)
!
! --- The partial for station 2 are the same as station 1 but with the
! --- sign reversed.
!
      DO I=1,3
!
! ------ Running over XYZ
!
         DO K=9,11
            SPAR(I+3,K) = -SPAR(I,K)
         END DO
      END DO ! running over XYZ
!
      RETURN
      END  !#!  GTLHV  #!#
