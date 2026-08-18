      SUBROUTINE REORDER ( IX1T3, NPARM, FAST_MODE, L, G, ARR1, ARR2 )
      IMPLICIT NONE
!
! 1.  REORDER PROGRAM SPECIFICATION
!
! 1.1 Reorder the parameters in the SOLVE matrix and vector. Input order is
!     the order of PROC. OUTPUT order is trhe ARCPE order: first all local
!     parameters, then all global parameters for this arc
!
! 1.2 REFERENCES:
!
! 2.  REORDER INTERFACE
!
! 2.1 Parameter File
      INCLUDE  'solve.i'
      INCLUDE  'fast.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IX1T3(*)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 NPARM
      REAL*8    ARR1(*), ARR2(*)
!
! IX1T3 -- Cross reference between list of parameters in PROC order to
!          ARCPE order
!     L -- Number of local parameters
!     G -- Number of global parameters for this arc
!  ARR1 -- Reordered matrix/vector from ABMOVE in ARCPE order
!  ARR2 -- Original matrix/vector as read from NRMFIL in PROC order
! NPARM -- Number of parms in ARR2 (total number of l,ocal parameters and global
!          parameters for this arc)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: abmove
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   JB, JA, FAST_MODE, L, G
      INTEGER*8   AD_AGG, AD_ALG, AD_ALL, AD_BG, AD_BL, AD_SC
      INTEGER*4   INT4_ARG, I4_ARG, J4_ARG
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jmg  960610  Fix bug which occurs because
!                the fixed arc is treated differently than the free arcs.
!                Previously the EOP parameters were not estimated in the
!                forward solution -- only in the back.  Now they will be
!                estimated in both directions.  This requires the application
!                of constraints in arcpe to constrain the adjustments to zero.
!   pet  970219  Added support B1D algorithm
!   pet  970228  Simplified comments
!
! 5.  REORDER PROGRAM STRUCTURE
!
!
! Rearrange the parameters
!
      JB = 1 + 2*M_GPA
      JA = 1 + 3*M_GPA
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &     FAST_MODE .EQ. F__B3D      )  THEN
!
           CALL ABMOVE ( ARR1(JA), ARR1(JB), ARR2(JA), ARR2(JB), IX1T3, NPARM )
        ELSE IF ( FAST_MODE .EQ. F__B1D ) THEN
!
! -------- Calculation addresses for different submatrices
!
           AD_ALL = LOC( ARR1(JA) )                    ! local-local   normal martix
           AD_ALG = AD_ALL + 8* (INT8(L)*INT8(L+1))/2  ! local-global  normal martix
           AD_AGG = AD_ALG + 8*  INT8(L)*INT8(G)       ! global-global normal martix
           AD_SC  = LOC( ARR1(1)  )          ! vector of scales
           AD_BL  = LOC( ARR1(JB) )          ! local normal vector
           AD_BG  = AD_BL  + 8* L            ! global normal vector
!
           CALL ABMOVE_B1D ( L, G, IX1T3, ARR2(JA), ARR2(JB), %VAL(AD_ALL), &
     &                       %VAL(AD_ALG), %VAL(AD_AGG), %VAL(AD_BL), %VAL(AD_BG) )
      END IF
!
      RETURN
      END  !#!  REORDER  #!#
