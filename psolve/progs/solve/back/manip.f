      SUBROUTINE MANIP ( A, B )
      IMPLICIT NONE
!
! 1.  MANIP PROGRAM SPECIFICATION
!
! 1.1 Perform the matrix manipulation for production of local
!     parameter adjustments for an arc.
!
! 1.2 REFERENCES:
!
! 2.  MANIP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 A(*), B(*)
!
! A - Sub-matrice from the saved arc file
! B - Sub-vector
!
!  NB!
!
!  Dimension of A = 2 *(nparm3*(nparm3+1)/2)  (TWICE!)
!  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'baccm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: back
!       CALLED SUBROUTINES: local
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 GLBLS
      INTEGER*8, EXTERNAL :: INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5. MANIP PROGRAM STRUCTURE
!
      GLBLS=IGLBLS
      IF ( CORLN ) GLBLS = TGLBLS
!
      CALL LOCAL ( A, B, GLBLS, IARCS, NPARM3, IGLBLS, &
     &             A(INDX8(NPARM3,NPARM3)+1) )
      RETURN
      END  !#!  MANIP  #!#
