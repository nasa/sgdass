      SUBROUTINE NRMST ( A, B, NPARAM, NELEM )
      IMPLICIT NONE
!
! 1.  NRMST PROGRAM SPECIFICATION
!
! 1.1 Zero out the normal equations matrix.
!
! 1.2 REFERENCES:
!
! 2.  NRMST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*8 NELEM
      INTEGER*2 NPARAM
!
! NELEM - Number of elements of matrix array A
! NPARAM - Number of parameters
!
! 2.3 OUTPUT Variables:
!
      REAL*8 A(NELEM),B(*)
!
! A - Normal equation matrix, zeroed out
! B -
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: prelp
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 NFIN, I
      REAL*8 Z
      INTEGER*4 iblas0,iblas1,nblas
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  NRMST PROGRAM STRUCTURE
!
      Z      = 0.0D0
      IBLAS0 = 0
      IBLAS1 = 1
      NBLAS  = NPARAM
!
! --- Clear out B vector
!
      CALL DCOPY ( Z, IBLAS0, B, IBLAS1, NBLAS )
!
      NFIN  = DBLE(NPARAM)
      NFIN  = NFIN * (NFIN+1) / 2
      NBLAS = NFIN
!
! --- Clear out A matrix
!
      CALL DCOPY ( Z, IBLAS0, A, IBLAS1, NBLAS )
!
      RETURN
      END   SUBROUTINE  NRMST  !#!#
