      SUBROUTINE SIGMAS ( MATRIX, SIG, NPARAM )
      IMPLICIT   NONE
!
! 1.  SIGMAS PROGRAM SPECIFICATION
!
! 1.1 Put square roots of diagonal of SOLVE format matrix in SIG.
!
! 1.2 REFERENCES:
!
! 2.  SIGMAS INTERFACE
      INCLUDE 'solve.i'
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NPARAM
      REAL*8    MATRIX(*)
!
! MATRIX - SOLVE format matrix
! NPARAM - Number of parameters
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SIG(*)
!
! SIG - Square roots of diagonal elements of MATRIX
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   I
      INTEGER*8   II
      CHARACTER   ERRSTR*128
!
! I - Loop index
! II - Index into MATRIX
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!
! 5.  SIGMAS PROGRAM STRUCTURE
!
      II=0
      DO I = 1, NPARAM
         II=II+I
         IF ( MATRIX(II).LT.0) THEN
              WRITE ( ERRSTR, 9901 ) I
 9901         FORMAT ( 'SIGMAS: Negative diagonal element in ', &
     &                 'covariance matrix at row ',I6, ' : ', 1PD15.7 )
              CALL FERR ( INT2(215), ERRSTR, INT2(0), INT2(0) )
         ENDIF
         SIG(I) = DSQRT ( MATRIX(II) )
      END DO
!
      RETURN
      END  SUBROUTINE  SIGMAS  !#!#
