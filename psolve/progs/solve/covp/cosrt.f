      SUBROUTINE COSRT(A,I,J,FCORR,IXC,IYC)
      IMPLICIT NONE
!
! 1.  COSRT PROGRAM SPECIFICATION
!
! 1.1 COSRT IS THE SUBROUTINE WHICH WILL SELECT THE TEN LARGEST
!     ELEMENTS OF THE CORRELATION MATRIX AND PLACE THEM INTO ARRAY
!     FCORR.
!
! 1.2 REFERENCES:
!
! 2.  INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
!      EMA A
      INTEGER*2 I,J
      REAL*8    A
!
!  I,J - Index for the matrix
!  A - Correlation matrix
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IXC(10),IYC(10)
      REAL*8    FCORR(10)
!
!  IXC - X-coordinate index array
!  IYC - Y-coordinate index array
!  FCORR - Array containing the ten largest elements of the correlation matrix
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: covp
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 K,L
!
! 4.  HISTORY
!  WHO   WHEN    WHAT
!
! 5.  COSRT PROGRAM STRUCTURE
!
      IF(DABS(A).LT.ABS(FCORR(10))) GO TO 900
      FCORR(10) = A
      IXC(10) = I
      IYC(10) = J
      DO 100 K = 1,9
          L = 10 - K
          IF(DABS(A).LT.ABS(FCORR(L))) GO TO 900
          FCORR(L+1) = FCORR(L)
          IXC(L+1) = IXC(L)
          IYC(L+1) = IYC(L)
          FCORR(L) = A
          IXC(L) = I
          IYC(L) = J
  100 CONTINUE
  900 CONTINUE
      RETURN
      END
