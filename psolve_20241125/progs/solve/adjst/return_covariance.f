      FUNCTION RETURN_COVARIANCE ( M, N, MAT )
      IMPLICIT NONE
! ************************************************************************
! *                                                                      *
! *   This function returns the covariance between m'th parameter and    *
! *   the n'th parameter.                                                *
! *                                                                      *
! ************************************************************************
!
!
      INCLUDE 'solve.i'
!
!     Input:
!
      integer*4 n ! The 1st parameter number.
      integer*4 m ! The 2nd parameter number.
!
!     Output:
      real*8 return_covariance ! the requested covariance in units comparabe to adjustents.
!
      real*8    mat(*) !the covariance, adjustement, sigma array.
      INTEGER*4 JA
      INTEGER*8 INDX8
      LOGICAL*2 OKAY
!
! --- if either parameter number is zero, set covariance ot zero and return.
!
      IF ( M.EQ.0 .OR. N.EQ.0 ) THEN
           RETURN_COVARIANCE = 0.D0
          OKAY = .FALSE.
        ELSE
          OKAY = .TRUE.
      ENDIF
!
! --- The offset to get to the covariance is 3 times M_GPA
!
      JA = 3*M_GPA
!
      IF ( OKAY ) RETURN_COVARIANCE = MAT ( JA+INDX8(N,M) )
!
      RETURN
      END  !#!  RETURN_COVARIANCE  #!#
