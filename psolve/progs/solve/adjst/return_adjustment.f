      function return_adjustment(nparm,mat)
      implicit none
!
!     This function returns the adjustment for the n'th parameter. It
!     hides away the details of how the information is stored in the
!     matrix 'mat'.
!
      INCLUDE 'solve.i'
!
!     Input:
      integer*2 nparm ! The parameter number in question.
!
!     Output:
      real*8 return_adjustment ! the adjustment for the nparm'th parameter
!
      real*8 mat(*) !the covariance, adjustement, sigma array.
      integer*4 jb
!
!     The offset to get to the adjustments is 2 times M_GPA
      jb = 2*M_GPA
!
      return_adjustment = mat(jb+nparm)
!
      return
      end
