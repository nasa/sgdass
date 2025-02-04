      function return_sigma(nparm,mat)
      implicit none
!
!     This function returns the sigma for the n'th parameter. It
!     hides away the details of how the information is stored in the
!     matrix 'mat'.
!
      INCLUDE 'solve.i'
!
!
!     Input:
      integer*2 nparm ! The parameter number in question.
!
!     Output:
      real*8 return_sigma ! the adjustment for the nparm'th parameter
!
      real*8 mat(*) !the covariance, adjustement, sigma array.
      integer*4 js
!
!     The offset to get to the sigmas is 3 times M_GPA
      js = M_GPA
!
      return_sigma = mat(js+nparm)
!
      return
      end
