      logical*2 function l4tol2(log)
!
      implicit none
!
!     Gives a logical*2 variable for a logical*2 variable.
!     Implimented to help in the elimination of the (-i2)
!     compilier directive from SOLVE. When  -i2 is removed
!     logical literials (.true. and .false.) change from L*2
!     to L*4.
!
      logical*4 log
!
      l4tol2 = log
!
      return
      end
