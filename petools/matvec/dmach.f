      REAL*8     FUNCTION DMACH ( JOB )
      INTEGER*4  JOB
!
!     smach computes machine parameters of floating point
!     arithmetic for use in testing only.  not required by
!     linpack proper.
!
!     if trouble with automatic computation of these quantities,
!     they can be set by direct assignment statements.
!     assume the computer has
!
!        b = base of arithmetic
!        t = number of base  b  digits
!        l = smallest possible exponent
!        u = largest possible exponent
!
!     then
!
!        eps = b**(1-t)
!        tiny = 100.0*b**(-l+t)
!        huge = 0.01*b**(u-t)
!
!     dmach same as smach except t, l, u apply to
!     double precision.
!
!     cmach same as smach except if complex division
!     is done by
!
!        1/(x+i*y) = (x-i*y)/(x**2+y**2)
!
!     then
!
!        tiny = sqrt(tiny)
!        huge = sqrt(huge)
!
!
!     job is 1, 2 or 3 for epsilon, tiny and huge, respectively.
!
      REAL*8     eps, tiny, huge, s
!
      eps = 1.0d0
   10 eps = eps/2.0d0
      s = 1.0d0 + eps
      if (s .gt. 1.0d0) go to 10
      eps = 2.0d0*eps
!
      s = 1.0d0
   20 tiny = s
      s = s/16.0d0
      if (s*1.0 .ne. 0.0d0) go to 20
      tiny = (tiny/eps)*100.0
      huge = 1.0d0/tiny
!
      if (job .eq. 1) dmach = eps
      if (job .eq. 2) dmach = tiny
      if (job .eq. 3) dmach = huge
      return
      end
