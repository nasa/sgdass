!     Last change:  JG   19 Sep 97    5:21 pm
! This common block contains stuff for the UT part of the Kalman filter
!
      integer*2 max_pm,max_pm_tri
      parameter (max_pm=8,max_pm_tri=max_pm*(max_pm+1)/2)
!
      integer*2 num_pm                      !Total number of Ut parameters
      LOGICAL kpm_annual                    !Include annual term in UT1
      LOGICAL kpm_linear                    !Allow for linear drift in PM
      common /pm/num_pm,kpm_annual,kpm_linear
!
      DOUBLE PRECISION sigma            !Polar motion chandler wobble period
      DOUBLE PRECISION gamma            !Polar motion decay constant
      DOUBLE PRECISION a_PA,b_PA            !Seasonal parameters
      double precision LM_real(2,2),LM_imag(2,2)
      COMMON /PM/sigma,gamma,a_PA,b_PA,LM_real,LM_imag
!
!
