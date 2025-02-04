!     Last change:  JG   19 Sep 97    5:11 pm
! This common block contains stuff for the UT part of the Kalman filter
!
      integer*2 max_ut,max_ut_tri
      parameter (max_ut=6,max_ut_tri=max_ut*(max_ut+1)/2)
!
      integer*2 num_ut                      !Total number of Ut parameters
      LOGICAL kut_annual                    !Include annual term in UT1
      LOGICAL kut_semi                      !Include seasonal term in UT1
      common /ut/num_ut,kut_annual,kut_semi
!
      DOUBLE PRECISION a_UA,b_UA           !Parameters for annual term
      DOUBLE PRECISION a_US,b_US           !Parameters for semi-annual term
      COMMON /UT/a_UA,b_UA,A_US,b_US
!
!
