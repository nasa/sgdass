      SUBROUTINE MAKE_PHI ( PHI, T )
! here we make the propagation matrix which transforms from one
! time to next.
      implicit none
      INCLUDE 'eopkal_pm.i'
      INCLUDE 'eopkal_ut.i'
!
      REAL*8     phi(num_ut+num_pm,num_ut+num_pm)    !Transition matrix
      REAL*8     t               !time
!
! transition matrix decomposes into two pieces: One for PM,one for UT
      REAL*8      phi_ut(num_ut,num_ut),phi_pm(num_pm,num_pm)
!      REAL*8     phi_ut(2,2),phi_pm(4,4)
!
      phi=0.
      call make_PHI_PM(phi_pm,t)
      phi(1:num_pm,1:num_pm)=phi_pm
      call make_phi_ut(phi_ut,t)
      phi(num_pm+1:num_pm+num_ut,num_pm+1:num_pm+num_ut)=phi_ut
!
      RETURN
      END  !#!  MAKE_PHI  #!#
