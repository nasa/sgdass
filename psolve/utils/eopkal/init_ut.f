!
      SUBROUTINE INIT_UT ()
      implicit none
!
      INCLUDE 'eopkal_ut.i'
!
      REAL*8     twopi/6.283185307179959d0/
!
      REAL*8     decay_time_annual/1000.d0/     !Decay time of seasonal term
      REAL*8     period_annual/365.254d0/       !period of annual term
!
      REAL*8     decay_time_semi/1000.d0/       !Decay time of seasonal term
      REAL*8     period_semi/365.254d0/         !period of annual term
!
      a_US= 2.d0/decay_time_semi
      b_US= (1./decay_time_semi)**2+(twopi/period_semi)**2
!
      a_UA= 2.d0/decay_time_annual
      b_UA= (1./decay_time_annual)**2+(twopi/period_annual)**2
!
      RETURN
      END  !#!  INIT_UT  #!#
