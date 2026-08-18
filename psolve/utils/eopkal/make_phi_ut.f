      SUBROUTINE MAKE_PHI_UT ( PHI, T )
      implicit none
      INCLUDE 'eopkal_ut.i'
!
      REAL*8     phi(num_ut,num_ut)      !UT1 transition matrix
      REAL*8      t            !time
      INTEGER*2 ioff
! phi is a 2Nx2N matrix of the form:
!
!                   |D   E   E   ..|
!     phi= exp t    |0   A1  0   ..|
!                   |0   0   A2
!
!
! where the A1 represent seasonal processes. And
!        |0  1 |      |1 0 |
!     D= |0  0 |    E=|0 0 |    Note:  In deriving above, use DE=0.
!
!     A= |  0    1  |
!        |-b   -a |
!
!   you can show that:
!            |exp tD   E*(exp_tA1-1)/A1  E*(exp_tA1-1)/A1      ..|
!            |0           exp_tA1        0                     ..|
!    phi=    |0   0                      exp_tA2
!
! Note that   E/A1= (1/a2) * | a1   -1 |
!                            | 0     0 |
!
!
      REAL*8     Exp_A(2,2)      !Matrix describing seasonal term
!
      phi=0.
      phi(1,1)=1.
      phi(1,2)=t
      phi(2,2)=1.
!
      ioff=2
      IF(kut_annual) then
         call make_seasonal(A_UA,B_UA,t,exp_A)
         phi(ioff+1:ioff+2,ioff+1:ioff+2)=exp_A(1:2,1:2)
         phi(1,ioff+3)=-(exp_A(1,1)-1.)*a_UA+(exp_A(2,1))/B_UA
         phi(1,ioff+4)=-(exp_A(1,2)*a_UA +exp_A(2,2))/B_UA
         ioff=ioff+2
      endif
!
      IF(kut_semi) then
         call make_seasonal(a_US,b_US,t,exp_A)
         phi(ioff+1:ioff+2,ioff+1:ioff+2)=exp_A(1:2,1:2)
!
         phi(1,ioff+3)=-(exp_A(1,1)-1.)*a_US+(exp_A(2,1))/B_US
         phi(1,ioff+4)=-(exp_A(1,2)*a_US +exp_A(2,2))/B_US
         ioff=ioff+2
      endif
!
      RETURN
      END  !#!  MAKE_PHI_UT  #!#
