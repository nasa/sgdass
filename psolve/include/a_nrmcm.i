!
!  this is a representation of the a-900 nrmfil
!
      character*8 A_SCAL(A_MAX_PAR),A_SIG(A_MAX_PAR),A_B(A_MAX_PAR)
      character*8 A_A(A_JMAX_TRI)
      integer*2 a_mat
      COMMON/NORM/A_SCAL,A_SIG,A_B,A_A
      equivalence (a_scal,a_mat)
!
