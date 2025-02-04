      FUNCTION post_fit_cor(A,D,nparm)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! calculate the post-fit residual sigma correction.
!
!  A is inverse of normal equation in upper-triangular form.
!  A=N^-1+N^-1 C N^-1
!  D is derivative vector for this measurement. Made by partl.
!
      double precision A(*),D(*)
      INTEGER*2 nparm
      double precision post_fit_cor
!
!
      INCLUDE "solve.i"
! temporary vector:  A*D
      INTEGER*4 indx(m_gpa)
      INTEGER*4 num_ind
!
      double precision DAD0
      INTEGER*4 iparm,i,j,ind,jnd
      INTEGER*8 IPTR
      INTEGER*8, EXTERNAL :: INDX8
!
! build an index into the non-zero values of the derivative array.
      num_ind=0
      do iparm=1,nparm
        if(D(iparm) .ne. 0) then
           num_ind=num_ind+1
           indx(num_ind)=iparm
        endif
      enddo
!
!
      DAD0=0.
      do ind=1,num_ind
        i=indx(ind)
        iptr=indx8(i,i)
        DAD0=DAD0+A(iptr)*D(i)*D(i)
        do jnd=ind+1,num_ind
           j=indx(jnd)
           iptr=indx8(i,j)
           DAD0=DAD0+2.*A(iptr)*D(i)*D(j)
        end do
      end do
      post_fit_cor=DAD0
      return
      end
