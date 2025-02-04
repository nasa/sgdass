! add a constraint tothe normal equations
      SUBROUTINE add_indexed_constraint(a,v,ixref,nelem,wt)
      IMPLICIT NONE                         !Added by IMP/jwr
      double precision a(*),v(*),wt
      integer*4 ixref(*),nelem
      integer*4 i,j,k
      integer*8 indx8,ind
!
      do i=1,nelem
      do j=1,i
          ind=indx8(ixref(i),ixref(j))
          a(ind) = a(ind)+v(i)*v(j)*wt
      enddo
      enddo
!
      RETURN
      END
