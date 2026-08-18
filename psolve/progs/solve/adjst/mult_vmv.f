      real*8 function mult_vmv(nsize,vec1,matrix,vec2)
      implicit none
!
!     Multiplies a vector times a square matrix times vector producing one number.
!
!     Input:
      integer*4 nsize         !the length of the vectors and the size of the square matrix.
      real*8    vec1(nsize)   !The first vector
      real*8    vec2(nsize)   !The second vector
      real*8    matrix(nsize,nsize) ! The matrix
!
!     Output:
!     the value of the function.
!
      integer*4 i,j
      real*8 vhold(nsize), fhold
!
!     :97,05.20:jwr: Created.
!
      Do i = 1,nsize
        vhold(i)=0.d0
        do j=1, nsize
          vhold(i) = vhold(i) + vec1(j)*matrix(i,j)
        enddo
      enddo
!
      fhold = 0.d0
      do i=1,nsize
        fhold = fhold + vhold(i)*vec2(i)
      enddo
!
      mult_vmv = fhold
!
      return
      end
