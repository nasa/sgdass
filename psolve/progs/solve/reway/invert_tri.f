      SUBROUTINE invert_norm_tri(a,b,nDIM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 max_dim
!-----END of imp added lines.
!
      DOUBLE PRECISION a(*),b(*)
      INTEGER*2 ndim
      PARAMETER(max_dim=10000)
      DOUBLE PRECISION SCALE(maX_dim)
!
      CALL SCALER ( A, B, SCALE, NDIM )
      CALL DPPFA ( A, NDIM )
      CALL DPPSL ( A, B, NDIM )
      CALL DPPIN ( A, NDIM )
      CALL UNSCALER ( A, B, SCALE, NDIM )
!
      return
      end
      SUBROUTINE invert_tri(a,ndim)
! invert square matrix given in triangular form.
!     Updated to specificaly type integers which
!-------------------------------------------------
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      DOUBLE PRECISION a(*)
      INTEGER*2 ndim
!
      call dppfa(a,ndim )
      call dppin(a,ndim )
!
      return
      end
