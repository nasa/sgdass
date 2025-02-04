      SUBROUTINE COVAR2COREL ( NUM_PARMS, COV_MAT, COR_MAT )
      IMPLICIT NONE
!
!     Makes a correlation matrix from a covariance matrix.
!     If the covariance matrix has a row and column of zeros, then
!     the correlation matrix has the same.
!
      integer*4 num_parms,i,j
      real*8 cov_mat(num_parms,num_parms), cor_mat(num_parms,num_parms)
      real*8 scaler(8)
!
!     :97:11:14:jwr: Created.
!
!     Handle zeros (or really small numbers) on the diagonal.
!
      DO I = 1,8
         SCALER(I) = DSQRT ( COV_MAT(I,I) )
         IF ( SCALER(I) .LT. 1.D-20 ) SCALER(I) = 1.D0
      ENDDO
!
      DO I = 1,8
         DO J=1,8
            COR_MAT(I,J) = COV_MAT(I,J)/(SCALER(I)*SCALER(J))
         ENDDO
      ENDDO
!
      RETURN
      END  !#!  COVAR2COREL  #!#
