      SUBROUTINE DXMPY ( N, DX, INCX, DY, INCY )
      IMPLICIT NONE
!
!     vector times a vector
!     mwh, vis 730 , 1/27/92
!
      REAL*8     DX(*), DY(*)
      INTEGER*4  INCX, INCY, IX, IY, N
#ifdef HPUX
      IF ( N .LE. 0 ) RETURN
      IX = 1
      IY = 1
      IF ( INCX .LT. 0 ) IX = (-N+1)*INCX + 1
      IF ( INCY .LT. 0 ) IY = (-N+1)*INCY + 1
      CALL VEC_$DMULT_VECTOR_I ( DX(IX), INCX, DY(IY), INCY, N, DY(IY), INCY )
#else
      integer i, m, mp1
!
      IF ( N .LE. 0 )RETURN
      IF ( INCX .EQ. 1 .AND. INCY .EQ. 1 ) GOTO 20
!
! --- Code for unequal increments or equal increments
! --- NOT EQUAL TO 1
!
      IX = 1
      IY = 1
      IF ( INCX .LT. 0 ) IX = (-N+1)*INCX + 1
      IF ( INCY .LT. 0 ) IY = (-N+1)*INCY + 1
      DO 10 I = 1,N
         DY(IY) = DY(IY)*DX(IX)
         IX = IX + INCX
         IY = IY + INCY
   10 CONTINUE
      RETURN
!
! --- code for both increments equal to 1
!
! --- clean-up loop
!
   20 M = MOD(N,4)
      IF ( M .EQ. 0 ) GOTO 40
      DO 30 I = 1,M
        DY(I) = DY(I)*DX(I)
   30 CONTINUE
      IF ( N .LT. 4 ) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,4
         DY(I) = DY(I)*DX(I)
         DY(I + 1) = DY(I + 1)*DX(I + 1)
         DY(I + 2) = DY(I + 2)*DX(I + 2)
         DY(I + 3) = DY(I + 3)*DX(I + 3)
   50 CONTINUE
#endif
      RETURN
      END  !#!  DXMPY  #!#
