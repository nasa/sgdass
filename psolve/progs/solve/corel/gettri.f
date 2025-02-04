      SUBROUTINE GETTRI(KEND,MAT,ROWS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ROWS
      LOGICAL*2 KEND
      REAL*8 MAT(ROWS,ROWS)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2  I, J
      INTEGER*4  IOS
!
      IF(KEND) RETURN
!
      DO I=2,ROWS
        READ ( LUIN, 9910, END=99, IOSTAT=IOS ) (MAT(I,J),J=1,MIN(5,I-1))
        CALL FERR ( INT2(IOS), "Reading covariance matrix", INT2(0), INT2(0) )
        IF ( I-1 .GE. 6 ) READ ( LUIN, 9910, END=99, IOSTAT=IOS) &
     &                           (MAT(I,J),J=6,I-1)
        CALL FERR ( INT2(IOS), "Reading covariance matrix", INT2(0), INT2(0) )
9910    FORMAT (7X,5D23.16)
      ENDDO
      RETURN
!
99    CONTINUE
      KEND=.TRUE.
      RETURN
      END
