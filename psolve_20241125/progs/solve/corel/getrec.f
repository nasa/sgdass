      SUBROUTINE GETREC(KEND,MAT,ROWS,COLS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ROWS,COLS
      LOGICAL*2 KEND
      REAL*8 MAT(ROWS,COLS)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2 I, J
      INTEGER*4 IOS
!
      IF ( KEND ) RETURN
!
      DO I=1,ROWS
         IF ( COLS .GE. 1 ) READ ( LUIN, 9910, END=99, IOSTAT=IOS) &
     &                      (MAT(I,J),J=1,MIN(INT2(5),COLS))
         CALL FERR ( INT2(IOS), "Reading covariance record", INT2(0), INT2(0) )
         IF ( COLS .GE. 6 ) READ ( LUIN, 9910, END=99, IOSTAT=IOS) &
     &                             (MAT(I,J),J=6,COLS)
         CALL FERR ( INT2(IOS), "Reading covariance record", INT2(0), INT2(0) )
9910     FORMAT (7X,5D23.16)
      ENDDO
      RETURN
!
99    CONTINUE
      KEND=.TRUE.
      RETURN
      END
