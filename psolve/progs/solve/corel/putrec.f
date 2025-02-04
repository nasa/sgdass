      SUBROUTINE PUTREC(MAT,ROWS,COLS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ROWS,COLS
      REAL*8 MAT(ROWS,COLS)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2 I,J
      INTEGER*4 IOS
!
      DO I=1,ROWS
         IF ( COLS .GE. 1 ) WRITE ( LUOUT, 9905, IOSTAT=IOS) &
     &                              I, ( MAT(I,J),J=1 ,MIN(INT2(20),COLS) )
         CALL FERR ( INT2(IOS), "Writing correlations", INT2(0), INT2(0) )
         IF ( COLS .GE. 21 ) WRITE ( LUOUT, 9910, IOSTAT=IOS) &
     &                       ( MAT(I,J),J=21, COLS )
         CALL FERR ( INT2(IOS), "Writing correlations", INT2(0), INT2(0) )
9905     FORMAT(" ",I5,20F6.3)
9910     FORMAT(" ",5X,20F6.3)
      ENDDO
!
      RETURN
      END
