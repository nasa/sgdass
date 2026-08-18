      SUBROUTINE PUTTRI(MAT,ROWS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ROWS
      REAL*8 MAT(ROWS,ROWS)
!
      INCLUDE 'corpar.i'
      INCLUDE 'corcom.i'
!
      INTEGER*2  I, J
      INTEGER*4  IOS
!
      DO I=2,ROWS
         WRITE ( LUOUT, 9905, IOSTAT=IOS ) I,(MAT(I,J),J=1,MIN(20,I-1))
         CALL FERR ( INT2(IOS), "Writing correlations", INT2(0), INT2(0) )
         IF ( I-1 .GE. 21 ) WRITE ( LUOUT, 9910 ) (MAT(I,J),J=21,I-1)
9905     FORMAT(" ",I5,20F6.3)
9910     FORMAT(" ",5X,20F6.3)
      ENDDO
!
      RETURN
      END
