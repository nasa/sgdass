      SUBROUTINE REDTRI(MAT,SIG,ROWS)
      IMPLICIT NONE
      INTEGER*2 ROWS
      REAL*8 MAT(ROWS,ROWS),SIG(ROWS)
!
      INTEGER*2 I,J
      REAL*8 DIV
!
      DO I=2,ROWS
        DO J=1,I-1
          DIV=SIG(I)*SIG(J)
          IF(DIV.GT.1.0D-34) THEN
            MAT(I,J)=MAT(I,J)/DIV
          ELSE
            MAT(I,J)=0.0D0
          ENDIF
        ENDDO
      ENDDO
!
      RETURN
      END
