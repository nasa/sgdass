      SUBROUTINE REDREC(MAT,SIGROW,SIGCOL,ROWS,COLS)
      IMPLICIT NONE
      INTEGER*2 ROWS,COLS
      REAL*8 SIGROW(ROWS),SIGCOL(COLS),MAT(ROWS,COLS)
!
      INTEGER*2 I,J
      REAL*8 DIV
!
      DO I=1,ROWS
        DO J=1,COLS
          DIV=SIGROW(I)*SIGCOL(J)
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
