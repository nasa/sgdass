      SUBROUTINE reformat(A1,A2,B1,B2, IXREF, NPARM)
! reformat a CGM
      IMPLICIT NONE
      INCLUDE 'solve.i'
      REAL*8 A1(*),A2(*),B1(*),B2(*)
      INTEGER*2 IXREF(*),NPARM
!
      INTEGER*4 INDX4, IJX
      INTEGER*2 I,J,IX
!
        DO I = 1, NPARM
          IF (IXREF(I) .NE. 0) THEN
            IX = IXREF(I)
            B1(IX) = B2(I)
            DO J = 1, I
              IF (IXREF(J) .NE. 0) THEN
                IJX     = INDX4(IX,IXREF(J))
                A1(IJX) = A2(INDX4(I,J))
              END IF
            END DO
          END IF
        END DO
      RETURN
      END
