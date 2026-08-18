      SUBROUTINE MRRR(N1,LEFT,N2,RIGHT,N3,PROD)
      IMPLICIT NONE
!
! 1.  MRRR PROGRAM SPECIFICATION
!
! 1.1 Multiply two rectangular matrices. [LEFT] [RIGHT] = [PROD]
!
! 1.2 REFERENCES:
!
! 2.  MRRR INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N1,N2,N3
      REAL*8 LEFT(N1,N2),RIGHT(N2,N3)
!
! N1 - Number of rows in LEFT and PROD
! N2 - Number of columns in LEFT, rows in RIGHT
! N3 - Number of columns in RIGHT and PROD
! LEFT - The first matrix to be multiplied, N1 X N2
! RIGHT - The second matrix to be multiplied, N2 X N3
!
! 2.3 OUTPUT Variables:
!
      REAL*8 PROD(N1,N3)
!
! PROD - The product matrix, N1 X N3
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: clrmt
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K
      INTEGER*4 N4
!
! I,J,K - Loop indices
! N4 - Number of elements in output array PROD
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  MRRR PROGRAM STRUCTURE
!
! First clear out the output array
!
      N4=N1
      N4=N1*N3
      CALL CLRMT(PROD,N4)
!
! Now do the multiplication
!
      DO I=1,N1
        DO J=1,N3
          DO K=1,N2
            PROD(I,J)=PROD(I,J)+LEFT(I,K)*RIGHT(K,J)
          ENDDO
        ENDDO
      ENDDO
!
      RETURN
      END
