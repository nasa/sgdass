      SUBROUTINE ADD_VELC(CONSTRAINT,APARM,N,A)
      IMPLICIT NONE
!
! 1.  ADD_VELC PROGRAM SPECIFICATION
!
! 1.1 Add appropriate velocity constraints.
!
! 1.2 REFERENCES:
!
! 2.  ADD_VELC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 A(*),CONSTRAINT
      INTEGER*4 N,APARM(M_GPA)
!
! A - Normal equation matrix
! APARM - Cross reference for atmosphere parameter numbers
! CONSTRAINT - Hard-wired as 1/(SIGMA**2) (SIGMA is provided by user)
! N - Number of atmosphere parameters
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraint added
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: do_atm
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 I, J
      INTEGER*8 POS1, POS2, POS3 
      INTEGER*8, EXTERNAL :: INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  910524  Modify for new parameterization scheme to include
!                 off-diagonal elements
!
! 5.  ADD_VELC PROGRAM STRUCTURE
!
      DO I=1,N
         POS1= INDX8 ( APARM(I), APARM(I) )
         IF ( A(POS1) .NE. 0.0d0 ) GOTO 100
      ENDDO
      RETURN
100   DO I=1,N
         POS1 = INDX8 ( APARM(I), APARM(I) )
         A(POS1) = A(POS1) + CONSTRAINT
      ENDDO
      RETURN
      END
