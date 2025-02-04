      SUBROUTINE ADD_CLK ( N, CPARM, CONSTRAINT, CNSTROBJ )
      IMPLICIT NONE
!
! 1.  ADD_CLK PROGRAM SPECIFICATION
!
! 1.1 Add clock constraints as requested.
!
! 1.2 REFERENCES:
!
! 2.  ADD_CLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 N, CPARM(M_GPA)
      REAL*8    CONSTRAINT(MAX_CLK)
!
! A - Normal equation matrix
! CONSTRAINT - Hard-wired as 1/(SIGMA**2) (SIGMA provided by user)
! CPARM - Cross index for clock parameter numbers
! N - Number of clock parameters
!
! 2.3 OUTPUT Variables:
!
! A - Normal equation matrix with constraints added
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: do_clk
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I
!C
      INCLUDE   'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
!     Transformation INT2 --> INT4
!C
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  910524  Modify for new parameterization scheme to include
!                       off-diagonal elements
!   pet  970117  Added support of B3D parametrization
!   pet  970226  Added support of B1B3D parametrization
!   pet  980119  Entirely re-wrote to support data structure CNSTR
!
! 5.  ADD_CLK PROGRAM STRUCTURE
!
!C
      DO I=2,N
         CALL ADD_CNSTR ( INT4(CPARM(I-1)), INT4(CPARM(I-1)),  CONSTRAINT(I), &
     &                    CNSTROBJ )
         CALL ADD_CNSTR ( INT4(CPARM(I-1)), INT4(CPARM(I)),   -CONSTRAINT(I), &
     &                    CNSTROBJ )
         CALL ADD_CNSTR ( INT4(CPARM(I)),   INT4(CPARM(I)),    CONSTRAINT(I), &
     &                    CNSTROBJ )
      ENDDO
!
      RETURN
      END  !#!  ADD_CLK  #!#
