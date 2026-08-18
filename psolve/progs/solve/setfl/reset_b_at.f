      SUBROUTINE RESET_B_AT(NLINE)
      IMPLICIT NONE
!
! 1.  RESET_B_AT PROGRAM SPECIFICATION
!
! 1.1 Clear out batch mode parameterization for atmospheres and
!     set up default non-batch mode.
!
! 1.2 REFERENCES:
!
! 2.  RESET_B_AT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NLINE
!
! NLINE - Screen line at which to place the cursor
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
      integer*4 nl4
      INTEGER*4 I4P0
      DATA  I4P0 / 0 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  RESET_B_AT PROGRAM STRUCTURE
!
      nl4 = nline
      CALL SETCR_MN(I4P0,NL4)
      CALL clrtobot_mn()
!
!     Put one atmosphere epoch at each station
!
      BMODE_AT = .FALSE.
      DO I = 1,NUMSTA
        NUMATM(I) = 1
        IATSTR(I) = I-1
        TATM(I)   = TATM(1)
      ENDDO
!
!     Reset all atmosphere flag to zero.
!
      DO I = 1,7
        DO J = 1,3
          LATM(I,J) = 0
        ENDDO
      ENDDO
!
      RETURN
      END
