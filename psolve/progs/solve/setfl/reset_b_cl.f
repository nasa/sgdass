      SUBROUTINE RESET_B_CL(NLINE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RESET_B_CL PROGRAM SPECIFICATION
!
! 1.1 Clear out batch mode parameterization for clocks and set
!     up default non-batch mode.
!
! 1.2 REFERENCES:
!
! 2.  RESET_B_CL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NLINE
!
! NLINE - Line at which to leave screen cursor
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
      integer*4 nl4
      INTEGER*4 I4P0
      DATA  I4P0 / 0 /
      LOGICAL*2, EXTERNAL :: KBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SET_B_CL PROGRAM STRUCTURE
!
      nl4 = nline
      CALL SETCR_MN(I4P0,NL4 )
      CALL clrtobot_mn()
!
!     Put one clock epoch at each station
!
      BMODE_CL = .FALSE.
      DO I = 1,NUMSTA
         NUMCLK(I) = 1
         ICLSTR(I) = I-1
         FJDCL(I)  = FJDCL(1)
         CALL SBIT( ICLSTA(1,I), I, INT2(1) )
      ENDDO
!
!     Reset all clock flags to zero.
!
      DO I = 1,100
        LCLK(I) = 0
      ENDDO
!
!
      RETURN
      END
