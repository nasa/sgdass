      SUBROUTINE SET_TESTV(STATE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SET_TESTV PROGRAM SPECIFICATION
!
! 1.1 Set test version control bit and logical in precm.
!
! 1.2 REFERENCES:
!
! 2.  SET_TESTV INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      logical*2 state
!
! STATE - New value to be set (TRUE if test version of program is to be run).
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!    WHO   WHEN   WHAT
!
! 5.  SET_TESTV PROGRAM STRUCTURE
!
      KTESTV=STATE
      IF(KTESTV) THEN
        CALL SBIT( PRE_IP(2), INT2(1), INT2(1) )
      ELSE
        CALL SBIT( PRE_IP(2), INT2(1), INT2(0) )
      ENDIF
      PRE_INTERA=PRE_IP(2)
!
      RETURN
      END
