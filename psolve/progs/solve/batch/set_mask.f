      SUBROUTINE SET_MASK()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SET_MASK PROGRAM SPECIFICATION
!
! 1.1 Set WVR mask.
!
! 1.2 REFERENCES:
!
! 2.  SET_MASK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bwvrm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
      LOGICAL*2 EQUAL
      REAL*8 MASK
!
! 4.  HISTORY
!    WHO   WHEN   WHAT
!
! 5.  SET_MASK PROGRAM STRUCTURE
!
! Loop over stations
!
      DO I=1,NUMSTA
!
! Set default mask
!
        MASK=WVMDEF
        DO J=1,NWVM
!
! Pick up any exceptions
!
          IF(EQUAL( IWVMNM(1,J), INT2(1), ISITN(1,I), INT2(1), INT2(8) )) THEN
            MASK=WVM(J)
            GO TO 220
          ENDIF
        ENDDO
220     CONTINUE
!
! Assign mask for this station
!
        WVMASK(I)=MASK
      ENDDO
!
      RETURN
      END
