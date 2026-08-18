      SUBROUTINE SETSEC(SEC)
      IMPLICIT NONE
!
! 1.  SETSEC PROGRAM SPECIFICATION
!
! 1.1 Set to next section in DCALI.
!
! 1.2 REFERENCES:
!
! 2.  SETSEC INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 SEC
!
! SEC - Section position to position to. If 0 then go to first section
!
! 2.3 OUTPUT Variables:
!
! SEC - Index of the next section; negative if there are no more sections
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: fndcls
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SETSEC PROGRAM STRUCTURE
!
!
      IF(SEC.EQ.0) THEN
        SEC=1
      ELSE IF(SEC.GT.0) THEN
        DO WHILE(IACALI(SEC).GT.0)
          SEC=SEC+IACALI(SEC)*4+1
        ENDDO
        IF(IACALI(SEC).LT.0) THEN
          SEC=SEC+ABS(IACALI(SEC))*4+1
        ENDIF
      ENDIF
!
      IF(SEC.GT.0) THEN
        IF(IACALI(SEC).EQ.0) SEC=-SEC
      ENDIF
!
      RETURN
      END
