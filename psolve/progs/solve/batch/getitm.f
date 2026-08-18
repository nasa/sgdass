      SUBROUTINE GETITM(SEC,FRAME,ITEM,CAL)
      IMPLICIT NONE
!
! 1.  GETITM PROGRAM SPECIFICATION
!
! 1.1 Get item from frame.
!
! 1.2 REFERENCES:
!
! 2.  GETITM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 SEC,FRAME
!
! FRAME - Frame number
! SEC - Section number
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) CAL
      INTEGER*2 ITEM
!
! CAL - Calibration name
! ITEM - Item number within frame
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'solve.i'
      INCLUDE 'dcali.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: calsfnd,fndcls
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 CTOK
      INTEGER*2 ITOK(4),I
      EQUIVALENCE (CTOK,ITOK(1))
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GETITM PROGRAM STRUCTURE
!
!  ITEM <= 0 IS AN IMPLICIT ELSE
!
      IF(SEC.LE.0.OR.FRAME.LE.0) THEN
        ITEM=-1
      ELSE IF(ITEM.GE.0) THEN
        IF(ABS(IACALI(FRAME)).LE.ITEM) THEN
          ITEM=-ITEM
        ELSE
          DO I=1,4
            ITOK(I)=IACALI(FRAME+4*ITEM+I)
          ENDDO
          ITEM=ITEM+1
          CAL=CTOK
        ENDIF
      ENDIF
!
      RETURN
      END
