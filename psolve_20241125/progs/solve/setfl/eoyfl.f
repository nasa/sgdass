      SUBROUTINE EOYFL(IFIRST,ILAST)
      IMPLICIT NONE
!
! 1.  EOYFL PROGRAM SPECIFICATION
!
! 1.2 REFERENCES:
!
! 2.  EOYFL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IFIRST(3)
      INTEGER*2 ILAST(3)
!
! IFIRST - Starting coords for x-wobble, y and UT1 sections
! ILAST  - Ending coords for x-wobble, y and UT1 sections
!
! 2.3 OUTPUT Variables:
!
! IFIRST - Starting coords for x-wobble, y and UT1 sections
! ILAST  - Ending coords for x-wobble, y and UT1 sections
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: nmflg,rotfl,rotin
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IEOP,IXYU,ISTART,IFINISH,INUMBER
!
! 4.  HISTORY
!   WHO  WHEN      WHAT
!   KDB  08/3/90   determine the y-coordinates of the
!                  first and last lines of the
!                  earth orientation sections displayed on the last page.
!   KDB  11/29/90  Modified to handle re-designed last page.
!
! 5.  EOYFL PROGRAM STRUCTURE
!
      ISTART = 1 !x-wobble starts on the line whose y-coord is 1
!
      DO IXYU = 1,3
        IF (IXYU .LE. 2) THEN
          IEOP = 1
        ELSE
          IEOP = 2
        END IF
        IF (IXYU .EQ. 2 .AND. EOP_STYLE(IEOP) .EQ. 1) THEN
!
!         In the new-style mode, one section is used to display both x-wobble
!         and y-wobble.
!
          IFIRST(IXYU) = IFIRST(1)
          ILAST(IXYU) = ILAST(1)
          ISTART = ILAST(IXYU) + 1
        ELSE
          IFIRST(IXYU) = ISTART
          IF (EOP_STYLE(IEOP) .EQ. 1) THEN
            INUMBER = 3
          ELSE
            INUMBER = NROT
          END IF
          ILAST(IXYU) = IFIRST(IXYU) + INUMBER - 1
          ISTART = IFIRST(IXYU) + INUMBER
        END IF
      END DO
!
      RETURN
      END
