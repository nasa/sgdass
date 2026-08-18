      SUBROUTINE CALC_BOUNDS(NNONE,IXFILE,IXBOUNDLO,IXBOUNDHI)
!
      IMPLICIT NONE
!
! 1.  CALC_BOUNDS PROGRAM SPECIFICATION
!
! 1.1 Find boundaries for the fields for displaying possible files
!     and choosing them via cursor positioning.
!
! 1.2 REFERENCES:
!
! 2.  CALC_BOUNDS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IXFILE                  !Lowest x coordinate of area for
!selecting files
      INTEGER*2 NNONE                     !Line position of field for not
!using any file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IXBOUNDLO(5)              !Lowest x coordinate of field for
!selecting a file
      INTEGER*2 IXBOUNDHI(5)              !Highest x coordinate of field
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: fledit
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IWIDTH                    !Width of field for selecting a
!given file
      INTEGER*2 I                         !Loop control
      INTEGER*2 IACCUM                    !Used to calculate each field's
!x coordinate boundaries
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   KDB  870507  Created
!
! 5.  CALC_BOUNDS PROGRAM STRUCTURE
!
      IACCUM = IXFILE
      DO I = 1,5
        IF (I .EQ. NNONE) THEN
          IWIDTH = 4
        ELSE
          IWIDTH = 10
        END IF
        IXBOUNDLO(I) = IACCUM
        IXBOUNDHI(I) = IACCUM + IWIDTH - 1
        IACCUM = IACCUM + IWIDTH + 2
      END DO
!
      RETURN
      END
