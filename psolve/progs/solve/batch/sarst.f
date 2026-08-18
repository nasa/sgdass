      SUBROUTINE SARST (SARREC,R_OR_W)
      IMPLICIT     NONE
!
! 1.  SARST PROGRAM SPECIFICATION
!
! 1.1
!   SARfile STate:  if R_OR_W(read or write) = 0, then SARST returns
!   the physical position of the last record in the type 2 file
!   This number is stored as the first 2 integers of
!   the first record of the SARFxx.
!
!                  if R_OR_W = 1, then the number in SARREC is written
!   to the first record of SARFxx, as the physical position of the last
!   record of the SARFxx.  This is necessary if the batch run crashes
!   during the least squares chain of programs after CMBIN has executed.
!
! 1.2 REFERENCES:
!
! 2.  SARST INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 SARREC
      INTEGER*2 R_OR_W
!
! R_OR_W - 0 for read or 1 for write position of last record
! SARREC - Position of last record in SARFxx
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'sareq.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rstors
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
      INTEGER*4 I4P1
      DATA I4P1 / 1 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SARST PROGRAM STRUCTURE
!
      CALL ACS_SARFIL('O')
!
!   process R_OR_W
!
      IF (R_OR_W .EQ. 0) THEN
!
!   read first record
!   first INTEGER*2 of first record contains record counter of # records in file
!
        CALL USE_SARFIL('R',I4P1)
        SARREC = N4BF(1)
      ELSE IF (R_OR_W .NE. 0) THEN
!
!   write SARREC to first integers of first record to be # records in file
!
        N4BF(1) = SARREC
        CALL USE_SARFIL('W',I4P1)
      END IF
!
!   close and end
!
      CALL ACS_SARFIL('C')
!
      RETURN
      END
