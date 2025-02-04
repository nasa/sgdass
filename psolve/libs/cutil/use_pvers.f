      SUBROUTINE USE_PVERS(STRING)
      IMPLICIT NONE
!
! 1.  USE_PVERS PROGRAM SPECIFICATION
!
! 1.1 Access utility for program versions; passes control through to USE_FILE.
!      (accesses file VERSxx)
!
! 1.2 REFERENCES:
!
! 2.  USE_PVERS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Requested access type
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'pvers.i'
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*4 FILDES
      EXTERNAL  INIT_PVERS
      COMMON  / SAVVER / FILDES
      SAVE    / SAVVER /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
      INTEGER*4 I4P1
      DATA  I4P1 / 1 /
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!
!   pet  1999.10.11  Re-wrote
!
! 5.  USE_PVERS PROGRAM STRUCTURE
!
! Construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'VERS'//PRE_LETRS
!
! --- Access the file
!
      CALL USE_FILE ( FNAME, FILDES, PVERS_FIRST, JPVERS_BLOCKS, I4P1, STRING )
!
      RETURN
      END
