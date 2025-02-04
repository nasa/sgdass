      SUBROUTINE USE_PLIST(STRING)
      IMPLICIT NONE
!
! 1.  USE_PLIST PROGRAM SPECIFICATION
!
! 1.1 Access utility for PLIST; passes control through to USE_FILE.
!      (accesses file COMMxx)
!
! 1.2 REFERENCES:
!
! 2.  USE_PLIST INTERFACE
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
      INCLUDE 'plist.i'
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*4 FILDES
      COMMON /SAVPLS/ FILDES
      SAVE /SAVPLS/
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
!   WHO   WHEN   WHAT
!
! 5.  USE_PLIST PROGRAM STRUCTURE
!
! Construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PLST'//PRE_LETRS
!
! Access the file
!
      CALL USE_FILE(FNAME,FILDES,parm_names,JPLIST_BLOCKS,I4P1,STRING)
!
      RETURN
      END
