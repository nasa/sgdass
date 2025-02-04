      SUBROUTINE USE_GLBFIL_3(STRING)
      IMPLICIT NONE
!
! 1.  USE_GLBFIL_3 PROGRAM SPECIFICATION
!
! 1.1 Access utility for GLBFIL section 3.  Passes control through
!     to USE_FILE.
!
! 1.2 REFERENCES:
!
! 2.  USE_GLBFIL_3 INTERFACE
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
      INCLUDE 'glbc3.i'
      INTEGER*4 FILDES
      COMMON /SAVGLB/ FILDES
      SAVE /SAVGLB/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*(NAME_SIZE) FNAME
!
! FNAME - Name of the file to be accessed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_GLBFIL_3 PROGRAM STRUCTURE
!
! First construct the name of the file
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'GLBF'//PRE_LETRS
!
! Now access the file
!
      CALL USE_FILE ( FNAME, FILDES, BEGMARK_GLBC3_I2, JGLBC3_BLOCKS, &
     &                JGLBC3_POS, STRING )
!
      RETURN
      END
