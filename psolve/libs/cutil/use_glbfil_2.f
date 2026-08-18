      SUBROUTINE USE_GLBFIL_2 ( STRING )
      IMPLICIT NONE
!
! 1.  USE_GLBFIL_2 PROGRAM SPECIFICATION
!
! 1.1 Access utility for GLBFIL section 2.  Pass control through
!     to USE_FILE.
!
! 1.2 REFERENCES:
!
! 2.  USE_GLBFIL_2 INTERFACE
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
      INCLUDE 'glbc2.i'
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
! FNAME - Name of file to be accessed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_GLBFIL_2 PROGRAM STRUCTURE
!
! --- First construct name of file to be accessed
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'GLBF'//PRE_LETRS
!
! --- Now access it
!
      CALL USE_FILE ( FNAME, FILDES, FIRST_GLBC2_I2, JGLBC2_BLOCKS, &
     &                JGLBC2_POS, STRING )
!
      RETURN
      END  !#!  USE_GLBFIL_2  #!#
