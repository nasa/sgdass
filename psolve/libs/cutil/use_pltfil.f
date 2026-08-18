      SUBROUTINE USE_PLTFIL(STRING)
      IMPLICIT NONE
!
! 1.   USE_PLTFIL PROGRAM SPECIFICATION
!
! 1.1 Access utility for PLTFIL.  Passes control through
!     to USE_FILE.
!
! 1.2 REFERENCES:
!
! 2.  USE_PLTFIL INTERFACE
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
      INCLUDE 'pltfl.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*(NAME_SIZE) FNAME
      Integer*4 fildes
      INTEGER*4 I4P1
      DATA  I4P1 / 1 /
      Save fildes
!
! FILDES - File descriptor
! FNAME - Name of file to be accessed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_PLTFIL PROGRAM STRUCTURE
!
! First construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'PLTF'//PRE_LETRS
!
! Now access the file
!
      CALL USE_FILE(FNAME,fildes,IPLTFIL,JPLTFIL_BLOCKS,I4P1,STRING)
!
      RETURN
      END
