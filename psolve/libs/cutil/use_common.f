      SUBROUTINE USE_COMMON(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_COMMON PROGRAM SPECIFICATION
!
! 1.1 Access utility for COMMON; passes control through to USE_FILE.
!      (accesses file COMMxx)
!
! 1.2 REFERENCES:
!
! 2.  USE_COMMON INTERFACE
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
      INCLUDE 'erm.i'
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      CHARACTER FNAME*(NAME_SIZE)
      INTEGER*4 FILDES
      COMMON  / SAVCOM / FILDES
      SAVE    / SAVCOM /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
      INTEGER*4 I4P1, ACTUAL_LEN, DECLARED_LEN
      DATA  I4P1 / 1 /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_COMMON PROGRAM STRUCTURE
!CCCCC
!
! --- Construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'COMM'//PRE_LETRS
!
! --- Test the length of the block
!
      ACTUAL_LEN   = LOC(SOCOM_LAST_I2) - LOC(ISOCOM(1)) + 2
      DECLARED_LEN = JSOCOM_BLOCKS * BLOCK_BYTES
      IF ( ACTUAL_LEN .NE. DECLARED_LEN ) THEN
           WRITE ( 6, * ) ' ACTUAL_LEN   = ',ACTUAL_LEN
           WRITE ( 6, * ) ' DECLARED_LEN = ',DECLARED_LEN
           call ferr( INT2(88), "Fatal error in use_common: actual length "// &
     &         "of common area socom does not coincide with "// &
     &         "constant JSOCOM_BLOCKS declared in solve.i "// &
     &         "Hint: change properly parameter "//"ifree_len in socom.i", &
     &          INT2(0), INT2(0) )
           STOP 'use_common'
      END IF
!
! --- Access to the file
!
      CALL USE_FILE ( FNAME, FILDES, ISOCOM, JSOCOM_BLOCKS, I4P1, STRING )
!
      RETURN
      END  !#!  USE_COMMON  #!#
