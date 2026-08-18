      SUBROUTINE USE_GLBFIL_4 ( STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_GLBFIL_4 PROGRAM SPECIFICATION
!
! 1.1 Access utility for GLBFIL section 4.  Passes control through
!     to USE_FILE.
!
! 1.2 REFERENCES:
!
! 2.  USE_GLBFIL_4 INTERFACE
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
      INCLUDE 'glbc4.i'
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
      INTEGER*4  ACTUAL_LEN, DECLARED_LEN
!
!
! FNAME - Name of the file to be accessed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   pet   17-MAR-97  Added error handler
!   pet   29-SEP-97  Corrected a bug connected with uncorrect pointing to the
!                    beginning of the glbc4 common area
!
! 5.  USE_GLBFIL_4
!
! First construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'GLBF'//PRE_LETRS
!
! --- Test the length of the block
!
      ACTUAL_LEN   = LOC(ENDMARK_GLBC4_I4) - LOC(BEGMARK_GLBC4_I4) + 4
      DECLARED_LEN = JGLBC4_BLOCKS * BLOCK_BYTES
      IF ( ACTUAL_LEN .NE. DECLARED_LEN ) THEN
           WRITE ( 6, * ) ' ACTUAL_LEN   = ',ACTUAL_LEN
           WRITE ( 6, * ) ' DECLARED_LEN = ',DECLARED_LEN
           call ferr( INT2(88), &
     &         "Fatal error in use_glbfil_4: actual length "// &
     &         "of common area GLBC4 doesn not coincide with "// &
     &         "constant JGLBC4_BLOCKS declared in solve.i "// &
     &         "Hint: change properly parameter "//"LEN_GLBC4_FILLER in glbc4.i", &
     &          INT2(0), INT2(0) )
           STOP 'use_glbfil_4'
      END IF
!
! --- Now access the file
!
      CALL USE_FILE ( FNAME, FILDES, BEGMARK_GLBC4_I4, JGLBC4_BLOCKS, &
     &                JGLBC4_POS, STRING )
!
      RETURN
      END  !#!  USE_GLBFIL_4  #!#
