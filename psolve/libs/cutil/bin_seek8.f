      SUBROUTINE BIN_SEEK8 ( FNAME, FILDES, JREC )
      IMPLICIT NONE
!
! 1.  BIN_SEEK PROGRAM SPECIFICATION
!
! 1.1 Position at the specified record of the specified file
!
! 1.2 REFERENCES:
!
! 2.  BIN_SEEK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4     FILDES
      INTEGER*8     JREC
      CHARACTER*(*) FNAME
!
! FILDES - File descriptor of file to be positioned
! FNAME - Name of file to be positioned
! JREC - Record number to be sought
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_lseek, fatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 ME, STR1*12, STR2*12, STR3*12, STR4*12
      INTEGER*4   WHENCE
      INTEGER*8   JERR, OFFSET
      DATA ME  / 'BIN_SEEK' /
      INTEGER*8, EXTERNAL :: LSEEK
!
! JERR - Value returned from fc_lseek
! ME - Name of this routine
! OFFSET - File offset calculated from record number
! WHENCE - Flag to tell fc_lseek to position in file, not just check position
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!   PET   980728 Made error messages more verbose. Error condition may occur
!                when JREC < 1
!   pet   2006.12.22  Updated for support case of files larger than 2Gb
!   pet   2021.07.12  Updated for INTEGER*8
!
! 5.  BIN_SEEK PROGRAM STRUCTURE
!
! POSITION TO BEGINNING OF REQUESTED RECORD
!
      OFFSET = INT8(JREC-1)*INT8(BLOCK_WORDS)*INT8(WORD_BYTES)
      WHENCE = 0
      JERR   = LSEEK ( %VAL(FILDES), %VAL(OFFSET), %VAL(WHENCE) )
!
      IF ( JERR == -1 ) THEN
           STR1 = '            '
           STR2 = '            '
           STR3 = '            '
           WRITE ( UNIT=STR1, FMT='(I12)' ) JREC
           WRITE ( UNIT=STR2, FMT='(I12)' ) OFFSET
           WRITE ( UNIT=STR3, FMT='(I12)' ) JERR
           WRITE ( UNIT=STR4, FMT='(I12)' ) FILDES
!
           CALL FATAL_FILE ( JERR, 'seeking. offset = '//STR1//' records, '// &
     &          STR2//' bytes; lseek returned '//STR3//' fildes: '//STR4// &
     &          ' File =', FNAME, ME )
      END IF
!
      RETURN
      END  !#!  BIN_SEEK8  #!#
