      SUBROUTINE BIN_TELL ( FNAME, FILDES, JREC )
      IMPLICIT NONE
!
! 1.  BIN_TELL PROGRAM SPECIFICATION
!
! 1.1 Find out where in a file we are currently positioned
!
! 1.2 REFERENCES:
!
! 2.  BIN_TELL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 FILDES
      CHARACTER*(*) FNAME
!
! FILDES - File descriptor of file to be checked
! FNAME - Name of file to be checked
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 JREC
!
! JREC - Record number at which we are positioned
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_lseek,fatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 ME
      INTEGER*4  JERR, OFFSET, WHENCE
      DATA ME/'BIN_TELL'/
      INTEGER*8   POS_I8, REC_I8
!
! JERR - Current file position (byte), or error return
! ME - Name of this routine
! OFFSET - Amount by which to change file position
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed haed coded path for fclib.i
!   pet   2006.12.22  Updated for support case of files larger than 2Gb
!
! 5.  BIN_TELL PROGRAM STRUCTURE
!
! WHERE ARE WE CURRENTLY POSITIONED
!
      OFFSET= 0
      WHENCE = 1
      JERR=FC_LSEEK(FILDES,OFFSET,WHENCE)
      IF ( JERR == -1 ) THEN
           CALL FATAL_FILE ( JERR, 'telling', FNAME, ME )
      END IF
!
      IF ( JERR .GE. 0 ) THEN
           JREC=1+(JERR/(BLOCK_WORDS*WORD_BYTES))
!
          IF ( JERR .NE. (JREC-1)*BLOCK_WORDS*WORD_BYTES ) THEN
               CALL FATAL_FILE ( JERR, 'impossible position', FNAME, ME )
          ENDIF
        ELSE
#ifdef GNU
          POS_I8 = 4294967296_8 + JERR
#else
          POS_I8 = 4294967296 + JERR
#endif
          REC_I8 = 1 + ( POS_I8 /(BLOCK_WORDS*WORD_BYTES) )
          JREC = REC_I8 
          IF ( POS_I8 .NE. (REC_I8-1)*BLOCK_WORDS*WORD_BYTES ) THEN
               CALL FATAL_FILE ( JERR, 'impossible position', FNAME, ME )
          ENDIF
      ENDIF
!
      RETURN
      END  SUBROUTINE  !#!#  BIN_TELL  !#!#
