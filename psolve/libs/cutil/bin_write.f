      SUBROUTINE BIN_WRITE(FNAME,FILDES,IARR,JCOUNT)
      IMPLICIT NONE
!
! 1.  BIN_WRITE PROGRAM SPECIFICATION
!
! 1.1 Write an array to the specified binary file
!
! 1.2 REFERENCES:
!
! 2.  BIN_WRITE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 FILDES,JCOUNT
      INTEGER*2 IARR(*)
      CHARACTER*(*) FNAME
!
! FILDES - File descriptor of file to be written to
! FNAME - Name of file to be written to
! IARR - The array to be written
! JCOUNT - Number of blocks to be written
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_write,fatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*9 ME
      INTEGER*4 JERR, JBYTE
      INTEGER*8 BYTES_I8, WRITE_BYTES_I8
      DATA ME/'BIN_WRITE'/
      LOGICAL*2  KBIT
      INTEGER*4  IBLOCK, OFFSET, NUM_CHUNKS, CHUNK_I4
      ADDRESS__TYPE :: IADR
!
! JBYTE - Number of bytes to be written
! JERR - Number of bytes actually written, or error return
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!   pet   2006.12.21  Addded support of reading an arrays > 2Gb and < 4Gb
!
! 5.  BIN_WRITE PROGRAM STRUCTURE
!
! Calculate number of bytes to write
!
!
! Write array to file
!
      IF ( KBIT( TEST_FIELD, INT2(1) ) ) THEN
           OFFSET = 1
           JBYTE = BLOCK_WORDS/4
           DO 410 IBLOCK=1,JCOUNT*WORD_BYTES*4
              JERR  = FC_WRITE ( FILDES, PTR_NC(IARR(OFFSET)), JBYTE )
              IF ( JERR .NE. JBYTE ) GOTO 810
              OFFSET = OFFSET + JBYTE/WORD_BYTES
 410       CONTINUE
 810       CONTINUE
         ELSE
           BYTES_I8 = INT8(JCOUNT)*INT8(BLOCK_WORDS)*INT8(WORD_BYTES)
           IF ( BYTES_I8 < 2147483647 ) THEN ! 2^31 -1
!
! ------------- If the array less than 2Gb write it in one operation
!
                JBYTE=JCOUNT*BLOCK_WORDS*WORD_BYTES
                JERR = FC_WRITE ( FILDES, PTR_NC(IARR), JBYTE )
              ELSE 
!
! ------------- Array is longer than 2Gb. We split it in 1Gb chunks and 
! ------------- write each chunk separately
!
                CHUNK_I4 = 1073741824
                IADR = LOC(IARR)
                NUM_CHUNKS = BYTES_I8/INT8(CHUNK_I4)
                WRITE_BYTES_I8 = 0
                DO 420 IBLOCK=1,NUM_CHUNKS
                   JERR  = FC_WRITE ( FILDES, IADR, CHUNK_I4 )
                   IF ( JERR .NE. CHUNK_I4 ) THEN
                        WRITE ( 6, * ) 'Expected number of bytes to be read:  ',JBYTE
                        WRITE ( 6, * ) 'The number of bytes actually has been read: ',JERR
                        WRITE ( 6, * ) 'Please run command psolve_reset '//PRE_LETRS
                        JERR=-1
                        CALL FATAL_FILE ( JERR, 'a: incorrect transfer', FNAME, ME )
                   END IF
                   WRITE_BYTES_I8 = WRITE_BYTES_I8 + CHUNK_I4 
                   IADR = IADR + CHUNK_I4
 420            CONTINUE
!
                CHUNK_I4 = BYTES_I8 - WRITE_BYTES_I8 
                IF ( CHUNK_I4 > 0 ) THEN
!
! ------------------ Write remaining incomplete chunk
!
                     JERR  = FC_WRITE ( FILDES, IADR, CHUNK_I4 )
                     IF ( JERR .NE. CHUNK_I4 ) THEN
                          WRITE ( 6, * ) 'Expected number of bytes to be read:  ',JBYTE
                          WRITE ( 6, * ) 'The number of bytes actually has been read: ',JERR
                          WRITE ( 6, * ) 'Please run command psolve_reset '//PRE_LETRS
                          JERR=-1
                          CALL FATAL_FILE ( JERR, 'b: incorrect transfer', FNAME, ME )
                     END IF
                     WRITE_BYTES_I8 = WRITE_BYTES_I8 + CHUNK_I4 
                END IF
                RETURN 
           END IF
      END IF
!
! --- Report any problems encountered
!
      CALL FATAL_FILE ( JERR, 'writing', FNAME, ME )
!
      IF ( JERR .NE. JBYTE ) THEN
           JERR = -1
           CALL FATAL_FILE ( JERR, 'incorrect transfer', FNAME, ME )
      ENDIF
!
      END  SUBROUTINE  !#!# BIN_READ  #!#!
