      SUBROUTINE BIN_READ ( FNAME, FILDES, IARR, JCOUNT )
      IMPLICIT NONE
! 
! 1.  BIN_READ PROGRAM SPECIFICATION
!
! 1.1 Read an array from the specified binary file
!
! 1.2 REFERENCES:
!
! 2.  BIN_READ INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 FILDES, JCOUNT
      CHARACTER FNAME*(*)
!
! FILDES - File descriptor of file to be read
! FNAME - Name of file from which to read
! JCOUNT - Number of blocks to read
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IARR(*)
!
! IARR - Array to be read into
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'fclib.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_read,fatal_file
!
! 3.  LOCAL VARIABLES
!
      CHARACTER ME*8, STR*128
      INTEGER*4 JERR, JBYTE
      INTEGER*8 BYTES_I8, READ_BYTES_I8
      DATA ME  / 'BIN_READ' /
      LOGICAL*2  KBIT
      INTEGER*4  IBLOCK, OFFSET, NUM_CHUNKS, CHUNK_I4
      ADDRESS__TYPE :: IADR
!
! JBYTE - Number of bytes to be read
! JERR - Number of bytes actually read, or error
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   AEE   920204      Removed hard coded path for fclib.i
!   pet   2006.12.21  Addded support of reading an arrays > 2Gb and < 4Gb
!
! 5.  BIN_READ PROGRAM STRUCTURE
!
!  READ IT
!
      IF ( KBIT( TEST_FIELD, INT2(1) ) ) THEN
           OFFSET = 1
           JBYTE = BLOCK_WORDS/4
           DO 410 IBLOCK=1,JCOUNT*WORD_BYTES*4
              JERR  = FC_READ ( FILDES, PTR_NC(IARR(OFFSET)), JBYTE )
              IF ( JERR .NE. JBYTE ) GOTO 810
              OFFSET = OFFSET + JBYTE/WORD_BYTES
 410       CONTINUE
 810       CONTINUE
         ELSE
           BYTES_I8 = INT8(JCOUNT)*INT8(BLOCK_WORDS)*INT8(WORD_BYTES)
           IF ( BYTES_I8 < 2147483647 ) THEN ! 2^31 -1
!
! ------------- If the Array less than 2Gb read it in one operation
!
                JBYTE = JCOUNT*BLOCK_WORDS*WORD_BYTES
                JERR  = FC_READ ( FILDES, PTR_NC(IARR), JBYTE )
              ELSE 
!
! ------------- Array is longer than 2Gb. We split it in 1Gb chunks and 
! ------------- read each chunk separately
!
                CHUNK_I4 = 1073741824
                IADR = LOC(IARR)
                NUM_CHUNKS = BYTES_I8/INT8(CHUNK_I4)
                READ_BYTES_I8 = 0
                DO 420 IBLOCK=1,NUM_CHUNKS
                   JERR  = FC_READ ( FILDES, IADR, CHUNK_I4 )
                   IF ( JERR .NE. CHUNK_I4 ) THEN
                        WRITE ( 6, * ) 'Expected number of bytes to be read:  ',JBYTE
                        WRITE ( 6, * ) 'The number of bytes actually has been read: ',JERR
                        WRITE ( 6, * ) 'Please run psolve_reset '//PRE_LETRS
                        JERR=-1
                        CALL FATAL_FILE ( JERR, 'a: incorrect transfer', FNAME, ME )
                   END IF
                   READ_BYTES_I8 = READ_BYTES_I8 + CHUNK_I4 
                   IADR = IADR + CHUNK_I4
 420            CONTINUE
                CHUNK_I4 = BYTES_I8 - READ_BYTES_I8 
                IF ( CHUNK_I4 > 0 ) THEN
!
! ------------------ Read remaining incomplete chunk
!
                     JERR  = FC_READ ( FILDES, IADR, CHUNK_I4 )
                     IF ( JERR .NE. CHUNK_I4 ) THEN
                          WRITE ( 6, * ) 'Expected number of bytes to be read:  ',JBYTE
                          WRITE ( 6, * ) 'The number of bytes actually has been read: ',JERR
                          JERR=-1
                          CALL FATAL_FILE ( JERR, 'b: incorrect transfer', FNAME, ME )
                     END IF
                     READ_BYTES_I8 = READ_BYTES_I8 + CHUNK_I4 
                END IF
                RETURN 
           END IF
      END IF
!
      IF ( JERR .NE. JBYTE ) THEN
           WRITE ( 6, * ) 'Expected number of bytes to be read:        ', JBYTE
           WRITE ( 6, * ) 'The number of bytes actually has been read: ', JERR
           WRITE ( 6, * ) 'Please run command psolve_reset '//PRE_LETRS
           JERR=-1
           CALL FATAL_FILE ( JERR, 'incorrect transfer', FNAME, ME )
      ENDIF
!
      CALL FATAL_FILE ( JERR, 'reading', FNAME, ME )
!
      RETURN
      END  !#!  BIN_READ  #!#
