      SUBROUTINE USE_FILE8 ( FNAME, FILDES, IARR, RECS8, POS8, STRING )
      IMPLICIT NONE
!
! 1.  USE_FILE PROGRAM SPECIFICATION
!
! 1.1 Access utility for files. Program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_FILE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER FNAME*(*), STRING*(*)
      INTEGER*1 IARR(*)
      INTEGER*4 FILDES
      INTEGER*8 RECS8, POS8
      INTEGER*8, EXTERNAL :: LSEEK
!
! FILDES - File descriptor
! FNAME - File name
! IARR - Array to be read or written
! POS8 - Record at which file is to be positioned
! RECS8 - Number of records to read or write
! STRING - Requested access type ('O'=open; 'R'=read;
!           'W'=write; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: bin_open,bin_seek,bin_read,bin_write,bin_close
!
! 3.  LOCAL VARIABLES
!
      CHARACTER  ME*9, STR*128 
      INTEGER*2  COUNT, MCOUNT, IL
      INTEGER*4  LN, LSEEK_SET
      INTEGER*8  OFF8, IS8
      CHARACTER*1 TOKEN
      LOGICAL*2 OKAY
      DATA   ME /'USE_FILE8' /
      INTEGER*2, EXTERNAL :: TRIMLEN
      INTEGER*8, EXTERNAL :: READ64, WRITE64
!
! COUNT - Counter for STRING character being processed
! MCOUNT - Number of characters in STRING
! ME - Name of this routine
! TOKEN - Single character from STRING
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_FILE PROGRAM STRUCTURE
!
      MCOUNT = LEN(STRING)
      COUNT=1
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', LSEEK_SET, LN ) 
      DO WHILE ( COUNT .LE. MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
 710     CONTINUE
!
! ----- Open
!
         IF ( TOKEN .EQ. 'O' ) THEN
              CALL BIN_OPEN(FNAME,FILDES,'O')
           ELSE IF ( TOKEN .EQ. 'R' ) THEN
! 
! ----------- Read
! 
              OFF8 = (POS8-1)*256
              IS8 = LSEEK ( %VAL(FILDES), %VAL(OFF8), %VAL(LSEEK_SET) )
              IF ( IS8 < 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Error in seek  of file '// &
     &                                FNAME(1:TRIMLEN(FNAME)) )
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
                ELSE IF ( IS8 .NE. OFF8 ) THEN
                   CALL IINCH8 ( OFF8, STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Could not seek '// &
     &                 'position '//STR(1:TRIMLEN(STR))//' of file '// &
     &                  FNAME(1:TRIMLEN(FNAME)) )
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
              END IF
              IS8 = READ64 ( FILDES, IARR, INT8(256)*RECS8 )
              IF ( IS8 < 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Error in reading file '// &
     &                                FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
                ELSE IF ( IS8 .NE. INT8(256)*RECS8 ) THEN
                   CALL CLRCH  ( STR )
                   CALL IINCH8 ( INT8(256)*RECS8, STR )
                   write ( 6, * ) 'is8 = ', is8, ' re= ', INT8(256)*RECS8 !%%%
                   CALL FILE_REPORT ( FNAME, ME, 'Could not read '// &
     &                  ' '//STR(1:TRIMLEN(STR))//' bytes from file '// &
     &                  FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
              END IF
           ELSE IF ( TOKEN .EQ. 'W' ) THEN
! 
! ----------- Read
! 
              OFF8 = (POS8-1)*256
              IS8 = LSEEK ( %VAL(FILDES), %VAL(OFF8), %VAL(LSEEK_SET) )
              IF ( IS8 < 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Error in seek  of file '// &
     &                                FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
                ELSE IF ( IS8 .NE. OFF8 ) THEN
                   CALL IINCH8 ( OFF8, STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Could not seek '// &
     &                 'position '//STR(1:TRIMLEN(STR))//' of file '// &
     &                  FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
              END IF
              IS8 = WRITE64 ( FILDES, IARR, INT8(256)*RECS8 )
              IF ( INT8(256)*RECS8 < 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL GERROR ( STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Error in writing to file '// &
     &                                FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
                ELSE IF ( IS8 .NE. INT8(256)*RECS8 ) THEN
                   CALL CLRCH  ( STR )
                   CALL IINCH8 ( INT8(256)*RECS8, STR )
                   CALL FILE_REPORT ( FNAME, ME, 'Could not write '// &
     &                  ' '//STR(1:TRIMLEN(STR))//' bytes to file '// &
     &                  FNAME(1:TRIMLEN(FNAME)) )
!
! ---------------- Deliberately crash the process to unwind the stack of calls
!
                   IS8 = -1 ; WRITE ( 6, * ) IARR(IS8) ! Deliberately crash in order to unwind stack
              END IF
          ELSE IF(TOKEN.EQ.'C') THEN
!
! ---------- Close
!
             CALL BIN_CLOSE ( FNAME, FILDES )
          ELSE
!
! ---------- Unkown control
!
             CALL FILE_REPORT ( FNAME, ME, 'Unknown control' )
             GOTO 710
         ENDIF
         COUNT = COUNT + 1
      ENDDO
!
      RETURN
      END  SUBROUTINE  USE_FILE8  !#!#
