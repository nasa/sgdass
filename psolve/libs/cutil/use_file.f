      SUBROUTINE USE_FILE(FNAME,FILDES,IARR,JRECS,JPOS,STRING)
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
      CHARACTER*(*) FNAME,STRING
      INTEGER*2 IARR(*)
      INTEGER*4 FILDES,JRECS,JPOS
!
! FILDES - File descriptor
! FNAME - File name
! IARR - Array to be read or written
! JPOS - Record at which file is to be positioned
! JRECS - Number of records to read or write
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
      CHARACTER*8 ME
      INTEGER*2 COUNT,MCOUNT,IL,TRIMLEN
      CHARACTER*1 TOKEN
      LOGICAL*2 OKAY
      DATA ME/'USE_FILE'/
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
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
        TOKEN=STRING(COUNT:COUNT)
1       CONTINUE
!
!     Open
!
        IF(TOKEN.EQ.'O') THEN
          CALL BIN_OPEN(FNAME,FILDES,'O')
!
!     Read
!
        ELSE IF(TOKEN.EQ.'R') THEN
          CALL BIN_SEEK(FNAME,FILDES,JPOS)
          CALL BIN_READ(FNAME,FILDES,IARR,JRECS)
!
!  WRITE
!
        ELSE IF(TOKEN.EQ.'W') THEN
          CALL BIN_SEEK(FNAME,FILDES,JPOS)
          CALL BIN_WRITE(FNAME,FILDES,IARR,JRECS)
!
!  CLOSE
!
        ELSE IF(TOKEN.EQ.'C') THEN
          CALL BIN_CLOSE(FNAME,FILDES)
!
!  UNKOWN CONTROL
!
        ELSE
          CALL FILE_REPORT(FNAME,ME,'UNKNOWN CONTROL')
          GO TO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END
