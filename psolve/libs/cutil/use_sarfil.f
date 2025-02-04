      SUBROUTINE USE_SARFIL ( STRING, NUM )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_SARFIL PROGRAM SPECIFICATION
!
! 1.1 Read or write SARFIL.  Program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_SARFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NUM
      CHARACTER*(*) STRING
      CHARACTER  STR*256
!
! NUM - Record to READ or WRITE; -1 = next record.
! STRING - Requested access type ('R'=read; 'W'=write)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'sareq.i'
      INTEGER*4 FILDES,NEXT_REC
      CHARACTER*(NAME_SIZE) FNAME
      COMMON  / SAVSAR / FNAME, FILDES, NEXT_REC
      SAVE    / SAVSAR /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,ferr
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*4 REC, IREC, IERR
!
      DATA ME/'USE_SARFIL'/
!
! IERR - IOSTAT return from READ, WRITE
! ME - Name of this routine
! REC - Record number to be accessed
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   pety  2023.07.31  Improved code
!
! 5.  USE_SARFIL PROGRAM STRUCTURE
!
! Check for invalid input STRING
!
      IF ( LEN(STRING) .NE. 1  .OR. INDEX('RW',STRING) .EQ. 0 ) THEN
           CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING' )
           CALL EXIT ( 1 )
      ENDIF
!
!  Position file
!
      REC = NUM
      IF ( REC .LT. 0 ) REC = NEXT_REC
!
      IF ( STRING .EQ. 'R' ) THEN
!
! -------- Read
!
           READ(FILDES,REC=REC,IOSTAT=IERR) IABF
           IF ( IERR .NE. 0 ) CALL FERR ( INT2(IERR), 'Reading SARFIL', &
     &                                    INT2(0), INT2(0) )
        ELSE
!
! -------- WRITE
!
           WRITE(FILDES,REC=REC,IOSTAT=IERR) IABF
           IF ( IERR .NE. 0 ) CALL FERR ( INT2(IERR), 'Writing SARFIL', &
     &                                    INT2(0), INT2(0) )
      ENDIF
!
      NEXT_REC = REC + 1
!
      RETURN
      END  SUBROUTINE  USE_SARFIL  !#!#
