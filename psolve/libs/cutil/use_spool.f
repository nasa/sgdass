      SUBROUTINE USE_SPOOL ( STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_SPOOL PROGRAM SPECIFICATION
!
! 1.1 Access utility for spool file.  Requires that SOCOM be loaded
!     before a call for 'O', and that SOCOM be written out after
!     a call for 'C'.  It also requires PRE_PROG to have been used.
!     Program pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_SPOOL INTERFACE
!
! 2.1 Parameter File
!
      INCLUDE 'precm.i'
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Requested access type: O - open
!                                 I - initialize
!                                 C - close
!                                 S - seek end of file
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fcfreopen
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 COUNT, MCOUNT, IDUM
      INTEGER*4 IERR, DUM, OFFSET, WHENCE, FD, UNIT_TO_FILDESC
      CHARACTER ERRSTR*130, TOKEN*1
!
! COUNT - Number of STRING character currently being processed
! IDUM - Dummy to read into while seeking end of file
! IERR - IOSTAT return from OPEN, REWIND, etc.
! IL - Length of file name
! MCOUNT - Number of characters in STRING
! TOKEN - Single character from STRING being processed
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   AEE   910515      Enhanced error messages written to the error file.
!   MWH   910912      Use lseek instead of freopen for append mode
!   pet   2001.12.13  Changed the logic: now use_spool takes the name of the
!                     spool file from precm common block:
!                     PRE_SPL_NAM, PRE_SPL_LEN
!
! 5.  USE_SPOOL PROGRAM STRUCTURE
!
! Build name of spool file
!
!
! --- Loop through characters in input STRING
!
      MCOUNT=LEN(STRING)
      DO WHILE ( COUNT .LE. MCOUNT )
         TOKEN=STRING(COUNT:COUNT)
 910     CONTINUE
         IF ( TOKEN .EQ. 'O' ) THEN
!
! ----------- Open
!
              OPEN ( UNIT=23, FILE=PRE_SPL_NAM(1:PRE_SPL_LEN), STATUS='UNKNOWN', &
     &               IOSTAT=IERR )
              IF ( IERR .NE. 0)  THEN
                    WRITE ( ERRSTR, "('Error ',I7,' opening ',A)") IERR, &
     &                      PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(233), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              ENDIF
!
              FD = UNIT_TO_FILDESC(23)
              OFFSET=0
              WHENCE=2
#ifdef GNU
              CALL FSEEK ( 23, 0, 2, DUM )
#else
              DUM = FC_LSEEK  ( FD, OFFSET, WHENCE )
#endif
              CALL FATAL_FILE ( DUM, 'seeking', PRE_SPL_NAM(1:PRE_SPL_LEN), &
     &                               'ftn_open' )
!
! ----------- Initialize
!
            ELSE IF ( TOKEN .EQ. 'A' ) THEN
              OPEN ( UNIT=23, FILE=PRE_SPL_NAM(1:PRE_SPL_LEN), STATUS='UNKNOWN', &
     &               ACCESS='APPEND', IOSTAT=IERR )
              IF ( IERR .NE. 0)  THEN
                    WRITE ( ERRSTR, "('Error ',I7,' opening ',A)") IERR, &
     &                      PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(233), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              ENDIF
            ELSE IF ( TOKEN .EQ. 'I' ) THEN
              REWIND ( UNIT=23, IOSTAT=IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, "('Error ',I7,' rewinding 1, ',A)" ) IERR, &
     &                                PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(235), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              END IF
!
              ENDFILE ( UNIT=23, IOSTAT=IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, "('Error ',I7,' endfile, ',A)" ) IERR, &
     &                     PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(236), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              END IF
!
              REWIND ( UNIT=23, IOSTAT=IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, "('Error ',I7,' rewinding 2, ',A)") IERR, &
     &                     PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(237), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              END IF
            ELSE IF(TOKEN.EQ.'C') THEN
!
! ----------- CLOSE
!
              CLOSE ( UNIT=23, IOSTAT=IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, "('Error ',I7,' closing, ',A)") IERR, &
     &                     PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(238), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              END IF
            ELSE IF(TOKEN.EQ.'S') THEN
!
! ----------- Seek end of file
!
              REWIND ( UNIT=23, IOSTAT=IERR )
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR , "('Error ',I7,' seeking the end',A)" ) IERR, &
     &                     PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(239), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              ENDIF
!
98            CONTINUE
!
              READ ( UNIT=23, FMT='(A2)', END=99, IOSTAT=IERR ) IDUM
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, "('Error ',I7,' reading, ',A)" ) IERR, &
     &                     PRE_SPL_NAM(1:PRE_SPL_LEN)
                   CALL FERR ( INT2(240), ERRSTR, INT2(0), INT2(0) )
                   GOTO 910
              ENDIF
              GOTO 98
99            CONTINUE
#ifdef GNU
              BACKSPACE ( 23 )
#endif
            ELSE
!
! ---------- Unkown control
!
             WRITE ( ERRSTR, "('Unknown USE_SPOOL access control: ',A)" ) TOKEN
             CALL FERR ( INT2(241), ERRSTR, INT2(0), INT2(0) )
             GOTO 910
         ENDIF
         COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  !#!  USE_SPOOL  #!#
