      SUBROUTINE EOF_SPOOL ( POS, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  EOF_SPOOL PROGRAM SPECIFICATION
!
! 1.1 Set and retrieve EOF for SPLFIL.  Requires PRE_PROG to have been
!      used.  Pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  EOF_SPOOL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
      INTEGER*4 POS
!
! POS - Position at whichto set EOF
! STRING - Character string with one character per operation
!            M = Mark POS as EOF     G = Get the current EOF
!
! 2.3 OUTPUT Variables:
!
! POS - Position of current EOF
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rstors,saves
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  IERR, COUNT, MCOUNT, TRIMLEN, IL
      CHARACTER  FNAME*128, ERRSTR*124
      INTEGER*4  IOS
      CHARACTER  CBUF*64, TOKEN*1
      INTEGER*2  IBUF(64)
      EQUIVALENCE (IBUF,CBUF)
!
! 4.  HISTORY
!   WHO   WHEN        WHAT
!   AEE   910515      Enhanced error messages written to the error file.
!   pet   2002.03.18  Forced the routine to take the name of spool file from
!                     precm
!   pet   2004.11.16  Fixed a bug: the previous version checked uninitialized 
!                     variable
!
! 5.  EOF_SPOOL PROGRAM STRUCTURE
!
      FNAME = PRE_SPL_NAM
      IL = PRE_SPL_LEN
      IL = TRIMLEN(FNAME)
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE ( COUNT .LE.MCOUNT )
         TOKEN=STRING(COUNT:COUNT)
1        CONTINUE
!
! ------ Mark eof at specified position of the file
!
         IF ( TOKEN .EQ. 'M' ) THEN
              OPEN ( UNIT=23, FILE=FNAME, IOSTAT=IOS )
              IERR = IOS
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( ERRSTR, '("Error ",I7," opening ",A)') IERR, &
     &                     FNAME(1:IL)
                   CALL FERR ( INT2(116), ERRSTR, INT2(0), INT2(0) )
                   GOTO 1
              ENDIF
              CALL FTN_SEEK( INT2(23), FNAME, POS )
!
!#ifdef GNU
!              WRITE ( 23, '(1X)' )
!#endif
!
              ENDFILE ( 23, IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( ERRSTR, '("Error ",I7," endfile, ",A)') IERR, &
     &                     FNAME(1:IL)
                   CALL FERR ( INT2(117), ERRSTR, INT2(0), INT2(0) )
                   GOTO 1
              END IF
!
              CLOSE ( 23, IOSTAT=IOS )
              IF ( IERR .NE. 0 ) THEN
                   IOS = IERR
                   WRITE ( ERRSTR, '("Error ",I7," closing, ",A)' ) IERR, &
     &                     FNAME(1:IL)
                   CALL FERR ( INT2(118), ERRSTR, INT2(0), INT2(0) )
                   GOTO 1
              END IF
            ELSE IF ( TOKEN .EQ. 'G' ) THEN
!
! ----------- Get currrent EOF
!
              CALL USE_SPOOL ( 'O' )
              CALL FTN_TELL ( INT2(23), FNAME, POS )
#ifdef GNU
              POS = POS + 1
#endif
              CALL USE_SPOOL ( 'C' )
            ELSE
!
! ----------- Unkown control
!
              WRITE ( ERRSTR, &
     &                '("Unknown access control: ",A," for file ",A)')TOKEN, FNAME(1:IL)
              CALL FERR ( INT2(119), ERRSTR, INT2(0), INT2(0) )
              GOTO 1
         ENDIF
         COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  !#!  EOF_SPOOL  #!#
