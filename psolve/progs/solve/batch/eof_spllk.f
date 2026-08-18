      SUBROUTINE EOF_SPLLK ( POS, FILDIR, FILBASE, IFILLU, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  EOF_SPLLK PROGRAM SPECIFICATION
!
! 1.1 EOF SETTING AND RETREIVING for files which are handled
!     like the spool file is handled.
!     IT ALSO REQUIRES PRE_PROG TO HAVE BEEN USED.
!     PAUSES IF AN ERROR IS DETECTED.
!
! 1.2 REFERENCES:
!
! 2.  EOF_SPLLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 POS
      INTEGER*2 IFILLU
      CHARACTER*(*) STRING,FILDIR,FILBASE
!
!     FILDIR + '/' + FILBASE + user_initials = file to be handled
!     IFILLU - lu of file
!     STRING - CHARACTER STRING WITH ONE CHARACTER PER OPERATION
!              M - Mark POS as EOF
!              G - Get the CURRENT EOF and report through pos
!     POS - Position in the file
!
! 2.3 OUTPUT Variables: None
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
      INTEGER*2  IERR, COUNT, MCOUNT, IL, TRIMLEN
      INTEGER*4  IOS
      CHARACTER*63 FNAME
      CHARACTER*120 errstr
      CHARACTER*1 TOKEN
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!  KDB  90????  Created
!  AEE  910515  Enhanced error messages written to the error file.
!
! 5.  EOF_SPLLK PROGRAM STRUCTURE
!
!      FNAME=FILDIR// '/' //FILBASE//PRE_LETRS
      FNAME=FILDIR(:TRIMLEN(FILDIR))//FILBASE//PRE_LETRS
      IL=TRIMLEN(FNAME)
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE ( COUNT .LE. MCOUNT )
         TOKEN = STRING ( COUNT:COUNT )
1        CONTINUE
!
! ------ Mark
!
         IF ( TOKEN .EQ. 'M' ) THEN
              OPEN(IFILLU,FILE=FNAME,IOSTAT=IOS)
              IF ( IOS .NE. 0 ) THEN
                   IERR = IOS
                   WRITE ( ERRSTR, &
     &                     '("Error ",I7," opening ",A)')IERR, FNAME(1:IL)
                   CALL FERR ( INT2(112), ERRSTR, INT2(0), INT2(0) )
                   GOTO 1
              ENDIF
              CALL FTN_SEEK ( IFILLU, FNAME, POS )
!
              ENDFILE ( IFILLU, IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   IERR = IOS
                   WRITE ( ERRSTR, &
     &                     '("Error ",I7," endfile, ",A)')IERR, FNAME(1:IL)
                   CALL FERR ( INT2(113), errstr, INT2(0), INT2(0) )
                   GOTO 1
              END IF
!
              CLOSE ( IFILLU, IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   IERR = IOS
                   WRITE ( ERRSTR, '("Error ",I7," closing, ",A)') &
     &                     IERR, FNAME(1:IL)
                   CALL FERR ( INT2(114), errstr, INT2(0), INT2(0) )
                   GOTO 1
              END IF
            ELSE IF ( TOKEN .EQ. 'G' ) THEN
!
! ----------- Get currrent EOF
!
              CALL USE_SPLLK ( FILDIR, FILBASE, IFILLU, 'O' )
              CALL FTN_TELL  ( IFILLU, FNAME, POS )
              CALL USE_SPLLK ( FILDIR, FILBASE, IFILLU, 'C' )
            ELSE
!
! -----------  UNKOWN CONTROL
!
              WRITE ( ERRSTR, &
     &                '("Unknown access control: ",A," for file ",A)')TOKEN, FNAME(1:IL)
              CALL FERR ( INT2(115), ERRSTR, INT2(0), INT2(0) )
              GOTO 1
         ENDIF
         COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  !#!  EOF_SPLLK  #!#
