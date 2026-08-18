      SUBROUTINE EOF_CVRF ( POS, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  EOF_CVRF PROGRAM SPECIFICATION
!
! 1.1 Sets and retrieves EOF for CVRFIL.  It is necessary for
!      PRE_PROG to have been used. Pauses if an error is detected.
!
! 1.2 REFERENCES:
!
! 2.  EOF_CVRF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 POS
      CHARACTER*(*) STRING
!
! POS - Position at which to mark EOF
! STRING - Character string with one character per operation
!            M = Mark POS as EOF   G = Get the current EOF
!
! 2.3 OUTPUT Variables: None
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
      INTEGER*2 IERR,COUNT,MCOUNT,TRIMLEN,IL,IDUM,ERROR
      INTEGER*4  IOS
      CHARACTER*63 FNAME
      CHARACTER*1 TOKEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  EOF_CVRF PROGRAM STRUCTURE
!
! Construct covariance file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'CVRF'//PRE_LETRS
      IL=TRIMLEN(FNAME)
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
1        CONTINUE
!
!     Mark eof at specified file position
!
         IF ( TOKEN .EQ. 'M' ) THEN
              OPEN ( 88, FILE=FNAME, IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(IOS), 'opening CVRFIL', INT2(0), INT2(0) )
                   IERR = IOS
                   GOTO 1
              ENDIF
              CALL FTN_SEEK( INT2(88), FNAME, POS )
!
              ENDFILE ( 88, IOSTAT=IOS )
              IF ( IOS .NE. 0 )  THEN
                   CALL FERR ( INT2(IOS), 'endfile error in CVRFIL', INT2(0), &
     &                         INT2(0) )
                   IERR = IOS
                   GOTO 1
              END IF
!
              CLOSE ( 88, IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(IOS), 'closing CVRFIL', INT2(0), INT2(0) )
                   GOTO 1
              END IF
           ELSE IF(TOKEN.EQ.'G') THEN
!
! ----------- Get currrent EOF
!
              CALL FTN_OPEN( INT2(88), FNAME, 'A' )
              CALL FTN_TELL( INT2(88), FNAME, POS )
              CLOSE ( 88 )
           ELSE
!
! ----------- UNKOWN CONTROL
!
              CALL FERR ( INT2(111), 'Unknown access control in EOF_CVRF', &
     &                    INT2(0), INT2(0) )
              GOTO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  !#!  EOF_CVRF  #!#
