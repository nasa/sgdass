      SUBROUTINE ACS_OBSFIL ( STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ACS_OBSFIL PROGRAM SPECIFICATION
!
! 1.1 Access (open or close) an observation source (either an obsfil or
!     a superfile)
!
! 1.2 REFERENCES:
!
! 2.  ACS_OBSFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Type(s) of access requested (`O`=open; `C`=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'ibufm.i'
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*4  FILDES
      INTEGER*8  IOBS_OFF
      COMMON  / SAVOBS / FNAME, FILDES, IOBS_OFF
      SAVE    / SAVOBS /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: bin_open,bin_close,bin_read,file_report
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*1 TOKEN
      INTEGER*2   MCOUNT, COUNT, LEN
      LOGICAL*2   KBIT
!
!
! COUNT - Number of character in STRING currently being processed
! FILDES - File descriptor, returned from bin_open
! FNAME - Path/name of the observation source (obsfil or superfile)
!         to be accessed
! MCOUNT - Number of characters in STRING
! TOKEN - Character from STRING currently being processed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb 960307   Revive the read-only open of superfiles, now that they are
!                read-only.  Some documentation.
!
! 5.  ACS_OBSFIL PROGRAM STRUCTURE
!
!  Construct name of the observation source (obsfil or superfile).
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'OBSF'//PRE_LETRS
      IOBS_OFF  = INT8(1)
!
! Get length of STRING and loop over individual characters
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
1        CONTINUE
!
!   The goal is to get observations either from the obsfil or from a superfile.
!   In either case, start by opening the obsfil.
!   If the observations are supposed to come from a superfile, the name of
!   the superfile will be in the obsfil, so read the obsfil and close it
!   and open the superfile instead.
!
         IF ( TOKEN .EQ. 'O' ) THEN
              CALL BIN_OPEN(FNAME,FILDES,'O' )
              IF ( KBIT( PRE_IBATCH, INT2(11)) ) THEN
                   CALL BIN_READ  ( FNAME, FILDES, IBUFM, 1 )
                   CALL BIN_CLOSE ( FNAME, FILDES )
                   CALL BIN_OPEN_RD ( MDESCRP, FILDES, 'O' )
                   FNAME=MDESCRP
                   IOBS_OFF = MREC
              ENDIF
           ELSE IF(TOKEN.EQ.'C') THEN
!
! ----------- Close
!
             CALL BIN_CLOSE(FNAME,FILDES )
           ELSE
!
! ---------- UNKOWN CONTROL
!
             CALL FILE_REPORT ( FNAME, 'ACS_OBSFIL', 'UNKNOWN CONTROL: '//TOKEN )
             GOTO 1
         ENDIF
         COUNT=COUNT+1
      ENDDO
      RETURN
      END  SUBROUTINE  ACS_OBSFIL 
