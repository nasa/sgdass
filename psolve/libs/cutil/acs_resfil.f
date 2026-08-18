      SUBROUTINE ACS_RESFIL(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ACS_RESFIL PROGRAM SPECIFICATION
!
! 1.1 Access (open or close) residual file
!
! 1.2 REFERENCES:
!
! 2.  ACS_RESFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Type(s) of access requested (`O'=open; `C`=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*4 FILDES
      COMMON /SAVRES/ FNAME,FILDES
      SAVE /SAVRES/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ferr,file_report
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*1 TOKEN
      INTEGER*2   MCOUNT,COUNT,LEN
      INTEGER*4   IERR
!
! COUNT - Number of character in STRING currently being processed
! FILDES - Unit number for fortran open and close calls
! FNAME - Path/name of file to be accessed
! IERR - Error return from open or close calls
! MCOUNT - Number of characters in STRING
! TOKEN - Character from STRING currently being processed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ACS_RESFIL PROGRAM STRUCTURE
!
! Construct the name of the residual file
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'RESF'//PRE_LETRS
!
! Get the length of STRING and loop over individual characters
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
        TOKEN=STRING(COUNT:COUNT)
1       CONTINUE
!
!   Open
!
        IF(TOKEN.EQ.'O') THEN
!         CALL BIN_OPEN(FNAME,FILDES,'O')
          FILDES=99
          OPEN(FILDES,FILE=FNAME,ACCESS='DIRECT', &
     &      RECL=JRESREC_WORDS*2,IOSTAT=IERR)
          CALL FERR( INT2(IERR), 'OPENING RESFIL', INT2(0), INT2(0) )
!
!   Close
!
        ELSE IF(TOKEN.EQ.'C') THEN
!         CALL BIN_CLOSE(FNAME,FILDES)
          CLOSE(FILDES,IOSTAT=IERR)
          CALL FERR( INT2(IERR), 'CLOSING RESFIL', INT2(0), INT2(0) )
!
!  UNKOWN CONTROL
!
        ELSE
          CALL FILE_REPORT(FNAME,'ACS_RESFIL','UNKNOWN CONTROL' )
          GO TO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END
