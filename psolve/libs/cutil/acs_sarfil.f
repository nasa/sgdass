      SUBROUTINE ACS_SARFIL ( STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ACS_SARFIL PROGRAM SPECIFICATION
!
! 1.1 Access sar file; options include open, close, initialize, load
!       record info, save record info.
!
! 1.2 REFERENCES:
!
! 2.  ACS_SARFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Type(s) of access requested ('O'=open; 'I'=initialize; 'L'=load;
!               'S'=save; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'sareq.i'
      CHARACTER FNAME*(NAME_SIZE) 
      INTEGER*4 FILDES, NEXT_REC
      COMMON / SAVSAR / FNAME, FILDES, NEXT_REC
      SAVE   / SAVSAR /
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ferr,use_sarfil,file_report
!
! 3.  LOCAL VARIABLES
!
      CHARACTER   TOKEN*1 
      INTEGER*2   MCOUNT, COUNT, LEN
      INTEGER*4   IERR
      INTEGER*4,  EXTERNAL :: GET_UNIT
!
!
! COUNT - Number of character in STRING currently being processed
! FILDES - Unit number used in fortran open and close calls
! FNAME - Path/name of file to be accessed
! IERR - Error return from open or close call
! MCOUNT - Number of characters in STRING
! NEXT_REC - Number of record in file at which we are positioned
! TOKEN - Character in STRING currently being processed
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ACS_SARFIL PROGRAM STRUCTURE
!
! Construct file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'SARF'//PRE_LETRS
!
! Get length of STRING and loop over individual characters
!
      MCOUNT=LEN(STRING)
      COUNT=1
      DO WHILE(COUNT.LE.MCOUNT)
         TOKEN=STRING(COUNT:COUNT)
1        CONTINUE
         IF ( TOKEN .EQ. 'O' ) THEN
!
! ----------- Open
!
              FILDES=GET_UNIT()
              OPEN ( FILDES, FILE=FNAME, ACCESS='DIRECT', &
     &               RECL=JSARREC_WORDS*2, IOSTAT=IERR )
              IF ( IERR .GT. 0 ) CALL FERR ( INT2(IERR), 'OPENING SARFIL', &
     &                                INT2(0), INT2(0) )
           ELSE IF ( TOKEN .EQ. 'I' ) THEN
!
! ----------- Initialize
!
              NEXT_REC=2
#ifdef GNU
#else
              REWIND ( FILDES, IOSTAT=IERR )
              IF ( IERR .GT. 0 ) CALL FERR ( INT2(IERR), 'REWINDING SARFIL', &
     &                                INT2(0), INT2(0) )
              ENDFILE ( FILDES, IOSTAT=IERR )
              IF ( IERR .GT. 0 ) CALL FERR( INT2(IERR), 'WRITING EOF TO SARFIL', &
     &                                INT2(33), INT2(0) )
#endif
           ELSE IF(TOKEN.EQ.'L') THEN
!
! ----------- Load
!
              CALL USE_SARFIL ( 'R', 1 )
              NEXT_REC=N4BF(1)
           ELSE IF(TOKEN.EQ.'S') THEN
!
! ----------- Save
!
              N4BF(1)=NEXT_REC
              CALL USE_SARFIL ( 'W', 1 )
           ELSE IF(TOKEN.EQ.'C') THEN
!
! ----------- Close
!
              CLOSE ( FILDES, IOSTAT=IERR )
              IF ( IERR .GT. 0 ) CALL FERR( INT2(IERR), 'CLOSING SARFIL', &
     &                                INT2(0), INT2(0) )
           ELSE
!
! ----------- Unknown control
!
              CALL FILE_REPORT ( FNAME, 'ACS_SARFIL', 'UNKNOWN CONTROL: '// &
     &                           TOKEN  )
          GOTO 1
        ENDIF
        COUNT=COUNT+1
      ENDDO
!
      RETURN
      END  SUBROUTINE  ACS_SARFIL
