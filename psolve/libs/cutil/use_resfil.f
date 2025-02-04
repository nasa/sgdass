      SUBROUTINE USE_RESFIL ( IOBS, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  USE_RESFIL PROGRAM SPECIFICATION
!
! 1.1 Read or write RESFIL.  Program pauses if error is detected.
!
! 1.2 REFERENCES:
!
! 2.  USE_RESFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IOBS
      CHARACTER*(*) STRING
!
! IOBS - Observation number to be read or written
! STRING - Requested access type ('R'=read; 'W'=write)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'resfl.i'
      INTEGER*4 FILDES
      CHARACTER*(NAME_SIZE) FNAME
      COMMON/SAVRES/FNAME,FILDES
      SAVE /SAVRES/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,bin_read,ferr,bin_write
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*4 JPOS, IERR
!
      DATA ME/'USE_RESFIL'/
!
! IERR - IOSTAT return from READ, WRITE
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!     95.12.04:kdb Allow integer*4 number of observations
!
! 5.  USE_RESFIL PROGRAM STRUCTURE
!
!  Make sure STRING is either 'R' or 'W'
!
1     CONTINUE
      IF(LEN(STRING).NE.1.OR.INDEX('RW',STRING).EQ.0) THEN
        CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING' )
        GO TO 1
      ENDIF
!
!  Read
!
      IF(STRING.EQ.'R') THEN
        READ(FILDES,REC=IOBS,IOSTAT=IERR) IRESCM
        CALL FERR( INT2(IERR), 'Reading RESFIL', INT2(0), INT2(0) )
!
!  WRITE
!
      ELSE
        WRITE(FILDES,REC=IOBS,IOSTAT=IERR) IRESCM
        CALL FERR( INT2(IERR), 'Writing RESFIL', INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  USE_RESFIL  #!#
