      SUBROUTINE ACS_COVFIL(STRING)
      IMPLICIT NONE
!
! 1.  ACS_COVFIL PROGRAM SPECIFICATION
!
! 1.1 Access a Covariance file
!
! 1.2 REFERENCES:
!
! 2.  ACS_COVFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Type of access requested ('O'=open; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      COMMON/SAVCOV/FILDES,IDIRECT
      SAVE /SAVCOV/
      COMMON/SAVCOVCH/FNAME
      SAVE /SAVCOVCH/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: index,file_report,cgm_access
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
!
      DATA ME/'ACS_COVFIL'/
!
! FILDES - File descriptor, returned from cgm_access
! FNAME - Path/name of file to be accessed
! IDIRECT - Array to be read or written
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ACS_COVFIL PROGRAM STRUCTURE
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'COVF'//PRE_LETRS
!
1     CONTINUE
      IF(INDEX('OC',STRING).EQ.0.OR.LEN(STRING).NE.1) THEN
        CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING')
        GO TO 1
      ENDIF
      CALL CGM_ACCESS(IDIRECT,FNAME,FILDES,STRING)
!
      RETURN
      END
