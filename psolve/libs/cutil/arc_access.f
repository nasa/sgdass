      SUBROUTINE ARC_ACCESS(IDIRECT,FNAME,FILDES,KCOM,STRING)
      IMPLICIT NONE
!
! 1.  ARC_ACCESS PROGRAM SPECIFICATION
!
! 1.1 Low level open and close for arc file
!
! 1.2 REFERENCES:
!
! 2.  ARC_ACCESS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDIRECT(*)
      CHARACTER*(*) FNAME,STRING
      LOGICAL*2 KCOM
!
! FNAME - Name of file to open or close
! IDIRECT - Array to be read from or written to file
! KCOM - True if arc commons saved in arc file
! STRING - Type of access requested ('O'=open; 'C'=close; 'P'=purge)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 FILDES
!
! FILDES - File descriptor, returned from bin_open
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: acs_arcfil
!       CALLED SUBROUTINES: bin_open,bin_close,bin_unlink,file_report,
!                           cgm_access,file_mess
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*2 IERR
!
      DATA ME/'ARC_ACCESS'/
!
! IERR - Error return from bin_unlink
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5. ARC_ACCESS PROGRAM STRUCTURE
!
1     CONTINUE
!
! Check for valid STRING
!
      IF(INDEX('OCP',STRING).EQ.0.OR.LEN(STRING).NE.1) THEN
        CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING')
!
! Simple open or close
!
      ELSE IF(STRING.EQ.'O'.AND..NOT.KCOM) THEN
        CALL BIN_OPEN(FNAME,FILDES,'O')
      ELSE IF(STRING.EQ.'C'.AND..NOT.KCOM) THEN
        CALL BIN_CLOSE(FNAME,FILDES)
!
! Open or close, reading or writing arc commons
!
      ELSE IF(KCOM.AND.INDEX('OC',STRING).NE.0) THEN
        CALL CGM_ACCESS(IDIRECT,FNAME,FILDES,STRING)
!
! Purge file
!
      ELSE IF(STRING.EQ.'P') THEN
        CALL BIN_UNLINK(FNAME,IERR)
        IF(IERR.NE.0) THEN
          CALL FILE_MESS(IERR,FNAME,ME,'unlink')
        ENDIF
      ENDIF
!
      RETURN
      END
