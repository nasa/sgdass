      SUBROUTINE CGM_ACCESS(IDIRECT,FNAME,FILDES,STRING)
      IMPLICIT NONE
!
! 1.  CGM_ACCESS PROGRAM SPECIFICATION
!
! 1.1 Low level open and close for CGM file
!
! 1.2 REFERENCES:
!
! 2.  CGM_ACCESS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDIRECT(*)
      INTEGER*4 FILDES
      CHARACTER*(*) FNAME,STRING
!
! FILDES - File descriptor of file to be opened or closed
! FNAME - Name of file to be opened or closed
! IDIRECT - Array to read or write
! STRING - Type of access requested ('O'=open; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: acs_cgmfil,acs_covfil
!       CALLED SUBROUTINES: file_report,use_file,index
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      INTEGER*4  I4P1
      DATA  I4P1 / 1 /
!
      DATA ME/'CGM_ACCESS'/
!
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CGM_ACCESS PROGRAM STRUCTURE
!
1     CONTINUE
      IF(LEN(STRING).NE.1.OR.INDEX('OC',STRING).EQ.0) THEN
        CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING')
        GO TO 1
      ELSE IF(STRING.EQ.'O')  THEN
        CALL USE_FILE(FNAME,FILDES,IDIRECT,I4P1,I4P1,'OR')
      ELSE IF(STRING.EQ.'C') THEN
        CALL USE_FILE(FNAME,FILDES,IDIRECT,I4P1,I4P1,'C')
      ENDIF
!
      RETURN
      END
