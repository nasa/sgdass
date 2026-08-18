      SUBROUTINE ACS_ARCFIL(FNAME,KCOM,STRING)
      IMPLICIT NONE
!
! 1.  ACS_ARCFIL PROGRAM SPECIFICATION
!
! 1.1 Access an arc file
!
! 1.2 REFERENCES:
!
! 2.  ACS_ARCFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,STRING
      LOGICAL*2 KCOM
!
! FNAME - Name of file to be accessed
! KCOM - TRUE if arc commons are saved in arc file
! STRING - Type of access requested ('O'=open; 'C'=close; 'P'=purge)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: index,file_report,arc_access
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*10 ME
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      LOGICAL*2 KSCOM
      COMMON/SAVARC/FILDES,IDIRECT,KSCOM
      COMMON/NAMARC/SAVNAM
      SAVE /SAVARC/,/NAMARC/
!
      DATA ME/'ACS_ARCFIL'/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ACS_ARCFIL PROGRAM STRUCTURE
!
!  Check for valid access type string (single character)
      IF(LEN(STRING).NE.1.OR.INDEX('OCP',STRING).EQ.0) THEN
        CALL FILE_REPORT(FNAME,ME,'ILLEGAL STRING')
      ELSE IF (STRING.EQ.'O') THEN
        KSCOM=KCOM
        SAVNAM=FNAME
      ENDIF
!
!  Call lower level routine to perform file access
      CALL ARC_ACCESS(IDIRECT,FNAME,FILDES,KCOM,STRING)
!
      RETURN
      END
