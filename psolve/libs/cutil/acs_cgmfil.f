      SUBROUTINE ACS_CGMFIL(FNAME,STRING)
      IMPLICIT NONE
!
! 1.  ACS_CGMFIL PROGRAM SPECIFICATION
!
! 1.1 Access a CGM (Combined Global Matrix) file
!
! 1.2 REFERENCES:
!
! 2.  ACS_CGMFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,STRING
!
! FNAME - Name of file to be accessed
! STRING - Type of access requested ('O'=open; 'C'=close)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      INTEGER*4 FILDES
      COMMON/SAVCGM/FILDES,IDIRECT
      COMMON/NAMCGM/SAVNAM
      SAVE /SAVCGM/,/NAMCGM/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: cgm_access
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ACS_CGMFIL PROGRAM STRUCTURE
!
! If we're opening a new CGM, save its name in common
!
      IF(STRING.EQ.'O') THEN
        SAVNAM=FNAME
      ENDIF
      CALL CGM_ACCESS(IDIRECT,SAVNAM,FILDES,STRING)
!
      RETURN
      END
