      SUBROUTINE USE_ARCF_COM(STRING)
      IMPLICIT NONE
!
! 1.  USE_ARCF_COM PROGRAM SPECIFICATION
!
! 1.1 Access an ARCFIL's commons.
!
! 1.2 REFERENCES:
!
! 2.  USE_ARCF_COM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - Requested access type ('R'=read; 'W'=write)
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      CHARACTER*(NAME_SIZE) SAVNAM
      INTEGER*4 FILDES
      INTEGER*2 IDIRECT(BLOCK_WORDS)
      LOGICAL*2 KSCOM
      COMMON/SAVARC/FILDES,IDIRECT,KSCOM
      COMMON/NAMARC/SAVNAM
      SAVE /SAVARC/,/NAMARC/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: file_report,cgm_com
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*12 ME
      DATA ME/'USE_ARCF_COM'/
!
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_ARCF_COM PROGRAM STRUCTURE
!
! Check to make sure commons are being saved in arc files
!
1     CONTINUE
      IF(.NOT.KSCOM) THEN
        CALL FILE_REPORT(SAVNAM,ME,'NO SAVED COMMON IN ARC FILE')
        GO TO 1
      ENDIF
!
! Access arc file commons
!
      CALL CGM_COM(IDIRECT,SAVNAM,FILDES,STRING)
!
      RETURN
      END
