      SUBROUTINE ARC_CREATE8 ( IDIRECT, FNAME, FILDES, NP, KCOM )
      IMPLICIT NONE
!
! 1.  ARC_CREATE PROGRAM SPECIFICATION
!
! 1.1 Low level routine for creating arc file
!
! 1.2 REFERENCES:
!
! 2.  ARC_CREATE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IDIRECT(*)
      INTEGER*4 NP
      CHARACTER*(*) FNAME
      LOGICAL*2 KCOM
!
! FNAME - Name of arc file to be created
! IDIRECT - Array to be written to file
! KCOM - True if arc commons to be saved in arc file
! NP - Number of parameters
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 FILDES
!
! FILDES - File descriptor, returned from bin_create or cgm_create
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: mat_e,jrnd_blocks,bin_create,cgm_create
!
! 3.  LOCAL VARIABLES
!
      integer*2 ierr
!
      CHARACTER  ME*11
      DATA ME / 'ARC_CREATE' /
      INTEGER*8 LEN8_BYTES, LEN8_BLOCKS
!
! LENGTH - Minimum number of blocks to be allocated
! ME - Name of this routine
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ARC_CREATE PROGRAM STRUCTURE
!
1     CONTINUE
!@      IF(.NOT.KCOM) THEN
!@        LENGTH=JRND_BLOCKS(MAT_E(MAX_PAR,NP)*REALL_WORDS)
!@        CALL BIN_CREATE(FNAME,FILDES,LENGTH)
!@      ELSE
!@        CALL CGM_CREATE(IDIRECT,FNAME,FILDES,NP,'M',ierr)
!      ENDIF
      LEN8_BYTES = 8*(3*M_GPA + INT8(NP)*INT8(NP+1)/2)
      LEN8_BLOCKS  = (LEN8_BYTES + 255)/256
      IF ( .NOT. KCOM ) THEN
           CALL BIN_CREATE8 ( FNAME, FILDES, LEN8_BLOCKS )
        ELSE
          CALL CGM_CREATE8 ( IDIRECT, FNAME, FILDES, NP, 'M', IERR )
      ENDIF
!
      RETURN
      END
