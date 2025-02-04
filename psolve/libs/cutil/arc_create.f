      SUBROUTINE ARC_CREATE(IDIRECT,FNAME,FILDES,NP,KCOM)
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
      INTEGER*2 IDIRECT(*),NP
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
      CHARACTER*10 ME
      INTEGER*4 LENGTH,JRND_BLOCKS,MAT_E
      integer*2 ierr
!
      DATA ME/'ARC_CREATE'/
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
      IF(.NOT.KCOM) THEN
        LENGTH=JRND_BLOCKS(MAT_E(MAX_PAR,NP)*REALL_WORDS)
        CALL BIN_CREATE(FNAME,FILDES,LENGTH)
      ELSE
        CALL CGM_CREATE(IDIRECT,FNAME,FILDES,NP,'M',ierr)
      ENDIF
!
      RETURN
      END
