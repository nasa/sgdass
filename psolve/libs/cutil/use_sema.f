      INTEGER*4 FUNCTION USE_SEMA(FNAME,MODE)
      IMPLICIT NONE
!
! 1.  USE_SEMA PROGRAM SPECIFICATION
!
! 1.1 Waits on file lock as a semaphore.
!
! 1.2 REFERENCES:
!
! 2.  USE_SEMA INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) FNAME,mode
!
! FNAME - Name of file to be opened
! MODE - 'R' for read lock, 'W' for write lock
!
! 2.3 OUTPUT Variables:
!
! USE_SEMA - File descriptor for later closing
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: make_sema
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 MAKE_SEMA
      INTEGER*2 IL,TRIMLEN
!
! IL - Length of file name
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_SEMA PROGRAM STRUCTURE
!
! Check whether file is currently locked
!
      USE_SEMA=MAKE_SEMA(FNAME,MODE,'N')
!
! If so, keep trying, every 10 seconds, until unlocked
!
      IF(USE_SEMA.EQ.-1) THEN
        IL=TRIMLEN(FNAME)
        WRITE(*,*) FNAME(:IL)//' locked, sleeping until available.'
        USE_SEMA=MAKE_SEMA(FNAME,MODE,'Y')
        IF(USE_SEMA.EQ.-1) THEN
          WRITE(*,*) FNAME(:IL)// ' locking failed.'
          RETURN
        ENDIF
      ENDIF
!
      RETURN
      END
