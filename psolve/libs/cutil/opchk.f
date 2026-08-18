      SUBROUTINE OPCHK( IUNIT,NAME,IERR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OPCHK PROGRAM SPECIFICATION
!
! 1.1 Open a file and check for successful open.  If already open
!     or lock rejected, suspend for 10 seconds and try again.
!
! 1.2 REFERENCES:
!
! 2.  OPCHK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IUNIT
      CHARACTER*(*) NAME
!
! IUNIT - Fortran unit number to use in open
! NAME - Name of file to open
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
      INTEGER*4 IERR4
!
! IERR - IOSTAT return from OPEN
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: susp
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IL,TRIMLEN
      logical*2 kbit
      character*80 errstr
!
! IL - Length of file name
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  OPCHK PROGRAM STRUCTURE
!
! Attempt to open file
!
      IL=TRIMLEN(NAME)
      OPEN(IUNIT,FILE=NAME(1:IL),IOSTAT=IERR4)
      IERR = IERR4
!
!  Start loop to check for locked file and to retry if necessary
!
      DO WHILE ( IERR .EQ. 508 .OR. IERR .EQ. 513 )
         WRITE(*,1000) NAME(1:IL)
 1000    FORMAT(1X,A," LOCKED - SUSPENDING FOR 10 SECONDS.")
         CALL SUSP ( INT2(2), INT2(10) )
         OPEN(IUNIT,FILE=NAME(1:IL),IOSTAT=IERR4)
         IERR = IERR4
      END DO
      WRITE ( ERRSTR, '("Opening ",A)' ) NAME(1:IL)
      CALL FERR( IERR, ERRSTR, INT2(0), INT2(0) )
!
      RETURN
      END
