      FUNCTION FCNTL_LOCK_CHECK(FILENAME)
!
      IMPLICIT NONE
!
!     FCNTL_LOCK_CHECK checks whether the input file is locked via the fcntl
!                function (and if so, also determines the lock type).
!                As a side benefit, the non-existence of the file is reported.
!
!     INPUT VARIABLES:
!
      CHARACTER*(*) FILENAME
!
!       filename - full path to file name
!
!     OUTPUT VARIABLES:
!
      INTEGER*4 FCNTL_LOCK_CHECK
!
!      0 - file exists and is unlocked
!     -1 - file exists, but has read lock
!     -2 - file exists, but has write lock
!     -3 - file does not exist
!     -4 - general open error
!     -5 - general lock check error
!
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 FDD
      CHARACTER*255 tfilename
      integer*4  ftseopen, chk_lock, ierr4, ilock
      integer*2 irw_type, mode_bits
      integer*4 i4p0, i4p1, i4p2
      integer*4       i4n1, i4n2, i4n3, i4n4, i4n5
      data i4p0 /0/, i4p1 /1/,   i4p2 /2/
      data           i4n1 /-1/, i4n2 /-2/,  i4n3 /-3/, &
     &               i4n4 /-4/, i4n5 /-5/
!
!
! 6.  PROGRAMMER: K. BAVER JANUARY 11, 2001
!
!     PROGRAM STRUCTURE
!
!     1. LOCK THE FILE
!
      tfilename=filename
      call zterm(tfilename,ierr4)
!
!     Open the file to prepare for checking the lock.  Open for writing so
!     that a check can be made for a write lock.
!
      mode_bits = 0
      irw_type = 2  !write lock
      fdd = ftseopen(tfilename,irw_type,mode_bits)
!
      if (fdd.eq.I4N2) then !file not found
        fcntl_lock_check = I4N3
      else if (fdd.lt.I4P0) then !general opening error
        fcntl_lock_check = I4N4
      else
!
!       No error found.  Check for a lock
!
        ilock = chk_lock(fdd)
        if (ilock.eq.I4P0) then !no lock
          fcntl_lock_check = I4P0
        else if (ilock.eq.I4P1) then !read lock
          fcntl_lock_check = I4N1
        else if (ilock.eq.I4P2) then !write lock
          fcntl_lock_check = I4N2
        else !general lock check error
          fcntl_lock_check = I4N5
        endif
        call ftclose(fdd)
      endif
!
      RETURN
      END
