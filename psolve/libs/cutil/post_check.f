      SUBROUTINE POST_CHECK(KEXIST)
      IMPLICIT NONE
!
      INCLUDE 'solve.i'
!
! 1.  POST_CHECK PROGRAM SPECIFICATION
!
! 1.1 Check for the existence of the file that indicates that solve is being
!     posted and therefore no solve session should be started.
!
! 1.2 REFERENCES:
!
! 2.  POST_CHECK INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 KEXIST
!
! KEXIST - TRUE if file exists (indicating that Solve etc. should not be run)
!          FALSE if the file doesn't exist (indicating it's okay to run Solve)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      character*63 fname
      logical*2 loop
      integer*2 icount,luo,iunit,ilen,trimlen
      INTEGER*4  ios
      character*120 post_buffer
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  960506  Created.
!
! 5.  POST_CHECK PROGRAM STRUCTURE
!
      LUO = 6
      IUNIT = 101
!
!     Check for the file's existence.
!
      FNAME = SOLVE_SAVE_DIR//SOLVE_POST_FILNAM
      CALL BIN_EXIST(FNAME,KEXIST)
!
      IF (KEXIST) THEN
!
!       If the file exists, Solve etc. should not be run,
!       so write a standard error message to the user.
!
        WRITE(LUO,"(//, &
     &    'Solve, enter, etc. are currently unavailable.',//)")
!
!       A more detailed explanation may be placed in the post file.
!       Dump the file to dump any messages.
!       If the file is empty, assume that new Solve code is being posted.
!       Ignore any errors in opening or reading the file.  The user
!       has already gotten the main message.
!
        Open(iunit,File=fname,iostat=ios,err=90,status='old', &
     &       access='sequential',form='formatted')
   90   If(ios.eq.0) then
          icount = 0
          loop = .true.
          Do while(loop)
            Read(iunit,'(a)',iostat=ios,err=95,end=100) post_buffer
            icount = icount + 1
            ilen = trimlen(post_buffer)
            if (ilen.gt.0) then
              write(luo,'(a)') post_buffer(1:ilen)
            else
              write(luo,'(" ")')
            endif
          enddo
  95      icount = -1
 100      if (icount .eq. 0) then
            write(luo,"('New Solve code is being installed.')")
          endif
          close(iunit)
        endif
        write(luo,"(//)")
      ENDIF
!
      RETURN
      END
