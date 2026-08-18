        SUBROUTINE zeroterm(string,ierr)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 i
!-----END of imp added lines.
!
!
!  ZEROTERM terminates a character string for suitability for calling C
!  or system routines.  The string used must have been originally defined
!  in FORTRAN.  If ierr is less than zero on return, the string is already
!  full and no zero was added.
!
        character*(*) string
        character*1 blank/' '/
        integer*4 leng, ierr
!
        ierr = 0
        leng = len(string)
        do i = leng,1,-1
          if(string(i:i).ne.blank) then
            if(leng.gt.i) then
              string(i+1:i+1) = char(0)
            else
              ierr = -1
            endif
!                             break out of loop because we are done.
            go to 100
          endif
        enddo
!
        string(1:1) = char(0)
 100    return
        end
