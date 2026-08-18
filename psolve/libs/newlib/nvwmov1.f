        SUBROUTINE nvwmov1(source,dest,num)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!  replaces VWMOV and WVMOV and the code that moves the "odd" 16-bit word.
!  (These were the vector instruction set moves.  The equivalent source
!  and dest were real*4 variables in that case.)  The code that calls
!  this should be cleaned up at some later date.  - lef 12/21/88
!
        integer*2 num, source(num), dest(num), i
!
        do i = 1,num
          dest(i) = source(i)
        enddo
!
        return
        end
