      SUBROUTINE xrsort(iarr,n,xr)
      IMPLICIT NONE
!
! 1.  XRSORT PROGRAM SPECIFICATION
!
! 1.1  Produce an array of pointers sorted to point at the site names
!      alphabetically (i.e. xr(1) contains the array index of the
!      alphabetically first station, and so on).
!
! 1.2 REFERENCES:
!
! 2.  XRSORT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 n,iarr(4,n)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 xr(n)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: bwork
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,K,slot
      character*8 csite1,csite2
      integer*2 isite1(4),isite2(4)
      equivalence (isite1(1),csite1),(isite2(1),csite2)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  911113  Created
!
! 5. XRSORT PROGRAM STRUCTURE
!
      xr(1) = 1
      do i=2,n
        do j=1,i-1
          do k=1,4
            isite1(k) = iarr(k,i)
            isite2(k) = iarr(k,xr(j))
          enddo
          if (csite1.lt.csite2) then
            slot = j
            goto 100
          endif
        enddo
        slot=i
100     continue
        do j=i,slot+1,-1
          xr(j) = xr(j-1)
        enddo
        xr(slot) = i
      enddo
!
      RETURN
      END
