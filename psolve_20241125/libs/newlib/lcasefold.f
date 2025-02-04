      SUBROUTINE lcasefold(string)
      implicit none
      character*(*) string
!
! convert any upper case to lower
!
      integer*2 i,ilen,ival
!
      ilen=len(string)
!
      do i=1,ilen
        ival=ichar(string(i:i))
        if(ival.gt.64.and.ival.lt.91) string(i:i)=char(ival+32)
      enddo
!
      return
      end
