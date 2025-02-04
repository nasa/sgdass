      SUBROUTINE casefold(string)
      implicit none
      character*(*) string
!
! convert any lower case to upper
!
      integer*2 i,ilen,ival
!
      ilen=len(string)
!
      do i=1,ilen
        ival=ichar(string(i:i))
        if(ival.gt.96.and.ival.lt.123) string(i:i)=char(ival-32)
      enddo
!
      return
      end
