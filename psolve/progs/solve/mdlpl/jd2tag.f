      SUBROUTINE jd2tag(jul_day,iyear,imon,iday,ihr,imin)
!
      implicit none
      real*8 jul_day, xday, fract
      integer*2 iyear,imon,iday,ihr,imin,itime
      integer*4 day_num
!
      day_num = jul_day
      if(jul_day - day_num  .ge. 0.5d0) then
        xday = day_num +0.5d0
      else
        xday = day_num -0.5d0
      endif
      fract = jul_day - xday
!
      call mdyjl(imon,iday,iyear,itime,xday)
      ihr  = fract*24.d0   + .00000001d0
      imin = fract*1440.d0 + .00000001d0 - 60.d0*ihr
!
      return
      end
