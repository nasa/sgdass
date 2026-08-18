      integer*4 function usecnow()
      implicit none
      INCLUDE 'fclib.i'
!
!  get the time in seconds since 70/01/01.00:00:00 UT
!
      usecnow=fc_clock(ptr_nc(usecnow))
!
      return
      end
