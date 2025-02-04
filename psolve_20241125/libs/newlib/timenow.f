      FUNCTION  TIMENOW()
      IMPLICIT  NONE
      INTEGER*4 TIMENOW
      INTEGER*4, EXTERNAL :: TIME
!
! --- get the time in seconds since 70/01/01.00:00:00 UT
!
      TIMENOW = TIME ( %VAL(0) )
!
      RETURN
      END  !#! TIMENOW  #!#
!
! ------------------------------------------------------------------------
!
      integer*4 function timenow_homegrown()
      implicit none
      INCLUDE 'fclib.i'
      character*100, output,message
      character*3 mon,months(12)
      integer*2 nout,mxout,kerr,trimlen,month
      integer*2 date,year
      real*8 fjldy,hr,min,sec, tnow, t1970, tdif
      data months /'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',&
     &'Sep','Oct','Nov','Dec'/
!
!  get the time in seconds since 70/01/01.00:00:00 UT
!
!     :2004.06.27:jwr: Heavily modified for PC-Linux.  May not be the
!     simplest way to do the job, but it works. (The system call used 
!     on the HP does not work.)
!
#ifdef LINUX
      mxout = 100
      call read_systemf('date',mxout,output,nout,message,kerr)
     
      read(output,'(4x,a3,i3,f3.0,1x,f2.0,1x,f2.0,5x,i4)') &
     & mon, date, hr, min, sec,year

      month = 1
      do while(months(month).ne.mon)
        month = month+1
        if(month .eq. 13) stop 'timenow_homegrown - bad month'
      enddo
      
      tnow = fjldy(month,date,year)
      tnow = (tnow + (((sec/60.+min)/60.)+hr)/24.)*86400. 
      month = 1
      date = 1
      year = 1970
      t1970 = fjldy(month,date,year)*86400.
      tdif = tnow-t1970
      timenow_homegrown = tdif
#else
      timenow_homegrown = fc_time(ptr_nc(timenow_homegrown))
#endif
!
      return
      end
