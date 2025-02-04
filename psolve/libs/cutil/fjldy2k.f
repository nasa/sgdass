      real*8 FUNCTION FJLDY2K(IMONTH,IDAY,IYEAR)
      IMPLICIT NONE
!
! 1.  FJLDY PROGRAM SPECIFICATION
!
! 1.1 Calculate the Julian date at midnight
!     NOTE: This routine fails at 2100 A.D.
!
!     This is an improved version of 'fjldy.f'.  It works correctly
!     from January 1, 1970 to December 31, 2069.
!     The year is expected to be a two digit integer.
!     0 <= iyear <  70 are mapped to 2000 to 2070
!     70<= iyear <= 99 are mapped to 1970 to 1999
!
!
! 1.2 REFERENCES:
!
! 2.  FJLDY INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IMONTH,IDAY,IYEAR
!
! IDAY - Day of the month (1-31)
! IMONTH - Month (1-12)
! IYEAR - See above.
!
! 2.3 OUTPUT Variables:
!
      REAL*8 FJLDY
!
! FJLDY - Julian date at midnight of the specified date
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 MONTOT(12),IYR1,IYR
      character*60 errstr
      REAL*8 STJD,FYR,YRDY
      DATA MONTOT/0,31,59,90,120,151,181,212,243,273,304,334/
!
! FYR - Year in double precision
! IYR,IYR1 - Used to handle leap years
! MONTOT - Array containing number of days in year before the start
!          of each month
! STJD - Julian date at start of 1900
! YRDY - Length of year in days
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!K
! 5.  FJLDY PROGRAM STRUCTURE
!
      If (iyear .lt. 70) iyear = iyear+100
!
      if (imonth.lt.1.or.imonth.gt.12) then
        write(6,'("Month out of range in FJLDY (=",i5,")")')imonth
        stop 1
      endif
      IYR1 = 0
      STJD = 2415020.0D0
      FYR = IYEAR
      YRDY = 365.0
      IYR = IYEAR/4
      IF(IYEAR) 3,21,7
    3 IYR1 = IYEAR/100
      IF(IYEAR.NE.IYR1*100) GO TO 7
      IF(IMONTH.GT.2) IYR1=IYR1+1
    7 IF(IYEAR.NE.IYR*4) GO TO 21
      IF(IYR) 11,21,15
   11 IF(IMONTH.LE.2) GO TO 21
      IYR = IYR +1
      GO TO 21
   15 IF(IMONTH.GT.2) GO TO 21
      IYR=IYR-1
   21 FJLDY = (MONTOT(IMONTH)+IDAY+IYR-IYR1)
      FJLDY=FJLDY + STJD + 365.*FYR
      FJLDY2K = FJLDY - 0.5D0
      RETURN
      END
