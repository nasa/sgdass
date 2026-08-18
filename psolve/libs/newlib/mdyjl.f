      SUBROUTINE MDYJL(IMONTH,IDAY,IYEAR,ITIME,FJD)
      implicit none
!
! 1.  MDYJL PROGRAM SPECIFICATION
!
! 1.1 Convert Julian date at midnight to month, day and year.
!     Copied from PEP plus an additional .5 day to convert
!     from Julian date at midnight to the PEP Julian day number.
!
! 1.2 REFERENCES:
!
! 2.  MDYJL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 FJD
!
! FJD - Julian date at midnight
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IMONTH,IDAY,IYEAR,ITIME
!
! IDAY - Day of the month
! IMONTH - Month of the year (1-12)
! IYEAR - Year of the century (0-99)
! ITIME - Number of centuries since 1900
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
      INTEGER*2 MDN(13),nyr,ic,inyr,i
      real*8 xjd
      DATA MDN/0,31,59,90,120,151,181,212,243,273,304,334,365/
!
! IC - Number of centuries sice 0 January 1600
! INYR - Leap year days since 0 January 1600
! MDN - Array containing day of year at end of each month
! NYR - Number of years since 0 January 1600
! XJD - Days since 0 January 1600
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   DSR  7608??  Created
!
! 5.  MDYJL PROGRAM STRUCTURE
!
      XJD = FJD - 2305447.0D0 + 0.5D0
      NYR = XJD/365.
   16 IC = NYR/100
!     DAYS DUE TO LEAP YEARS
      INYR = XJD - NYR*365.0
      IDAY = INYR - (NYR-1)/4 + (NYR + 99)/100 - (NYR + 399)/400 - 1
      IF(IC .NE.0) GO TO 20
      IF(NYR.NE.0) GO TO 20
      IDAY = IDAY + 1
   20 IF(IDAY .GT. 0) GO TO 23
      NYR = NYR - 1
      GO TO 16
!**** IYEAR (O THRU 99) YEAR OF THE CENTURY
   23 IYEAR = NYR - IC * 100
      ITIME = IC - 3
      NYR = IYEAR
      IF(NYR .NE. 0) GO TO 27
      IF(MOD(IC,INT2(4)) .NE. 0) GO TO 34
      GO TO 30
   27 IF(MOD(NYR,INT2(4)) .NE. 0) GO TO 34
   30 IF(IDAY - 60) 34,39,32
   32 IDAY = IDAY - 1
   34 DO 36 I=2,13
          IF(IDAY .LE. MDN(I)) GO TO 40
   36 CONTINUE
   39 IMONTH = 2
      IDAY = 29
      GO TO 45
   40 IMONTH = I - 1
      IDAY = IDAY - MDN(IMONTH)
   45 RETURN
      END
