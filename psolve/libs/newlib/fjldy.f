      FUNCTION FJLDY(IMONTH,IDAY,IYEAR_INPUT)
      IMPLICIT NONE
!
      INCLUDE 'param.i'
!
! 1.  FJLDY PROGRAM SPECIFICATION
!
! 1.1 Calculate the Julian date at midnight
!     NOTE: This routine fails at 2100 A.D.
!           Also only years in a 100 year range may be input.  This will
!           probably be 1970 through 2069, but the actual starting year of
!           the range (e.g., 70) is parameterized.
!     NOTE: Multiple unofficial copies of fjldy with minor variations had been
!           generated on various directories as of 10/98.  This copy is based
!           on the solve/cutil version and incorporates y2k changes plus
!           a minor change to properly express two real*8 constants.
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
      INTEGER*2 IMONTH,IDAY,IYEAR_INPUT
!
! IDAY - Day of the month (1-31)
! IMONTH - Month (1-12)
! IYEAR_INPUT - supports 3 formats representing the following year ranges:
!          (but see note at the top about the parameterization of the range)
!
!                                1970-1999       2000-2069
!                                ---------       ---------
!    1. full (4 digit year)      1970-1999       2000-2069
!    2. full - 1900                70-99          100-169
!    3. last 2 digits of year      70-99            0-69
!         (represented as an
!           integer)
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
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 MONTOT(12),IYR1,IYR
      INTEGER*2 IYEAR
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
!
!   modifications
!
!   kdb  981027 Final official Y2K version.  (Changes include more input
!                formats for the year - final two digits of the year and
!                full four digits of the year.)  (The original input was years
!                past 1900.)
!               Also merges various copies
!               of fjldy that have been generated unofficially.  This copy
!               is based on the solve/cutil version and incorporates y2k
!               changes plus a minor change to properly express two real*8
!               constants.
!K
! 5.  FJLDY PROGRAM STRUCTURE
!
!
!     Internally, fjldy needs the year expressed as years past 1900.
!     Convert the input year to this format.
!
      if (iyear_input.lt.0) then
!       negative year - error
        iyear = -1
      else if (iyear_input.lt.Y2K_START_YEAR) then
!       2000+, last 2 digits (0 through Y2K_START_YEAR-1, e.g., 0 to 69)
        iyear = iyear_input + 100
      else if (iyear_input .lt.Y2K_START_YEAR+100) then
!       Y2K_START_YEAR through Y2K_START_YEAR + 99 (e.g., 70-99 and 100-169)
!       (1900s , last 2 digits OR full year - 1900 (1900s or 2000+))
        iyear = iyear_input
      else if (iyear_input.lt.Y2K_START_YEAR+1900) then
!       unknown format (e.g., 170 - 1969)
        iyear = -1
      else if (iyear_input.lt.Y2K_START_YEAR + 2000) then
!       full 4 digit format (e.g., 1970-2069)
        iyear =  iyear_input - 1900
      else
!       error - full year beyond range - e.g., 2070 and beyond
        iyear = -1
      endif
!
      if (iyear.eq.-1) then
        write(*,'(/,"Year out of range in FJLDY (=",i5,")",/)') &
     &            iyear_input
        stop 'fjldy year error'
      endif
!
      if (imonth.lt.1.or.imonth.gt.12) then
        write(*,'(/,"Month out of range in FJLDY (=",i5,")",/)')imonth
        stop 'fjldy month error'
      endif
      IYR1 = 0
      STJD = 2415020.0D0
      FYR = IYEAR
      YRDY = 365.0D0
      IYR = IYEAR/4
!@      IF(IYEAR) 3,21,7
      IF ( IYEAR < 0  ) GOTO  3
      IF ( IYEAR == 0 ) GOTO 21
      IF ( IYEAR > 0  ) GOTO  7
    3 IYR1 = IYEAR/100
      IF(IYEAR.NE.IYR1*100) GO TO 7
      IF(IMONTH.GT.2) IYR1=IYR1+1
    7 IF(IYEAR.NE.IYR*4) GO TO 21
!@      IF(IYR) 11,21,15
      IF ( IYR <  0 ) GOTO 11
      IF ( IYR == 0 ) GOTO 21
      IF ( IYR >  0 ) GOTO 15
   11 IF(IMONTH.LE.2) GO TO 21
      IYR = IYR +1
      GO TO 21
   15 IF(IMONTH.GT.2) GO TO 21
      IYR=IYR-1
   21 FJLDY = (MONTOT(IMONTH)+IDAY+IYR-IYR1)
      FJLDY=FJLDY + STJD + 365.0D0*FYR
      FJLDY = FJLDY - 0.5D0
      RETURN
      END
