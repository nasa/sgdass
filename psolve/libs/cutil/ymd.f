      SUBROUTINE YMD (QDB, IDA)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  YMD PROGRAM SPECIFICATION
!
! 1.1 Convert a database name into year, month, day format
!     and store it in a three element integer array.
!
! 1.2 REFERENCES:
!
! 2.  YMD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*10 QDB
!
! QDB - Database name (in $YYMONDDXX form)
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IDA(3)
!
! IDA - year (since 1900), month, day
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
      INTEGER*4 IOS, I
      CHARACTER MON(12)*3
      DATA MON/ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &          'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'/
!
! I - Loop index
! MON - Array of month abbreviations
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   IS   8412??  CREATED
!
! 5.  YMD PROGRAM STRUCTURE
!
! --- Year
!
      READ ( QDB(2:3),'(I2)', IOSTAT=IOS ) IDA(1)
      CALL FERR ( INT2(IOS), "Reading year", INT2(0), INT2(0) )
!
! --- Month
!
      DO 100 I = 1, 12
        IF (MON(I) .EQ. QDB(4:6)) IDA(2) = I
  100 CONTINUE
!
! --- Day
!
      READ ( QDB(7:8),'(I2)', IOSTAT=IOS ) IDA(3)
      CALL FERR ( INT2(IOS), "Reading day of month", INT2(0), INT2(0) )
!
      RETURN
      END  !#!  YMD  #!#
