      FUNCTION WTHN_10_MIN(DATE1,DATE2,DATET,I)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  WTHN_10_MIN PROGRAM SPECIFICATION
!
! 1.1 Check whether a specified time is within ten minutes of a
!      clock break.
!
! 1.2 REFERENCES:
!
! 2.  WTHN_10_MIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 I
      REAL*8 DATE1,DATE2,DATET
!
! DATE1,DATE2 - Clock epochs before and after DATET
! DATET - Time being checked
! I - Epoch number of DATE1
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 WTHN_10_MIN
!
! WTHN_10_MIN - True if DATET is within ten minutes of a clock break
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 DIFF1,OFST1,DIFF2,OFST2,KBIT
      REAL*8 TEN_MIN
!
! DIFF1,DIFF2 - Time differences between DATET and DATE1,DATE2
! OFST1,OFST2 - True if DATE1,DATE2 is a clock break
! TEN_MIN - Ten minutes expressed as fraction of a day
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
! 5.  WTHN_10_MIN PROGRAM STRUCTURE
!
      TEN_MIN = 10.0/1440.0
      DIFF1 = DABS(DATET-DATE1).LT.TEN_MIN
      DIFF2 = DABS(DATE2-DATET).LT.TEN_MIN
      IF(I.GT.1) THEN
        OFST1 = KBIT( LCLK(I-1), INT2(1) ).OR.KBIT( LCLK(I), INT2(1))
      ELSE
        OFST1 = KBIT( LCLK(I), INT2(1) )
      ENDIF
      OFST2 = KBIT( LCLK(I+1), INT2(1) )
      WTHN_10_MIN = (DIFF1.AND.OFST1).OR.(DIFF2.AND.OFST2)
      RETURN
      END
