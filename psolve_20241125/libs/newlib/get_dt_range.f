      SUBROUTINE GET_DT_RANGE(DTBUF,DATE1,DATE2,KERRMAJ,KERRMIN)
!
      IMPLICIT NONE
!
!     Purpose: given a buffer potentially containing a valid date range,
!        attempts to resolve the buffer into a date range consisting
!       of 2 year, month and day pairs.
!     Written: 7/29/93 by KDB.
!
!     modifications:
!
!     kdb 960430 Lower the lower limit on year from (19)80 to (19)50
!     kdb 981008 Y2K fixes.
!
!     restrictions: does not screen out requests for days like February 30.
!
!     Input variables
!
!     DTBUF - input buffer to be resolved.
!       Only 3 formats can be handled,
!           YR MN (which produces a range of YR MN 0 YR MN 0, or all days in
!                  that single month)
!           YR1 MN1 YR2 MN2 (which produces a range of YR1 MN1 0 YR2 MN2 0,
!                  or all days in those months)
!           YR1 MN1 DY1 YR2 MN2 DY2 (which produces the given range,
!                between the 2 days, inclusive)
!       In each case, the year value is expected to represent the last two digits of
!            the year (99 for 1999 or 0 for 2000)
!
      CHARACTER*(*) DTBUF
!
!     Output variables
!
!     DATE1(3), DATE2(3) - the range's start and stop dates.  The values are
!                     year, month and day, expressed as:
!                        year -  a number representing the last two digits
!                                of the year (99 for 1999 or 0 for 2000),
!                        month - 1-12 and
!                        day   - 1-31 (or 0 if only the year and month are requested)
!
!     KERRMAJ - major error return
!                 0 - success
!                -1 - error return from var_intput call
!                -2 - bad number of values input
!                -3 - error in individual year, month or day (e.g., month = 13)
!                -4 - error in the relationship between input fields
!                     (e.g., year1 > year2)
!     KERRMIN - minor error return, providing additional info tailored to
!               the particular error code in kerrmaj.
!
!    for kerrmaj of ___, kerrmin is:
!            0 - 0
!           -1 - the error return from var_intput
!           -2 - the bad number of values entered as input
!           -3 - identification of the bad field:
!                        tens digit gives type, year (1), month(2) or day(3)
!                        ones digit gives date, 1 or 2
!           -4 - identification of the bad pair of fields:
!                    1 = year, 2 = month, 3 = day
!
      INTEGER*2 DATE1(3),DATE2(3),KERRMAJ,KERRMIN
!
!     Local variables
!
      INTEGER*2 ICT,MOST_WANTED,DATE_INS(6),NUM_VALS,JERR
      INTEGER*2 DATE_CH(3,2),LOW_YEAR,HIGH_YEAR
      INTEGER*2 IY_CHECK1_DIG4,IY_CHECK2_DIG4
!
      DATA LOW_YEAR /0/
      DATA HIGH_YEAR /99/
!
      KERRMAJ = 0
      KERRMIN = 0
!
!     Resolve the buffer into a set of up to 6 numbers.
!
      MOST_WANTED = 6
      CALL VAR_INTPUT(DTBUF,MOST_WANTED,DATE_INS,NUM_VALS,JERR)
!
      IF (JERR.NE.0.AND.JERR.NE.1) THEN
        KERRMAJ = -1
        KERRMIN = JERR
        RETURN
      END IF
!
!     See if the request can be rejected on the basis of the number of
!     fields given, given the fact that the only acceptable formats are
!     YR MN, YR1 MN1 YR2 MN2 and YR1 MN1 DY1 YR2 MN2 DY2.
!
      IF (NUM_VALS.NE.2.AND.NUM_VALS.NE.4.AND.NUM_VALS.NE.6) THEN
        KERRMAJ = -2
        KERRMIN = NUM_VALS
        RETURN
      END IF
!
!     Load the input fields into temporary variables, so that their
!     validity as date fields can be easily checked.
!
      IF (NUM_VALS.EQ.2) THEN !YR MN
        DATE_CH(1,1) = DATE_INS(1) !year
        DATE_CH(2,1) = DATE_INS(2) !month
        DATE_CH(3,1) = 0           !day of date 1
        DATE_CH(1,2) = DATE_INS(1)
        DATE_CH(2,2) = DATE_INS(2)
        DATE_CH(3,2) = 0
      ELSE IF (NUM_VALS.EQ.4) THEN !YR1 MN1 YR2 MN2
        DATE_CH(1,1) = DATE_INS(1)
        DATE_CH(2,1) = DATE_INS(2)
        DATE_CH(3,1) = 0
        DATE_CH(1,2) = DATE_INS(3)
        DATE_CH(2,2) = DATE_INS(4)
        DATE_CH(3,2) = 0
      ELSE !YR1 MN1 DY1 YR2 MN2 DY2
        DATE_CH(1,1) = DATE_INS(1)
        DATE_CH(2,1) = DATE_INS(2)
        DATE_CH(3,1) = DATE_INS(3)
        DATE_CH(1,2) = DATE_INS(4)
        DATE_CH(2,2) = DATE_INS(5)
        DATE_CH(3,2) = DATE_INS(6)
      END IF
!
!     Check the individual numbers to see if they make sense as date fields.
!
      DO ICT = 1,2 !check alleged years
        IF (DATE_CH(1,ICT).LT.LOW_YEAR &
     &       .OR.DATE_CH(1,ICT).GT.HIGH_YEAR) THEN
          KERRMAJ = -3
          KERRMIN = 10 + ICT
        END IF
      END DO
      IF (KERRMAJ.NE.0) RETURN
!
      DO ICT = 1,2 !check alleged months
        IF (DATE_CH(2,ICT).LT.1.OR.DATE_CH(2,ICT).GT.12) THEN
          KERRMAJ = -3
          KERRMIN = 20 + ICT
        END IF
      END DO
      IF (KERRMAJ.NE.0) RETURN
!
      IF (DATE_CH(3,1).NE.0.OR.DATE_CH(3,2).NE.0) THEN
        DO ICT = 1,2 !check alleged days
          IF (DATE_CH(3,ICT).LT.1.OR.DATE_CH(3,ICT).GT.31) THEN
            KERRMAJ = -3
            KERRMIN = 30 + ICT
          END IF
        END DO
      END IF
      IF (KERRMAJ.NE.0) RETURN
!
!     Now check the relationships between the numbers, to make sure they
!     make sense.  (For example, don't want a starting date greater than a
!     stop date.)
!
      CALL NEWCENTS(DATE_CH(1,1),IY_CHECK1_DIG4)
      CALL NEWCENTS(DATE_CH(1,2),IY_CHECK2_DIG4)
      IF (IY_CHECK1_DIG4.GT.IY_CHECK2_DIG4) THEN
!       Year 1 exceeds year 2.
        KERRMAJ = -4
        KERRMIN = 1
      ELSE IF (DATE_CH(1,1).EQ.DATE_CH(1,2)) THEN
!       Years are equal.  OK so far.
        IF (DATE_CH(2,1).GT.DATE_CH(2,2)) THEN
!         Month 1 exceeds month 2
          KERRMAJ = -4
          KERRMIN = 2
        ELSE IF (DATE_CH(2,1).EQ.DATE_CH(2,2)) THEN
!         Months are equal.  Still may be ok.
          IF (DATE_CH(3,1).GT.DATE_CH(3,2)) THEN
!           Day 1 exceeds day 2.
            KERRMAJ = -4
            KERRMIN = 3
          END IF
        END IF
      END IF
      IF (KERRMAJ.NE.0) RETURN
!
!     Since we've gotten this far, everything must be ok.  Load up the
!     return variables.
!
      DO ICT = 1,3
        DATE1(ICT) = DATE_CH(ICT,1)
        DATE2(ICT) = DATE_CH(ICT,2)
      END DO
!
      RETURN
      END
