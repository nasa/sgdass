      SUBROUTINE SPOOL_TOP()
!
!     purpose: 1. Parse the top of a spool file and place useful values in
!                 gsnoop common.  The "top" of a spool file is the part
!                 prior to the "Parameter adjustments for run xxxxx-xxxx line".
!              2. Determine any relevant spoolfile formats (e.g., spoolfiles
!                 after May 2002 have i5 parameter indices (previously i4)).
!
!
      IMPLICIT NONE
      INCLUDE 'gsnoop_com.i'
!
!
! 2.2 INPUT Variables:   None
!
! 2.3 OUTPUT Variables:  None
!
! 3.  LOCAL VARIABLES
      REAL*8    FJLDY 
      INTEGER*2 SR_INDEX, IYEAR, IMONTH, IDAY
      CHARACTER SR_SEARCH_KEY*15, QSTR*78 
      LOGICAL*2 SR_FOUND
      INTEGER*2 ICHANGE
!
! 4.  HISTORY
!
!   written 5/15/02 by kdb
!
!   modifications
!
!   kdb 030109 Change from separate changeover variables to an array.
!              Set new julian date changeover array here rather than in
!              individual routines.
!   kdb 030109 Spoolfile revision 2, for solve revision 12.26.2002.
!
!  A. Run through the top of the spoolfile, gathering information.
!
      qstr = 'now entering spool_top'
      call asnl(qstr)
      SR_SEARCH_KEY = "Solve_revision:"
      SR_FOUND = .FALSE.
      READ(40,'(a)') cbuf
      DO WHILE(INDEX(CBUF,"Parameter adjustments for run").eq.0)
!       Look for the SOLVE revision statement in the top of the spoolfile
!       (as opposed to the one repeated in each arc listing).
        IF (INDEX(CBUF,SR_SEARCH_KEY).ne.0) THEN
          SR_INDEX = INDEX(CBUF,SR_SEARCH_KEY) + 18
          SPOOL_SOLVE_REVISION_DATE_CHAR = CBUF(SR_INDEX:SR_INDEX+9)
          READ(CBUF(SR_INDEX:SR_INDEX+3),"(I4)") IYEAR
          READ(CBUF(SR_INDEX+5:SR_INDEX+6),"(I2)") IMONTH
          READ(CBUF(SR_INDEX+8:SR_INDEX+9),"(I2)") IDAY
          SPOOL_SOLVE_REVISION_DATE_JD =  FJLDY(IMONTH,IDAY,IYEAR)
          SR_FOUND = .TRUE.
        END IF
        READ(40,'(a)') cbuf
      END DO
!
      IF (.NOT.SR_FOUND) THEN
!
!       Spool file is too old to have a solve revision date in the top part.
!       Set the "solve revision date" to 1/1/1979.
!
        IYEAR = 1979
        IMONTH = 1
        IDAY = 1
        SPOOL_SOLVE_REVISION_DATE_JD =  FJLDY(IMONTH,IDAY,IYEAR)
      END IF
!
!  B. Set up common block variables that identify any spoolfile formats.
!
!
!     Until April 2002, there was a standard spoolfile format in place
!     for years.  That is, adjustment values were in a stable set of columns.
!     Some older formats existed, but for the most part the "normal" columns
!     were used by most spoolfiles.  This is changing in May 2002, with the
!     expansion of the adjustment line parameter index from i4 to i5.
!     Use the pre-May 2002 column positions as a baseline, and reference to
!     them, tracking the date at which each new spoolfile change or set of
!     changes is added to solve and then comparing the solve revision date
!     in the spool file to the various changeover dates to determine the
!     presumed format of the spoolfile.
!
!     1.  Identify the May 2002 changeover from i4 parameter adjustment line
!         indices to i5.
!
      ichangeover_year(1) = 2002
      ichangeover_month(1) = 5
      ichangeover_day(1) = 31
!
!     2.  Identify the 12.26.2002 changeover for the following reference
!           date/epoch changes:
!           expand epoch to yyyy.mm.dd-yy:hh:mm in the
!             station position epoch and
!             station correlation reference epochs
!           adding a reference date to the end of the source correlation line
!
      ichangeover_year(2) = 2002
      ichangeover_month(2) = 12
      ichangeover_day(2) = 26
!
!     Set the total number of changeover dates used so far and set up
!     all julian dates.
!
      num_changeovers = 2
      do ichange = 1,num_changeovers
        changeover_jd(ichange) = fjldy(ichangeover_month(ichange), &
     &                       ichangeover_day(ichange), &
     &                       ichangeover_year(ichange))
      end do
!
      RETURN
      END
