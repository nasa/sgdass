! *     ----------------------------------------------------------------
! *
! *                               sgp4ext.for
! *
! *    this file contains extra routines needed for the main test program for sgp4.
! *    these routines are derived from the astro libraries.
! *
! *                            companion code for
! *               fundamentals of astrodynamics and applications
! *                                    2007
! *                              by david vallado
! *
! *       (w) 719-573-2600, email dvallado@agi.com
! *
! *    current :
! *               2 apr 07  david vallado
! *                           misc updates for new baseline
! *    changes :
! *              14 aug 06  david vallado
! *                           original baseline
! *       ----------------------------------------------------------------
! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE TLE_INVJDay
! *
! *  This subroutine finds the Year, month, day, hour, Minute and second
! *  given the Julian date. TU can be UT1, TDT, TDB, etc.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    JD          - Julian Date                    days from 4713 BC
! *
! *  OutPuts       :
! *    Year        - Year                           1900 .. 2100
! *    Mon         - Month                          1 .. 12
! *    Day         - Day                            1 .. 28,29,30,31
! *    Hr          - Hour                           0 .. 23
! *    Min         - Minute                         0 .. 59
! *    Sec         - Second                         0.0D0 .. 59.999D0
! *
! *  Locals        :
! *    Days        - Day of year plus fractional
! *                  portion of a day               days
! *    Tu          - Julian Centuries from 0 h
! *                  Jan 0, 1900
! *    Temp        - Temporary real values
! *    LeapYrs     - Number of Leap years from 1900! 
! *
! *  Coupling      :
! *    DAYS2MDHMS  - Finds MD HMS given Days and Year
! *
! *  References    :
! *    Vallado       2007, 208, Alg 22, Ex 3-13
! * -----------------------------------------------------------------------------

      SUBROUTINE TLE_INVJDAY ( jd, jdFrac, Year,Mon,Day,Hr,minute, Sec )
      IMPLICIT NONE
      INTEGER Year, Mon, Day, Hr, minute
      REAL*8  Sec, jd, jdFrac
! * ----------------------------  Locals  -------------------------------
      INTEGER LeapYrs
      REAL*8  Days, Tu, Temp, dt, dtt

      ! --------------------  Implementation   ----------------------
		! check jdfrac for multiple days
		if (ABS(jdFrac) .ge. 1.0D0) THEN
			dtt = IDINT(jdFrac);
			jd = jd + dtt
			jdFrac = jdFrac - dtt
		ENDIF

		! check for fraction of a day included in the jd
		dt = jd - IDINT(jd) - 0.5
		if (ABS(dt) .gt. 0.00000001D0) THEN
			jd = jd - dt;
			jdFrac = jdFrac + dt
        ENDIF 
         
        ! ---------------- Find Year and Days of the year -------------
        Temp   = jd-2415019.5D0
        Tu     = Temp / 365.25D0
        Year   = 1900 + IDINT( Tu )
        LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
        Days   = IDINT(Temp - ((Year-1900)*365.0D0 + LeapYrs ))

        ! -------------- Check for case of beginning of a year --------
        IF ( Days + jdFrac .lt. 1.0D0 ) THEN
            Year   = Year - 1
            LeapYrs= IDINT( ( Year-1901 )*0.25D0 )
            Days   = IDINT(Temp - ((Year-1900)*365.0D0 + LeapYrs ))
          ENDIF

        ! ------------------ Find remaing data  -----------------------
        CALL TLE_DAYS2MDHMS( Year,Days + jdFrac, Mon,Day,Hr,minute,Sec )

      RETURN
      END
