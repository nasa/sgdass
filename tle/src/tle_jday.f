! * --------------------------------------------------------------------------!--
! *
! *                           SUBROUTINE TLE_JDAY
! *
! *  This subroutine finds the Julian date given the Year, Month, Day, and Time.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Year        - Year                           1900 .. 2100
! *    Mon         - Month                          1 .. 12
! *    Day         - Day                            1 .. 28,29,30,31
! *    Hr          - Universal Time Hour            0 .. 23
! *    Min         - Universal Time Min             0 .. 59
! *    Sec         - Universal Time Sec             0.0D0 .. 59.999D0
! *    WhichType   - Julian .or. Gregorian calender   'J' .or. 'G'
! *
! *  Outputs       :
! *    JD          - Julian Date                    days from 4713 BC
! *
! *  Locals        :
! *    B           - Var to aid Gregorian dates
! *
! *  Coupling      :
! *    None.
! *
! *  References    :
! *    Vallado       2007, 189, Alg 14, Ex 3-14
! * -----------------------------------------------------------------------------

      SUBROUTINE TLE_JDAY ( Year,Mon,Day,Hr,minute, Sec, JD, JDFrac )
        IMPLICIT NONE
        INTEGER Year, Mon, Day, Hr, minute
        REAL*8  Sec, JD, dtt, JDFrac

        ! --------------------  Implementation   ----------------------
        JD= 367.0D0 * Year                                              &
     &        - INT( (7* (Year+INT ( (Mon+9)/12) ) ) * 0.25D0 )         &
     &        + INT( 275*Mon / 9 )                                      &
     &        + Day + 1721013.5D0  ! use - 678987.0 to go to mjd directly
        JDFrac = ( (Sec/60.0D0 + minute ) / 60.0D0 + Hr ) / 24.0D0
     
! *       check that the day and fractional day are correct
		IF (DABS(JDFrac).gt.1.0D0) THEN
			dtt = INT(JDFrac)
			JD = JD + dtt
			JDFrac = JDFrac - dtt
		ENDIF	
! *     &      - 0.5D0*DSIGN(1.0D0, 100.0D0*Year + Mon - 190002.5D0) + 0.5D0
      RETURN
      END
