! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE TLE_DAYS2MDHMS
! *
! *  This subroutine converts the day of the year, days, to the equivalent month
! *    day, hour, Minute and second.
! *
! *  Algorithm     : Set up array for the Number of days per month
! *                  Find Leap Year - be sure to account for the 400 years
! *                  Loop through a Temp value for WHILE the value is .lt. the days
! *                  Perform INTEGER conversions to the correct day and month
! *                  Convert remainder into H M S using type conversions
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Year        - Year                          +1900 .. 2100+
! *    Days        - Julian Day of the year         0.0D0  .. 366.0D0
! *
! *  OutPuts       :
! *    Mon         - Month                          1 .. 12
! *    Day         - Day                            1 .. 28,29,30,31
! *    Hr          - Hour                           0 .. 23
! *    Min         - Minute                         0 .. 59
! *    Sec         - Second                         0.0D0 .. 59.999D0
! *
! *  Locals        :
! *    DayofYr     - Day of year
! *    Temp        - Temporary REAL*8 values
! *    IntTemp     - Temporary INTEGER value
! *    i           - Index
! *    LMonth[12]  - INTEGER Array containing the Number of days per month
! *
! *  Coupling      :
! *    None.
! * -----------------------------------------------------------------------------

      SUBROUTINE TLE_DAYS2MDHMS ( Year, Days, Mon,Day,Hr,Min,Sec )
        IMPLICIT NONE
        REAL*8 Days,Sec
        INTEGER Year, Mon, Day, Hr, Min
! * ----------------------------  Locals  -------------------------------
        INTEGER IntTemp,i,DayofYr, LMonth(12)
        REAL*8 Temp

        ! --------------------  Implementation   ----------------------
        ! -------------- Set up array of days in month  ---------------
        DO i = 1,12
            LMonth(i) = 31
          ENDDO
        LMonth( 2) = 28
        LMonth( 4) = 30
        LMonth( 6) = 30
        LMonth( 9) = 30
        LMonth(11) = 30

        DayofYr= IDINT(Days )

        ! ---------------- Find month and Day of month ----------------
        IF (MOD(Year,4).eq.0) THEN
            LMonth(2)= 29
          ENDIF
        i= 1
        IntTemp= 0
        DO WHILE ( (DayofYr.gt.IntTemp + LMonth(i) ) .and. ( i.lt.12 ))
            IntTemp= IntTemp + LMonth(i)
            i= i+1
          ENDDO
        Mon= i
        Day= DayofYr - IntTemp

        ! ---------------- Find hours Minutes and seconds -------------
        Temp= (Days - DayofYr )*24.0D0
        Hr  = IDINT( Temp )
        Temp= (Temp-Hr) * 60.0D0
        Min = IDINT( Temp )
        Sec = (Temp-Min) * 60.0D0

      RETURN
      END  ! end tle_days2mdhms
