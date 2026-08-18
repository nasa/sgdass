      SUBROUTINE INTRP_EOVR(TIME,UT1_VAL,SHORTP,XWOB_VAL,YWOB_VAL, &
     &    INTRP_RATES,UT1_RATE,XWOB_RATE,YWOB_RATE,ITAB_POINTS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Interpolate to compute a priori value from the eop series used
!     by calc.
!
!     Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
!     INPUT Variables:
      REAL*8 TIME
      LOGICAL*2 INTRP_RATES
!
!      TIME   Full Real*8 Julian date at which to interpolate.
!      INTRP_RATES - whether or not to calculate rates
!
!     OUTPUT Variables:
      REAL*8 SHORTP,UT1_VAL,XWOB_VAL,YWOB_VAL,UT1_RATE,XWOB_RATE, &
     &       YWOB_RATE
      INTEGER*2 ITAB_POINTS(2)
!
!      UT1_VAL - interpolated  value of "TAI - UT1" including the
!                effect of the Yoder tidal model.  Note the sense of
!                of the difference. (seconds)
!      SHORTP  - The contribution of the Yoder tidal model to UT1-IAI.
!                Note the sign (or sense) of this value. (seconds)
!      XWOB_VAL- interpolated value of x-pole (radians)
!      YWOB_VAL- interpolated value of y-pole (radians)
!
!      UT1_RATE, -  apriori ut1, x-wobble and y-wobble rates at input
!      XWOB_RATE,   julian date.  Derivatives of interpolated values.
!                   xwob_rate & ywob_rate are in radians/day.
!                   ut1_rate is in seconds/day.
!      ITAB_POINTS  No longer used. Set to zero.
!
! NOTE:
!     The interpolation tables are assumed to be in SOCOM
!
!     COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
!     CALLING SUBROUTINES: a2jst, blkcl, check_erot, rotin
!     CALLED SUBROUTINES: ut1zt
!
!     LOCAL VARIABLES
      INTEGER*2 KERR, KKK, JERR, IDSP, J, I, IIROT, N, ICXX, II, ICHAR, &
     &IY, IX, K, NUM_MASTER,MASTER(40), IH, IMIN, IYR, ID, IM, &
     &NRXY,NXY,NRU,NU,NNXY,ILASTXY,INTXY,NNU,ILASTU,INTU,JJ,ICOUNT, &
     &ISTOP
      LOGICAL*2 ITEST
!
      REAL*8 XJD,FJLDY,TU,TXY,TC2000, &
     &       XINTU(4),XINTX(4),XINTY(4),F2U,F2X,F2Y, &
     &       Y1U(2),Y2U(2),Y1X(2),Y2X(2),Y1Y(2),Y2Y(2),SU,SXY
      REAL*8 PTU1,PTU2,PTX1,PTX2,PTY1,PTY2
      REAL*8 RPS_TO_SPD
      REAL*8 FA(5), FAD(5), FA2K(14), FAD2K(14)
      character*80 bufstr
!
      DATA RPS_TO_SPD /1188085283.983275D0/ !rad/time sec to time sec/day
!
!  HISTORY
!  WHO  WHEN     WHAT
!
!  JWR  89.05.02 Cobbled up from Tony's INTRPT.  Interface changed
!                to make it more modular (for general CUTIL use.)
!                Documentation added and error checks improved.
!  JWR  89.05.18 Additional logic added to support UT1 versus UT1R.
!  JWR  89.05.19 Out of range error message improved.
!  JWR  89.05.19 Logic added to handle the case when the requested
!                time is the time of a tabular point.  Also flow
!                reordered to facilate this change.
!  KDB  90.12.05 Added ability to calculate rates, renamed module
!                from INTRP_EROT.
!  jwr  90.06.17 Changed algoritm for calculating rate and intoduced
!                linear interpolation as needed.
!                Special handling of times at the times of tablular points
!                eliminated.
! :91:08:15:JWR: Code for pathological case of hitting exactily on a
!                tabular point implimented.
! :93.12.14:jwr: At the time cubic spine was implimented all the lines
!                in this routine which did the actual interpolation were
!                striped out and replaced with a call to ut1pm_int. Thus
!                all eop interpolation in solve is in one routine. Good
!                practice.
! :93.12.27:jwr: Code added for ut1s.
!  16-APR-99 PET Put some variables which previously were "saved" to a common
!                block defined in flyby.i
!  1999.11.08  PET  Updated for compatibility with CALC 9.0
!  2000.01.31  pet  Added NUTF96 instead of NUTFA call for Calc 9 databases
!  2000.03.19  pet  Added support of value 'N' of UT1_RS (no zonal tide
!                   interpolation magic for mapping EOP)
!  2005.12.29  DG   Updated for Calc 10, allows true UT1, the normal Calc
!                   10 mode (no UT1 smoothing).
!  2006.07.18  DG   Updated to duplicate Calc 10's zonal tide removal
!                   non-default option. 
!
!     EOVR PROGRAM STRUCTURE
!
!     Compute the fortnight tidal (so called short period) correction
!     needed to be added to UT1R to make it into UT1.  If already
!     UT1, there's a problem - sdbh was supposed to convert it to ut1r,
!     for use in solve
!
!CCCCC
!
      TC2000 = (TIME - 2451545.0D0) / 36525.D0
      SHORTP = 0.D0
      DOMEGA = 0.D0
      IF ( .NOT. SHORT_UT1_IN ) THEN ! ut1r - convert
           IF ( UT1_RS .EQ. 'R' ) THEN
                CALL UT1ZT ( TC2000, DUT, DLOD, DOMEGA )
                SHORTP = -DUT
             ELSE IF ( UT1_RS .EQ. 'S' ) THEN
!
                IF ( CALCV .LE. 8.200001 ) THEN
                    CALL NUTFA  ( TIME, 0.D0, TC2000, FA, FAD )
                  ELSE IF (CALCV .GE. 9.0 .AND. CALCV .LT. 9.99) THEN
                    CALL NUTF96 ( TIME, 0.D0, TC2000, FA, FAD )
                  ELSE IF (CALCV .GT. 9.99 .AND. CALCV .LT. 99.99) THEN
                    CALL NUTFA10 (TIME, 0.D0, TC2000, FA2K, FAD2K)
                END IF
                IF ( CALCV .LT. 9.0 ) THEN
                     CALL UT1S_82 ( FA, DUT, DLOD, DOMEGA )
                   ELSE IF (CALCV .GE. 9.0 .AND. CALCV .LT. 9.99) THEN
                     CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                  ELSE IF (CALCV .GT. 9.99 .AND. CALCV .LT. 99.99) THEN
                    CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
                END IF
                SHORTP = -DUT
             ELSE IF ( UT1_RS .EQ. 'N' ) THEN
                SHORTP = 0.0
                DOMEGA = 0.0
             ELSE
               CALL FERR ( INT2(9001), "INTERP_EOVR: ut1_rs not defined. "// &
     &             "Must be R or S or N.", INT2(0), INT2(0) )
           ENDIF
         ELSE
           IF ( CALCV .GT. 6.00  .AND. CALCV .LT. 9.99 ) THEN
                CALL FERR ( INT2(9002), "INTRP_EOVR: ut1 data not true ut1. "// &
     &                      "Quitting!", INT2(0), INT2(0) )
              ELSE
                SHORTP = 0.0
                DOMEGA = 0.0
           ENDIF
      ENDIF
!
! --- Convert the ut1r conversion from rad/sec to time secs/day.
!
      DOMEGA = -DOMEGA * RPS_TO_SPD
!
! --- If cubic spline used, then let the subroutines do the work.
!
      If((interpolation_ut1.eq.4 .and. interpolation_pm .ne.4) .or. &
     &   (interpolation_pm .eq.4 .and. interpolation_ut1.ne.4))then
        call addstr_f("UT1 and pm must both or neither use spline." )
        call nl_mn()
        call refresh_mn()
        stop 'intrp_eovr: Bad interpolation scheme'
      endif
!
!     Do ut1 and restore short period correction computed above,
      call ut1pm_int( time, ut1inb, ut1ptb, ut1_val, ut1_rate, &
     &     interpolation_ut1, INT2(1), ut1_zp1, ut1_zpn, ut1_z2, ut1_epochs, &
     &     ut1_zinitialized )
      ut1_val  = ut1_val  + shortp
      ut1_rate = ut1_rate + domega
!
!     Do x-pole and convert from milliarcseconds to radians
      call ut1pm_int( time, wobinb, wobxxb, xwob_val, xwob_rate, &
     &     interpolation_pm, INT2(1), wx_zp1, wx_zpn, wx_z2, wx_epochs, &
     &     wx_zinitialized )
      xwob_val  = xwob_val *2.D0*PI__NUM/(1000.d0*3600.d0*360.d0)
      xwob_rate = xwob_rate*2.D0*PI__NUM/(1000.d0*3600.d0*360.d0)
!
!     Do y-pole and convert from milliarcseconds to radians
      call ut1pm_int( time, wobinb, wobyyb, ywob_val, ywob_rate, &
     &     interpolation_pm, INT2(1), wx_zp1, wx_zpn, wy_z2, wy_epochs, &
     &     wy_zinitialized )
      ywob_val  = ywob_val *2.D0*PI__NUM/(1000.d0*3600.d0*360.d0)
      ywob_rate = ywob_rate*2.D0*PI__NUM/(1000.d0*3600.d0*360.d0)
!
      if(.not.intrp_rates) then
        ut1_rate = 0.d0
        xwob_rate= 0.d0
        ywob_rate= 0.d0
      endif
!
      RETURN
      END
