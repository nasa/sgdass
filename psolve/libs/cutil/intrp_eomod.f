      SUBROUTINE INTRP_EOMOD(TIME_IN,UT1_VAL,SHORTP,XWOB_VAL,YWOB_VAL, &
     &    INTRP_RATES,UT1_RATE,XWOB_RATE,YWOB_RATE,ITAB_POINTS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  INTRP_EOMOD PROGRAM SPECIFICATION
!
! 1.1 Interpolate to compute a priori value (adopted from CALC) via
!     Tony Mallama old routine INTRP in SETFL.
!     Modified form of INTRP_EOVR to use values in E.O. mod file rather
!     than E.O. values from data base. 91JUN25  -DG-
!     Modified by JMGipson Nov 5, 1993 so that values on tabular
!     points are handled the same as in intrp_eovr.
!
! 1.2 REFERENCES:
!
! 2.  INTRP_EOMOD INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 TIME,TIME_IN
      LOGICAL*2 INTRP_RATES
      integer*4 itime
!
!      TIME   Full Real*8 Julian date at which to interpolate.
!      INTRP_RATES - whether or not to calculate rates
!
! 2.3 OUTPUT Variables:
!
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
!      UT1_RATE,   apriori ut1, x-wobble and y-wobble rates at input
!      XWOB_RATE,  julian date.  Derivatives of interpolated values.
!      YWOB_RATE   (ASSUMPTIONS:
!                  currently rates are only needed for epochs which do not
!                  fall on a tabular point.  It is not anticipated that
!                  rates will be needed at the tabular points, so the
!                  calculations for this case will not be coded at this time,
!                  and this case will just return zeroes.
!            (xwob_rate & ywob_rate are in radians/day)
!            (ut1_rate is in time seconds/day)
!      ITAB_POINTS - now obsolete.
!
! NOTE:
!      The interpolation tables are assumed to be in glbc4
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: a2jst
!       CALLED SUBROUTINES: ut1zt
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 KERR, KKK, JERR, IDSP, J, I, IIROT, N, ICXX, II, ICHAR, &
     &IY, IX, K, NUM_MASTER,MASTER(40), IH, IMIN, IYR, ID, IM, &
     &NRXY,NXY,NRU,NU,NNXY,ILASTXY,INTXY,NNU,ILASTU,INTU,JJ,ICOUNT, &
     &ISTOP,mode2
      LOGICAL*2 ITEST
!
      REAL*8 XJD,FJLDY,TU,TXY,TC2000, &
     &       XINTU(4),XINTX(4),XINTY(4),F2U,F2X,F2Y, &
     &       Y1U(2),Y2U(2),Y1X(2),Y2X(2),Y1Y(2),Y2Y(2),SU,SXY
      REAL*8 PTU1,PTU2,PTX1,PTX2,PTY1,PTY2
      REAL*8 RPS_TO_SPD
      REAL*8 FA(5), FAD(5), FA2K(14), FAD2K(14)
!
      DATA RPS_TO_SPD /1188085283.983275D0/ !rad/time sec to time sec/day
!
! 4.  HISTORY
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
!  DG   91JUN25  Copied INTRP_EOVR, changed name to INTRP_EOMOD,
!                modified to work with E.O. mod file.
!  KDB  91.08.16 Modified to calculate rate aprioris at tabular points
!                (before just returned zeroes and set itab_points to
!                 indicate error)
!  DG    91AUG27 Replaced interpolations with calls to ut1pm_int and
!                enabled linear interpolation. Interpolation controlled
!                by 'flyby_map': 1 ==> linear interpolation, 3==> cubic
!                interpolation.
!
!                Modified by JMGipson Nov 5, 1993 so that values on tabular
!                points are handled the same as in intrp_eovr.
!  :93.12.16:jwr:Modified to support cubic spine. Call to ut1pm_int changed.
!  :93.12.28:jwr:Modified to support ut1r and ut1s.
!  16-APR-99 PET Put some variables which previously were "saved" to a common
!                block defined in flyby.i
!  2000.01.31  pet  Fixed a bug: TC2000 was unintiliazed in UT1S mode
!  2000.01.31  pet  Added NUTF96 instead of NUTFA call for Calc 9 databases
!  2000.03.19  pet  Added support of value 'N' of UT1_RS_FLYBY (no zonal tide
!                   interpolation magic for mapping EOP)
!  2005.12.28  DG   Mod to support mapping of EOP into Calc 10 databases
!                   when optional UT1 smoothing applied. 
!  2006.04.05  pet  Fixed a bug related to Calc-10 transition
!
!     INTRP_EOMOD program structure
!
!CCCC
!
! --- Compute the fortnight tidal (so called short period) correction
! --- needed to be added to UT1R to make it into UT1.
!
      IF ( DABS(DMOD(TIME_IN+0.5D0,1.D0)) .LT. 1.D-9 ) THEN
           TIME = TIME_IN - 1.D-8
        ELSE
           TIME = TIME_IN
      ENDIF
!
      SHORTP = 0.D0
      DOMEGA = 0.D0
!
!     ut1r to ut1 conversion:
!     no check needed here to make sure data comes in as ut1r - check
!     already done before solution done
!
      TC2000 = (TIME - 2451545.0D0) / 36525.D0
      IF ( UT1_RS_FLYBY .EQ. 'R' ) THEN
           CALL UT1ZT ( TC2000, DUT, DLOD, DOMEGA )
           SHORTP = -DUT
        ELSE IF ( UT1_RS_FLYBY .EQ. 'S' ) THEN
           IF ( CALCV .LE. 8.200001 ) THEN
                CALL NUTFA  ( TIME, 0.D0, TC2000, FA, FAD )
                CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
              ELSE IF ( CALCV .GE. 9.0 .AND. CALCV .LT. 9.99 ) THEN
                CALL NUTF96 ( TIME, 0.D0, TC2000, FA, FAD )
                CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
              ELSE IF ( CALCV .GT. 9.99 .AND. CALCV .LT. 99.99 ) THEN
                CALL NUTFA10 (TIME, 0.D0, TC2000, FA2K, FAD2K)
                CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
           END IF
           SHORTP = -DUT
        ELSE IF ( UT1_RS_FLYBY .EQ. 'N' ) THEN
           SHORTP = 0.0
           DOMEGA = 0.0
        ELSE
           CALL FERR ( INT2(9001), &
     &         "INTERP_EOMOD: UT1_RS_FLYBY not defined. "// &
     &         "Must be R or S or N.", INT2(0), INT2(0) )
      ENDIF
!
! --- Convert the rate part of the ut1r to ut1 conversion from
! --- rad/sec to time secs/day
!
      DOMEGA = -DOMEGA * RPS_TO_SPD
!
! --- Done with ut1r to ut1 conversion
!
! --- Now generate the ut1 offset and rate aprioris
! --- Values will be generated for all input epochs via cubic interpolation,
! --- even if the epoch is a tabular point (agrees at a level of 1.d-8 days)
!
! --- Interpolation code replaced with call to ut1pm_int which does the
! --- same thing
!
      IF ( INTRP_RATES ) THEN
           MODE2 = 1
        ELSE
           MODE2 = 0
      ENDIF
!
      call ut1pm_int(TIME,UT1INV,UT1PTV,UT1_VAL,UT1_RATE, &
     &               flyby_interp,mode2,ut1_yp1,ut1_ypn, &
     &               ut1_y2,ut1_table,ut1_initialized )
!
      UT1_VAL = UT1_VAL + SHORTP
!     Note: units for ut1_rate are time sec/day
      UT1_RATE = UT1_RATE + DOMEGA
!
! *** Now for polar motion
!
      call ut1pm_int(TIME,WOBINV,WOBXXV,XWOB_VAL,XWOB_RATE, &
     &               flyby_interp,mode2, xw_yp1, xw_ypn, &
     &                xw_y2, xw_table, xw_initialized )
!
!
      call ut1pm_int(TIME,WOBINV,WOBYYV,YWOB_VAL,YWOB_RATE, &
     &               flyby_interp,mode2, yw_yp1, yw_ypn, &
     &                yw_y2, yw_table, yw_initialized )
!
!  Convert to radians (per day)
      XWOB_VAL = XWOB_VAL/206264806.2D0
      YWOB_VAL = YWOB_VAL/206264806.2D0
      if(mode2.eq.1) then
       XWOB_RATE = XWOB_RATE/206264806.2D0
       YWOB_RATE = YWOB_RATE/206264806.2D0
      endif
!
      RETURN
      END
