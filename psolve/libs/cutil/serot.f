      SUBROUTINE SEROT ( LNAME, KFBDSP )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Call subroutine GEROT to read an earth orientation data substitution
!     file and to check for compatibility of UT1 (Yoder terms in/out)
!     in the two E.O. series
!
!     Parameter File
      INCLUDE 'solve.i'
!
!     INPUT Variables:
      LOGICAL*2 KFBDSP
      CHARACTER*(*) LNAME
!
!     KFBDSP - TRUE if flyby info is to be displayed
!     LNAME - Earth orientation mod file name
!
!     Output data:
!     UT1PTV - UT1-TAI's from mod file
!     UT1SIG - Sigma for UT1, from mod file
!     WOBXXV - X-wobble values, from mod file
!     WOBYYV - Y-wobble values, from mod file
!     WBXSIG - Sigma for X wobble, from mod file
!     WBYSIG - Sigma for Y Wobble, from mod file
!     WXUCOR - Correlation between X wobble and UT1, from mod file
!     WXYCOR - Correlation between X wobble and Y wobble, from mod file
!     WYUCOR - Correlation between Y wobble and UT1, from mod file
!
!     COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES: gerot
!
!     LOCAL VARIABLES
!
      REAL*8 tc2000,dut,dlod,domega,fa(5),fad(5),time
      REAL*8 FA2K(14), FAD2K(14)
      INTEGER*2 n,nn,i
      character*4 Yoder_cond,char1
      Character*150 errstr
      character*80 bufstr
      Character*1 xinterp
      integer*4 ichar1,ix,iy
      equivalence (ichar1,char1)
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   DG    910701   Added GLBC4 common block, changed E.O. differences to
!                  totals from E.O. mod file, changed call to GEROT,
!                  removed call to DEROT, removed conversion to radians.
!                  Added UT compatibility check. Must be UT1R from data
!                  base's series. If UT1 from mod file, convert to UT1R.
!   DG    910823   Checks value of flyby_interp, the flag controlling the
!                  UT1PM mod file interpolation scheme and sets default
!                  value if necessary. Defaults are 1 (linear) for a
!                  1-day series, 3 (cubic) for a 5-day series.
!
!     :93.12.15:jwr: Substantial changes to support cubic spline interpolation.
!     :94.01.27:jwr: Text in dialog improved and some blank lines added.
!     990108    pet  Updated comments
!   2000.01.31  pet  Fixed a bug: TC2000 was unintiliazed in UT1S mode
!   2000.01.31  pet  Added NUTF96 instead of NUTFA call for Calc 9 databases
!   2000.03.19  pet  Added support of value 'N' for EOP interpolation algorithm
!   2005.12.22  DG   Mods for Calc 10 to allow true UT1 interpolation.
!
!     SEROT PROGRAM STRUCTURE
!
!CCCCC
!
! --- Get values from the earth orientation data substitution file
!
      CALL GEROT ( LNAME, KFBDSP, YODER_COND )
      IF ( LNAME .EQ. 'NONE' ) RETURN
!
! --- Check for UT1 compatibility between the two series. The underlying
! --- series from the data base in SOCOM is supposed to be UT1R (Yoder
! --- short period terms removed.) We send a nasty message to the user
! --- and quit if this is not the case. The series from the mod file can
! --- be either full UT1 or UT1R. If it's UT1, we convert the tabular
! --- points to UT1R. All interpolations must be done using UT1R (in
! --- subroutine flyby_map).
!
! --- For Calc 10: The underlying UT1 series from the data base in SOCOM 
! --- will normally be true UT1 (tidal terms not removed). The series from
! --- the mod file should be the same.
!
! --- Handle the selection of ut1R versus ut1S smoothing for interpolation.
!
      IF ( .NOT. KBATCH ) THEN
           IF ( CALCV .GT. 6.0  .AND.  CALCV .LT. 9.99 ) THEN
                DO I = 1,3 ! Write 3 blank lines
                   CALL ADDSTR_F ( "  " )
                   CALL NL_MN()
                ENDDO
!
 910            CONTINUE
                CALL ADDSTR_F ( " Which algorithm in the EOP interpolation "// &
     &                          " to use: UT1R, UT1S or NO_ZONAL? " )
                CALL NL_MN()
                CALL ADDSTR_F ( " Use UT1S or UT1R or NO_ZONAL ((S)/R/N)?  " )
                CALL NL_MN()
!
                CALL SENKR_MN ( IX, IY, ICHAR1 )
                UT1_RS_FLYBY = CHAR1(4:4)
                IF ( CHAR1(4:4) .EQ. CHAR(13) ) CHAR1(4:4) = ' '
                CALL CASEFOLD ( UT1_RS_FLYBY )
                IF ( UT1_RS_FLYBY .EQ. ' ' ) UT1_RS_FLYBY = 'S'
                IF ( UT1_RS_FLYBY .NE. 'R'  .AND. &
     &               UT1_RS_FLYBY .NE. 'S'  .AND. &
     &               UT1_RS_FLYBY .NE. 'N'         ) THEN
!
                     CALL NL_MN()
                     CALL ADDSTR_F ( " Enter R or S or N " )
                     CALL NL_MN()
                     GOTO 910
               ENDIF
             ELSE IF ( CALCV .GT. 9.99 .AND. CALCV .LT. 99.99 ) THEN
!
! ------------ For Calc 10. make UT1 flyby smoothing match the database, 
! ------------ normally true UT1
!
               UT1_RS_FLYBY = UT1_RS
               CALL NL_MN()
               IF (UT1_RS_FLYBY .EQ. 'N') CALL ADDSTR_F ( " NO UT1 Smoothing  " )
               IF (UT1_RS_FLYBY .EQ. 'S') CALL ADDSTR_F ( " UT1S Smoothing  " )
           ENDIF
      ENDIF
!
      IF ( CALCV .GT. 6.00 .AND. CALCV .LT. 9.99 ) THEN
           IF ( SHORT_UT1_IN ) THEN ! Full UT1 in data base -- not allowed!
                WRITE ( 6, * ) ' CALCV=', CALCV 
                ERRSTR = '(serot) SOCOM file contains full UT1! '// &
     &                   'Terminating in subroutine serot.'
                CALL FERR ( INT2(159), ERRSTR, INT2(0), INT2(0) )
           ENDIF
      ENDIF
!
      IF ( YODER_COND .EQ. 'UT1 ' ) THEN ! Remove short period terms
           NN = UT1INV(3)                ! from mod file tabular values
           DO N=1,NN
              IF ( UT1_RS_FLYBY .EQ. 'S' ) THEN ! Use use new ut1s algorithm.
                   TC2000 = (UT1INV(1) + (N-1)*UT1INV(2)-2451545.0D0)/36525.D0
                   TIME = UT1INV(1) + (N-1)*UT1INV(2)
                   IF ( CALCV .LE. 8.200001 ) THEN
                        CALL NUTFA  ( TIME, 0.D0, TC2000, FA, FAD )
                        CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GE. 9.0 .AND. CALCV .LT. 9.99 ) THEN
                        CALL NUTF96 ( TIME, 0.D0, TC2000, FA, FAD )
                        CALL UT1S_83 ( FA, FAD, DUT, DLOD, DOMEGA )
                     ELSE IF ( CALCV .GT. 9.99 .AND. CALCV .LT. 99.99 ) THEN
                        CALL NUTFA10 ( TIME, 0.D0, TC2000, FA2K, FAD2K )
                        CALL UT1S2K (FA2K, FAD2K, DUT, DLOD, DOMEGA)
                   END IF
                ELSE IF ( UT1_RS_FLYBY .EQ. 'R' ) THEN ! Use old Yoder ut1r algorithm
                   TC2000 = (UT1INV(1) + (N-1)*UT1INV(2)-2451545.0D0)/36525.D0
                   CALL UT1ZT ( TC2000, DUT, DLOD, DOMEGA )
                ELSE IF ( UT1_RS_FLYBY .EQ. 'N' ) THEN !
                   DUT    = 0.0
                   DLOD   = 0.0
                   DOMEGA = 0.0
                ELSE
                   ERRSTR = "serot: ut1_rs_flyby undefined. Must be R or S."
                   CALL FERR ( INT2(160), ERRSTR, INT2(0), INT2(0) )
              ENDIF
!
              UT1PTV(N) = UT1PTV(N) + DUT
           ENDDO
         ELSE ! All flyby files must be true ut1.
           ERRSTR = "serot: yoder_cond="//YODER_COND//" -- it is wring! Eop "// &
     &              "flyby file must be true ut1."
           CALL FERR ( INT2(161), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Checking the mod file interpolation control flag, flyby_interp:
! --- Check BATCH mode flag, KBATCH. In Batch mode, flyby_interp will
! --- already be set. If its zero, we set it to 1 (linear) for a 1-day
! --- series or to 3 (cubic) for a 5-day series. If we are in interactive
! --- mode, we reset it each time a new mod file is read in, then we give
! --- the user a chance to change from the default interpolation mode.
!
! --- Mod 2005.12.22. Change default to cubic spline.
!
      IF ( KBATCH ) THEN   ! In BATCH mode
           IF ( FLYBY_INTERP .NE. 1  .AND. &
     &          FLYBY_INTERP .NE. 3  .AND. &
     &          FLYBY_INTERP .NE. 4         ) THEN
!
!!!!!!          IF ( ABS(UT1INV(2) - 1.0) .LT. 0.001 ) FLYBY_INTERP = 1
                IF ( ABS(UT1INV(2) - 1.0) .LT. 0.001 ) FLYBY_INTERP = 4
                IF ( ABS(UT1INV(2) - 5.0) .LT. 0.001 ) FLYBY_INTERP = 3
           ENDIF
!
          ELSE       ! In interactive mode
           IF ( ABS(UT1INV(2) - 1.d0) .LT. 0.001 ) THEN
!!!!!!          FLYBY_INTERP = 1
                FLYBY_INTERP = 4
!
                DO I = 1,3
                   CALL ADDSTR_F("  " )
                   CALL NL_MN()
                ENDDO
!
                CALL ADDSTR_F ( " 1-day Earth Orientation mod file:" )
                CALL NL_MN()
!
                CALL ADDSTR_F ( "L      for linear interpolation" )
                CALL NL_MN()
!
                CALL ADDSTR_F ( "C      for old style cubic interpolation." )
                CALL NL_MN()
!
                CALL ADDSTR_F ( "S      for cubic spline interpolation." )
                CALL NL_MN()
!
!!!!!!!         XINTERP = 'L'
                XINTERP = 'S'
                CALL SENKR_MN ( IX, IY, ICHAR1 )
                XINTERP = CHAR1(4:4)
                IF ( XINTERP .EQ. 'L'  .OR.  XINTERP .EQ. 'l' ) FLYBY_INTERP=1
                IF ( XINTERP .EQ. 'C'  .OR.  XINTERP .EQ. 'c' ) FLYBY_INTERP=3
                IF ( XINTERP .EQ. 'S'  .OR.  XINTERP .EQ. 's' ) FLYBY_INTERP=4
           ENDIF
!
           IF ( ABS(UT1INV(2) - 5.0) .LT. 0.001 ) THEN
                FLYBY_INTERP = 3
!
                CALL ADDSTR_F ( " 5-day Earth Orientation mod file will " )
                CALL NL_MN()
!
                CALL ADDSTR_F ( " interpolate with simple cubic." )
                CALL NL_MN()
!
                CALL ADDSTR_F ( " Hit RETURN to continue" )
                CALL NL_MN()
!
                CALL ADDSTR_F ( " L to get linear interpolation." )
                CALL NL_MN()
!
!
                CALL ADDSTR_F (" S to get cubic spline interpolation." )
                CALL NL_MN()
!
                XINTERP = 'C'
                CALL GETSTR_F ( BUFSTR )
                READ ( BUFSTR, '(A1)' ) XINTERP
                IF ( XINTERP.EQ.'L' .OR. XINTERP.EQ.'l' ) FLYBY_INTERP=1
                IF ( XINTERP.EQ.'S' .OR. XINTERP.EQ.'s' ) FLYBY_INTERP=4
           ENDIF
      ENDIF
!
! --- Make sure flyby_interp has been set
!
      IF ( FLYBY_INTERP .NE. 1  .AND. &
     &     FLYBY_INTERP .NE. 3  .AND. &
     &     FLYBY_INTERP .NE. 4        ) THEN
!
          WRITE  ( ERRSTR, 150 ) UT1INV(2)
  150     FORMAT ( "SEROT has unsupported value. Mod file increment = ", &
     &              F6.1," days.")
          CALL FERR ( INT2(200), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Now make sure the data base UT1PM interpolation flags (interpolation_pm
! --- and interpolation_UT1) are set properly. These do not exist for CALC
! --- versions earlier than version 7.4 and for the old superfiles made
! --- using the earlier CALC versions. Set flags to 3 (cubic interpolatoion)
! --- for these cases. 91AUG23 -DG-
!
      IF ( CALCV .LT. 7.39D0 ) THEN
           INTERPOLATION_UT1 = 3
           INTERPOLATION_PM  = 3
      ENDIF
!
      RETURN
      END  !#!  SEROT  #!#
