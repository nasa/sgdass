      SUBROUTINE TLE_TO_CRS ( EPH, MJD, TAI, X, XDOT, IUER )

! **************************************************************************************
! *                                                                                    *
! *   Routine TLE_TO_CRS reads a given TLE file and propagates the position and        *
! *   velocity of the object to the stated time (MJD and TAI).                         *
! *   N.B: This routine assumes 1 TLE per file.                                        * 
! *        We will expand to more TLE's per file later                                 *
! *        The move to multiple files                                                  *
! *                                                                                    *
! *   INPUT:                                                                           *
! *           EPH        =  File Ephemiris data               { DERIVED TYPE }         *
! *                                                                                    *
! *           MJD       =  Propagation Mean Julian Date       { INT*4 }                *
! *                                                                                    *
! *           TAI       =  Time                               { REAL*8 }               *
! *                                                                                    *
! *           IUER       =  Error Handler                     { INT*4, OPT }           *
! *                         If IUER=0 no error message will be printed,                *
! *                         even in the event of an error. However, for                *
! *                         other possible values, i.e. IUER=-1,-2, & -3,              *
! *                         the error message will print to screen. For                *
! *                         the latter case, i.e. IUER=-3, after printing              *
! *                         the program will terminate.                                *
! *                         Default, IUER = -1                                         *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *                                                                                    *
! *           X         =  Position Vector                    { REAL*8 } [m] (3x1)     *
! *                                                                                    *
! *           XDOT      =  Velocity Vector                    { REAL*8 } [m/s] (3x1)   *
! *                                                                                    *
! *                                                                                    *
! *                                                                                    *
! *  ###   16-NOV-2021    TLE_TO_CRS      v5.0 (c)    N. Habana     30-MAR-2023   ###  *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'sgp4.i'
      INCLUDE    'tle_sgp4.i'
      TYPE ( EPH__TYPE ) :: EPH
      Character  typerun, typeinput
      Character  MonStr*3,Monthtitle(12)*3
      Integer    Code, NumSats, TotalNumSats, k, error, whichconst
      Real*8     ro(3),vo(3), startmfe, stopmfe, deltamin
      REAL*8     p, ecc, incl, node, argp, nu, m,arglat,truelon,lonper

! * ----------------------------  Locals  -------------------------------
      REAL*8     J2, TwoPi, Rad, mu, RadiusEarthKm, VKmPerSec, xke, jdf,    &
     &           de2ra, xpdotp, T, sec, jd, pi, j3, j4, j3oj2, tumin,       &
     &           jdFrac
      INTEGER    i, j, Year, yr, mon, day, hr, min
      Real*8     deg2rad
      Integer    elnum, revnum
      REAL*8     tsince
      INTEGER*4  MJD, MJD_END, MJD_TLE, IUER, IER
      REAL*8     TAI, TAI_END, TAI_TLE, DT(2)
      CHARACTER  SAT_NAM*24, SAT_CLASS, INT_DES*8
      REAL*8     MM_DOT, MM_DOTDOT, B_STAR, ELEM(6)
      REAL*8     MM, EPOCH
      INTEGER*4  SAT_CAT, LY, LNY, ET, NTLE, LUN, NREV
      INTEGER*4  EPH__MEPOC, NPTS
      PARAMETER  ( EPH__MEPOC = 1024*1024 )
      LOGICAL*1  FL_DEBUG
      REAL*8     X(3), XDOT(3)
      INTEGER*4, EXTERNAL :: GET_UNIT
!
      COMMON / DEBUGHELP / HELP
      CHARACTER HELP*1
      HELP = 'N'
      FL_DEBUG  = .FALSE.
!
! * ------------------------  Implementation   --------------------------
!
      Opsmode    = 'a'
      typerun    = 'c'
      typeinput  = 'E'
      WHICHCONST = 72
!
      pi            =    4.0D0 * datan(1.0D0)  ! 3.14159265358979D0
      TwoPi         =    2.0D0 * pi            ! 6.28318530717959D0
      Rad           =   180.0D0 / pi           ! 57.29577951308230D0
      DE2RA         =    pi / 180.0D0          ! 0.01745329251994330D0
      xpdotp        =  1440.0 / (2.0 *pi)      ! 229.1831180523293D0
!
! --- sgp4fix identify constants and allow alternate values
!
      CALL GETGRAVCONST( WHICHCONST, TUMIN, MU, RADIUSEARTHKM, XKE,     &
     &       J2, J3, J4, J3OJ2 )
      VKMPERSEC     =  RADIUSEARTHKM * XKE/60.0D0
!
! ---------------- Setup files for operation ------------------
!
! --- If we are debugging
!
      IF ( FL_DEBUG ) THEN
!
! ------- 14 Debug file
!
          OPEN ( 14, FILE = '/tmp/sgp4test.dbg' ,STATUS='UNKNOWN', &
     &                   ACCESS = 'SEQUENTIAL' )
!
! ------- 15 temporary file of record for 2 line element sets ---
!
          OPEN ( 15, FILE = '/tmp/Sgp4Rec.bak', ACCESS = 'DIRECT', &
     &          FORM = 'UNFORMATTED', RECL = 1100, STATUS = 'UNKNOWN' )
      END IF
!
! ----------------- Get initial values -------------------
! --- Convert the TLE set character to variables
!
      NUMSATS = 1
      CALL TWOLINE2RVSGP4 ( EPH, FL_DEBUG, NUMSATS, TYPERUN, TYPEINPUT, &
     &                      WHICHCONST, STARTMFE, STOPMFE, DELTAMIN,    &
     &                      CODE )
!
! --- Compute [X, XDOT] at given epoch
!
!@@      T = ((MJD - MJD_TLE)*86400.D0 + (TAI - TAI_TLE))/60.D0
      TAI_TLE = EPH%TLE(1)%UTC - EPH%TLE(1)%UTC_MTAI
      T = ( (MJD - EPH%TLE(1)%MJD)*86400.D0 + (TAI - TAI_TLE) )/60.D0
      CALL SGP4 ( WHICHCONST, T, X, XDOT, ERROR )
! ---
      X    = X*1.D3             ! [m]
      XDOT = XDOT*1.D3          ! [m/s]
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TLE_TO_CRS  !#!#
