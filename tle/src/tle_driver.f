      SUBROUTINE TLE_DRIVER ( FIL_TLE, MJD, TAI, X, XDOT, IUER )
!
!    TLE_TO_CRS  / TRS    TLE_TO_CRS
!
      IMPLICIT   NONE
      INCLUDE    'sgp4.i'
      Character  typerun, typeinput
      Character  FIL_TLE*128, OUT_FIL*128
      
      Character*3 MonStr,Monthtitle(12)
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
      INTEGER*4  SAT_CAT, LY, LNY, ET, NTLE, NREV
      INTEGER*4  EPH__MEPOC, NPTS
      PARAMETER  ( EPH__MEPOC = 1024*1024 )
      REAL*8     X(3), XDOT(3)
!
      COMMON /DebugHelp/ Help
      CHARACTER Help
      Help = 'N'
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
! --- We assume only one satellite per TLE
!
      IUER = -1 
      CALL READ_TLE_FILE ( FIL_TLE, SAT_NAM, SAT_CAT, SAT_CLASS,        &
     &                     LY, LNY, INT_DES, MJD_TLE, TAI_TLE, EPOCH,   &
     &                     MM_DOT, MM_DOTDOT, B_STAR, ET, NTLE,         &
     &                     ELEM, MM, NREV, IUER )
!
! --- sgp4fix identify constants and allow alternate values
!
      CALL GETGRAVCONST( WHICHCONST, TUMIN, MU, RADIUSEARTHKM, XKE,     &
     &       J2, J3, J4, J3OJ2 )
      VKMPERSEC     =  RADIUSEARTHKM * XKE/60.0D0
!
! ---------------- Setup files for operation ------------------
! --- 10 input 2-line element set file
      OPEN( 10, FILE = FIL_TLE, STATUS='OLD', ACCESS = 'SEQUENTIAL' )

      ! 14 Debug file
      OPEN(14,FILE = 'sgp4test.dbg' ,STATUS='UNKNOWN',                  &
     &          ACCESS = 'SEQUENTIAL' )

      ! ----- 15 temporary file of record for 2 line element sets ---
      OPEN(15,FILE = 'Sgp4Rec.bak', ACCESS = 'DIRECT',                  &
     &          FORM = 'UNFORMATTED', RECL = 1100, STATUS = 'UNKNOWN' )
!
! ----------------- Get initial values -------------------
! --- Convert the TLE set character to variables
!
      NUMSATS = 1
      CALL TWOLINE2RVSGP4 ( NUMSATS, TYPERUN, TYPEINPUT, WHICHCONST,    &
     &                      STARTMFE, STOPMFE, DELTAMIN, CODE )
!
! --- Compute [X, XDOT] at given epoch
!
      T = ((MJD - MJD_TLE)*86400.D0 + (TAI - TAI_TLE))/60.D0
      CALL SGP4 ( WHICHCONST, T, X, XDOT, ERROR )
! ---
      X    = X*1.D3             ! [m]
      XDOT = XDOT*1.D3          ! [m/s]
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE
