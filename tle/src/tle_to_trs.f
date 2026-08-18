      SUBROUTINE TLE_TO_TRS ( NERS, EPH, MJD, TAI, STA_X, X, XDOT, AZ, EL, IUER )
! ***************************************************************************************
! *                                                                                     *
! *   Routine TLE_TO_TRS computes the XYZ from a TLE file for a given epoch and         *
! *   converts it to a terestrial reference system (Earth Centred, Earth Fixed)         *
! *                                                                                     *
! *   INPUT:                                                                            *
! *           EPH    =  File Ephemiris data                       { DERIVED TYPE }      *
! *                                                                                     *
! *           MJD    =  Mean Julian Date                          { INT*4 }             *
! *                                                                                     *
! *           TAI    =  TAI                                       { REAL*8 }   [s]      *
! *                                                                                     *
! *           IUER   =  Error Handler                             { INT*4, OPT }        *
! *                         If IUER=0 no error message will be printed,                 *
! *                         even in the event of an error. However, for                 *
! *                         other possible values, i.e. IUER=-1,-2, & -3,               *
! *                         the error message will print to screen. For                 *
! *                         the later case, i.e. IUER=-3, after printing               *
! *                         the program will terminate.                                 *
! *                         Default, IUER = -1                                          *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *          X      =  TRS position                            { REAL*8 }  (3x1) [m]    *
! *                                                                                     *
! *          XDOT   =  TRS velocity                            { REAL*8 }  (3x1) [m/s]  *
! *                                                                                     *
! *          AZ     =  Azimuth                                 { REAL*8 }  [rad]        *
! *                                                                                     *
! *          EL     =  Elevation                               { REAL*8 }  [rad]        *
! *                                                                                     *
! *   Copyright (c) 1975-2025 United States Government as represented by                *
! *   the Administrator of the National Aeronautics and Space                           *
! *   Administration. All Rights Reserved.                                              *
! *   License: NASA Open Source Software Agreement (NOSA).                              *
! *                                                                                     *
! *  ###  15-JUL-2022    TLE_TO_TRS       v3.5 (d)    N. Habana     06-SEP-2025   ###   *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'sgp4.i'
      INCLUDE    'ners.i'
      INCLUDE    'ners_local.i'
      INCLUDE    'astro_constants.i'
      INCLUDE    'tle_sgp4.i'
      TYPE ( EPH__TYPE ) :: EPH
      TYPE ( NERS__TYPE ) :: NERS
      INTEGER*4  IUER, IER, M_PAR, L_PAR
      INTEGER*4  M_PAR_EOP, L_PAR_EOP
      REAL*8     X(3), XDOT(3), THETA_GMST
      REAL*8     X_TEME(3), XDOT_TEME(3)
      REAL*8     ROT1_T(3,3), ROT2_T(3,3), ROT3_T(3,3)
      REAL*8     ROT1(3,3), ROT2(3,3), ROT3(3,3)
      REAL*8     R2R1(3,3), R2R1R3(3,3)
      REAL*8     OM_ROT(3), V_CROSS(3), R3VTEME(3)
      REAL*8     X_PEF(3), XDOT_PEF(3)
      INTEGER*4  K0, K1, K2, K3, K4
      CHARACTER  DATE_STR*32, STR*32, REFR_MODE*5
      CHARACTER  C_PAR*16, C_PAR_EOP*16
      PARAMETER  ( M_PAR = NERS__MPAR )
      PARAMETER  ( M_PAR_EOP = NERS__MPAR )
      REAL*8     PARS(M_PAR), PARS_EOP(M_PAR_EOP)
      REAL*8     MIN__ELV, MAX__ELV
      PARAMETER  ( MIN__ELV = 30.D0*DEG__TO__RAD)
      PARAMETER  ( MAX__ELV = 85.D0*DEG__TO__RAD)
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      LOGICAL*1   LEX
      INTEGER*4  MJD, MJD_END, MJD_TLE
      REAL*8     TAI, TAI_END, TAI_TLE, TAI_CUR, DT(2)
      CHARACTER  SAT_NAM*24, SAT_CLASS, INT_DES*8
      REAL*8     MM_DOT, MM_DOTDOT, B_STAR, ELEM(6)
      REAL*8     MM, EPOCH, STA_X(3)
      INTEGER*4  SAT_CAT, LY, LNY, ET, NTLE, LUN, NREV
      REAL*8     RD, RA, DEC, AZ, EL, HA
      REAL*8     AZ_RATE, EL_RATE, HA_RATE
      REAL*8     CXP, SXP, CYP, SYP, CTG, STG
      REAL*8     JDUT1, JDUTC
      INTEGER*4  EPH__MEPOC, NPTS
      PARAMETER  ( EPH__MEPOC = 1024*1024 )
      REAL*8, EXTERNAL :: GSTIME, MJD_SEC_TO_JD
! * ----------------------------------------------------------------------
      Character  typerun, typeinput
      Character  MonStr*3,Monthtitle(12)*3
      Integer    Code, NumSats, TotalNumSats, k, error, whichconst
      Real*8     ro(3),vo(3), startmfe, stopmfe, deltamin
      REAL*8     p, ecc, incl, node, argp, nu, m,arglat,truelon,lonper
! * --------------------------  Local Variables  -------------------------
      REAL*8     J2, TwoPi, Rad, mu, RadiusEarthKm, VKmPerSec, xke, jdf,    &
     &           de2ra, xpdotp, T, sec, jd, pi, j3, j4, j3oj2, tumin,       &
     &           jdFrac
      INTEGER    i, j, Year, yr, mon, day, hr, min
      Integer    elnum, revnum
      REAL*8     tsince
      LOGICAL*1  FL_DEBUG
      INTEGER*4, EXTERNAL :: GET_UNIT
! * ----------------------------------------------------------------------
      COMMON / DEBUGHELP / HELP
      CHARACTER HELP*1
      HELP = 'N'
      FL_DEBUG  = .FALSE.
!
! --------------------------  Implementation   --------------------------
! --- Define the running modes and how you will implement the SGP4
!     package
!
      Opsmode    = 'a'
      typerun    = 'c'
      typeinput  = 'E'
      WHICHCONST = 72
!
! --- Define Trig Constants
!
      pi            =     4.0D0 * datan(1.0D0)  ! 3.14159265358979D0
      TwoPi         =     2.0D0 * pi            ! 6.28318530717959D0
      Rad           =   180.0D0 / pi            ! 57.29577951308230D0
      DE2RA         =    pi / 180.0D0           ! 0.01745329251994330D0
      xpdotp        =  1440.0 / (2.0 *pi)       ! 229.1831180523293D0
!
! --- Check whether NERS object has been initialized and loaded
!
      IF ( NERS%FCS_STATUS == NERS__LOAD .OR. &
     &     NERS%FCS_STATUS == NERS__INIT      ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 1931, IUER, 'TLE_TO_TRS', 'Trap of internal '// &
     &         'control: NERS object is not loaded' )
           RETURN
      END IF
!
! --- sgp4fix identify constants and allow alternate values
!
      CALL GETGRAVCONST( WHICHCONST, TUMIN, MU, RADIUSEARTHKM, XKE,     &
     &                   J2, J3, J4, J3OJ2 )
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
! ----------------- Get initial values from TLE -------------------
!
      NUMSATS = 1       ! Number of satellites in the TLE file
!
! --- Convert the TLE set character to variables
!
      CALL TWOLINE2RVSGP4 ( EPH, FL_DEBUG, NUMSATS, TYPERUN, TYPEINPUT, &
     &                      WHICHCONST, STARTMFE, STOPMFE, DELTAMIN,    &
     &                      CODE )
!
! --- Compute the time in TLE and seconds passed
!
      TAI_TLE = EPH%TLE(1)%UTC - EPH%TLE(1)%UTC_MTAI
      T = ( (MJD - EPH%TLE(1)%MJD)*86400.D0 + (TAI - TAI_TLE) )/60.D0
      TAI_CUR = ( MJD - J2000__MJD)*86400.D0 + TAI
!
! --- Convert the current time to JD
!
      JD = MJD_SEC_TO_JD (MJD, TAI)
!
! --- Compute the position of the satellite in TLE coordinate
!     system.
!
      CALL SGP4 ( WHICHCONST, T, X_TEME, XDOT_TEME, ERROR )
!
! --- Convert the coordinates to standard units
!
      X_TEME    = X_TEME*1.D3                                    ! [m]
      XDOT_TEME = XDOT_TEME*1.D3                                 ! [m/s]
!
! --- NERS parameter to compute, and the reference mode for computing
!     the azimuth and elevation
!
      C_PAR = 'matall'
      C_PAR_EOP = 'eop3'
      REFR_MODE = 'none'
!
! --- Check if you are observing within 3 days of the TLE file
! --- N.B: For this we are just using the MJD's to check. Later we 
!          can implement using the UTC's as well.
!
!@      IF ( ABS(EPH%TLE(1)%MJD - MJD) .GT. 3 ) THEN 
!@         WRITE( 6, * ) ' TLE_TO_TRS WARNING 101: '
!@         WRITE( 6, * ) ' More than 3 days between TLE date, and '
!@         WRITE( 6, * ) ' the planned computation date. Therefore '
!@         WRITE( 6, * ) ' the results may not be reliable. USE TLE '
!@         WRITE( 6, * ) ' closer to your date(s) of interest. '
!@      END IF
!
! --- Get the polar coordinates, and ut1-tai
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( NERS, TAI_CUR, C_PAR_EOP, M_PAR_EOP,         &
    &                     L_PAR_EOP, PARS_EOP, IER)
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1932, IUER, 'TLE_TO_TRS', 'Error in NERS_GET_EOP'// &
     &         ' for computation of the EOP matrix' )
           RETURN
      END IF
      
!
! --- Compute JDUT1 by adding UT1MTAI to TAI
!
      JDUT1 = MJD_SEC_TO_JD (MJD, TAI + PARS_EOP(3) )
!
! --- Compute the Greenwich Sidereal Time at JDUT1
!
      THETA_GMST = GSTIME ( JDUT1 )
!
! --- Compute the Trig functions of the Polar coordinates and the 
!     greenwhich sidereal time
!
      CXP = DCOS(PARS_EOP(1)*ARCSEC__TO__RAD)                           ! cos(x_p)
      SXP = DSIN(PARS_EOP(1)*ARCSEC__TO__RAD)                           ! sin(x_p)
      CYP = DCOS(PARS_EOP(2)*ARCSEC__TO__RAD)                           ! cos(y_p)
      SYP = DSIN(PARS_EOP(2)*ARCSEC__TO__RAD)                           ! sin(y_p)
      CTG = DCOS(THETA_GMST)
      STG = DSIN(THETA_GMST)
      K3  = 3
      k1  = 1
!
! --- Define the Rotation matrices then transpose them
! --- R1(y_p)
!
      ROT1(1,1) =  1.D0
      ROT1(1,2) =  0.D0
      ROT1(1,3) =  0.D0
      ROT1(2,1) =  0.D0
      ROT1(2,2) =  CYP
      ROT1(2,3) = -SYP
      ROT1(3,1) =  0.D0
      ROT1(3,2) =  SYP
      ROT1(3,3) =  CYP
! ---
      ROT1_T = TRANSPOSE( ROT1 )
!
! --- R2(x_p)
!
      ROT2(1,1) =  CXP
      ROT2(1,2) =  0.D0
      ROT2(1,3) =  SXP
      ROT2(2,1) =  0.D0
      ROT2(2,2) =  1.D0
      ROT2(2,3) =  0.D0
      ROT2(3,1) = -SXP
      ROT2(3,2) =  0.D0
      ROT2(3,3) =  CXP
! ---
      ROT2_T = TRANSPOSE( ROT2 )
!
! --- R3(THETA_GMST)
!
      ROT3(1,1) =  CTG
      ROT3(1,2) = -STG
      ROT3(1,3) =  0.D0
      ROT3(2,1) =  STG
      ROT3(2,2) =  CTG
      ROT3(2,3) =  0.D0
      ROT3(3,1) =  0.D0
      ROT3(3,2) =  0.D0
      ROT3(3,3) =  1.D0
! ---
      ROT3_T = TRANSPOSE( ROT3 )
!
! --- Define the Earth Rotation Vector
!
      OM_ROT(1) = 0.D0
      OM_ROT(2) = 0.D0
      OM_ROT(3) = OM__EAR  !  7.292115146706979D-05 ! rad/sec
!
! --- Multiply the transpose of the 2nd and 1st rotation matrices
!     to account for the polar motion transformations
!     R2R1 = ROT2_T*ROT1_T
!
      R2R1 = MATMUL( ROT2_T, ROT1_T )
!
! --- Account for the UT1 transformation using the GMST angle
! --- R2R1R3 = (ROT2_T*ROT1_T)*ROT3_T
!
      R2R1R3 = MATMUL( R2R1, ROT3_T )  
!
! --- Compute the Position vector in an Earth Centred, Earth Fixed
!     terrestrial system.
! --- X = ((ROT2_T*ROT1_T)*ROT3_T)*X_TEME
!    
      X = MATMUL( R2R1R3, X_TEME )
!
! --- Rotate the position vector from the TLE coordinates by the 
!     GMST angle.
!
      X_PEF =  MATMUL( ROT3_T, X_TEME )
!
! --- Rotate the velocity vector from the TLE coordinates by the 
!     GMST angle.
!
      R3VTEME = MATMUL( ROT3_T, XDOT_TEME )
!
! --- Compute the cross product of the Earth rotation vector and 
!     the X_PEF vector
!
      V_CROSS(1) = (OM_ROT(2)*X_PEF(3)) - (OM_ROT(3)*X_PEF(2))
      V_CROSS(2) = (OM_ROT(3)*X_PEF(1)) - (OM_ROT(1)*X_PEF(3))
      V_CROSS(3) = (OM_ROT(1)*X_PEF(2)) - (OM_ROT(2)*X_PEF(1))
!
! --- Difference between the GMST angle transformation of the velocity and
!     the cross product above.
!
      XDOT_PEF = R3VTEME - V_CROSS
!
! --- Transform into the Earth Centred coordinates by accounting for the
!     polar motion
!
      XDOT = MATMUL( R2R1, XDOT_PEF )
!
!
! --- Compute the right ascension and declination of the sat
!
      CALL ERR_PASS ( IUER, IER )
      CALL DECPOL ( 3, X, RD, RA, DEC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1933, IUER, 'TLE_TO_TRS', &
     &         'Error in transforming satellite position from '// &
     &         'the Cartesian to the polar coordinate system' )
           RETURN
      END IF
!
      IF ( .NOT. ( STA_X(1) == 0.0D0 .AND. STA_X(2) == 0.0D0 .AND. STA_X(3) == 0.0D0 ) ) THEN
!
! -------- Compute azimuth and elevation, from a given station
!
           CALL ERR_PASS ( IUER, IER )
           CALL NERS_AZELHA_COMP( NERS, TAI_CUR, STA_X, RA, DEC, REFR_MODE, AZ,  &
     &                            EL, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1934, IUER, 'TLE_TO_TRS', 'Error in '// &
     &              'computing azimuth and elevation' )
                RETURN
           END IF
         ELSE 
           AZ = 0.0D0
           EL = 0.0D0
           HA = 0.0D0
           AZ_RATE = 0.0D0
           EL_RATE = 0.0D0
           HA_RATE = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE  TLE_TO_TRS  !#!#
