      SUBROUTINE TLE_TO_CRS ( NERS, EPH, MJD, TAI, X, XDOT, IUER )

! **************************************************************************************
! *                                                                                    *
! *   Routine TLE_TO_CRS reads a given TLE file and propagates the position and        *
! *   velocity of the object to the stated time (MJD and TAI).                         *
! *   N.B: This routine assumes 1 TLE per file.                                        * 
! *        We will expand to more TLE's per file later                                 *
! *        The move to multiple files                                                  *
! *                                                                                    *
! *   INPUT:                                                                           *
! *           NERS
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
! *   Copyright (c) 1975-2025 United States Government as represented by               *
! *   the Administrator of the National Aeronautics and Space                          *
! *   Administration. All Rights Reserved.                                             *
! *   License: NASA Open Source Software Agreement (NOSA).                             *
! *                                                                                    *
! *  ###   16-NOV-2021    TLE_TO_CRS      v5.0 (d)    N. Habana     30-MAR-2023   ###  *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'sgp4.i'
      INCLUDE    'tle_sgp4.i'
      INCLUDE    'ners.i'
      INCLUDE    'ners_local.i'
      INCLUDE    'astro_constants.i'
      TYPE      ( EPH__TYPE ) :: EPH
      TYPE      ( NERS__TYPE ) :: NERS
      CHARACTER  TYPERUN, TYPEINPUT
      CHARACTER  MONSTR*3, MONTHTITLE(12)*3
      Integer    Code, NumSats, TotalNumSats, k, error, whichconst
      INTEGER*4  M_PAR, L_PAR
      REAL*8     XTRS(3), VTRS(3)
      PARAMETER  ( M_PAR = NERS__MPAR )
      REAL*8     PARS(M_PAR)
      CHARACTER  C_PAR*16, REFR_MODE*5, STR*32
      LOGICAL*1   LEX
      REAL*8     STA_X(3), AZ, EL
! * ----------------------------  Locals  -------------------------------
      REAL*8     J2, TwoPi, Rad, mu, RadiusEarthKm, VKmPerSec, xke, jdf,    &
     &           de2ra, xpdotp, T, sec, jd, pi, j3, j4, j3oj2, tumin,       &
     &           jdFrac
      INTEGER    i, j, Year, yr, mon, day, hr, min
      Real*8     deg2rad
      Integer    elnum, revnum
      REAL*8     tsince
      INTEGER*4  MJD, MJD_END, MJD_TLE, IUER, IER
      REAL*8     TAI, TAI_END, TAI_TLE, TAI_CUR, DT(2)
      CHARACTER  SAT_NAM*24, SAT_CLASS, INT_DES*8
      LOGICAL*1  FL_DEBUG
      REAL*8     X(3), XDOT(3)
      INTEGER*4, EXTERNAL :: GET_UNIT
! ---

!
      COMMON / DEBUGHELP / HELP
      CHARACTER HELP*1
      HELP = 'N'
      FL_DEBUG  = .FALSE.
!
! --- Check whether NERS object has been initialized and loaded
!
      IF ( NERS%FCS_STATUS == NERS__LOAD .OR. &
     &     NERS%FCS_STATUS == NERS__INIT      ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 1941, IUER, 'TLE_TO_CRS', 'Trap of '// &
     &         'internal control: NERS objectt is not loaded' )
           RETURN
      END IF
!
! --- NERS parameter to compute, and the reference mode for computing
!     the azimuth and elevation
!
      C_PAR = 'matall'
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
! --- Compute the TRS coordinates from the TLE ephemerides
! --- N.B: For this routine we are not interested in the Look angles
!          therefore, we define an arbitrary station coordinate
!
      STA_X(1)  =  1130730.331D0   ! GGAO12M
      STA_X(2)  = -4831246.540D0   ! GGAO12M
      STA_X(3)  =  3994228.904D0   ! GGAO12M
!
! ---
!
      CALL ERR_PASS ( IUER, IER )
      CALL TLE_TO_TRS ( NERS, EPH, MJD, TAI, STA_X, XTRS, VTRS, AZ, EL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1942, IUER, 'TLE_TO_CRS', 'Error in '// &
     &         'computiong satetllite positions in the TRS coordinate '// &
     &         'system' )
           RETURN 
      END IF
!
! --- Compute the current TAI
!
      TAI_CUR = ( MJD - J2000__MJD)*86400.D0 + TAI
!
! --- Compute the rotation matrix from terestrial to celestial coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_GET_EOP ( NERS, TAI_CUR, C_PAR, M_PAR, L_PAR, PARS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1943, IUER, 'TLE_TO_CRS', 'Error in '// &
     &         'NERS_GET_EOP' )
           RETURN 
      END IF
!
! --- Compute the satellite's celestial position coordinates
!
      X(1) = PARS(1)*XTRS(1) +  PARS(4)*XTRS(2) + PARS(7)*XTRS(3)
      X(2) = PARS(2)*XTRS(1) +  PARS(5)*XTRS(2) + PARS(8)*XTRS(3)
      X(3) = PARS(3)*XTRS(1) +  PARS(6)*XTRS(2) + PARS(9)*XTRS(3)
!
! --- Velocities
!
      XDOT(1) = PARS(10)*XTRS(1) + PARS(13)*XTRS(2) + PARS(16)*XTRS(3) + &
    &           PARS(1)*VTRS(1)  + PARS(4)*VTRS(2)  + PARS(7)*VTRS(3)

      XDOT(2) = PARS(11)*XTRS(1) + PARS(14)*XTRS(2) + PARS(17)*XTRS(3) + &
    &           PARS(2)*VTRS(1)  + PARS(5)*VTRS(2)  + PARS(8)*VTRS(3)

      XDOT(3) = PARS(12)*XTRS(1) + PARS(15)*XTRS(2) + PARS(18)*XTRS(3) + &
    &           PARS(3)*VTRS(1)  + PARS(6)*VTRS(2)  + PARS(9)*VTRS(3)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TLE_TO_CRS  !#!#
