      SUBROUTINE TLE_TO_TRS ( EPH, MJD, TAI, STA_X, X, XDOT, AZ, EL, IUER )
!
! ***************************************************************************************
! *                                                                                     *
! *   Routine TLE_TO_TRS computes the XYZ from a TLE file for a given epoch and         *
! *   converts it to a terestrial reference system                                      *
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
! *                         the latter case, i.e. IUER=-3, after printing               *
! *                         the program will terminate.                                 *
! *                         Default, IUER = -1                                          *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *          X      =  TRS position                            { REAL*8 }  (3x1) [m]    *
! *                                                                                     *
! *          XDOT   =  velocity                                { REAL*8 }  (3x1) [m/s]  *
! *                                                                                     *
! *          AZ     =  Azimuth                                 { REAL*8 }  [rad]        *
! *                                                                                     *
! *          EL     =  Elevation                               { REAL*8 }  [rad]        *
! *                                                                                     *
! *  ###  15-JUL-2022    TLE_TO_TRS       v3.3 (c)    N. Habana     04-MAY-2023   ###   *
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
      INTEGER*4  MJD, MJD_TLE
      REAL*8     TAI, TAI_TLE, TAI_CUR
      REAL*8     X_CRS(3), XDOT_CRS(3), X(3), XDOT(3)
      INTEGER*4  J0, J1, J2, J3, J4
      CHARACTER  DATE_STR*32, STR*32, REFR_MODE*5, C_PAR*16
      PARAMETER  ( M_PAR = NERS__MPAR )
      REAL*8     PARS(M_PAR)
      REAL*8     MIN__ELV, MAX__ELV
      PARAMETER  ( MIN__ELV = 30.D0*DEG__TO__RAD)
      PARAMETER  ( MAX__ELV = 85.D0*DEG__TO__RAD)
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      LOGICAL*1   LEX
      CHARACTER  SAT_NAM*24, SAT_CLASS, INT_DES*8
      REAL*8     MM_DOT, MM_DOTDOT, B_STAR, ELEM(6)
      REAL*8     MM, EPOCH, STA_X(3)
      INTEGER*4  SAT_CAT, LY, LNY, ET, NTLE, NREV
      REAL*8     RD, RA, DEC, AZ, EL, HA
      REAL*8     AZ_RATE, EL_RATE, HA_RATE

!
! --- Get NERS Environment variables
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
!
! --- Initialise NERS
!
      IUER = -1
      CALL NERS_INIT ( 'NERS_CONFIG', NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
         CALL ERR_LOG ( 1930, IUER, 'TLE_TO_TRS',                       &
     &           'Error in initializing NERS data structure' )
         RETURN
      END IF
!
! --- Load NERS
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
         CALL ERR_LOG ( 1931, IUER, 'TLE_TO_TRS',                       &
     &           'Error in an attempt to retrieve NERS forecast '//     &
     &           'parameters from the remote server' )
         RETURN
      END IF
!
! --- NERS parameter to compute, and the reference mode for computing
!     the azimuth and elevation
!
      C_PAR = 'matall'            
      REFR_MODE = 'none'
!
! --- Check if you are observing within 3 days of the TLE file
! --- N.B: For this we are just using the MJD's to check. Later we 
!          can implement using the UTC's as well.
!
      IF ( ABS(EPH%TLE(1)%MJD - MJD) .GT. 3 ) THEN 
         WRITE( 6, * ) ' TLE_TO_TRS WARNING 101: '
         WRITE( 6, * ) ' More than 3 days between TLE date, and '
         WRITE( 6, * ) ' the planned computation date. Therefore '
         WRITE( 6, * ) ' the results may not be reliable. USE TLE '
         WRITE( 6, * ) ' closer to your date(s) of interest. '
!@@NH@@!         IUER = -1
!@@NH@@!         CALL ERR_LOG ( 1930, IUER, 'TEME_TO_ITRF',                     &
!@@NH@@!     &                  'Trying to estimate over 4 days from TLE date' )
      END IF
!
! --- Compute the coordinates from TLE File
!
      CALL TLE_TO_CRS ( EPH, MJD, TAI, X_CRS, XDOT_CRS, IUER )
!
! --- Compute the current TAI
!
      TAI_CUR = ( MJD - J2000__MJD)*86400.D0 + TAI
!
! --- Compute the rotation matrix from terestrial to celestial coordinates
!
      CALL NERS_GET_EOP (NERS, TAI_CUR, C_PAR, M_PAR, L_PAR, PARS, IUER)
!
! --- Compute the satellite's terestrial coordinates, i.e., use the 
!     transpose of the rotation matrix (which is equivalent to its 
!     inverse)
!
      X(1) = PARS(1)*X_CRS(1) +  PARS(2)*X_CRS(2) + PARS(3)*X_CRS(3)
      X(2) = PARS(4)*X_CRS(1) +  PARS(5)*X_CRS(2) + PARS(6)*X_CRS(3)
      X(3) = PARS(7)*X_CRS(1) +  PARS(8)*X_CRS(2) + PARS(9)*X_CRS(3)
!
! --- Velocities
!
      XDOT(1) = PARS(8)*X_CRS(1)    +  PARS(11)*X_CRS(2)   +            &
     &          PARS(12)*X_CRS(3)   +  PARS(1)*XDOT_CRS(1) +            &
     &          PARS(2)*XDOT_CRS(2) +  PARS(3)*XDOT_CRS(3)

      XDOT(2) = PARS(13)*X_CRS(1)   +  PARS(14)*X_CRS(2)   +            &
     &          PARS(15)*X_CRS(3)   +  PARS(4)*XDOT_CRS(1) +            &
     &          PARS(5)*XDOT_CRS(2) +  PARS(6)*XDOT_CRS(3)


      XDOT(3) = PARS(16)*X_CRS(1)   +  PARS(17)*X_CRS(2)   +            &
     &          PARS(18)*X_CRS(3)   +  PARS(7)*XDOT_CRS(1) +            &
     &          PARS(8)*XDOT_CRS(2) +  PARS(9)*XDOT_CRS(3)
!
! --- Compute the right ascension and declination of the sat
!
      IUER = -1
      CALL DECPOL( 3, X, RD, RA, DEC, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -1
         CALL ERR_LOG ( 1932, IUER, 'TLE_TO_TRS',                       &
     &           'Error in computing the celestial coordinates' )
         RETURN
      END IF
!
! --- Compute azimuth and elevation, from a given station
!
      IUER = -1
      CALL NERS_AZELHA_COMP( NERS, TAI_CUR, STA_X, RA, DEC, REFR_MODE, AZ,  &
     &                       EL, HA, AZ_RATE, EL_RATE, HA_RATE, IUER )
      IF ( IUER .NE. 0 ) THEN
         CALL ERR_LOG ( 1933, IUER, 'TLE_TO_TRS',                       &
     &                 'Error in computing azimuth and elevation' )
         RETURN
      END IF
!
      RETURN
      END   SUBROUTINE
