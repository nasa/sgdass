      SUBROUTINE TLE_TO_COO ( FIL_TLE, MJD, TAI, X, XDOT, IUER)
!
! ***************************************************************************************
! *                                                                                     *
! *   Routine TLE_TO_COO reads a given TLE file and gives the file epoch and            *
! *   coordinates of the satellite.                                                     *
! *                                                                                     *
! *   INPUT:                                                                            *
! *           FIL_TLE    =  Two Line Element File                 { CHAR }              *
! *                                                                                     *
! *           IUER       =  Error Handler                         { INT, OPT }          *
! *                         If IUER=0 no error message will be printed,                 *
! *                         even in the event of an error. However, for                 *
! *                         other possible values, i.e. IUER=-1,-2, & -3,               *
! *                         the error message will print to screen. For                 *
! *                         the latter case, i.e. IUER=-3, after printing               *
! *                         the program will terminate.                                 *
! *                         Default, IUER = -1                                          *
! *                                                                                     *
! *   OUTPUT:                                                                           *
! *           MJD        =  Mean Julian Date                      { INT }               *
! *                                                                                     *
! *           TAI        =  Tme                                   { REAL }              *
! *                                                                                     *
! *           X          =  Position Vector                       { REAL } [m] (1x3)    *
! *                                                                                     *
! *           XDOT       =  Velocity Vector                       { REAL } [m/s] (1x3)  *
! *                                                                                     *
! *  ###  16-NOV-2021    TLE_TO_COO        v2.0 (c)    N. Habana     17-DEC-2021   ###  *
! *                                                                                     *
! ***************************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'astro_constants.i'
      CHARACTER   FIL_TLE*(*)
      CHARACTER   SAT_NAM*24, SAT_CLASS, INT_DES*8
      REAL*8      TAI, MM_DOT, MM_DOTDOT, BSTAR
      REAL*8      INC, RAN, ECC, AOP, MA, MM
      INTEGER*4   MJD, SAT_CAT, LY, LNY, ET, NTLE, NREV, IUER
      REAL*8      RAD__EAR, GM__EAR, SEMI_MAJ
      PARAMETER   ( RAD__EAR = 6378136.3D0 )     ! Earth equtorial radius {GRS80}
      PARAMETER   ( GM__EAR  = 3.986004415D14)   ! Earth Geocentric Constant [m^3/s^2]
      REAL*8      ELEM(6), X(3), XDOT(3)
!
! --- Read the TLE file
!
      IUER = -1
      CALL READ_TLE ( FIL_TLE, SAT_NAM, SAT_CAT, SAT_CLASS, LY, LNY,    &
     &                INT_DES, MJD, TAI, MM_DOT, MM_DOTDOT, BSTAR,      &
     &                ET, NTLE, INC, RAN, ECC, AOP, MA, MM, NREV,       &
     &                IUER )
!
! --- Compute the semi-major axis
!
      SEMI_MAJ = (GM__EAR/(MM**2))**(1.D0/3.D0)
!
! --- Copy the Kepplerian elements
!
      ELEM(1) = SEMI_MAJ  ! Semi-major axis  [m]
      ELEM(2) = ECC       ! Eccentricity     
      ELEM(3) = INC       ! Inclination      [rad]
      ELEM(4) = AOP       ! Arg. of perigee  [rad]
      ELEM(5) = RAN       ! Right asc. node  [rad]
      ELEM(6) = MA        ! Mean anomaly
!
      CALL KEPLER_B ( ELEM, MM, X, XDOT )

      RETURN
      END SUBROUTINE !#!#!#! 3
