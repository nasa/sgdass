      SUBROUTINE GEOD_TO_CFS ( LAT, LON, HEI, R_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine GEOD_TO_CFS transforms vector of coordinates geodetic      *
! *   latitude, longitude, and height above the reference ellipsoid to   *
! *   the Cartesian coordinate system.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    LAT ( REAL*8     ) -- Geodetic latitude. Units: rad.              *
! *    LON ( REAL*8     ) -- Longitude. Units: rad.                      *
! *    HEI ( REAL*8     ) -- Height above the reference ellipsoid.       *
! *                          Units: meters.                              *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *  R_VEC ( REAL*8     ) -- Vector of Cartesian coordinates. Units:     *
! *                          meters.                                     *
! *                                                                      *
! *  ### 23-MAR-2020  GEOD_TO_CFS  v1.0 (c)  L. Petrov  23-MAR-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     LAT, LON, HEI, R_VEC(3)
      REAL*8     REA, FE, EXC_SQ
      PARAMETER  ( FE      = 1.D0/298.257D0 )    ! Earth's flattening
      PARAMETER  ( REA     = 6378136.3D0 )       ! Earth's equatorial radius
      PARAMETER  ( EXC_SQ  = 2.D0*FE - FE**2 )   ! Earth's eccentricity
!
      R_VEC(1) = (REA/DSQRT(1.0D0 - EXC_SQ*DSIN(LAT)**2) + HEI)*DCOS(LAT)*DCOS(LON)
      R_VEC(2) = (REA/DSQRT(1.0D0 - EXC_SQ*DSIN(LAT)**2) + HEI)*DCOS(LAT)*DSIN(LON)
      R_VEC(3) = (REA*(1.0D0 - EXC_SQ)/DSQRT(1.0D0 - EXC_SQ*DSIN(LAT)**2) &
     &            + HEI)*DSIN(LAT)
      RETURN
      END  SUBROUTINE  GEOD_TO_CFS  !#!#
