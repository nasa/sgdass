! * -----------------------------------------------------------------------------
! *
! *                           function getgravconst
! *
! *  this function gets constants for the propagator. note that mu is identified to
! *    facilitiate comparisons with newer models.
! *
! *  author        : david vallado                  719-573-2600   21 jul 2006
! *
! *  inputs        :
! *    whichconst  - which set of constants to use  721, 72, 84
! *
! *  outputs       :
! *    tumin       - minutes in one time unit
! *    mu          - earth gravitational parameter
! *    radiusearthkm - radius of the earth in km
! *    xke         - reciprocal of tumin
! *    j2, j3, j4  - un-normalized zonal harmonic values
! *    j3oj2       - j3 divided by j2
! *
! *  locals        :
! *
! *  coupling      :
! *
! *  references    :
! *    norad spacetrack report #3
! *    vallado, crawford, hujsak, kelso  2006
! *  ---------------------------------------------------------------------------- 

       SUBROUTINE GETGRAVCONST ( WHICHCONST, TUMIN, MU, RADIUSEARTHKM,  &
     &                           XKE, J2, J3, J4, J3OJ2 )
       IMPLICIT NONE     
       REAL*8 RADIUSEARTHKM, XKE, J2, J3, J4, J3OJ2, MU, TUMIN
       INTEGER WHICHCONST

       IF (WHICHCONST.EQ.721) THEN
           ! -- WGS-72 LOW PRECISION STR#3 CONSTANTS --
           RADIUSEARTHKM = 6378.135D0     ! km
           XKE    = 0.0743669161D0
           MU     = 398600.79964D0            ! in km3 / s2
           TUMIN  = 1.0D0 / XKE
           J2     =   0.001082616D0
           J3     =  -0.00000253881D0
           J4     =  -0.00000165597D0
           J3OJ2  =  J3 / J2
         ENDIF
       if (whichconst.eq.72) THEN
           ! ------------ wgs-72 constants ------------
           MU     = 398600.8D0            ! in km3 / s2
           RADIUSEARTHKM = 6378.135D0     ! km
           XKE    = 60.0D0 / DSQRT(RADIUSEARTHKM**3/MU)
           TUMIN  = 1.0D0 / XKE
           J2     =   0.001082616D0
           J3     =  -0.00000253881D0
           J4     =  -0.00000165597D0
           J3OJ2  =  J3 / J2
         ENDIF  
       IF (WHICHCONST.EQ.84) THEN
           ! ------------ WGS-84 CONSTANTS ------------
           MU     = 398600.5D0            ! in km3 / s2
           RADIUSEARTHKM = 6378.137D0     ! km
           XKE    = 60.0D0 / DSQRT(RADIUSEARTHKM**3/MU)
           TUMIN  = 1.0D0 / XKE
           J2     =   0.00108262998905D0
           J3     =  -0.00000253215306D0
           J4     =  -0.00000161098761D0
           J3OJ2  =  J3 / J2
         ENDIF

       RETURN
       END  !  SUBROUTINE getgravconst
