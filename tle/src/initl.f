
! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE INITL
! *
! *  this subroutine initializes the spg4 propagator. all the initialization is
! *    consolidated here instead of having multiple loops inside other routines.
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    ecco        - eccentricity                           0.0 - 1.0
! *    epoch       - epoch time in days from jan 0, 1950. 0 hr
! *    inclo       - inclination of satellite
! *    no          - mean motion of satellite
! *    satn        - satellite number
! *
! *  outputs       :
! *    ainv        - 1.0 / a
! *    ao          - semi major axis
! *    con41       -
! *    con42       - 1.0 - 5.0 cos(i)
! *    cosio       - cosine of inclination
! *    cosio2      - cosio squared
! *    eccsq       - eccentricity squared
! *    method      - flag for deep space                    'd', 'n'
! *    omeosq      - 1.0 - ecco * ecco
! *    posq        - semi-parameter squared
! *    rp          - radius of perigee
! *    rteosq      - square root of (1.0 - ecco*ecco)
! *    sinio       - sine of inclination
! *    gsto        - gst at time of observation               rad
! *    no          - mean motion of satellite
! *
! *  locals        :
! *    ak          -
! *    d1          -
! *    del         -
! *    adel        -
! *    po          -
! *
! *  coupling      :
! *    getgravconst-
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE INITL( Satn , whichconst, Ecco  , EPOCH , Inclo ,      &
     &         no_kozai, operationmode,                                 &
     &         Method, AINV  , AO    , CON41 , CON42 , COSIO , COSIO2,  &
     &         Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO ,          &
     &         GSTo, no_unkozai )
        IMPLICIT NONE
        CHARACTER Method, operationmode
        INTEGER Satn, whichconst
        REAL*8 Ecco  , EPOCH , Inclo , no_kozai, no_unkozai,            &
     &         AINV  , AO    , CON41 , CON42 , COSIO , COSIO2,          &
     &         Eccsq , OMEOSQ, POSQ  , rp    , RTEOSQ, SINIO , GSTo

        COMMON /DebugHelp/ Help
        CHARACTER Help

! * -------------------------- Local Variables --------------------------
! c        sgp4fix use old way of finding gst
        Integer ids70
        REAL*8 ts70, ds70, tfrac, c1, thgr70, fk5r, c1p2p, thgr, thgro

        REAL*8  RadPerDay, Temp, TUT1
        REAL*8  ak, d1, del, adel, po
        REAL*8  X2o3, J2, XKE, tumin, mu, radiusearthkm, j3, j4, j3oj2
        INCLUDE 'astmath.i'

! * ------------------------ WGS-72 EARTH CONSTANTS ---------------------
        X2o3   = 2.0D0/3.0D0
        ! sgp4fix identify constants and allow alternate values
        CALL getgravconst( whichconst, tumin, mu, radiusearthkm, xke,   &
     &       j2, j3, j4, j3oj2 )

! * ----------------- CALCULATE AUXILLARY EPOCH QUANTITIES --------------
        Eccsq  = Ecco*Ecco
        OMEOSQ = 1.0D0 - Eccsq
        RTEOSQ = DSQRT(OMEOSQ)
        COSIO  = DCOS(Inclo)
        COSIO2 = COSIO*COSIO

! * ---------------------- UN-KOZAI THE MEAN MOTION ---------------------
        AK   =  (XKE/no_kozai)**X2O3
        D1   =  0.75D0*J2* (3.0D0*COSIO2-1.0D0) / (RTEOSQ*OMEOSQ)
        DEL  =  D1/(AK*AK)
        ADEL =  AK * ( 1.0D0 - DEL*DEL - DEL*                           &
     &                 (1.0D0/3.0D0 + 134.0D0*DEL*DEL / 81.0D0) )
        DEL  =  D1/(ADEL*ADEL)
        no_unkozai   =  no_kozai/(1.0D0 + DEL)

        AO   =  (XKE/no_unkozai)**X2O3
        SINIO=  DSIN(Inclo)
        PO   =  AO*OMEOSQ
        CON42=  1.0D0-5.0D0*COSIO2
        CON41=  -CON42-COSIO2-COSIO2
        AINV =  1.0D0/AO
        POSQ =  PO*PO
        rp   =  AO*(1.0D0-Ecco)
        METHOD = 'n'

! * ----------------- CALCULATE GREENWICH LOCATION AT EPOCH -------------
! c       sgp4fix modern approach to finding sidereal time
        IF (operationmode .ne. 'a') THEN
            RadPerDay  = twopi * 1.002737909350795D0  !6.30038809866574D0
            Temp = Epoch + 2433281.5D0
            TUT1= ( DINT(Temp-0.5D0) + 0.5D0 - 2451545.0D0 ) / 36525.0D0
            Gsto= 1.75336855923327D0 + 628.331970688841D0*TUT1          &
     &             + 6.77071394490334D-06*TUT1*TUT1                     &
     &             - 4.50876723431868D-10*TUT1*TUT1*TUT1                &
     &             + RadPerDay*( Temp-0.5D0-DINT(Temp-0.5D0) )
          ELSE
            ! sgp4fix use old way of finding gst
            ! count integer number of days from 0 jan 1970
           TS70  = EPOCH-7305.0D0
           IDS70 = TS70 + 1.0D-8
           TFRAC = TS70-IDS70
            ! find greenwich location at epoch
           C1    = 1.72027916940703639D-2
           THGR70= 1.7321343856509374D0
            FK5R  = 5.07551419432269442D-15
           C1P2P = C1+TWOPI
           gsto  = THGR70+C1*IDS70+C1P2P*TFRAC+TS70*TS70*FK5R
         ENDIF
         
        ! ------------------------ Check quadrants ---------------------
        Gsto = DMOD( Gsto,TwoPi )
        IF ( Gsto .lt. 0.0D0 ) THEN
            Gsto= Gsto + TwoPi
          ENDIF

! c      write(*,*) Satn,'  gst delta ', gsto-gsto1

!@@NH@@!        INCLUDE 'debug5.f' 

      RETURN
      END  ! end initl
