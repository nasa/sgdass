
! * -----------------------------------------------------------------------------
! *
! *                           SUBROUTINE DSPACE
! *
! *  This Subroutine provides deep space contributions to mean elements for
! *    perturbing third body.  these effects have been averaged over one
! *    revolution of the sun and moon.  for earth resonance effects, the
! *    effects have been averaged over no revolutions of the satellite.
! *    (mean motion)
! *
! *  author        : david vallado                  719-573-2600   28 jun 2005
! *
! *  inputs        :
! *    d2201, d2211, d3210, d3222, d4410, d4422, d5220, d5232, d5421, d5433       -
! *    dedt        -
! *    del1, del2, del3  -
! *    didt        -
! *    dmdt        -
! *    dnodt       -
! *    domdt       -
! *    irez        - flag for resonance           0-none, 1-one day, 2-half day
! *    argpo       - argument of perigee
! *    argpdot     - argument of perigee dot (rate)
! *    t           - time
! *    tc          -
! *    gsto        - gst
! *    xfact       -
! *    xlamo       -
! *    no          - mean motion
! *    atime       -
! *    em          - eccentricity
! *    ft          -
! *    argpm       - argument of perigee
! *    inclm       - inclination
! *    xli         -
! *    mm          - mean anomaly
! *    xni         - mean motion
! *    nodem       - right ascension of ascending node
! *
! *  outputs       :
! *    atime       -
! *    em          - eccentricity
! *    argpm       - argument of perigee
! *    inclm       - inclination
! *    xli         -
! *    mm          - mean anomaly
! *    xni         -
! *    nodem       - right ascension of ascending node
! *    dndt        -
! *    nm          - mean motion
! *
! *  locals        :
! *    delt        -
! *    ft          -
! *    theta       -
! *    x2li        -
! *    x2omi       -
! *    xl          -
! *    xldot       -
! *    xnddt       -
! *    xndt        -
! *    xomi        -
! *
! *  coupling      :
! *    none        -
! *
! *  references    :
! *    hoots, roehrich, norad spacetrack report #3 1980
! *    hoots, norad spacetrack report #6 1986
! *    hoots, schumacher and glover 2004
! *    vallado, crawford, hujsak, kelso  2006
! * ------------------------------------------------------------------------------

      SUBROUTINE DSPACE( IRez  , D2201 , D2211 , D3210 , D3222 , D4410 , &
     &                   D4422 , D5220 , D5232 , D5421 , D5433 , Dedt  , &
     &                   Del1  , Del2  , Del3  , Didt  , Dmdt  , Dnodt , &
     &                   Domdt , Argpo , ArgpDot, T    , TC    , GSTo  , &
     &                   Xfact , Xlamo , no_kozai,                       &
     &                   Atime , Eccm  , Argpm , Inclm , Xli   , Mm    , &
     &                   XNi   , nodem, Dndt  , XN  )
        IMPLICIT NONE
        INTEGER  IRez
        Real*8   D2201 , D2211 , D3210 , D3222 , D4410 , D4422 , D5220 , &
     &           D5232 , D5421 , D5433 , Dedt  , Del1  , Del2  , Del3  , &
     &           Didt  , Dmdt  , Dnodt , Domdt , Argpo , ArgpDot,T     , &
     &           TC    , GSTo  , Xfact , Xlamo ,no_kozai, Atime , Eccm , &
     &           Argpm , Inclm , Xli   , Mm    , Xni    , nodem , Dndt , &
     &           XN

! * -------------------------- Local Variables --------------------------
        INTEGER  iretn , iret
        REAL*8   Delt  , Ft    , theta , x2li  , x2omi , xl    , xldot , &
     &           xnddt , xndt  , xomi
        REAL*8   G22   , G32   , G44   , G52   , G54   , Fasx2 ,         &
     &           Fasx4 , Fasx6 , RPtim , Step2 , Stepn , Stepp

        COMMON /DebugHelp/ Help
        CHARACTER Help
        INCLUDE 'astmath.i'

! * ----------------------------- Constants -----------------------------
        FASX2 = 0.13130908D0
        FASX4 = 2.8843198D0
        FASX6 = 0.37448087D0
        G22   = 5.7686396D0
        G32   = 0.95240898D0
        G44   = 1.8014998D0
        G52   = 1.0508330D0
        G54   = 4.4108898D0
        RPTIM = 4.37526908801129966D-3
        STEPP =    720.0D0
        STEPN =   -720.0D0
        STEP2 = 259200.0D0

! * --------------- CALCULATE DEEP SPACE RESONANCE EFFECTS --------------
        DNDT  = 0.0D0
        THETA = DMOD(GSTo + TC*RPTIM,TwoPi)
        Eccm  = Eccm + DEDT*T

        Inclm = Inclm + DIDT*T
        Argpm = Argpm + DOMDT*T
        nodem = nodem + DNODT*T
        Mm    = Mm + DMDT*T

! c   sgp4fix for negative inclinations
! c   the following if statement should be commented out
! c        IF(Inclm .lt. 0.0D0) THEN
! c            Inclm  = -Inclm
! c            Argpm  = Argpm-PI
! c            nodem = nodem+PI
! c          ENDIF

! c   sgp4fix for propagator problems
! c   the following integration works for negative time steps and periods
! c   the specific changes are unknown because the original code was so convoluted
! c      sgp4fix take out atime = 0.0 and fix for faster operation
        Ft    = 0.0D0      ! Just in case - should be set in loops if used.

        IF (IREZ .ne. 0) THEN
! * ----- UPDATE RESONANCES : NUMERICAL (EULER-MACLAURIN) INTEGRATION ---
! * ---------------------------- EPOCH RESTART --------------------------
         ! sgp4fix streamline check
         IF ((atime .eq. 0.0D0) .or. (t * atime .le. 0.0D0) .or.        &
     &       (dabs(t) .lt. dabs(atime)) ) THEN
               atime  = 0.0D0
               xni    = no_kozai
               xli    = xlamo
            ENDIF
           ! sgp4fix move check outside loop
           IF (t .gt. 0.0D0) THEN
               delt = stepp
             else
               delt = stepn
             ENDIF

            iretn = 381 ! added for do loop
            iret  =   0 ! added for loop
            DO WHILE (IRetn.eq.381)

! * --------------------------- DOT TERMS CALCULATED --------------------
! * ------------------- NEAR - SYNCHRONOUS RESONANCE TERMS --------------
            IF (IREZ .ne. 2) THEN
                XNDT  = DEL1*DSIN(XLI-FASX2) +                          &
     &                  DEL2*DSIN(2.0D0*(XLI-FASX4)) +                  &
     &                  DEL3*DSIN(3.0D0*(XLI-FASX6))
                XLDOT = XNI + XFACT
                XNDDT = DEL1*DCOS(XLI-FASX2) +                          &
     &            2.0D0*DEL2*DCOS(2.0D0*(XLI-FASX4)) +                  &
     &            3.0D0*DEL3*DCOS(3.0D0*(XLI-FASX6))
                XNDDT = XNDDT*XLDOT
              ELSE

! * --------------------- NEAR - HALF-DAY RESONANCE TERMS ---------------
                XOMI = Argpo + ArgpDot*ATIME
                X2OMI= XOMI + XOMI
                X2LI = XLI + XLI
                XNDT = D2201*DSIN(X2OMI+XLI-G22) + D2211*DSIN(XLI-G22) +&
     &                 D3210*DSIN( XOMI+XLI-G32) +                      &
     &                 D3222*DSIN(-XOMI+XLI-G32) +                      &
     &                 D4410*DSIN(X2OMI+X2LI-G44)+ D4422*DSIN(X2LI-G44)+&
     &                 D5220*DSIN( XOMI+XLI-G52) +                      &
     &                 D5232*DSIN(-XOMI+XLI-G52) +                      &
     &                 D5421*DSIN( XOMI+X2LI-G54)+                      &
     &                 D5433*DSIN(-XOMI+X2LI-G54)
                XLDOT = XNI+XFACT
                XNDDT = D2201*DCOS(X2OMI+XLI-G22) + D2211*DCOS(XLI-G22)+ &
     &                  D3210*DCOS( XOMI+XLI-G32) +                      &
     &                  D3222*DCOS(-XOMI+XLI-G32) +                      &
     &                  D5220*DCOS( XOMI+XLI-G52) +                      &
     &                  D5232*DCOS(-XOMI+XLI-G52) +                      &
     &                  2.0D0*(D4410*DCOS(X2OMI+X2LI-G44) +              &
     &                  D4422*DCOS(X2LI-G44) +                           &
     &                  D5421*DCOS( XOMI+X2LI-G54) +                     &
     &                  D5433*DCOS(-XOMI+X2LI-G54))
                XNDDT = XNDDT*XLDOT
              ENDIF

! * ------------------------------- INTEGRATOR --------------------------
              !  sgp4fix move end checks to end of routine
              IF (DABS(T-ATIME).ge.STEPP) THEN
                  IRET  = 0
                  IRETN = 381
                ELSE
                  FT    = T-ATIME
                  IRETN = 0
                ENDIF

              IF (IRETN.EQ.381) THEN
                  XLI   = XLI + XLDOT*DELT + XNDT*STEP2
                  XNI   = XNI + XNDT*DELT + XNDDT*STEP2
                  ATIME = ATIME + DELT
                ENDIF

              ENDDO

            XN = XNI + XNDT*FT  + XNDDT*FT*FT*0.5D0
            XL = XLI + XLDOT*FT + XNDT*FT*FT*0.5D0
            IF(IREZ .ne. 1) THEN
                Mm   = XL-2.0D0*nodem+2.0D0*THETA
                DNDT = XN-no_kozai
              ELSE
                Mm   = XL-nodem-Argpm+THETA
                DNDT = XN-no_kozai
              ENDIF

            XN = no_kozai + DNDT
          ENDIF

! c        INCLUDE 'debug4.f' 

      RETURN
      END  ! end dspace
