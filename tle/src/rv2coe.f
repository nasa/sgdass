! * ------------------------------------------------------------------------------
! *
! *                           SUBROUTINE rv2coe
! *
! *  This subroutine finds the classical orbital elements given the Geocentric
! *    Equatorial Position and Velocity vectors.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    R           - IJK Position vector            km
! *    V           - IJK Velocity vector            km / s
! *    mu          - gravitational parameter        km3 / s2
! *
! *  Outputs       :
! *    P           - SemiLatus rectum               km
! *    A           - semimajor axis                 km
! *    Ecc         - Eccentricity
! *    Incl        - inclination                    0.0D0 to Pi rad
! *    Omega       - Longitude of Ascending Node    0.0D0 to 2Pi rad
! *    Argp        - Argument of Perigee            0.0D0 to 2Pi rad
! *    Nu          - True anomaly                   0.0D0 to 2Pi rad
! *    M           - Mean anomaly                   0.0D0 to 2Pi rad
! *    ArgLat      - Argument of Latitude      (CI) 0.0D0 to 2Pi rad
! *    LamTrue     - True Longitude            (CE) 0.0D0 to 2Pi rad
! *    LonPer      - Longitude of Periapsis    (EE) 0.0D0 to 2Pi rad
! *
! *   Locals        :
! *    HBar        - Angular Momentum H Vector      km2 / s
! *    EBar        - Eccentricity     E Vector
! *    NBar        - Line of Nodes    N Vector
! *    c1          - V**2 - u/R
! *    RDotV       - R DOT V
! *    Hk          - Hk norm vector
! *    SME         - Specfic Mechanical Energy      km2 / s2
! *    i           - index
! *    E           - Eccentric, Parabolic,
! *                  Hyperbolic Anomaly             rad
! *    Temp        - Temporary variable
! *    TypeOrbit   - Type of orbit                  EE, EI, CE, CI
! *
! *  Coupling      :
! *    TLE_MAG         - Magnitude of a vector
! *    TLE_CROSS       - CROSS product of two vectors
! *    TLE_DOT         - DOT product of two vectors
! *    TLE_ANGLE       - Find the TLE_ANGLE between two vectors
! *    NEWTONNU    - Find the mean anomaly
! *
! *  References    :
! *    Vallado       2007, 121, Alg 9, Ex 2-5
! *
! * ------------------------------------------------------------------------------

      SUBROUTINE rv2coe      ( R, V, mu, P, A, Ecc, Incl, Omega, Argp,  &
     &                         Nu, M, ArgLat, TrueLon, LonPer )
        IMPLICIT NONE
        REAL*8 R(3), V(3), mu, P, A, Ecc, Incl, Omega, Argp, Nu, M,     &
     &         ArgLat, TrueLon, LonPer
        EXTERNAL TLE_DOT, TLE_MAG
! * -----------------------------  Locals  ------------------------------
        REAL*8 c1, RDotV, hk, SME, Hbar(3), Ebar(3), Nbar(3),           &
     &         TLE_DOT, E, Temp, TLE_MAG, maghbar, magnbar, magr, magv
        INTEGER i
        CHARACTER*2 TypeOrbit

        INCLUDE 'astmath.i'

        ! --------------------  Implementation   ----------------------
        magr = TLE_MAG( R )
        magv = TLE_MAG( V )
        ! ------------------  Find H N and E vectors   ----------------
        CALL TLE_CROSS( R, V, HBar )
        maghbar = TLE_MAG(Hbar)
        IF ( maghbar .gt. Small ) THEN
            NBar(1)= -HBar(2)
            NBar(2)=  HBar(1)
            NBar(3)=   0.0D0
            magnbar = TLE_MAG( Nbar )
            c1 = magv**2 - mu/magr
            RDotV= TLE_DOT( R, V )
            DO i= 1 , 3
                EBar(i)= (c1*R(i) - RDotV*V(i))/mu
              ENDDO

            Ecc = TLE_MAG( EBar )

            ! ------------  Find a e and semi-Latus rectum   ----------
            SME= ( magv*magv*0.5D0 ) - ( mu/magr )
            IF ( DABS( SME ) .gt. Small ) THEN
                A= -mu / (2.0D0*SME)
              ELSE
                A= Infinite
              ENDIF
            P = maghbar*maghbar/mu

            ! -----------------  Find inclination   -------------------
            Hk= HBar(3)/maghbar
! c            IF ( DABS( DABS(Hk) - 1.0D0 ) .lt. Small ) THEN
! c                ! -------------  Equatorial Orbits   ------------------
! c                IF ( DABS(HBar(3)) .gt. 0.0D0 ) THEN
! c                    Hk= DSIGN(1.0D0, HBar(3))
! c                  ENDIF
! c              ENDIF
            Incl= DACOS( Hk ) 

            ! --------  Determine type of orbit for Later use  --------
            ! ------ Elliptical, Parabolic, Hyperbolic Inclined -------
            TypeOrbit= 'EI' 
            IF ( Ecc .lt. Small ) THEN
                ! ----------------  Circular Equatorial ---------------
                IF ( (Incl.lt.Small).or.(DABS(Incl-Pi).lt.Small) ) THEN
                    TypeOrbit= 'CE'
                  ELSE
                    ! --------------  Circular Inclined ---------------
                    TypeOrbit= 'CI'
                  ENDIF
              ELSE
                ! - Elliptical, Parabolic, Hyperbolic Equatorial --
                IF ( (Incl.lt.Small).or.(DABS(Incl-Pi).lt.Small) ) THEN
                    TypeOrbit= 'EE'
                  ENDIF
              ENDIF

            ! ----------  Find Longitude of Ascending Node ------------
            IF ( magnbar .gt. Small ) THEN
                Temp= NBar(1) / magnbar
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                Omega= DACOS( Temp ) 
                IF ( NBar(2) .lt. 0.0D0 ) THEN
                    Omega= TwoPi - Omega
                  ENDIF
              ELSE
                Omega= Undefined 
              ENDIF

            ! ---------------- Find Argument of perigee ---------------
            IF ( TypeOrbit .eq. 'EI' ) THEN
                CALL TLE_ANGLE( NBar, EBar, Argp )
                IF ( EBar(3) .lt. 0.0D0 ) THEN
                    Argp= TwoPi - Argp 
                  ENDIF
              ELSE
                Argp= Undefined 
              ENDIF

            ! ------------  Find True Anomaly at Epoch    -------------
            IF ( TypeOrbit(1:1) .eq. 'E' ) THEN
                CALL TLE_ANGLE( EBar, r, Nu )
                IF ( RDotV .lt. 0.0D0 ) THEN
                    Nu= TwoPi - Nu 
                  ENDIF
              ELSE
                Nu= Undefined 
              ENDIF

            ! ----  Find Argument of Latitude - Circular Inclined -----
            IF ( TypeOrbit .eq. 'CI' ) THEN
                CALL TLE_ANGLE( NBar, R, ArgLat )
                IF ( R(3) .lt. 0.0D0 ) THEN
                    ArgLat= TwoPi - ArgLat
                  ENDIF
              ELSE
                ArgLat= Undefined 
              ENDIF

            ! -- Find Longitude of Perigee - Elliptical Equatorial ----
            IF ( ( Ecc.gt.Small ) .and. (TypeOrbit.eq.'EE') ) THEN
                Temp= EBar(1)/Ecc
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                LonPer= DACOS( Temp ) 
                IF ( EBar(2) .lt. 0.0D0 ) THEN
                    LonPer= TwoPi - LonPer 
                  ENDIF
                IF ( Incl .gt. HalfPi ) THEN
                    LonPer= TwoPi - LonPer
                  ENDIF
              ELSE
                LonPer= Undefined
              ENDIF

            ! -------- Find True Longitude - Circular Equatorial ------
            IF ( ( magr.gt.Small ) .and. ( TypeOrbit.eq.'CE' ) ) THEN
                Temp= R(1)/magr
                IF ( DABS(Temp) .gt. 1.0D0 ) THEN
                    Temp= DSIGN(1.0D0, Temp)
                  ENDIF
                TrueLon= DACOS( Temp )
                IF ( R(2) .lt. 0.0D0 ) THEN
                    TrueLon= TwoPi - TrueLon
                  ENDIF
                IF ( Incl .gt. HalfPi ) THEN
                    TrueLon= TwoPi - TrueLon
                  ENDIF
              ELSE
                TrueLon= Undefined
              ENDIF

            ! ------------ Find Mean Anomaly for all orbits -----------
            CALL NEWTONNU(Ecc, Nu, E, M )

         ELSE
           P    = Undefined
           A    = Undefined
           Ecc  = Undefined
           Incl = Undefined
           Omega= Undefined 
           Argp = Undefined 
           Nu   = Undefined 
           M    = Undefined 
           ArgLat  = Undefined 
           TrueLon= Undefined 
           LonPer = Undefined 
         ENDIF 

      RETURN
      END  ! end rv2coe
