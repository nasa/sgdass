! * ------------------------------------------------------------------------------
! *
! *                           SUBROUTINE NEWTONNU
! *
! *  This subroutine solves Keplers equation when the true anomaly is known.
! *    The Mean and Eccentric, parabolic, or hyperbolic anomaly is also found.
! *    The parabolic limit at 168ø is arbitrary. The hyperbolic anomaly is also
! *    limited. The hyperbolic sine is used because it's not double valued.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Ecc         - Eccentricity                   0.0D0 to
! *    Nu          - True Anomaly                   -2Pi to 2Pi rad
! *
! *  Outputs       :
! *    E0          - Eccentric Anomaly              0.0D0 to 2Pi rad       153.02 deg
! *    M           - Mean Anomaly                   0.0D0 to 2Pi rad       151.7425 deg 
! *
! *  Locals        :
! *    E1          - Eccentric Anomaly, next value  rad
! *    SinE        - Sine of E
! *    CosE        - Cosine of E
! *    Ktr         - Index
! *
! *  Coupling      :
! *    TLE_ASINH       - Arc hyperbolic sine
! *    SINH        - Hyperbolic Sine
! *
! *  References    :
! *    Vallado       2007, 85, Alg 5
! *
! * ------------------------------------------------------------------------------

      SUBROUTINE NEWTONNU    ( Ecc, Nu, E0, M )
        IMPLICIT NONE
        REAL*8 Ecc, Nu, E0, M
        EXTERNAL TLE_ASINH
! * -----------------------------  Locals  ------------------------------
        REAL*8 SinE, CosE, TLE_ASINH

        INCLUDE 'astmath.i'

        ! --------------------  Implementation   ----------------------
        E0= 999999.9D0
        M = 999999.9D0
        ! --------------------------- Circular ------------------------
        IF ( DABS( Ecc ) .lt. 0.000001D0 ) THEN
            M = Nu
            E0= Nu 
          ELSE
            ! ---------------------- Elliptical -----------------------
            IF ( Ecc .lt. 0.999D0 ) THEN
                SinE= ( DSQRT( 1.0D0-Ecc*Ecc ) * DSIN(Nu) ) /           &
     &                ( 1.0D0+Ecc*DCOS(Nu) )
                CosE= ( Ecc + DCOS(Nu) ) / ( 1.0D0 + Ecc*DCOS(Nu) )
                E0  = DATAN2( SinE, CosE )
                M   = E0 - Ecc*DSIN(E0) 
              ELSE
                ! -------------------- Hyperbolic  --------------------
                IF ( Ecc .gt. 1.0001D0 ) THEN
                    IF ( ((Ecc .gt. 1.0D0) .and. (DABS(Nu)+0.00001D0    &
     &                     .lt. Pi-DACOS(1.0D0/Ecc)) ) ) THEN
                        SinE= ( DSQRT( Ecc*Ecc-1.0D0 ) * DSIN(Nu) ) /   &
     &                        ( 1.0D0 + Ecc*DCOS(Nu) )
                        E0  = TLE_ASINH( SinE )
                        M   = Ecc*DSINH(E0) - E0
                      ENDIF 
                  ELSE
                    ! ----------------- Parabolic ---------------------
                    IF ( DABS(Nu) .lt. 168.0D0/57.29578D0 ) THEN
                        E0= DTAN( Nu*0.5D0 )
                        M = E0 + (E0*E0*E0)/3.0D0 
                      ENDIF
                  ENDIF
              ENDIF
          ENDIF

        IF ( Ecc .lt. 1.0D0 ) THEN
            M = DMOD( M, 2.0D0*Pi )
            IF ( M .lt. 0.0D0 ) THEN
                M= M + 2.0D0*Pi 
              ENDIF
            E0 = DMOD( E0, 2.0D0*Pi )
          ENDIF 
      RETURN
      END  ! end newtonnu
