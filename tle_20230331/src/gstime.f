! * -----------------------------------------------------------------------------
! *
! *                           FUNCTION GSTIME
! *
! *  This function finds the Greenwich SIDEREAL time.  Notice just the INTEGER
! *    part of the Julian Date is used for the Julian centuries calculation.
! *    We use radper Solar day because we're multiplying by 0-24 solar hours.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    JD          - Julian Date                    days from 4713 BC
! *
! *  OutPuts       :
! *    GSTIME      - Greenwich SIDEREAL Time        0 to 2Pi rad
! *
! *  Locals        :
! *    Temp        - Temporary variable for reals   rad
! *    TUT1        - Julian Centuries from the
! *                  Jan 1, 2000 12 h epoch (UT1)
! *
! *  Coupling      :
! *
! *  References    :
! *    Vallado       2007, 194, Eq 3-45
! * -----------------------------------------------------------------------------

      REAL*8 FUNCTION GSTIME ( JD )
        IMPLICIT NONE
        REAL*8 JD
! * ----------------------------  Locals  -------------------------------
        REAL*8 Temp, TUT1

        INCLUDE 'astmath.i'

        ! --------------------  Implementation   ----------------------

        TUT1= ( JD - 2451545.0D0 ) / 36525.0D0
        Temp= - 6.2D-6*TUT1*TUT1*TUT1                                   &
     &        + 0.093104D0*TUT1*TUT1                                    &
     &        + (876600.0D0*3600.0D0 + 8640184.812866D0)*TUT1           &
     &        + 67310.54841D0
        Temp= DMOD( Temp*Deg2Rad/240.0D0,TwoPi ) ! 360/86400 = 1/240, to deg, to rad

        ! ------------------------ Check quadrants --------------------
        IF ( Temp .lt. 0.0D0 ) THEN
            Temp= Temp + TwoPi
          ENDIF

        GSTIME= Temp

      RETURN
      END  ! end gstime
