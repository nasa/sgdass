! * ------------------------------------------------------------------------------
! *
! *                           SUBROUTINE TLE_MAG
! *
! *  This subroutine finds the magnitude of a vector.  The tolerance is set to
! *    0.00000001D0, thus the 1.0D0E-16 for the squared test of underflows.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Vec       - Vector
! *
! *  OutPuts       :
! *    Vec       - Answer stored in fourth component
! *
! *  Locals        :
! *    None.
! *
! *  Coupling      :
! *    None.
! *
! * ------------------------------------------------------------------------------  

      REAL*8 FUNCTION TLE_MAG    ( Vec )
        IMPLICIT NONE
        REAL*8 Vec(3)
! * -----------------------------  Locals  ------------------------------
        Real*8 Temp

        ! --------------------  Implementation   ----------------------
        Temp= Vec(1)*Vec(1) + Vec(2)*Vec(2) + Vec(3)*Vec(3)

        IF ( DABS( Temp ) .ge. 1.0D-16 ) THEN
            TLE_MAG = DSQRT( Temp )
          ELSE
            TLE_MAG = 0.0D0
          ENDIF
      RETURN
      END  ! end tle_mag
