! * ------------------------------------------------------------------------------
! *
! *                           FUNCTION TLE_ASINH
! *
! *  This function evaluates the inverse hyperbolic sine.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    XVal        - angle Value                                  any real
! *
! *  OutPuts       :
! *    TLE_ASINH       - Result                                       any real
! *
! *  Locals        :
! *    None.
! *
! *  Coupling      :
! *    None.
! *
! * ------------------------------------------------------------------------------  

      REAL*8 FUNCTION TLE_ASINH( XVal )
        IMPLICIT NONE
        REAL*8 XVal

        ! --------------------  Implementation   ----------------------
        TLE_ASINH = DLOG( XVal + DSQRT( XVal*XVal + 1.0D0 ) )

      RETURN
      END  ! end TLE_ASINH
