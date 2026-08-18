! * ------------------------------------------------------------------------------
! *
! *                           FUNCTION TLE_DOT
! *
! *  This function finds the TLE_DOT product of two vectors.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Vec1        - Vector number 1
! *    Vec2        - Vector number 2
! *
! *  OutPuts       :
! *    TLE_DOT         - Result
! *
! *  Locals        :
! *    None.
! *
! *  Coupling      :
! *    None.
! *
! * ------------------------------------------------------------------------------  

      REAL*8 FUNCTION TLE_DOT    ( Vec1,Vec2 )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3)

        ! --------------------  Implementation   ----------------------
        TLE_DOT = Vec1(1)*Vec2(1) + Vec1(2)*Vec2(2) + Vec1(3)*Vec2(3)
      RETURN
      END  ! end tle_dot
