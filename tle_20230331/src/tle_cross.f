! * ------------------------------------------------------------------------------
! *
! *                           SUBROUTINE TLE_CROSS
! *
! *  This subroutine crosses two vectors.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Vec1        - Vector number 1
! *    Vec2        - Vector number 2
! * 
! *  OutPuts       :
! *    OutVec      - Vector result of A x B
! *
! *  Locals        :
! *    None.
! *
! *  Coupling      :
! *    None.
! *
! * ------------------------------------------------------------------------------  

      SUBROUTINE TLE_CROSS ( Vec1, Vec2, OutVec )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3), OutVec(3)

        ! --------------------  Implementation   ----------------------
        OutVec(1)= Vec1(2)*Vec2(3) - Vec1(3)*Vec2(2)
        OutVec(2)= Vec1(3)*Vec2(1) - Vec1(1)*Vec2(3)
        OutVec(3)= Vec1(1)*Vec2(2) - Vec1(2)*Vec2(1)

      RETURN
      END  ! end TLE_CROSS
