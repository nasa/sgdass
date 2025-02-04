! * ------------------------------------------------------------------------------
! *
! *                           SUBROUTINE TLE_ANGLE
! *
! *  This subroutine calculates the TLE_ANGLE between two vectors.  The output is
! *    set to 999999.1D0 to indicate an undefined value.  Be SURE to check  
! *    this at the output phase.
! *
! *  Author        : David Vallado                  719-573-2600    1 Mar 2001
! *
! *  Inputs          Description                    Range / Units
! *    Vec1        - Vector number 1
! *    Vec2        - Vector number 2
! *
! *  OutPuts       :
! *    Theta       - ANGLE between the two vectors  -Pi to Pi
! *
! *  Locals        :
! *    Temp        - Temporary REAL variable
! *
! *  Coupling      :
! *    TLE_DOT           DOT Product of two vectors
! *    DACOS         Arc Cosine FUNCTION
! *
! * ------------------------------------------------------------------------------  

      SUBROUTINE TLE_ANGLE ( Vec1, Vec2, Theta )
        IMPLICIT NONE
        REAL*8 Vec1(3), Vec2(3), Theta, magvec1, magvec2
        EXTERNAL TLE_DOT, TLE_MAG
! * -----------------------------  Locals  ------------------------------
        REAL*8 Temp, TLE_DOT, TLE_MAG
        INCLUDE 'astmath.i'

        ! --------------------  Implementation   ----------------------
        magvec1 = TLE_MAG(vec1)
        magvec2 = TLE_MAG(vec2)
        IF ( magVec1*magVec2 .gt. Small**2 ) THEN
            Temp= TLE_DOT(Vec1,Vec2) / (magVec1*magVec2)
            IF ( DABS( Temp ) .gt. 1.0D0 ) THEN
                Temp= DSIGN(1.0D0, Temp)
              ENDIF
            Theta= DACOS( Temp ) 
          ELSE
            Theta= Undefined
          ENDIF
      RETURN
      END  ! end TLE_ANGLE
