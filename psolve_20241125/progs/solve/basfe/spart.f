      SUBROUTINE SPART ( SPAR, BC, VSITE1 )
      IMPLICIT NONE
!
! 1.  SPART PROGRAM SPECIFICATION
!
! 1.1 SPART IS THE SUBROUTINE WHICH WILL CALCULATE THE PARTIALS
!     OF THE BASELINE COMPONENTS WITH RESPECT TO THE SITE
!     COMPONENTS FOR THE PURPOSE OF CALCULATING THE BASELINE
!     COMPONENT FORMAL ERRORS.
!
! 1.2 REFERENCES:
!
! 2.  SPART INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 BC(11),VSITE1(3)
!
! BC - Baseline coordinates for baseline being processed
! VSITE - Site coordinates for station being processed
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SPAR(6,11)
!
! SPAR - Partials of the baseline componentes wrt the site components
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: bwork
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IX,J
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  SPART PROGRAM STRUCTURE
!
      SPAR(1,1) = -1.0D0
      SPAR(2,1) =  0.0D0
      SPAR(3,1) =  0.0D0
!
      SPAR(1,2) =  0.0D0
      SPAR(2,2) = -1.0D0
      SPAR(3,2) =  0.0D0
!
      SPAR(1,3) =  0.0D0
      SPAR(2,3) =  0.0D0
      SPAR(3,3) = -1.0D0
!
      SPAR(1,4) = -BC(1)/BC(4)
      SPAR(2,4) = -BC(2)/BC(4)
      SPAR(3,4) = -BC(3)/BC(4)
!
      SPAR(1,5) =  VSITE1(2)/(VSITE1(1)**2+VSITE1(2)**2) - BC(2)/BC(7)**2
      SPAR(2,5) =  VSITE1(1)/(VSITE1(1)**2+VSITE1(2)**2) - BC(1)/BC(7)**2
      SPAR(3,5) =  0.0D0
!
      SPAR(1,6) =  BC(3)*BC(1)/(BC(7)*BC(4)**2)
      SPAR(2,6) =  BC(3)*BC(2)/(BC(7)*BC(4)**2)
      SPAR(3,6) = -BC(7)/BC(4)**2
!
      SPAR(1,7) = -BC(1)/BC(7)
      SPAR(2,7) = -BC(2)/BC(7)
      SPAR(3,7) =  0.0D0
!
      SPAR(1,8) =  BC(2)/BC(7)**2
      SPAR(2,8) = -BC(1)/BC(7)**2
      SPAR(3,8) =  0.0D0
!
!**** THE PARTIALS FOR SITE 2 ARE THE SAME AS FOR SITE 1
!**** EXCEPT FOR THE SIGN, BUT DON'T HANDLE THE TRASK COORDINATE
!     SYSTEM PARTIALS. THEY ARE HANDLED IN GTLHV.
!
      DO IX=1,8
         DO J=1,3
            SPAR(J+3,IX) = -SPAR(J,IX)
         ENDDO
      ENDDO
!
      RETURN
      END  !#!  SPART  #!#
