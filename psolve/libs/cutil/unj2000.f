!****************************************************************************
        SUBROUTINE UNJ2000(DAYIN,DAYOUT)
!
!   CONVERT to   JULIAN DATE from TIME IN JULIAN CENTURIES SINCE J2000 !
!
        IMPLICIT NONE
        REAL*8 DAYIN
        REAL*8 DAYOUT
!
        DAYOUT = DAYIN*36525.0d0+2451545.d0
!
        RETURN
        END
