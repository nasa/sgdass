      SUBROUTINE NUT_PARTS(NP,TU,DERIV,FLPSI,FLEPS,IDP,NDP,FCNPER,NUTP)
      IMPLICIT NONE
!
! 1.  NUT_PARTS PROGRAM SPECIFICATION
!
! 1.1  Evaluate nutation series and update array of partial derivatives
!      of the parameters
!
! 1.2 REFERENCES:
!
! 2.  NUT_PARTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NP, FLPSI, FLEPS, IDP, NDP
      REAL*8    FCNPER, NUTP(2,2)
      REAL*8    TU, DERIV(M_GPA,2)
!
! DERIV - Array of partial derivatives of the parameters
! FCNPER - Free core nutation period
! FLEPS - Additional EPS flag bits
! FLPSI - Additional PSI flag bits
! IDP - Nutation period flags ( see NUTW)
! NDP - Number of nutation period flags ( see NUTW)
! NP - Counter for the bits set ( see function KBIT)
! NUTP - Partial derivatives for daily nutation
! TU - Number of centuries since J2000
!
! 2.3 OUTPUT Variables:
!
! NP - Counter for the bits set ( see function KBIT)
! DERIV - Array of partial derivatives of the parameters
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: partl
!       CALLED SUBROUTINES: nutw,par_cmp
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 NPAR
      INTEGER*2 I,IBIT,IPOS
      LOGICAL*2 KBIT
      REAL*8 ARGP(2,20),DPSI(2),DEPS(2)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  NUT_PARTS PROGRAM STRUCTURE
!
      IF(NDP.GT.0) CALL NUTW(TU,DPSI,DEPS,ARGP,IDP,NDP)
!
      CALL PAR_CMP(NP,TU,DERIV,FLPSI,ARGP,FCNPER,NUTP(1,1),NUTP(1,2))
!
      CALL PAR_CMP(NP,TU,DERIV,FLEPS,ARGP,FCNPER,NUTP(2,1),NUTP(2,2))
!
      RETURN
      END
