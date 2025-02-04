      SUBROUTINE METFIX(ATMPR,RELHUM,TEMP,CHANGE,ALT)
      IMPLICIT NONE
!
! 1.  METFIX PROGRAM SPECIFICATION
!
! 1.1 COMPUTES DEFAULT METEROLOGICAL DATA BASED UPON ALTITUDE IN METERS
!     CALLED BY CFACALC AND LANCALC.
!
! 1.2 REFERENCES:
!
! 2.  METFIX INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 ALT
!
!     ALT - Altitude
!
! 2.3 OUTPUT Variables:
!
      REAL*8 ATMPR,RELHUM,TEMP
      LOGICAL*2 CHANGE
!
!     ATMPR  - Atmospheric presure
!     RELHUM - Relative humidity
!     TEMP   - Temperature (Celsius)
!     CHANGE - TRUE means made changes in any of ATMPR, RELHUM, and TEMP
!              FALSE means no changes made
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: lancalc, cfacalc
!       CALLED SUBROUTINES: none
!
! 3.  LOCAL VARIABLES
!
!
      REAL*8 X
      INTEGER*2 NOTRUST
      DATA NOTRUST/0/
!
!     NOTRUST IF 1 SETS DEFAULTS TO ORIGINAL VALUES OF PREVIOUS CFACALC
!
! 4.  HISTORY
!
!  WHO  WHEN      WHAT
!  DNM  89.11.16  PROGRAMMED BY D. MATSAKIS FROM CODE BY J. DAVIS
!
! 5.  METFIX PROGRAM STRUCTURE
!
      IF (TEMP.LT.-70.D0 .OR. TEMP.GT.50.D0) THEN
          TEMP=293.15D0-(6.5D-3)*ALT-273.16D0
          IF (NOTRUST.EQ.1) TEMP=20.D0
          CHANGE=.TRUE.
          ENDIF
      IF (ATMPR.LT.600.D0 .OR. ATMPR.GT.1100.D0) THEN
          X=1.D0-(6.5D-3)*ALT/293.15D0
          ATMPR=1013.25D0*(X**5.26D0)
          IF (NOTRUST.EQ.1) ATMPR=1000.D0
!@  write ( 6, * ) ' alt= ', alt, ' atmpr=',atmpr;  call pause ( 'metfix' ) ! %%%%
          CHANGE=.TRUE.
          ENDIF
      IF (RELHUM.LT.0.D0 .OR. RELHUM.GT.1.01D0) THEN
          RELHUM=.5D0
          CHANGE=.TRUE.
          ENDIF
      RETURN
      END
