      SUBROUTINE SCOORD_CONV(IRHR,IRMN,RASC, &
     &                      IDCDG,IDCMN,DECSC, &
     &                      RA_RAD,DEC_RAD)
      IMPLICIT NONE
!
! 1.  SCOORD_CONV PROGRAM SPECIFICATION
!
! 1.1 Convert input source coordinates in degrees to output radian coordinates.
!
! 2.   SCOORD_CONV INTERFACE
!
! 2.2 INPUT Variables:
!
!     IRHR, IRMN,RASC - right ascension degrees value in hours, minutes
!                               and seconds
!     IDCDG, IDCMN,DECSC - declination degrees value in hours, minutes
!                               and seconds
!
      INTEGER*2 IRHR,IRMN,IDCDG,IDCMN
      REAL*8 RASC,DECSC
!
! 2.3 OUTPUT Variables:
!
!     RA_RAD - right ascension in radians
!     DEC_RAD - declination in radians
!
      REAL*8 RA_RAD,DEC_RAD
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ISIGN
      REAL*8 TWOPI
      DATA TWOPI /.6283185307179587D+1/
!
! 4.  HISTORY
!     WHO  WHEN   WHAT
!     KDB 950419  Created using code from gsouc.f
!
! 5.  SCOORD_CONV PROGRAM STRUCTURE
!
      ISIGN = 1
      IF(IRHR.LT.0 .OR. IRMN.LT.0 .OR. RASC.LT.0.D0) ISIGN = -1
      IRHR = ABS(IRHR)
      IRMN = ABS(IRMN)
      RASC = ABS(RASC)
      RA_RAD=(IRHR/24.D0+IRMN/1440.D0+RASC/86400.D0)*TWOPI &
     &               *ISIGN
      IF(RA_RAD.LT.0.D0) RA_RAD=RA_RAD+TWOPI
      ISIGN = 1
      IF(IDCDG.LT.0 .OR. IDCMN.LT.0 .OR. DECSC.LT.0.D0) ISIGN=-1
      IDCDG = ABS(IDCDG)
      IDCMN = ABS(IDCMN)
      DECSC = ABS(DECSC)
      DEC_RAD=(((DECSC/60.D0+IDCMN)/60.D0+IDCDG)/360.D0) &
     &               *TWOPI*ISIGN
!
      RETURN
      END
