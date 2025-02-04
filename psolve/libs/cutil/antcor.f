      SUBROUTINE ANTCOR(AXTP,AXOF,AZ,EL,LAT,HSCALE,ZCORR,AWARN)
      IMPLICIT NONE
!
! 1.  ANTCOR PROGRAM SPECIFICATION
!
! 1.1 Calculates the corrections to the dry zenith troposphere delay
!     due to axis offsets and motion of feed relative to intersection
!     of axes.  correction depends on antenna mount type as well as
!     source position and axis offset value.
!
! 1.2 REFERENCES:
!
! 2.  ANTCOR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 AXTP
      REAL*8 AXOF,AZ,EL,LAT,HSCALE
!
! AXOF - Axis offset
! AXTP - Axis type (see below)
! AZ - Azimuth of source position
! EL - Elevation of source position
! HSCALE - Scale height of total atmospere (km)
! LAT - Latitude of antenna site
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 AWARN
      REAL*8 ZCORR
!
! AWARN - Nonzero value indicates no correction made
! ZCORR - Correction calculated by this routine
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      REAL*8 SINDEC,DEC,SINHA,COSHA,HA,XY
!
! COSHA - Cosine of the source hour angle
! DEC - Declination of source position
! HA - Hour angle of source position
! SINDEC - Sine of source declination
! SINHA - Sine of source hour angle
! XY -
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jrr  890209  Created
!   jrr  890216  Added warning feature to alert user to lack of
!                correction for axis types 4 and 5 and type 0
!                (no axis type information available).
!
! 5  ANTCOR PROGRAM STRUCTURE
!
!     Handle calculation for each mount type separately.  The possible
!     types are:
!        1 - equatorial mount
!        2 - XY mount, fixed axis N-S (Mojave)
!        3 - alt-az mount
!        4 - XY mount, fixed axis E-W
!        5 - Richmond, Fla., special case
!
!        0 - axis mount not identified in NCORT
!
!
      AWARN = 0
!
      IF (AXTP .EQ. 1) THEN
!
!       Find source hour angle first
        SINDEC = DSIN(EL)*DSIN(LAT) + DCOS(EL)*DCOS(LAT)*DCOS(AZ)
        DEC = DASIN(SINDEC)
!       SINHA = -DCOS(EL)*DSIN(AZ)/DCOS(DEC)
        COSHA = (DSIN(EL) - SINDEC*DSIN(LAT)) / (DCOS(DEC)*DCOS(LAT))
!       HA = DATAN2(SINHA,COSHA)
        ZCORR = (AXOF/HSCALE)*DCOS(LAT)*COSHA
        GO TO 999
!
      END IF
!
      IF (AXTP .EQ. 2) THEN
        XY = DSQRT(DSIN(AZ)*DSIN(AZ) + DSIN(EL)*DSIN(EL) &
     &             - DSIN(AZ)*DSIN(AZ)*DSIN(EL)*DSIN(EL))
        ZCORR = (AXOF/HSCALE) * DSIN(EL) / XY
        GO TO 999
      END IF
!
      IF (AXTP .EQ. 3) THEN
        ZCORR = 0.D0
        GO TO 999
      END IF
!
      IF (AXTP .EQ. 4) THEN
        ZCORR = 0.D0
        AWARN = 1
        GO TO 999
      END IF
!
      IF (AXTP .EQ. 5) THEN
        ZCORR = 0.D0
        AWARN = 1
        GO TO 999
      END IF
!
      IF (AXTP .EQ. 0) THEN
        ZCORR = 0.D0
        AWARN = 1
        GO TO 999
      END IF
!
!
  999 CONTINUE
!
      RETURN
      END
