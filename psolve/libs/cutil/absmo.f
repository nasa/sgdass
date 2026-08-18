      SUBROUTINE ABSMO(PSIT,T0,X0,Y0,Z0,T,X,Y,Z,scale)
      IMPLICIT NONE
!
!
!   ABSMO takes a site specified by its initial coordinates X0,Y0,Z0
!   at time T0, and computes its updated positions X,Y,Z at time T,
!   based on the geological 'absolute', (no net rotation) plate motion
!   model AM0-2 (Minster and Jordan, 1978).
!
!   Original author: J.B. Minster, Science Horizons.
!
!   Transcribed from USNO Circular 167 'Project Merit Standards' by
!   Tony Mallama with slight modification to the documentation and code.
!
!   Times are given in years, e.g. 1988.0 for Jan 1, 1988.
!
!
!   INPUT Variables:
!
      Character*4 PSIT       !The 4-letter plate name for the plate on which
!                             the site resides. If PSIT does not match one
!                             of the defined plate id's, no rotation is done and
!                             no error warning is issued.
      REAL*8             X0,Y0,Z0 !The input coordinates (meters)
      REAL*8             T0   !The inital time (years)
      REAL*8             T    !The target time (years)
      REAL*8             scale !A scale factor to multiply the
!                               rates by. (unitless)
!
!
! 2.3 OUTPUT Variables:
!
      REAL*8             X,Y,Z! The coordinates after rotation (meters)
!
!
!     LOCAL VARIABLES
!
      integer*4 num_plates
      parameter (num_plates = 13)
      character*4        PNM(num_plates)
      real*8             OMX(num_plates)
      real*8             OMY(num_plates)
      real*8             OMZ(num_plates)
      real*8             ORX,ORY,ORZ
      integer*4          IPSIT,I
!
      DATA    (PNM(I),     OMX(I),     OMY(I),     OMZ(I), &
     &                                            I = 1,num_plates) &
     &        /'PCFC',   -0.12276,    0.31163,   -0.65537, &
     &         'PHIL',   -0.12276,    0.31163,   -0.65537, &
     &         'COCO',   -0.63726,   -1.33142,    0.72556, &
     &         'NAZC',   -0.09086,   -0.53281,    0.63061, &
     &         'CARB',   -0.02787,   -0.05661,    0.10780, &
     &         'SOAM',   -0.05604,   -0.10672,   -0.08642, &
     &         'ANTA',   -0.05286,   -0.09492,    0.21570, &
     &         'INDI',    0.48372,    0.25011,    0.43132, &
     &         'AUST',    0.48372,    0.25011,    0.43132, &
     &         'AFRC',    0.05660,   -0.19249,    0.24016, &
     &         'ARAB',    0.27885,   -0.16744,    0.37359, &
     &         'EURA',   -0.03071,   -0.15865,    0.19605, &
     &         'NOAM',    0.03299,   -0.22828,   -0.01427/
!
! I - Loop index
! IPSIT - Integer index for plate PSIT
! OMX,OMY,OMZ - Arrays containing motion rates for each plate (deg/Myr)
! ORX,ORY,ORZ - Motion rates converted to radians/year
! PNM - Array containing recognized plate name abbreviations
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
! Mallama ????   Created
!   MWH  930930  Add scaling factor specified in PLATE_MODEL line of batch control file
!   KDB  951019 Cleaned up documentation and code to mimic Jim Ryan's 9/30/95
!               changes.  No changes which affect the results.
!
! 5.  ABSMO PROGRAM STRUCTURE
!
!   Initialize things properly
!
      IPSIT = -1
      X = 0.0D0
      Y = 0.0D0
      Z = 0.0D0
!
!   Look up the plate in the list.
!
      Do    I = 1,num_plates
        IF (PSIT .EQ. PNM(I)) IPSIT = I
      Enddo
!
!   If plate name is not recognized return the new plate position as zero.
!
      IF (IPSIT .EQ. -1) RETURN
!
!   Convert from degree/My to radians/yr.
!
      ORX = OMX(IPSIT) * 1.7453292D-08
      ORY = OMY(IPSIT) * 1.7453292D-08
      ORZ = OMZ(IPSIT) * 1.7453292D-08
!
!   Compute the new coordinates
!
      X = X0 + (ORY*Z0 - ORZ*Y0)*scale * (T-T0)
      Y = Y0 + (ORZ*X0 - ORX*Z0)*scale * (T-T0)
      Z = Z0 + (ORX*Y0 - ORY*X0)*scale * (T-T0)
!
!   Finish up
!
      RETURN
      END
