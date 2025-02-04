      SUBROUTINE ABSMO_NUVEL(PSIT,T0,X0,Y0,Z0,T,X,Y,Z,scale,plfix)
      IMPLICIT NONE
!
!
!   ABSMO_NUVEL takes a site specified by its initial coordinates X0,Y0,Z0
!   at time T0, and computes its updated positions X,Y,Z at time T,
!   based on the Nuvel model.  It also supports the specifying one plate
!   as being fixed.  Of course, when that is used the results are not
!   no-net-rotation.
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
!
      Character*4 plfix      !The 4-letter plate name for the plate which is
!                             fixed in this solution.  If it is not a valid
!                             plate name then no plate is fixed and the rotation
!                             is no-net-rotation.  This is not an error.
!
      REAL*8             X0,Y0,Z0 !The input coordinates (meters)
      REAL*8             T0   !The inital time (years)
      REAL*8             T    !The target time (years)
      REAL*8             scale !A scale factor to multiply
!                               the rates by. (unitless)
!                              !Set to 1 for full NUVEL rate.
!
!     OUTPUT Variables:
!
      REAL*8             X,Y,Z! The coordinates after rotation (meters)
!
!
!     LOCAL VARIABLES
!
      integer*4 num_plates
      parameter (num_plates = 14)
      character*4        PNM(num_plates)
      real*8             OMX(num_plates)
      real*8             OMY(num_plates)
      real*8             OMZ(num_plates)
      real*8             ORX,ORY,ORZ,xv,yv,zv
      integer*4          IPSIT,I,ifsit
      logical*2          found
!
!
!   DFA: NNR-NUVEL1
!
      DATA (PNM(I),   OMX(I),   OMY(I),     OMZ(I), I = 1,num_plates) &
     &     /'PCFC',  -0.0907,   0.2902,  -0.5976, &
     &      'AFRC',   0.0532,  -0.1856,   0.2348, &
     &      'ANTA',  -0.0494,  -0.1018,   0.2218, &
     &      'ARAB',   0.4003,  -0.0311,   0.4049, &
     &      'AUST',   0.4695,   0.3072,   0.3762, &
     &      'CARB',  -0.0109,  -0.2027,   0.0945, &
     &      'COCO',  -0.6249,  -1.2944,   0.6544, &
     &      'EURA',  -0.0590,  -0.1434,   0.1887, &
     &      'INDI',   0.3995,   0.0026,   0.4066, &
     &      'NAZC',  -0.0921,  -0.5138,   0.5756, &
     &      'NOAM',   0.0152,  -0.2155,  -0.0094, &
     &      'SOAM',  -0.0624,  -0.0906,  -0.0523, &
     &      'JUFU',   0.2995,   0.4805,  -0.2936, &
     &      'PHIL',   0.5913,  -0.4412,  -0.5976/
!
!   AM0-2
!
!
!      DATA (PNM(I),   OMX(I),   OMY(I),   OMZ(I), I = 1,num_plates)
!     &     /'PCFC', -0.12276,  0.31163, -0.65537,
!     &      'COCO', -0.63726, -1.33142,  0.72556,
!     &      'NAZC', -0.09086, -0.53281,  0.63061,
!     &      'CARB', -0.02787, -0.05661,  0.10780,
!     &      'SOAM', -0.05604, -0.10672, -0.08642,
!     &      'ANTA', -0.05286, -0.09492,  0.21570,
!     &      'INDI',  0.48372,  0.25011,  0.43132,
!     &      'AFRC',  0.05660, -0.19249,  0.24016,
!     &      'ARAB',  0.27885, -0.16744, 0.37359,
!     &      'EURA', -0.03071, -0.15865,  0.19605,
!     &      'NOAM',  0.03299, -0.22828, -0.01427/
!
! I - Loop index
! IPSIT - Integer index for plate PSIT
! IfSIT - Integer index for plate plfix
!
! OMX,OMY,OMZ - Arrays containing motion rates for each plate (deg/Myr)
! ORX,ORY,ORZ - Motion rates converted to radians/year
! PNM - Array containing recognized plate name abbreviations
!
!   HISTORY
!   WHO   WHEN   WHAT
!   JRR   1990   Created
!   MWH  930930  Added scaling factor, specified in PLATE_MODEL line of batch control file
!   jwr  950930  Cleaned up documentation and the code a small bit. No changes
!                which affect the results.
!
!   ABSMO_NUVEL PROGRAM STRUCTURE
!   Initialize things properly
!
      IPSIT = -1
      X = 0.0D0
      Y = 0.0D0
      Z = 0.0D0
      found = .false.
      ifsit = 0
      do i=1,num_plates
        if (plfix.eq.pnm(i)) then
          found = .true.
          ifsit = i
        endif
      enddo
!
!   Look up the plate in the list.
!
      Do I = 1,num_plates
        IF (PSIT .EQ. PNM(I)) IPSIT = I
      Enddo
!
!   If plate name is not recognized return the new plate position as zero.
!
      IF (IPSIT .EQ. -1) RETURN
!
!     Convert from degree/My to radians/yr.
      if(ifsit.eq.0) then !No fixed site
        ORX = OMX(IPSIT) * 1.7453292D-08
        ORY = OMY(IPSIT) * 1.7453292D-08
        ORZ = OMZ(IPSIT) * 1.7453292D-08
      else                !Fixed site specified.
        ORX = (OMX(IPSIT)-omx(ifsit)) * 1.7453292D-08
        ORY = (OMY(IPSIT)-omy(ifsit)) * 1.7453292D-08
        ORZ = (OMZ(IPSIT)-omz(ifsit)) * 1.7453292D-08
      endif
!
!   Compute the new coordinates
!
      Xv = ORY*Z0 - ORZ*Y0
      Yv = ORZ*X0 - ORX*Z0
      Zv = ORX*Y0 - ORY*X0
      X = X0 + xv*scale * (T-T0)
      Y = Y0 + yv*scale * (T-T0)
      Z = Z0 + zv*scale * (T-T0)
!
!   Finish up
!
      RETURN
      END
