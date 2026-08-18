      SUBROUTINE SPLAT(pltmod1,VSUBXYZ,nvel,lvelnam,plate_fact)
      IMPLICIT NONE
!
! 1.  SVELP PROGRAM SPECIFICATION
!
! 1.1 Call subroutines GVSTAP and DVSTAP to read a site position
!     velocity substitution file and to calculate site position differences.
!
! 1.2 REFERENCES:
!
! 2.  SVELP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) pltmod1
      real*8 plate_fact
!
! 2.3 OUTPUT Variables:
!
      REAL*8 VSUBXYZ(3,*)
      integer*2 nvel,lvelnam(4,*)
!
! LSINAM - Array of station names
! NVSITEV - Site coordinates after substitutions
! VSUBXYZ - Velocity coordinates from mod file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: flyby_init
!       CALLED SUBROUTINES: gvelp,dvelp
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NSITE,nsite2,i,j
!
! NSITE - Number of stations with alternate positions.
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   AEE   03/14/91  First version
!
! 5.  SVELP PROGRAM STRUCTURE
!
!  GET SUBSTITUTIONS
!
      do i=1,numsta
        do j=1,3
          vsubxyz(j,i) = 0.d0
        enddo
        do j=1,4
          lvelnam(j,i) = isitn(j,i)
        enddo
      enddo
      nvel=numsta
      CALL stectx(pltmod1,vsubxyz,time0,plate_fact)
!
!  DO SUBSTITUTIONS
!
      do i=1,numsta
        do j=1,3
          vsubxyz(j,i) = vsubxyz(j,i)*1000.d0
        enddo
      enddo
!
      RETURN
      END
