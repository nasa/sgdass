      SUBROUTINE LVECT ( LSCRD, ISCNT, NSCNT )
      IMPLICIT NONE
!
! 1.  LVECT PROGRAM SPECIFICATION
!
! 1.1 Set up array of site coordinate parameter numbers for each
!      component of each station.
!
! 1.2 REFERENCES:
!
! 2.  LVECT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISCNT(3,MAX_STA_CMP),NSCNT
!
! ISCNT - Array containing station numbers, station components and corresponding
!           parameter numbers, not necessarily in proper order
! NSCNT - Total number of site coordinate parameters
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSCRD(3,MAX_STA)
!
! LSCRD - Array of site coordinate parameter numbers for each component
!          of each station
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: reada
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5. LVECT PROGRAM STRUCTURE
!
      DO I=1,3
         DO J=1,MAX_STA
            LSCRD(I,J)=0
         ENDDO
      ENDDO
!
      DO J=1,NSCNT
         LSCRD ( ISCNT(3,J), ISCNT(2,J) ) = ISCNT(1,J)
      ENDDO
!
      RETURN
      END  !#!  LVECT  #!#
