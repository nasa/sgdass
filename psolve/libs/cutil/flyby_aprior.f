      SUBROUTINE FLYBY_APRIOR()
      IMPLICIT NONE
!
! 1.  FLYBY_APRIOR PROGRAM SPECIFICATION
!
! 1.1 Modify the common block PARFL to agree with the current
!     flyby a prioris. Care must be taken not to write PARFL
!     out to disk after calling this routine, except in ADDER
!     where it should be written to the CGMFIL or output CGM.
!
! 1.2 REFERENCES:
!
! 2.  FLYBY_APRIOR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: dcopy
!
! 3.  LOCAL VARIABLES
!
      integer*4 iblas1,nblas
      integer*2 i
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FLYBY_APRIOR PROGRAM STRUCTURE
!
      IBLAS1 = 1
      IF ( FL_NOFLYBY ) RETURN 
!
! --- Axis offsets
!
      DO I=1,NUMSTA
         VAXOF(I) = VAXOF(I) + AXDIF(I)
      ENDDO
!
! --- Stations positions
!
      IF ( KSTAP ) THEN
           NBLAS=3*NUMSTA
           CALL DCOPY ( NBLAS, NVSITEC(1,1), IBLAS1, VSITEC(1,1), IBLAS1 )
      ENDIF
!
! --- Stations velocities
!
      IF ( KSVEL ) THEN
           NBLAS=3*NUMSTA
           CALL DCOPY ( NBLAS, NVSITEV(1,1), IBLAS1, VSITEV(1,1), IBLAS1 )
      ENDIF
!
! --- Source positions
!
      IF ( KSOUC ) THEN
           NBLAS=2*NUMSTR
           CALL DCOPY ( NBLAS, NVSTARC(1,1), IBLAS1, VSTARC(1,1), IBLAS1 )
      ENDIF
!
! --- Nutation  series and precession
!
      IF ( KNUTS ) THEN
           VNUT(1,1) = DPRIN(1)
           VNUT(2,1) = DPRIN(2)
           VNUT(1,2) = DDECA(1)
           VNUT(2,2) = DDECA(2)
           VNUT(1,3) = DANNU(1)
           VNUT(2,3) = DANNU(2)
           VNUT(1,4) = DSEMA(1)
           VNUT(2,4) = DSEMA(2)
           VNUT(1,5) = D122D(1)
           VNUT(2,5) = D122D(2)
           VNUT(1,6) = DSEMM(1)
           VNUT(2,6) = DSEMM(2)
!
           VNUTOP(1,1) = DPRIN(3)
           VNUTOP(2,1) = DPRIN(4)
           VNUTOP(1,2) = DDECA(3)
           VNUTOP(2,2) = DDECA(4)
           VNUTOP(1,3) = DANNU(3)
           VNUTOP(2,3) = DANNU(4)
           VNUTOP(1,4) = DSEMA(3)
           VNUTOP(2,4) = DSEMA(4)
           VNUTOP(1,5) = D122D(3)
           VNUTOP(2,5) = D122D(4)
           VNUTOP(1,6) = DSEMM(3)
           VNUTOP(2,6) = DSEMM(4)
!
           VPREC=NVPREC
      ENDIF
!
      RETURN
      END  !#!  FLYBY_APRIOR  #!#
