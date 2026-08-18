      SUBROUTINE AUTOA(ISITE,INTRVL,FAIL)
      IMPLICIT    NONE
!
! 1.  AUTOA PROGRAM SPECIFICATION
!
! 1.1 Implement automatic parameterization of atmospheres.
!
! 1.2 REFERENCES:
!
! 2.  AUTOA INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   ISITE,INTRVL
!
! INTRVL - Time interval between successive epochs
! ISITE - Site number of station to be parameterized
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2   FAIL
!
! FAIL - True if number of atmosphere parameters exceeds maximum
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: stflg
!       CALLED SUBROUTINES: obstm,epocs,teste,setep_sta
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   NEPOCS,I,IERR,JATM
      REAL*8  FJDOBS,LJDOBS,INDAY
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  AUTOA PROGRAM STRUCTURE
!
! Convert interval from minutes to days
!
      INDAY=DBLE(INTRVL)/1440.0D0
!
! Get the start and stop time of the observation
!
      CALL OBSTM(FJDOBS,LJDOBS)
!
! Determine number of epochs of length INTRVL in observation
!
      CALL EPOCS(FJDOBS,LJDOBS,INDAY,NEPOCS)
!
! Test for excessive number of atmosphere parameters
!
      CALL TESTE(NEPOCS,ISITE,NUMATM,NUMSTA,FAIL)
!
! Set up atmosphere parameters
!
      IF(.NOT.FAIL) CALL SETEP_STA(ISITE,FJDOBS,NEPOCS,INDAY)
!
      RETURN
      END
!
