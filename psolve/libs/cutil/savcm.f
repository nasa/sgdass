      SUBROUTINE SAVCM()
      IMPLICIT NONE
!
! 1.  SAVCM PROGRAM SPECIFICATION
!
! 1.1 copy all the corresponding things out of common.
!
! 1.2 REFERENCES:
!
! 2.  SAVCM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'q_socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'q_prfil.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: lists
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 IDUM4
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   920204 Removed hard coded path for fclib.i
!
! 5.  SAVCM PROGRAM STRUCTURE
!
      IDUM4=FC_MEMCPY(ptr_nc(Q_IPARFIL),ptr_nc(IPARFIL),JPARFIL_BYTES)
      IDUM4=FC_MEMCPY(ptr_nc(Q_ISOCOM),ptr_nc(ISOCOM),JSOCOM_BYTES)
!
      RETURN
      END
