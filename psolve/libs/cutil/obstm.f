      SUBROUTINE OBSTM(FJDOBS,LJDOBS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OBSTM PROGRAM SPECIFICATION
!
! 1.1 Get the start and stop times for an observation from NAMFIL for
!     the first session ion session list in scratch file. It return the
!     moment of the first and last observation regradless whether it was
!     selected or deselected.
!
! WARNING!!! WARNING!!! WARNING!!! WARNING!!! WARNING!!! WARNING!!!
!
!  OBSTM returns time interval recorded in LCODE "INTERVAL"
!
!        It contains UTC time tag rounded up to 1 minute!!!
!
! WARNING!!! WARNING!!! WARNING!!! WARNING!!! WARNING!!! WARNING!!!
!
!
! 1.2 REFERENCES:
!
! 2.  OBSTM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      REAL*8 FJDOBS,LJDOBS
!
! FJDOBS - Julian date at start of observation
! LJDOBS - Julian date at end of observation
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: getcard
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*70 JBUF
      INTEGER*2   IERR
      INTEGER*4   IOS
!
! IERR - Error return from GETCARD
! JBUF - Buffer for reading from NAMFIL
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  OBSTM PROGRAM STRUCTURE
!
! Read the required information from NAMFIL
!
      CALL OPENNAMFIL()
      CALL GETCARD ( INT2(1), 'SPAN', INT2(1), JBUF, IERR )
      READ ( JBUF, 1101, IOSTAT=IOS ) FJDOBS, LJDOBS
 1101 FORMAT ( 5X, 2F20.4 )
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS= ', IOS
           CALL FERR ( INT2(IOS), "OBSTM: Reading SPAN card: "//JBUF, &
     &                 INT2(0), INT2(0) )
      END IF
      CALL CLOSENAMFIL()
      RETURN
      END  !#!  OBSTM  #!#
