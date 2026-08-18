      SUBROUTINE ROTFL(IY,IX,LROT,EOSTYLE,IDSP,JERR)
      IMPLICIT NONE
!
! 1.  ROTFL PROGRAM SPECIFICATION
!
! 1.1 Flip earth rotation flags starting from screen coordinates.
!     Pack the earth rotation bits into the array LROT.  The second
!     dimension, 3, refers to x-pole, y-pole and UT1.  The first
!     dimension merely sets up a long buffer of bits.  For a given
!     type, assign 4 bits for the 0th through third order.  The words
!     are filled from the right.
!
! 1.2 REFERENCES:
!
! 2.  ROTFL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 EOSTYLE(2)
      INTEGER*4 IX,IY
!
! IX,IY - Screen coordinates from which to start
! EOSTYLE - Flag designating either old (0) or new (1) style of
!           earth orientation parameterization
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LROT(25,3), IDSP, JERR
!
! IDSP - New value of flipped bit
! JERR - Error return ( not 0 means error)
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflg,six,parxc
!       CALLED SUBROUTINES: irotf
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IPOS, ITYP, IEPOCH, IROT, &
     & IROTF, IFIRST_LINE(3),ILAST_LINE(3),IEOP
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  840301  Created
!   KDB  9010??  Modified for new style EOP
!
! 5.  ROTFL PROGRAM STRUCTURE
!
      JERR = 0
      CALL EOYFL(IFIRST_LINE,ILAST_LINE)
!
!     Compute the type of parameter (x-wobble, y-wobble, UT1)
!
      ITYP = 0
      DO IEOP = 1,3
       IF(IY .GE. IFIRST_LINE(IEOP) .and. &
     &    IY .LE. ILAST_LINE(IEOP)  ) ITYP = IEOP
      END DO
!
      IF (ITYP .EQ. 0) THEN !not one of the eop lines
        JERR = 1
        RETURN
      END IF
!
!     make sure that it really is old style
!
      IF (ITYP .LE. 2) THEN
        IEOP = 1
      ELSE
        IEOP = 2
      END IF
      IF (EOSTYLE(IEOP) .NE.0) THEN
        JERR = 1
        RETURN
      END IF
!
!     Compute the number of the epoch
!
      IEPOCH = IY - IFIRST_LINE(ITYP) + 1
!
!     Compute the order of the parameter to be flipped
!
      IF (IX .EQ. 35) THEN
         IPOS = 1
      ELSE IF (IX .EQ. 37) THEN
         IPOS = 2
      ELSE IF (IX .EQ. 39) THEN
         IPOS = 3
      ELSE IF (IX .EQ. 41) THEN
         IPOS = 4
      ELSE  !error exit
       JERR = 1
       RETURN
      END IF  !error exit
!
!     FLIP IT
!
      IDSP = IROTF(IEPOCH,ITYP,IPOS,LROT)
!
      RETURN
      END
