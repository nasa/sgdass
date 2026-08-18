      SUBROUTINE toggle_eop_poly_flags(IY,IX,LROT,IDSP,redo_page)
      IMPLICIT NONE
!
! 1.1 Flip earth rotation flags starting from screen coordinates.
!     Pack the earth rotation bits into the array LROT.  The second
!     dimension, 3, refers to x-pole, y-pole and UT1.  The first
!     dimension merely sets up a long buffer of bits.  For a given
!     type, assign 4 bits for the 0th through third order.  The words
!     are filled from the right.
!
!
      INTEGER*4 IX,IY
!
! IX,IY - Screen coordinates from which to start
!
      INTEGER*2 LROT(25,3), IDSP, JERR
!
! IDSP - New value of flipped bit
!
      INTEGER*2 IPOS, ITYP, IEPOCH, IROT, &
     & IROTF, IFIRST_LINE(3),ILAST_LINE(3),IEOP
      logical*2 redo_page
      character*2 buf2
      integer*2   int2
      equivalence (buf2,int2)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  840301  Created
!   KDB  9010??  Modified for new style EOP
!
! 5.  ROTFL PROGRAM STRUCTURE
!
!     Compute the type of parameter (x-wobble, y-wobble, UT1)
!
      redo_page = .false.
!
!
      IF (IY .lt. 1  .or. IY.gt.3 )THEN !not one of the eop lines
        RETURN
      END IF
!
      IEPOCH = 1
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
       RETURN
      END IF  !error exit
!
!     Flip it!
      ITYP = IY
      IDSP = IROTF(IEPOCH,ITYP,IPOS,LROT)
      call setcr_mn(IX,IY)
      int2 = 0
      buf2(1:1) = ' '
      If(idsp .eq. 1) buf2 ='1'
      CALL WRITE_SCREEN(BUF2)
      redo_page = .true.
!
      RETURN
      END
