      FUNCTION IROTF(IEPOCH,ITYP,IPOS,LROT)
      implicit none
!
! 1.  IROTF PROGRAM SPECIFICATION
!
! 1.1 Flip the status of an earth rotation flag for a given
!     epoch number, type and polynomial order. (IPOS=1 means
!     0th order,etc.)
!
! 1.2 REFERENCES:
!
! 2.  IROTF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IEPOCH,ITYP,IPOS
!
! IEPOCH - The requested epoch
! IPOS - Polynomial order
! ITYP - The requested type
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LROT(ROT_BIT_WORDS,3),IROTF
!
! IROTF - Return value from KFLIP
! LROT - Earth rotation flags
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: kflip
!
! 3.  LOCAL VARIABLES
!
      integer*2 kflip,index,igroup,ibitf
!
! INDEX - First index of LROT for the given epoch
! IGROUP - Group that the given epoch is assigned to (1-4)
! IBITF - Bit position to be flipped
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  840302  Created
!
! 5.  IROTF PROGRAM STRUCTURE
!
!         COMPUTE THE FIRST INDEX OF 'LROT'
!
      INDEX  = (IEPOCH-1)/4 + 1
!
!         COMPUTE THE GROUP THE EPOCH IS ASSIGNED TO (MUST BE 1 - 4)
!
      IGROUP = IEPOCH - (INDEX - 1)*4
!
!         COMPUTE THE BIT POSITION
!
      IBITF  = (IGROUP-1)*4 + IPOS
!
!         NOW FLIP THE BIT
!
      IROTF = KFLIP(LROT(INDEX,ITYP),IBITF)
!
      RETURN
      END
