      FUNCTION IROTT(IEPOCH,ITYP,IPOS,LROT)
      implicit none
!
! 1.  IROTT PROGRAM SPECIFICATION
!
! 1.1 Test the status of an earth rotation flag for a given
!     epoch number, type, and polynamial order. (IPOS=1 means
!     0th order,etc.)
!
! 1.2 REFERENCES:
!
! 2.  IROTT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IEPOCH,ITYP,IPOS,LROT(ROT_BIT_WORDS,3)
!
! IEPOCH - The requested epoch number
! IPOS - Polynomial order
! ITYP - The requested type
! LROT - The earth rotation flags
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IROTT
!
! IROTT - 1 if the specified flag bit is set, 0 if not
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: kbit
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 KBIT
      INTEGER*2 INDEX,IGROUP,IBITF,n
!
! IBITF - Bit position to be tested
! INDEX - Starting index for specified epoch in LROT
! IGROUP - Group which epoch is assigned to (1-4)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  840302  Created
!
! 5.  IROTT PROGRAM STRUCTURE
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
!         NOW SENSE THE BIT
!
      IROTT = 0
        n=0
      IF(KBIT(LROT(INDEX,ITYP),IBITF)) IROTT = 1
!
      RETURN
      END
