      SUBROUTINE DCDG(ICORD,ICRD,NDIG)
      implicit none
!
! 1.  DCDG PROGRAM SPECIFICATION
!
! 1.1 Calculate the decimal digits for a decimal integer.
!
! 1.2 REFERENCES:
!
! 2.  DCDG INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICORD,NDIG
!
! NDIG - Number of digits to be determined
! ICORD - Decimal integer to be processed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 ICRD(*)
!
! ICRD - Array to hold individual digits
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 KCORD,I,J,IFCT
!
! I - Loop index
! IFCT - Ten raised to appropriate power for calculation
! J - Power to which to raise ten
! KCORD - Intermediate value containing unextracted digits
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DCDG PROGRAM STRUCTURE
!
      KCORD = ICORD
      DO 100 I=1,NDIG
          J = NDIG-I
          IFCT = 10**J
          ICRD(I) = KCORD/IFCT
          KCORD = KCORD - IFCT*ICRD(I)
  100 CONTINUE
!
      END
