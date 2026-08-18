      FUNCTION JRND_BLOCKS(WORDS)
      IMPLICIT NONE
!
! 1.  JRND_BLOCKS PROGRAM SPECIFICATION
!
! 1.1 Determine how many blocks are required to hold the
!     specified number of words.
!
! 1.2 REFERENCES:
!
! 2.  JRND_BLOCKS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! NOTE: BLOCK_WORDS, the number of words per block, is defined
!       as a parameter in solve.i
!
! 2.2 INPUT Variables:
!
      INTEGER*4 WORDS
!
! WORDS - Number of words
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 JRND_BLOCKS
!
! JRND_BLOCKS - Number of blocks required to hold WORDS
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
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  JRND_BLOCKS PROGRAM STRUCTURE
!
      JRND_BLOCKS=(WORDS+BLOCK_WORDS-1)/BLOCK_WORDS
!
      RETURN
      END
