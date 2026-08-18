      SUBROUTINE FUNCHOUT(CSTRING,CLEN)
!
!     purpose: given an input string of a given length, replace any
!              control characters (e.g, carriage returns, bells) with blanks.
!
      implicit none
!
!     input
!
!     CLEN - length of string
!
!     input and output
!
!     cstring - string
!
      integer*2 CLEN
      character*(*) cstring
!
!     local variables
!
      CHARACTER*2 CNULL
      INTEGER*2 INULL,J
      EQUIVALENCE(INULL,CNULL)
!
      INULL = 0
      DO J = 1,CLEN
        CNULL(2:2) = CSTRING(J:J)
        IF (INULL .LE. 31 .OR. INULL.EQ.127) CSTRING(J:J) = ' '
      END DO
!
      return
      end
