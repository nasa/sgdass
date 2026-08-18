      SUBROUTINE VAR_NUMGET(INBUF,NUM_ASKED,OARRAY,NUM_GOTTEN,KERR)
!
      IMPLICIT NONE
!
!     Purpose: decodes the input buffer INBUF into up to NUM_ASKED numbers,
!        which are returned in the first elements of OARRAY.
!     Written:  12/13/93 by KDB, based on var_intput.
!
!     Input variables
!
!     INBUF - input character buffer to be decoded
!     NUM_ASKED - number of numbers to get
!
      CHARACTER*(*) INBUF
      INTEGER*2 NUM_ASKED
!
!     Output variables
!
!     OARRAY - the numbers decoded from inbuf, one per element.
!     NUM_GOTTEN - number of numbers found
!     KERR - error return
!                 0 - success (user requested n numbers and there were exactly
!                      n in the input buffer)
!                 1 - input buffer contained fewer numbers than requested
!                 2 - found all the requested numbers, but also some extra
!                     input
!                -n - input field n was not an number.
!
      REAL*4 OARRAY(*)
      INTEGER*2 NUM_GOTTEN,KERR
!
!     Local variables
!
      REAL*4 RTEMP
      INTEGER*2 ICT,TRIMLEN
      INTEGER*4  IERR
      CHARACTER*80 NEXT_FIELD
!
      NUM_GOTTEN = 0
      KERR = 1000
      ICT = 1
!
!     Look for a new number, until the user's request has been filled,
!     or the buffer has been emptied or some error occurs.
!
      DO WHILE (ICT.LE.NUM_ASKED .AND. KERR.EQ.1000)
        CALL SPLITSTRING(INBUF,NEXT_FIELD,INBUF)
        IF (TRIMLEN(NEXT_FIELD).EQ.0) THEN
!         No more fields left in the input buffer.
!         The user found fewer numbers than he wanted.
          KERR = 1
        ELSE
!         Found SOMETHING in the buffer.  Decode it.
          READ(NEXT_FIELD,*,IOSTAT=IERR,ERR=150) RTEMP
 150      IF (IERR.NE.0) THEN
!           Error - not an number.
            KERR = -1 * ICT
          ELSE
!           Got another number.
            OARRAY(ICT) = RTEMP
            NUM_GOTTEN = NUM_GOTTEN + 1
            ICT = ICT + 1
          END IF
        END IF
      END DO
!
!     If the request was apparently successful, and the user got all the
!     numbers he wanted, do one final check to make sure there were not
!     extra numbers in the input buffer.
!
      IF (KERR.EQ.1000) THEN
        CALL SPLITSTRING(INBUF,NEXT_FIELD,INBUF)
        IF (TRIMLEN(NEXT_FIELD).GT.0) THEN
!         Warn the user that there was extra input in the buffer
          KERR = 2
        ELSE
!         The user got exactly the input he wanted.
          KERR = 0
        END IF
      END IF
!
      RETURN
      END
