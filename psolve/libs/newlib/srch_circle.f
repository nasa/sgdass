      SUBROUTINE SRCH_CIRCLE(TARGET,TARG_LEN,BUFF_ARRAY,NUM_BUFF,IFIELD, &
     &                       IPT_START,IFOUND)
!
      IMPLICIT NONE
!
! 1.  SRCH_CIRCLE
!
! 1.1.   SRCH_CIRCLE searches an array of character strings
!        for the occurrence of a target
!        string.  If the target is found, the next search begins at the next
!        element.  If the target is not found and the end of the array is
!        reached, the search circles back to the beginning of the array until
!        the target is found or the starting point is reached.  This type
!        of search is useful when there is a series of somewhat sorted input
!        target strings and the array is somewhat sorted by the same criteria.
!
!        Either the entire array element or a field may be matched.
!
!     INPUT VARIABLES:
!
      CHARACTER*(*) TARGET, BUFF_ARRAY(*)
      INTEGER*2 TARG_LEN,IPT_START,NUM_BUFF,IFIELD
!
!     TARGET - string for which the search is being made
!     TARG_LEN - length of input target string
!     BUFF_ARRAY - array to be searched
!     NUM_BUFF - number of entries
!     IFIELD - field to be matched OR zero to match entire line
!     IPT_START - pointer to element at which the search should start
!          (This must be explicitly passed in, rather than internally
!           saved, so that the caller can
!           alternate between searching multiple arrays.)
!
!     OUTPUT VARIABLES:
!
      INTEGER*2 IFOUND
!
!     IFOUND - pointer to array element where the target is located
!              (or 0 to indicate the target wasn't found)
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IPASS,IPT,ILIMIT,ICT,TEST_LEN,TRIMLEN
      LOGICAL*2 LOOP
      CHARACTER*256 WORK_BUFFER,TEST_FIELD
!
! 6.  PROGRAMMER: K. BAVER  980402
!     LAST MODIFIED:
!
!     PROGRAM STRUCTURE
!
!     initialize
!
      IFOUND = 0
      IPASS = 0
      LOOP = .TRUE.
!
!     Search the array in up to two sections.  First try the elements after the
!     starting point.  If that fails, go back to the start and try the elements
!     before the starting point.
!
      DO WHILE (LOOP)
        IPASS = IPASS + 1
        IF (IPASS.EQ.1) THEN
!
!       Search the sinex array from the last slot where something was
!       found plus one, until the final slot
!
          IPT = IPT_START + 1
          ILIMIT = NUM_BUFF
        ELSE
!
!         If no success, search the array from the first slot to the
!         last slot where something was found
!
          IPT = 1
          ILIMIT = IPT_START
        ENDIF
!       Run through the current section of the array.
        DO WHILE (IPT .LE. ILIMIT.AND.IFOUND.EQ.0)
          WORK_BUFFER = BUFF_ARRAY(IPT)
          IF (IFIELD.GT.0) THEN
            DO ICT = 1,IFIELD
              CALL SPLITSTRING(WORK_BUFFER,TEST_FIELD,WORK_BUFFER)
            ENDDO
          ELSE
            TEST_FIELD = WORK_BUFFER
          ENDIF
          TEST_LEN = TRIMLEN(TEST_FIELD)
          IF ( (TARG_LEN.EQ.TEST_LEN)  .AND. &
     &        (TARGET(1:TARG_LEN).EQ.TEST_FIELD(1:TEST_LEN))) THEN
            IFOUND = IPT
          ELSE
            IPT = IPT + 1
          ENDIF
        ENDDO
        IF (IFOUND.NE.0.OR.IPASS.EQ.2) LOOP = .FALSE.
      END DO
!
!     Move the starting point for next time
!
      IF (IFOUND.NE.0) THEN
        IPT_START = IFOUND
      ENDIF
!
      RETURN
      END
