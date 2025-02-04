!FTN77,L,X
!$CDS ON
      SUBROUTINE RANDMAX(NUM_VALUES,VALUE_LIST,VTYPE,MAX_OR_MIN,JFOUND)
!
!     RANDMAX
!
      implicit none
!
! 1.  RANDMAX PROGRAM SPECIFICATION
!
! 1.1.   given a array of integer*4 values, RANDMAX finds the largest/smallest
!        (depending on the value of max_or_min), or if multiple array elements
!        have the maximum/minimum value, randmax selects one, pseudo-randomly.
!        Vtype permits the input list of values, value_list,
!        to be interpreted as various variable types (e.g., real*8).
!        (But see RESTRICTIONS, below.)
!
!
! 1.2.   REStRICTIONS -
!           Initially, max_or_min is a dummy argument.  Only a maximum value
!              will be located, regardless of the value of max_or_min.
!              (MX will be used for  maximum in the future, and this is
!              recommended for calling subroutines.)
!           Initially vtype is also a dummy argument.  The list is interpreted
!             as integer*4 regardless of vtype's value.  (I4 for integer*4
!             is recommended.)
!
!
! 1.3.   REFERENCES - none
!
! 2.  RANDMAX INTERFACE
!
! 2.1.   CALLING SEQUENCE:
!
!
!     INPUT VARIABLES:
!
!     NUM_VALUES = number of values in the list
!     VALUE_LIST - input array of integer*4 values.
!     VTYPE - the way to interpret VALUE_LIST (as integer*4 or other values).
!         Initially all VTYPE values will interpret the array as integer*4
!         values.  But I4 (interpret as integer*4) is recommended for the
!         future implementation of this option.
!     MAX_OR_MIN = whether a maximum or minimum value should be located.
!         Initially all MAX_OR_MIN values will search for a maximum value.
!         But MX (interpret as a maximum) is recommended for the future.
!
      INTEGER*4 NUM_VALUES,VALUE_LIST(*)
      CHARACTER*2 VTYPE, MAX_OR_MIN
!
!     OUTPUT VARIABLES:
!
!        JFOUND - POINTER TO MAXIMUM (OR MINIMUM) VALUE FOUND.
!                 (negative values indicate an error)
!
      INTEGER*4 JFOUND
!
! 2.2.   COMMON BLOCKS USED: none
!
! 2.3.   DATA BASE ACCESSES: none
!
! 2.4.   EXTERNAL INPUT/OUTPUT: none
!
! 2.5.   SUBROUTINE INTERFACE:
!
!     CALLING SUBROUTINES: utility
!
!     CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*2 LOCAL_MAX_MIN
      INTEGER*4 JCT,NUM_MATCH,JCHOOSE,JCOUNTM
      INTEGER*4 MAX_SO_FAR,PSEUDO_VAL,TIMENOW
!
! 4.  CONSTANTS USED:
!
! 5.  INITIALIZED VARIABLES:
!
! 6.  PROGRAMMER: K. Baver 5/18/98
!
!     LAST MODIFIED:
!
!     PROGRAM STRUCTURE
!
      LOCAL_MAX_MIN = 'MX'
      IF (LOCAL_MAX_MIN.EQ.'MX') THEN  !maximum is chosen
!
!       Find the maximum value in the list (not the position of the array
!       element(s) that contain it, just the actual value).
!
        MAX_SO_FAR  = VALUE_LIST(1)
        DO JCT = 2,NUM_VALUES
          IF (VALUE_LIST(JCT).GT.MAX_SO_FAR) THEN
            MAX_SO_FAR = VALUE_LIST(JCT)
          ENDIF
        ENDDO
      ENDIF
!
!     Now find the number of values equal to the maximum
!
      NUM_MATCH = 0
      DO JCT = 1,NUM_VALUES
        IF (VALUE_LIST(JCT).EQ.MAX_SO_FAR) THEN
          NUM_MATCH = NUM_MATCH + 1
        ENDIF
      ENDDO
!
!     Now select one of these values pseudo-randomly.
!     First get a pseudo-random value (time in seconds past 1/1/70).
!
      pseudo_val = timenow()
      pseudo_val = iabs(pseudo_val)
!
!     Now use it to pseudo-randomly choose the nth one in the list of
!     array elements that match.
!
      JCHOOSE= mod(pseudo_val,num_match) + 1
!
!     Find the pointer to that element within the entire input list.
!
      JCOUNTM = 0
      DO JCT = 1,NUM_VALUES
        IF (VALUE_LIST(JCT).EQ.MAX_SO_FAR) THEN
          JCOUNTM = JCOUNTM + 1
          IF (JCOUNTM .EQ. JCHOOSE) THEN
            JFOUND = JCT
          ENDIF
        ENDIF
      ENDDO
!
      RETURN
      END
